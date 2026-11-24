# bike_functions.R
# Functions for estimating and simulating bike-share arrivals.
# Each origin-destination pair is modeled as an independent
# non-homogeneous Poisson process (NHPP) whose rate is constant
# within each hour of the day and repeats every day.

library(tidyverse)
library(testthat)

#' Build the inventory event table from trip data
#'
#' @param trips A data frame of trips with columns `row_id`, `start_station`,
#'   `end_station`, `start_time`, and `end_time`.
#'   
#' @return A data frame with one row per inventory change and columns
#'   `station`, `time`, `inventory_change`, `event_type`, `priority`, and
#'   `row_id`, sorted by time, tie priority, then original row.
#'   
make_events <- function(trips) {
  r_add <- trips |> filter(start_station == "R") |>
    transmute(station = end_station, time = end_time,
              inventory_change = 1, event_type = "R", priority = 1, row_id)
  r_rem <- trips |> filter(end_station == "R") |>
    transmute(station = start_station, time = start_time,
              inventory_change = -1, event_type = "R", priority = 4, row_id)
  cust  <- trips |> filter(start_station != "R", end_station != "R")
  c_dep <- cust |>
    transmute(station = start_station, time = start_time,
              inventory_change = -1, event_type = "C", priority = 2, row_id)
  c_arr <- cust |>
    transmute(station = end_station, time = end_time,
              inventory_change = 1, event_type = "C", priority = 3, row_id)
  
  bind_rows(r_add, c_dep, c_arr, r_rem) |>
    arrange(time, priority, row_id)
}

#' Estimate hourly NHPP arrival rates for each origin-destination pair
#'
#' @description Reconstructs station inventories from all trips and rebalancing events,
#' measures the fraction of each hour of the day with at least one bike at
#' each origin, and adjusts observed customer departures for that
#' availability: rate = observed_rate / availability.
#'
#' Input checks: `data` must be a data frame with columns `start_station`,
#' `end_station` (character), `start_time`, and `end_time` (date-times)
#' and no missing values, with no trip ending before it starts. The window
#' must be two single date-times with `window_end` after `window_start`,
#' covering a whole number of days. Any failed check stops with an error.
#' Rows are assumed to be in their original order (used to break ties).
#' Window times are read in the same time zone as the trip times.
#'
#' @param data Trip data, one row per trip or rebalancing move. A start
#'   station of "R" adds a bike; an end station of "R" removes one.
#' @param window_start Start of the observation window (included), a
#'   date-time at midnight.
#' @param window_end End of the observation window (excluded), a date-time
#'   a whole number of days after `window_start`.
#'   
#' @return A tibble with one row per origin, destination, and hour (0-23):
#'   `origin` and `destination` (character), `hour` (integer),
#'   `observed_rate` (customer departures per day in that hour),
#'   `availability` (fraction of that hour's observation time with at least
#'   one bike at the origin, between 0 and 1), and `rate` (estimated
#'   requests per hour; NA when availability is 0).
#'   
estimate_arrival_rates <- function(data, window_start, window_end) {
  # ---- Input checks ----
  needed <- c("start_station", "end_station", "start_time", "end_time")
  if (!is.data.frame(data) || !all(needed %in% names(data))) {
    stop("data must be a data frame with columns ",
         paste(needed, collapse = ", "))
  }
  if (!is.character(data$start_station) || !is.character(data$end_station)) {
    stop("start_station and end_station must be character")
  }
  if (!is.POSIXct(data$start_time) || !is.POSIXct(data$end_time)) {
    stop("start_time and end_time must be date-times (POSIXct)")
  }
  if (anyNA(data[needed])) {
    stop("data must not contain missing values in ",
         paste(needed, collapse = ", "))
  }
  if (any(data$end_time < data$start_time)) {
    stop("every end_time must be at or after its start_time")
  }
  if (!is.POSIXct(window_start) || length(window_start) != 1 ||
      !is.POSIXct(window_end) || length(window_end) != 1 ||
      is.na(window_start) || is.na(window_end)) {
    stop("window_start and window_end must be single date-times")
  }
  if (window_end <= window_start) {
    stop("window_end must be after window_start")
  }
  n_days <- as.numeric(difftime(window_end, window_start, units = "days"))
  if (n_days != round(n_days)) {
    stop("the window must cover a whole number of days")
  }
  
  # Use the trip data's time zone so hours line up
  data_tz      <- tz(data$start_time)
  window_start <- with_tz(window_start, data_tz)
  window_end   <- with_tz(window_end, data_tz)
  
  trips <- data |> mutate(row_id = row_number())
  
  # Step 1: inventory after each event
  # Events are sorted, so a running sum within each station gives the same
  # counts as the Week 10 loop.
  events <- make_events(trips) |>
    group_by(station) |>
    mutate(inventory = cumsum(inventory_change),
           next_time = lead(time, default = window_end)) |>
    ungroup()
  
  # Step 2: availability by station and hour
  # Each event starts an interval [time, next_time) at its new inventory.
  available <- events |>
    filter(inventory > 0, next_time > time)
  
  # One row per hour of the window
  hour_bins <- tibble(bin_start = seq(window_start, by = "hour",
                                      length.out = 24 * n_days)) |>
    mutate(bin_end = bin_start + hours(1))
  
  # Seconds of each hour with at least one bike at each station
  avail_secs <- available |>
    inner_join(hour_bins,
               by = join_by(time < bin_end, next_time > bin_start)) |>
    mutate(secs = as.numeric(difftime(pmin(next_time, bin_end),
                                      pmax(time, bin_start),
                                      units = "secs")),
           hour = hour(bin_start)) |>
    group_by(station, hour) |>
    summarise(avail_secs = sum(secs), .groups = "drop")
  
  # Step 3: observed customer departures in the window
  customer <- trips |>
    filter(start_station != "R", end_station != "R",
           start_time >= window_start, start_time < window_end)
  
  departures <- customer |>
    mutate(hour = hour(start_time)) |>
    count(origin = start_station, destination = end_station, hour)
  
  # Step 4: combine on a full origin x destination x hour grid
  stations <- sort(unique(c(trips$start_station, trips$end_station)))
  stations <- stations[stations != "R"]
  
  expand_grid(origin = stations, destination = stations, hour = 0:23) |>
    left_join(departures, by = c("origin", "destination", "hour")) |>
    left_join(avail_secs, by = c("origin" = "station", "hour")) |>
    mutate(n          = replace_na(n, 0),
           avail_secs = replace_na(avail_secs, 0),
           observed_rate = n / n_days,
           availability  = avail_secs / (n_days * 3600),
           rate = if_else(availability > 0, observed_rate / availability,
                          NA_real_)) |>
    select(origin, destination, hour, observed_rate, availability, rate)
}

#' Simulate customer requests from hourly NHPP rates using thinning
#'
#' @description Each origin-destination pair is simulated independently. For a pair with
#' maximum hourly rate M, candidate times are generated from a homogeneous
#' Poisson process with rate M (exponential waiting times with mean 1/M
#' hours). A candidate at time u is kept with probability rate(h(u)) / M,
#' where h(u) is the hour of the day of u.
#'
#' Input checks: `rates` must be a data frame with columns `origin`,
#' `destination`, `hour`, and `rate`, with hours 0-23 appearing exactly once
#' for each pair and rates that are nonnegative or NA. `start_time` and
#' `end_time` must be single date-times with `end_time` not before
#' `start_time`. Any failed check stops with an error. An NA rate (no bike
#' availability, so demand could not be estimated) is treated as 0, so no
#' requests are simulated in that hour.
#'
#' @param rates Rate table from `estimate_arrival_rates()`. `rate` is in
#'   requests per hour.
#' @param start_time Start of the simulation window (included), a date-time.
#' @param end_time End of the simulation window (excluded), a date-time.
#' 
#' @return A tibble with columns `origin` (character), `destination`
#'   (character), and `arrival_time` (date-time, in the time zone of
#'   `start_time`), one row per simulated request, sorted by arrival time.
#'   If there are no arrivals, a tibble with these columns and zero rows.
#'   
simulate_arrivals <- function(rates, start_time, end_time) {
  # ---- Input checks ----
  needed <- c("origin", "destination", "hour", "rate")
  if (!is.data.frame(rates) || !all(needed %in% names(rates))) {
    stop("rates must be a data frame with columns ",
         paste(needed, collapse = ", "))
  }
  if (!is.numeric(rates$rate) || any(rates$rate < 0, na.rm = TRUE)) {
    stop("rate must be numeric and nonnegative (or NA)")
  }
  hours_ok <- rates |>
    group_by(origin, destination) |>
    summarise(ok = length(hour) == 24 && setequal(hour, 0:23),
              .groups = "drop")
  if (!all(hours_ok$ok)) {
    stop("each origin-destination pair must have hours 0-23 exactly once")
  }
  if (!is.POSIXct(start_time) || length(start_time) != 1 ||
      !is.POSIXct(end_time) || length(end_time) != 1 ||
      is.na(start_time) || is.na(end_time)) {
    stop("start_time and end_time must be single date-times")
  }
  if (end_time < start_time) {
    stop("end_time must not be before start_time")
  }
  
  # Correctly structured empty table
  arrivals <- tibble(origin       = character(),
                     destination  = character(),
                     arrival_time = with_tz(as.POSIXct(character()),
                                            tz(start_time)))
  
  # NA rates (no availability) are treated as zero
  rates <- rates |>
    mutate(rate = replace_na(rate, 0)) |>
    arrange(origin, destination, hour)
  
  pairs   <- distinct(rates, origin, destination)
  results <- vector("list", nrow(pairs))
  
  for (k in seq_len(nrow(pairs))) {
    o <- pairs$origin[k]
    d <- pairs$destination[k]
    hourly_rate <- rates$rate[rates$origin == o & rates$destination == d]
    M <- max(hourly_rate)
    if (M == 0) next    # no arrivals for this pair
    
    # Candidates from a homogeneous Poisson process with rate M
    candidates     <- c()
    candidate_time <- start_time
    repeat {
      wait_hours     <- rexp(1, rate = M)
      candidate_time <- candidate_time + wait_hours * 3600
      if (candidate_time >= end_time) break
      candidates <- c(candidates, candidate_time)
    }
    if (length(candidates) == 0) next
    candidates <- as.POSIXct(candidates, origin = "1970-01-01",
                             tz = tz(start_time))
    
    # Thinning: keep each candidate with probability rate(hour) / M
    accept_prob <- hourly_rate[hour(candidates) + 1] / M
    keep <- runif(length(candidates)) < accept_prob
    
    results[[k]] <- tibble(origin       = o,
                           destination  = d,
                           arrival_time = candidates[keep])
  }
  
  bind_rows(arrivals, results) |>
    arrange(arrival_time)
}