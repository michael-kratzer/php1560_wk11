# Bike-share Arrival Simulation

This repository models when users arrive at bike-share stations to
request trips. Requests for each origin-destination pair follow an
independent non-homogeneous Poisson process (NHPP) whose rate is
constant within each hour of the day and repeats each day.

## Functions (in bike_functions.R)

- estimate_arrival_rates(): estimates the hourly arrival rate for each
  origin-destination pair from observed trips and bike availability.
- simulate_arrivals(): uses the estimated hourly rates to simulate
  arrival times of user requests.

## How to run an example

1. Open R in this folder (or set it as the working directory).
2. Load the functions:

       source("bike_functions.R")

3. Estimate rates, then simulate arrivals:

       rates <- estimate_arrival_rates(trips, availability)
       arrivals <- simulate_arrivals(rates)

   Here trips and availability are the bike-share data frames.

## Requirements

R with the tidyverse and testthat packages.