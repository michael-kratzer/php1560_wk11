# bike_functions.R
# Functions for estimating and simulating bike-share arrivals.
# Each origin-destination pair is modeled as an independent
# non-homogeneous Poisson process (NHPP) whose rate is constant
# within each hour of the day and repeats every day.

library(tidyverse)
library(testthat)

# estimate_arrival_rates(): estimate hourly arrival rates for each
# origin-destination pair from observed trips and bike availability.


# simulate_arrivals(): simulate user arrival times using the
# estimated hourly rates.
