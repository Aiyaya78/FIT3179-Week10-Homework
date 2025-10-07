# ---- Load libraries ----
library(tidyverse)
library(janitor)

# ---- Clear environment ----
rm(list = ls())

# ---------- GE15 ----------
ge15_raw <- read_csv("MALAYSIA_2022_PARLIAMENT_RESULTS.csv", show_col_types = FALSE) |>
  clean_names()
# Expect columns like: state, total_electorate, total_ballots_issued, `turnout_%` -> becomes "turnout"

ge15_state <- ge15_raw |>
  # keep only what we need
  select(state, total_electorate, total_ballots_issued) |>
  # aggregate to state
  group_by(state) |>
  summarise(
    registered_voters = sum(total_electorate, na.rm = TRUE),
    voted             = sum(total_ballots_issued, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    turnout_percent = round(100 * voted / registered_voters, 2),
    # make state names nice case for joining/legends later
    state = str_to_title(state)
  ) |>
  arrange(state)

write_csv(ge15_state, "state_turnout_ge15.csv")

# ---------- GE14 ----------
ge14_raw <- read_csv("MALAYSIA_2018_PARLIAMENT_RESULTS.csv", show_col_types = FALSE) |>
  clean_names()

ge14_state <- ge14_raw |>
  select(state, total_electorate, total_ballot_issued) |>
  group_by(state) |>
  summarise(
    registered_voters = sum(total_electorate, na.rm = TRUE),
    voted             = sum(total_ballot_issued, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    turnout_percent = round(100 * voted / registered_voters, 2),
    state = str_to_title(state)
  ) |>
  arrange(state)

write_csv(ge14_state, "state_turnout_ge14.csv")
