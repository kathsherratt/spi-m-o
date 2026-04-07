# Process R and growth rate data 
#
# Raw data available at: "https://assets.publishing.service.gov.uk/media/63a46e3ed3bf7f375c7d833d/221221_R_and_growth_rate_time_series_for_publication_v1.0.ods"
# Raw data are saved to .xlsx to preserve formatting, with initial data cleaning to csv
# Values with caveats were marked in red font colour, identified manually in initial data cleaning
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Tidy into long format with final schema:
# date, region, value_lower, value_upper, value_type, value_caveat

process_r_gr <- function() {
    data <- read_csv(here("data", "r-growth-values", "r-growth-rate.csv"))

# Pivot longer on metric, region, bound
long_data <- data |>
  pivot_longer(
    cols = -c(date, caveat_date),
    names_pattern = "^(R|GR|caveat)_(.+)_(Lower|Upper)$",
    names_to = c("value_type", "region", "bound"),
    values_to = "value"
  )

# Extract "reliability indicator"
reliability <- long_data |>
  filter(value_type == "caveat") |>
  select(date, region, value_unreliable = value) |>
  mutate(value_unreliable = as.logical(value_unreliable))

# Process R and GR values
r_gr_values <- long_data |>
  filter(value_type != "caveat") |>
  pivot_wider(
    names_from = bound,
    values_from = value,
    names_prefix = "value_"
  ) |>
  mutate(
    value_type = if_else(value_type == "R", "R", "growth rate")
  ) |>
  left_join(reliability, by = c("date", "region"),relationship = "many-to-many") |>
  mutate(date = dmy(date)) |>
  rename_with(tolower) |> 
  select(date, region, value_lower, value_upper, value_type, value_unreliable)

return(r_gr_values)
}