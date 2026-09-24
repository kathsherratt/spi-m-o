# Estimate COVID-19 Rt for England using EpiNow2
# Date range: 2021-01-20 to 2021-03-01

library(EpiNow2)
library(readr)
library(dplyr)
library(ggplot2)

cat("=======================================================\n")
cat("EpiNow2 Rt Estimation for England\n")
cat("Target period: 2021-01-20 to 2021-03-01\n")
cat("=======================================================\n\n")

# 1. Load cases data
cases_file <- "england_cases.csv"
if (!file.exists(cases_file)) {
  stop("File england_cases.csv not found in working directory.")
}

cases_raw <- read_csv(cases_file, show_col_types = FALSE)
cases_raw$date <- as.Date(cases_raw$date)

# 2. Filter to requested period
target_start <- as.Date("2021-01-20")
target_end <- as.Date("2021-03-01")

reported_cases <- cases_raw |>
  filter(date >= target_start & date <= target_end) |>
  select(date, confirm)

cat("Data summary:\n")
cat("  Observations:", nrow(reported_cases), "days\n")
cat("  Start date:  ", as.character(min(reported_cases$date)), "\n")
cat("  End date:    ", as.character(max(reported_cases$date)), "\n")
cat("  Total cases: ", sum(reported_cases$confirm), "\n\n")

# 3. Model setup
# Standard COVID-19 literature distributions provided by EpiNow2:
# Generation time: Ganyani et al.
gt <- gt_opts(example_generation_time)

# Delays: incubation period (Lauer et al.) + reporting delay
delays <- delay_opts(example_incubation_period + example_reporting_delay)

# Prior on initial Rt
rt <- rt_opts(prior = LogNormal(mean = 1, sd = 1))

# Observation model with negative binomial error & day-of-week effect
obs <- obs_opts(family = "negbin", week_effect = TRUE)

# Stan options: 4 chains in parallel, 1000 samples, 250 warmup, adapt_delta = 0.95
stan <- stan_opts(
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  warmup = 250,
  samples = 1000,
  control = list(adapt_delta = 0.95)
)

cat("Running EpiNow2 model fit (4 chains, adapt_delta = 0.95)...\n")
start_time <- Sys.time()

estimates <- epinow(
  data = reported_cases,
  generation_time = gt,
  delays = delays,
  rt = rt,
  obs = obs,
  stan = stan,
  logs = NULL,
  verbose = FALSE
)

end_time <- Sys.time()
cat(sprintf("Model fitting complete in %.1f seconds.\n\n", as.numeric(difftime(end_time, start_time, units = "secs"))))

# 4. Extract Rt parameter estimates
r_estimates <- summary(estimates, type = "parameters", params = "R")

# Filter to the requested date window (excluding out-of-sample forecast days)
r_target <- r_estimates |>
  filter(date >= target_start & date <= target_end) |>
  select(date, variable, type, median, mean, sd, lower_90, lower_50, upper_50, upper_90)

cat("Summary of Rt estimates for England (2021-01-20 to 2021-03-01):\n")
print(as.data.frame(r_target), digits = 3)

# 5. Save outputs
output_csv <- "data/england_rt_estimates_2021-01-20_2021-03-01.csv"
write_csv(r_target, output_csv)
cat(sprintf("\nSaved Rt estimates to: %s\n", output_csv))

# Save full summary table including all parameters
full_summary_csv <- "data/england_all_estimates_2021-01-20_2021-03-01.csv"
all_estimates <- summary(estimates, type = "parameters")
write_csv(all_estimates, full_summary_csv)

# 6. Save plot
fig_dir <- "notebook/figures"
if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

p <- plot(estimates)
plot_file <- file.path(fig_dir, "england_rt_epinow2_2021.png")
ggsave(plot_file, plot = p, width = 10, height = 8, dpi = 300)
cat(sprintf("Saved plot to: %s\n", plot_file))

# Also save standalone Rt plot
p_rt <- plot(estimates, "R")
plot_rt_file <- file.path(fig_dir, "england_rt_only_2021.png")
ggsave(plot_rt_file, plot = p_rt, width = 9, height = 4.5, dpi = 300)
cat(sprintf("Saved Rt plot to: %s\n", plot_rt_file))

# Save summary object for inspection or downstream analysis
saveRDS(estimates, file = "data/england_epinow2_fit_2021.rds")
cat("Saved RDS fit object to: data/england_epinow2_fit_2021.rds\n")

cat("\nDone!\n")
