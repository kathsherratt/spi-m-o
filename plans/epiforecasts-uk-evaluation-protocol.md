# Protocol: validation, verification and qualification of real-time COVID-19 projections for the UK, 2020 to 2023

Status: draft protocol, not yet agreed. Nothing has been scored or refitted.
Code chunks are untested sketches.
Written 2026-11-04. Revised twice on 2026-11-04 (see Amendments).
Handle: #2 (pre-specify analysis plan). Related: #18, #16.

People: Kath Sherratt (lead), with Sebastian Funk and Sam Abbott, who built and ran the pipelines.

Pre-registration: once agreed, the protocol is frozen with the git tag `protocol-v1` before any score is computed.
Later changes are logged under Amendments with date and reason.

## 1. Background

From April 2020 to September 2023 the LSHTM epiforecasts group made weekly projections of hospital admissions, bed occupancy and deaths for the UK, submitted to SPI-M-O and later UKHSA's EMRG.
Admissions were projected with EpiNow2, "assuming no changes in transmissibility beyond reductions in susceptibility due to infections" (`covid19-uk-nowcasts-projections/README.md`).
Occupancy and deaths were derived from projected admissions by convolution with estimated delays.

The question is what drove error in these projections.
Rather than test a list of candidate drivers, the protocol splits error along the modelling cycle, following the terminology of Schlesinger et al. (1979):

| Step | Question | Here |
|---|---|---|
| Validation | Does model output agree with reality well enough for its purpose? | Score the projections as made against observed data |
| Verification | Does the implemented model do what the conceptual model specifies? | Re-run the same method as it stood at the time, correctly implemented, and compare with what was submitted |
| Qualification | Is the conceptual model adequate for its purpose? | Remove the model's central assumption by supplying the transmission that actually happened, and see how much error goes |

The same structure is planned for the epiforecasts US COVID-19 forecasts (`covid-us-forecasts`, branch `evaluation`, `cuf-evaluation-handoff.md`).
The variant labels below follow that plan, so the two studies can be read side by side.

## 2. Design

Every variant projects the same targets: the same submission dates, geographies, outcomes and target dates.
The contrasts are therefore paired, and the difficulty of each target is held fixed by design.
This is the rung of the evaluation design ladder where comparison is identifiable without adjustment (eval-by-method, `attic/evaluation-design-sketch.qmd`), so the primary contrasts need no regression model.

### 2.1 Variants

| Label | What it is | How it is made |
|---|---|---|
| O | Original real-time projections | Already in `covid19-uk-nowcasts-projections`, as made |
| P | Correct rebuild with the method of the time | Public pipeline, EpiNow2 as of each submission date, data as available at that date |
| M | Modern rebuild | Same inputs as P, latest EpiNow2 release |
| M+R | Modern rebuild with realised transmission | As M, but future R fixed to retrospective estimates from `inc2prev` (ONS Infection Survey) instead of held constant |

### 2.2 Contrasts

| Contrast | Isolates | Step |
|---|---|---|
| O against observed | Total error as it reached decision makers | Validation |
| O against P | Implementation error: failed fits, manual fixes, code errors, run-to-run noise | Verification |
| P against M | Change in method and software since the response | Qualification (method) |
| M against M+R | Error from the assumption of no change in transmission | Qualification (assumption) |

What is left in M+R is error from everything else in the conceptual model: delays, observation model, and the convolution from admissions to beds and deaths.

The primary contrast is M against M+R, because it answers the original question directly: how much of the error came from the model's central assumption.

### 2.3 Why inc2prev for realised transmission

`inc2prev` estimates infections, and from them R, from the ONS Infection Survey.
That survey sampled infection directly, so its R is independent of the testing, admissions and death data the projections used.
An R from the same surveillance streams would feed back the data the projection was trying to forecast.

Two limits:

- R is a model quantity, not an observation. Here it is an input to M+R, never a ground truth to score against.
- The Infection Survey ended in March 2023, and it reported ONS regions rather than NHS England regions. M+R therefore covers April 2020 to March 2023 only, and regional R is mapped to NHS regions by aggregation (section 5.4).

## 3. First check: does the public version match the SPI-M version?

Run on 2026-11-04 by comparing file hashes and values across `epiforecasts/covid-uk` (the SPI-M pipeline, private) and `epiforecasts/covid19-uk-nowcasts-projections` (the public pipeline).

- The public repo was created on 18 February 2022. On 19 February 2022 a commit ("previous projections") copied in the 91 earlier submission files. These are byte for byte identical to the SPI-M files.
- So the public files up to 15 February 2022 are the SPI-M projections, made with SPI-M data inputs, some of them restricted.
- Five dates exist in both versions and differ: 22 February to 22 March 2022. On 22 March the public version has no `icu_prev`. Median relative differences in the median projection are -1.6% for admissions (range -78% to +44%), +8.7% for beds and -8.3% for deaths (range -100% to +140%).
- From April 2022 only the public pipeline ran. The files actually uploaded on three 2022 dates (`covid-uk-upload/remote/submitted/`) match the public values exactly on every shared row. They are subsets (2,380 of 2,720 rows on 3 May 2022), consistent with exclusions applied at upload (`data/mtp-exclusions.csv`).

Consequences:

1. "O" is not one thing. It is SPI-M-input projections to February 2022 and public-input projections afterwards. The switch is a known change point, and is shown on every time series.
2. A fully public O for 2020 to 2022 does not exist. P, run on public data, provides it.
3. Which version was submitted on the five overlapping dates is to be confirmed with Seb or Sam.

Cases were modelled but not submitted as a target.
The SPI-M pipeline forecast reported cases internally (`covid-uk/forecast/linelist-cases/summary/`) as the predictor for admissions.
They are out of scope here.

## 4. Scope

Targets: admissions (`hospital_inc`), beds occupied (`hospital_prev`), and deaths within 28 days by date of death (`type28_death_inc_line`).
Mechanical ventilation beds (`icu_prev`) are dropped, because they are missing from the public version from 2022.

Geographies: the seven NHS England regions, England, Scotland, Wales and Northern Ireland, as submitted.

Horizons: weeks 1 to 12 (days 1 to 84). Horizon 0 is excluded.

Submission dates:

- O: all 154 dates, 2020-04-07 to 2023-09-12, for validation.
- P, M and M+R: a sample, because each refit is an EpiNow2 MCMC run per geography. The sample is about 20 dates, stratified by calendar period and epidemic phase at the submission date. It is drawn by a seeded script and fixed before any refit. O is scored on the same sample for the paired contrasts.

Out of scope: R nowcasts (#18), scenario runs, and other groups' submissions.

## 5. Data

### 5.1 Projections (O)

`covid19-uk-nowcasts-projections/format-forecast/data/all/*-lshtm-forecast.csv`.
SPI-M template: one row per geography, target and target date, with `Value` (median) and quantiles 0.05 to 0.95 in steps of 0.05.

### 5.2 Inputs for P and M

Public UKHSA dashboard series as available at each submission date.
The public pipeline's own data download code defines which series.
Whether past releases can still be downloaded (the dashboard archive, or snapshots in the repo history) is the main feasibility question for P (section 9, step 2).
If only final data can be had, P and M run on final data, and the O against P contrast then mixes implementation error with data revision. That is reported as such.

### 5.3 Observed outcomes

The same public series, at a fixed vintage downloaded once and stored with the download date.
If deaths by NHS England region are not public, regional death targets are dropped and the gap is reported.

### 5.4 Realised transmission (for M+R)

`inc2prev` R estimates for England, the ONS regions and the devolved nations, posterior median and quantiles by day.
ONS regions are combined to NHS England regions by population weighting: Midlands = East Midlands and West Midlands; North East and Yorkshire = North East and Yorkshire and the Humber. The other regions map one to one.
Small boundary differences between ONS and NHS regions are accepted and noted.

## 6. Outcomes

Computed with `scoringutils` (2.x) on the predictive quantiles:

- Weighted interval score (WIS) on the log scale (`log_shift`, offset 1), primary (Bosse et al. 2023).
- WIS components: overprediction, underprediction, dispersion.
- Coverage of the 50% and 90% central intervals.
- Absolute error of the median.

For each contrast, the outcome is the paired difference in log-scale WIS for every target forecast by both variants.

## 7. Analysis

Primary (M against M+R) and the other contrasts are analysed the same way.

- The distribution of paired WIS differences: median, interquartile range, and the share of targets where the second variant scored better. By horizon and target.
- Time series of paired differences by submission date, with variant periods and the February 2022 pipeline change marked.

Uncertainty:

- The main source of uncertainty in a refit contrast is MCMC run-to-run noise. On five sampled dates, every refit variant is run five times with different seeds. The spread of WIS across seeds gives a noise floor. Paired differences smaller than the floor are not interpreted.
- No bootstrap or model-based intervals are reported for the primary contrasts.

Context for validation only (O against observed):

- Scores of a persistence baseline (last observed 7-day mean held flat) on the same targets, so that the scale of error can be read.
- Descriptive scores of O by horizon, target, geography, epidemic phase and variant period. These describe where error was. They are exploratory and are not tested.

## 8. EpiNow2 history

The pipelines did not pin an EpiNow2 version.
`DESCRIPTION` in both repos takes EpiNow2 from GitHub (`Remotes: epiforecasts/EpiNow2`), and the public pipeline's Dockerfile used `epinow2:latest`.
So the version behind each projection is inferred as the EpiNow2 `main` branch on the date the projection was made. This is to be checked against what Seb or Sam recall, and against any session information saved in the repos.

Release tags in `epiforecasts/EpiNow2`, from `git tag`:

| Date | Release | In the period |
|---|---|---|
| 2020-07-23 | v1.0.0 | First release |
| 2020-08-21 | v1.1.0 | |
| 2020-10-13 | v1.2.0 | |
| 2020-10-17 | v1.2.1 | |
| 2020-11-19 | v1.3.0 | |
| 2020-11-23 | v1.3.1 | |
| 2020-12-13 | v1.3.2 | Then development only for nearly two years |
| 2022-10-20 | v1.3.3 | |
| 2023-02-01 | v1.3.4 | |
| 2023-04-27 | v1.3.5 | |
| 2023-09-18 | v1.4.0 | Six days after the last projection |
| 2026-06-17 | v1.9.0 | Latest release, used for M |

Most of the period (December 2020 to October 2022) ran on development code between releases.
For P, EpiNow2 is checked out at the last `main` commit before each sampled submission date.
Changes are read from `NEWS.md` and the release notes, and summarised as a timeline that marks O's time series.
The timeline is fixed before scoring.

## 9. Steps

### 1. Audit O and confirm the first check

```r
# Packages for reading and tidying submission files
library(dplyr)
library(purrr)
library(readr)
library(tidyr)

# List every submission file in the public repo clone
files <- list.files(
  "covid19-uk-nowcasts-projections/format-forecast/data/all",
  pattern = "-lshtm-forecast\\.csv$",
  full.names = TRUE
)

# Read every file as character, keeping the file name
raw <- files |>
  set_names(basename) |>
  map(\(f) read_csv(f, col_types = cols(.default = "c"))) |>
  list_rbind(names_to = "file")

# Build dates and pivot quantiles to long format
o_forecasts <- raw |>
  mutate(
    creation_date = as.Date(paste(`Creation Year`, `Creation Month`, `Creation Day`, sep = "-")),
    target_date = as.Date(paste(`Year of Value`, `Month of Value`, `Day of Value`, sep = "-")),
    horizon_days = as.integer(target_date - creation_date),
    # Files before 22 February 2022 came from the SPI-M pipeline
    pipeline = if_else(creation_date < as.Date("2022-02-22"), "spim", "public")
  ) |>
  pivot_longer(
    starts_with("Quantile "),
    names_to = "quantile_level",
    names_prefix = "Quantile ",
    values_to = "predicted"
  ) |>
  mutate(across(c(quantile_level, predicted), as.numeric)) |>
  filter(ValueType != "icu_prev", horizon_days >= 1) |>
  select(creation_date, target_date, horizon_days, geography = Geography,
         target = ValueType, pipeline, quantile_level, predicted)
```

### 2. Check whether real-time inputs can be rebuilt

Try one past submission date: can the public pipeline's input series be downloaded as they stood on that date?
This decides whether P runs on real-time or final data (section 5.2).

### 3. Draw the refit sample

A seeded, stratified draw of about 20 submission dates (section 4), committed before any refit.

### 4. Refit P, M and M+R

For each sampled date and geography: P with EpiNow2 as of that date, M with the latest release, M+R with future R from `inc2prev`.
Five seeds on five dates for the noise floor.

### 5. Score

```r
# Score any set of variants on the log scale
library(scoringutils)

score_variants <- function(forecasts, observed) {
  forecasts |>
    left_join(observed, by = c("geography", "target", "target_date")) |>
    as_forecast_quantile(
      forecast_unit = c("variant", "creation_date", "target_date",
                        "horizon_days", "geography", "target")
    ) |>
    transform_forecasts(fun = log_shift, offset = 1, append = FALSE) |>
    score()
}

# Paired difference in WIS between two variants on shared targets
paired_difference <- function(scores, first, second) {
  scores |>
    filter(variant %in% c(first, second)) |>
    select(variant, creation_date, target_date, horizon_days, geography, target, wis) |>
    pivot_wider(names_from = variant, values_from = wis) |>
    filter(!is.na(.data[[first]]), !is.na(.data[[second]])) |>
    mutate(difference = .data[[second]] - .data[[first]])
}
```

### 6. Write up

Report against EPIFORGE 2020 (Pollett et al. 2021).

## 10. Repeating the analysis on the SPI-M version

Everything above can be repeated on the SPI-M pipeline (`epiforecasts/covid-uk`, April 2020 to March 2022), with its own inputs.
That would add the effect of restricted data (line lists, for example) to the decomposition.
It needs the data sharing terms of the time to be checked, and is not part of this protocol.

## 11. Data governance

- O up to 15 February 2022 consists of SPI-M projections, made partly from restricted inputs and already public in `covid19-uk-nowcasts-projections`. Seb to confirm this was cleared at the time.
- P, M and M+R use public inputs only, and `inc2prev` uses public Infection Survey outputs.
- Observed outcomes are public, so anyone can reproduce the scoring.

## 12. Outputs

1. A paper decomposing projection error into validation, verification and qualification parts.
2. The refit variants and scores as a dataset, alongside the US study.
3. A worked example for the anchor paper on evaluating predictions, models and modelling (kath-a-log #162).

## 13. Open questions

1. Which version was submitted on the five overlapping dates, 22 February to 22 March 2022?
2. Is about 20 refit dates enough, given the MCMC cost per date across 11 geographies?
3. If real-time inputs cannot be rebuilt, is P on final data still worth running, or should verification be dropped to a check of reproducibility only?
4. Is M+R best done by fixing future R to inc2prev, or by giving the model the inc2prev R trajectory as a prior? The first is cleaner as a counterfactual. The second is closer to how EpiNow2 would actually use it.
5. Should the persistence baseline stay, now that M provides a modern comparator?

## Amendments

- 2026-11-04 (first revision): primary aim moved from overall accuracy to drivers of error. R nowcasts moved to #18. Pre-registration by git tag only.
- 2026-11-04 (second revision): restructured around validation, verification and qualification, with variants O, P, M and M+R as in the US study. The six hypotheses, the severity ratio and the block bootstrap are removed. Sources narrowed to the public pipeline, with the SPI-M pipeline as a possible repeat. Added the result of the first check, the latest EpiNow2 as a comparator, inc2prev R for realised transmission, and an EpiNow2 release timeline. `icu_prev` dropped.

## Sources

- Schlesinger S, et al. (1979). Terminology for model credibility. Simulation 32(3): 103 to 104.
- Bosse NI, et al. (2023). Scoring epidemiological forecasts on transformed scales. PLoS Computational Biology 19(8): e1011393.
- Pollett S, et al. (2021). Recommended reporting items for epidemic forecasting and prediction research: the EPIFORGE 2020 guidelines. PLoS Medicine.
- Funk S, et al. (2020). Short-term forecasts to inform the response to the Covid-19 epidemic in the UK. medRxiv.
- Manley H, et al. (2024). Combining models to generate consensus medium-term projections of hospital admissions, occupancy and deaths relating to COVID-19 in England. Royal Society Open Science.
- `epiforecasts/covid-us-forecasts`, branch `evaluation`, `cuf-evaluation-handoff.md` (June 2026): the O, P and M design.
- `epiforecasts/eval-by-method`, `attic/evaluation-design-sketch.qmd` (September 2026): the ladder of evaluation designs.
- `kathsherratt/applications`, `nihr/nihr.qmd`: notes on validation, verification and qualification for responsive research.
- `epiforecasts/EpiNow2`: release tags and `NEWS.md`.
- Repository comparison of `epiforecasts/covid-uk`, `epiforecasts/covid19-uk-nowcasts-projections` and `epiforecasts/covid-uk-upload`, run on 2026-11-04 (section 3).
