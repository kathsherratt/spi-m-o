# Protocol: evaluating LSHTM epiforecasts real-time COVID-19 estimates for the UK, 2020 to 2023

Status: draft protocol, not yet agreed. Nothing has been scored.
Code chunks are untested sketches.
Written 2026-11-04.
Handle: #2 (pre-specify analysis plan). Related: #18, #16, #21.

Once agreed, the protocol is frozen by tagging the commit (`protocol-v1`) before any forecast is scored.
Any later change is logged under "Amendments" with its date and reason.

## 1. Background

From April 2020 to late 2023 the LSHTM epiforecasts group submitted weekly real-time estimates to SPI-M-O, and later to UKHSA's EMRG.
Two products came out of each round:

- projections of hospital admissions, bed occupancy, ventilated beds and deaths, up to 12 weeks ahead, which fed the combined medium-term projections (MTPs)
- nowcasts of the time-varying reproduction number (R) and growth rate, which fed the consensus R range

Published evaluations cover two pieces of this.
Funk et al. (2020) scored the SPI-M short-term forecasts, ours included, from 31 March to 13 July 2020.
Manley et al. (2024) scored the combined MTPs from November 2021 to December 2022.
Neither scored a single contributing model across the whole period.

A single model scored across three years has two uses.
It shows how one consistent method performed through changing variants, testing regimes and policy.
It also gives a reference point for the consensus work in #18, which asks whether combining many models did better or worse than one model on one consistent data stream.

## 2. Objectives

Primary:

1. How accurate and well calibrated were the LSHTM projections of hospital admissions and deaths, by horizon, over April 2020 to September 2023?

Secondary:

2. How did performance vary with epidemic phase (growth, peak, decline), variant period, geography and target?
3. Did the projections outperform simple baselines, including a baseline that continues the current trend, which is what the projections said they were doing?
4. How well did the real-time R nowcasts agree with later estimates and with the published consensus range, including the direction of change?
5. When did the model change, and did changes in method coincide with changes in performance?

Out of scope: the scenario runs (for example `2020-11-03-lshtm-rbc-*.csv`), which need a conditional evaluation, and the other groups' submissions, apart from the 2020 set already in `covid19.forecasts.uk`.

## 3. Design

A retrospective evaluation of prospective real-time forecasts, with the analysis specified before scoring.
Reported against the EPIFORGE 2020 guidelines for epidemic forecasting research (Pollett et al. 2021).

Unit of evaluation: one forecast, meaning one combination of submission date, geography, target and target date, given as a set of predictive quantiles.

## 4. Data

### 4.1 Forecasts

| Source | Visibility | Content | Period |
|---|---|---|---|
| `epiforecasts/covid19-uk-nowcasts-projections`, `format-forecast/data/all/` | Public | 154 weekly submission files | 2020-04-07 to 2023-09-12 |
| `epiforecasts/covid-uk`, `format-forecast/data/all/` and by-target folders | Private | 96 submission files, plus by-predictor variants (for example `admissions-using-cases`, `deaths-using-admissions`) | 2020-04-07 to 2022-03-22 |
| `epiforecasts/covid-uk-archive`, `forecast/data/all/` | Private | Early submissions | 2020-04-07 to 2020-07-21 |
| `epiforecasts/covid-uk-upload`, `remote/submitted/` | Public | Three 2022 rounds as uploaded | 2022-05 to 2022-07 |
| `epiforecasts/covid19.forecasts.uk`, `data/uk_forecasts.rdata` | Public | All 11 SPI-M models' short-term forecasts (anonymised) | 2020-03-31 to 2020-07-13 |

Format (SPI-M template, checked on five files): one row per geography, target and target date.
Columns are `Group`, creation date, value date, `Geography`, `ValueType`, `Model`, `Scenario`, `ModelType`, `Version`, `Value` (the median), `AgeBand`, and quantiles 0.05 to 0.95 in steps of 0.05.

From December 2020 the files carry `Model = EpiNow`, `Scenario = MTP`, horizons of 0 to 84 days, and these targets:

| `ValueType` | Meaning | Geographies |
|---|---|---|
| `hospital_inc` | New hospital admissions | 11 |
| `hospital_prev` | Beds occupied | 10 |
| `icu_prev` | Mechanical ventilation beds occupied (dropped by late 2022) | 10 |
| `type28_death_inc_line` | Deaths within 28 days of a positive test, by date of death | 11 |

Geographies: the seven NHS England regions, England, Scotland, Wales and Northern Ireland.
In April 2020 the model was a branching process with a 2-week horizon.

The first step is an audit of every file (step 1 below), since only five have been read so far.

### 4.2 R nowcasts

`format-rt/data/time-series/*-time-series-r-lshtm.csv`, from 2021-01-26 to 2023-10-31 in the public repo, in the same template, with `ValueType = R` and `Scenario = Nowcast`.
Whether earlier R submissions exist in `covid-uk-archive` is to be checked.

### 4.3 Observed data

- Source: public UKHSA dashboard series only (see section 11).
  Admissions, beds occupied and ventilated beds by nation and NHS England region; deaths within 28 days by date of death by nation.
- Deaths by NHS England region may not be published.
  If not, regional death forecasts are dropped from the primary analysis and noted.
- Final data: the latest available values, downloaded once and stored with a download date.
- Real-time data: where archived vintages exist (dashboard archive, or data snapshots kept in the forecast repos), scores are repeated against the data as known at each submission date (sensitivity analysis S1).
- Target definitions changed over time, for example deaths by line list and by dashboard.
  Each target is matched to the definition stated at the time, with the matching recorded in a lookup table.

## 5. Eligibility

Included:

- every submission file with `Scenario` of `Forecast` or `MTP` and a target in the table above
- every horizon up to the maximum submitted, with horizons summarised in weeks (week 1 = days 1 to 7, and so on)

Excluded:

- scenario runs
- forecasts for target dates beyond the end of the observed data
- target dates the group itself flagged for exclusion at the time (`data/mtp-exclusions.csv` in `covid-uk-upload`, and `data/exclude/` in `covid19_uk_forecast_data`); all such exclusions are listed in the report
- duplicate submissions for the same date: the last file committed before the submission deadline is kept, using git history

Horizon 0 (the nowcast day) is reported separately, not pooled with the forecasts.

## 6. Outcomes and metrics

Computed with `scoringutils` on the quantiles as submitted:

- Weighted interval score (WIS), on the natural scale and on the log scale (Bosse et al. 2023). The log scale is primary, so that regions and waves of different size can be compared.
- Relative WIS against each baseline, using pairwise comparison.
- Empirical coverage of the 50% and 90% central intervals.
- Bias, as defined in `scoringutils`.
- Absolute error of the median.
- Direction of change: whether the median predicted change from the last observed value to the target date has the same sign as the observed change. This links to the preliminary figure in #18, where about 19% of consensus estimates had the direction of the trend wrong.

## 7. Comparators

1. Persistence: the last observed 7-day mean held flat, with quantiles from the empirical distribution of past errors.
2. Trend continuation: the growth rate over the last two weeks held constant, with the same treatment of quantiles. This is the fair comparator for a product described as a continuation of current trends.
3. For March to July 2020 only: the other SPI-M models and ensembles in `covid19.forecasts.uk`, as already scored by Funk et al.

Baselines are built only from data available at each submission date.
`epiforecasts/forecastbaselines` may provide both.

## 8. Strata and analysis

Strata, all defined before scoring:

- Horizon, in weeks 1 to 12.
- Target and geography.
- Epidemic phase at the target date, from the observed series: the 7-day growth rate centred on the target date, with growth above +5% per week, decline below -5% per week, and stable or turning in between. The threshold is to be agreed.
- Variant period, from published dates when each variant first made up more than half of sequences: wild type, Alpha, Delta, Omicron BA.1, BA.2, BA.4/5, later Omicron.
- Model period, from the code history (section 9).

Analysis:

- Summaries of each metric by stratum, with medians and interquartile ranges across forecasts.
- Time series of scores by submission date, with variant and policy periods shaded.
- No formal hypothesis tests on the primary objective. Scores are strongly correlated across horizons, geographies and adjacent weeks, so the analysis is descriptive. Where a comparison is reported, uncertainty comes from a block bootstrap over submission dates.

## 9. Model history

The model changed during the response.
A timeline of changes is built from:

- the `Model`, `ModelType` and `Version` columns
- commit messages and diffs in `covid-uk` and `covid19-uk-nowcasts-projections`, for changes to the model code, delay distributions, predictors and data sources
- `NEWS.md` in both repos

Each change gets a date, a one-line description and a source commit.
This answers objective 5, and is a record in its own right for the history work.

## 10. Sensitivity analyses

- S1: scoring against real-time data rather than final data.
- S2: excluding horizon 1 (largely nowcast).
- S3: natural scale instead of log scale.
- S4: excluding periods the group flagged as unreliable at the time.
- S5: other phase thresholds (±2% and ±10% per week).

## 11. Data governance

- The forecasts are the group's own outputs. Two of the source repos are already public.
- Some submissions used restricted UKHSA line-list data as model input. The outputs are aggregated model projections, but the data sharing terms of the time should be checked before any use of the private repos is published.
- The evaluation uses public observed data only, so the scoring can be reproduced by anyone.
- Seb Funk and Sam Abbott built and ran these pipelines. Agreeing roles and authorship with them comes before step 1.

## 12. Outputs

1. A tidy, hub-format dataset of the LSHTM UK submissions from 2020 to 2023, with a data descriptor.
2. A paper reporting objectives 1 to 5.
3. Input to #18 (consensus calibration) and to the COVID chapter of the history work.

## 13. Steps

### 0. Agree the protocol

Agree it with Seb and Sam, then tag `protocol-v1`.

### 1. Audit the submission files

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

# One row per file: models, scenarios, targets and size
audit <- raw |>
  summarise(
    rows = n(),
    models = paste(unique(Model), collapse = "; "),
    scenarios = paste(unique(Scenario), collapse = "; "),
    targets = paste(sort(unique(ValueType)), collapse = "; "),
    geographies = n_distinct(Geography),
    .by = file
  )
```

### 2. Convert to long quantile format

```r
# Build dates and pivot quantile columns to long format
forecasts <- raw |>
  mutate(
    creation_date = as.Date(paste(`Creation Year`, `Creation Month`, `Creation Day`, sep = "-")),
    target_date = as.Date(paste(`Year of Value`, `Month of Value`, `Day of Value`, sep = "-")),
    horizon_days = as.integer(target_date - creation_date)
  ) |>
  pivot_longer(
    starts_with("Quantile "),
    names_to = "quantile_level",
    names_prefix = "Quantile ",
    values_to = "predicted"
  ) |>
  mutate(across(c(quantile_level, predicted), as.numeric)) |>
  select(creation_date, target_date, horizon_days, geography = Geography,
         target = ValueType, model = Model, scenario = Scenario,
         quantile_level, predicted)
```

### 3. Get observed data

Download the public dashboard series for each target and geography, save with the download date, and build the target lookup table (section 4.3).

### 4. Build baselines

Build both baselines from data available at each `creation_date` (section 7).

### 5. Score

```r
# Join forecasts to observations and score on the log scale
library(scoringutils)

scores <- forecasts |>
  left_join(observed, by = c("geography", "target", "target_date")) |>
  mutate(across(c(predicted, observed), \(x) log(x + 1))) |>
  as_forecast_quantile(
    forecast_unit = c("creation_date", "target_date", "horizon_days",
                      "geography", "target", "model")
  ) |>
  score()

# Summarise by target and horizon in weeks
scores |>
  mutate(horizon_weeks = ceiling(horizon_days / 7)) |>
  summarise_scores(by = c("target", "horizon_weeks"))
```

### 6. Model history timeline

See section 9.

### 7. Write up

Report against the EPIFORGE checklist.

## 14. Open questions

1. Who is on this, and in what roles? Seb and Sam built the pipelines.
2. Is the primary target admissions only, or admissions and deaths together?
3. Phase thresholds: is ±5% per week right?
4. Pre-register on OSF as well as with a git tag?
5. If the combined MTP files on the group server can be used (with DHSC or UKHSA permission), is the consensus comparison a secondary objective here, or a separate study under #18?

## Amendments

None yet.

## Sources

- Funk S, et al. (2020). Short-term forecasts to inform the response to the Covid-19 epidemic in the UK. medRxiv. Data and code: `epiforecasts/covid19.forecasts.uk`.
- Manley H, et al. (2024). Combining models to generate consensus medium-term projections of hospital admissions, occupancy and deaths relating to COVID-19 in England. Royal Society Open Science. doi:10.1098/rsos.231832.
- Bosse NI, et al. (2023). Scoring epidemiological forecasts on transformed scales. PLoS Computational Biology.
- Pollett S, et al. (2021). Recommended reporting items for epidemic forecasting and prediction research: the EPIFORGE 2020 guidelines. PLoS Medicine.
- Repository contents as read on 2026-11-04: file trees of the epiforecasts repos listed in section 4.1, five sampled submission files and one R time-series file from `covid19-uk-nowcasts-projections`.
