# Protocol: what drove error in real-time COVID-19 projections for the UK, 2020 to 2023

Status: draft protocol, not yet agreed. Nothing has been scored.
Code chunks are untested sketches.
Written 2026-11-04. Revised 2026-11-04 after scoping questions.
Handle: #2 (pre-specify analysis plan). Related: #18, #16, #21.

People: Kath Sherratt (lead), with Sebastian Funk and Sam Abbott, who built and ran the pipelines being evaluated.

Pre-registration: once agreed, the protocol is frozen by tagging the commit `protocol-v1` before any forecast is scored.
Any later change is logged under "Amendments" with its date and reason.

## 1. Background

From April 2020 to late 2023 the LSHTM epiforecasts group submitted weekly projections of hospital admissions, bed occupancy, ventilated beds and deaths to SPI-M-O, and later to UKHSA's EMRG.
These fed the combined medium-term projections (MTPs).

The projections were explicit about their main assumption.
They projected admissions "assuming no changes in transmissibility beyond reductions in susceptibility due to infections" (`covid19-uk-nowcasts-projections/README.md`).
Deaths, occupancy and ventilated beds were then derived from projected admissions as scaled convolutions with estimated delay distributions.

That structure says in advance where the projections should go wrong:

1. Admissions error should follow any change in transmission after the forecast date, from variants, behaviour, policy, season or immunity.
2. Error in deaths and occupancy should be admissions error plus error in the ratio and delay linking admissions to each outcome. Vaccination and Omicron changed both.
3. Where admissions were projected from cases, error should also follow changes in case ascertainment.

Published evaluations cover two parts of this period.
Funk et al. (2020) scored the SPI-M short-term forecasts, ours included, from 31 March to 13 July 2020.
Manley et al. (2024) scored the combined MTPs from November 2021 to December 2022.
Neither asked what drove the error of a single model across the whole response.

## 2. Objectives

Primary:

1. What drove error in the LSHTM projections of admissions, deaths and occupancy, and how much of it does the model's stated assumption explain?

Secondary:

2. How large was the error, and how well calibrated were the projections, by horizon, target, geography and period? (Descriptive, needed to interpret objective 1.)
3. Did the projections outperform simple baselines, including one that continues the current trend?
4. When did the model change, and did changes in method coincide with changes in error?

Out of scope:

- R and growth rate nowcasts. These have no ground truth and belong with the consensus calibration work in #18.
- Scenario runs (for example `2020-11-03-lshtm-rbc-*.csv`), which need a conditional evaluation.
- Other groups' submissions, apart from the 2020 set in `covid19.forecasts.uk`, used as context only.

## 3. Design

A retrospective evaluation of prospective real-time projections, with hypotheses and analysis specified before scoring.
Reported against the EPIFORGE 2020 guidelines (Pollett et al. 2021).

Unit of evaluation: one forecast, meaning one combination of submission date, geography, target and target date, given as predictive quantiles.

The analysis is observational.
Drivers are associated with error, not shown to cause it, apart from the structural argument in section 1, which the analysis tests.

## 4. Data

### 4.1 Forecasts

| Source | Visibility | Content | Period |
|---|---|---|---|
| `epiforecasts/covid19-uk-nowcasts-projections`, `format-forecast/data/all/` | Public | 154 weekly submission files | 2020-04-07 to 2023-09-12 |
| `epiforecasts/covid-uk`, `format-forecast/data/` | Private | 96 submission files, plus by-predictor variants (for example `admissions-using-cases`, `deaths-using-admissions`, `occupancy-using-admissions`) | 2020-04-07 to 2022-03-22 |
| `epiforecasts/covid-uk-archive`, `forecast/data/all/` | Private | Early submissions | 2020-04-07 to 2020-07-21 |
| `epiforecasts/covid-uk-upload`, `remote/submitted/` | Public | Three 2022 rounds as uploaded | 2022-05 to 2022-07 |
| `epiforecasts/covid19.forecasts.uk` | Public | All 11 SPI-M models' short-term forecasts, anonymised (context only) | 2020-03-31 to 2020-07-13 |

Format (SPI-M template, checked on five files): one row per geography, target and target date.
Columns are `Group`, creation date, value date, `Geography`, `ValueType`, `Model`, `Scenario`, `ModelType`, `Version`, `Value` (the median), `AgeBand`, and quantiles 0.05 to 0.95 in steps of 0.05.

Targets in scope (from December 2020 the files carry `Model = EpiNow`, `Scenario = MTP`, horizons of 0 to 84 days):

| `ValueType` | Meaning | Geographies | Derived from |
|---|---|---|---|
| `hospital_inc` | New hospital admissions | 11 | Cases (`ModelType = Cases`), to confirm across all files |
| `type28_death_inc_line` | Deaths within 28 days of a positive test, by date of death | 11 | Admissions |
| `hospital_prev` | Beds occupied | 10 | Admissions |
| `icu_prev` | Mechanical ventilation beds occupied, dropped by late 2022 | 10 | Admissions |

Geographies: the seven NHS England regions, England, Scotland, Wales and Northern Ireland.
In April 2020 the model was a branching process with a 2-week horizon.
The first step is an audit of every file (step 1), since only five have been read so far.

### 4.2 Observed outcomes

- Public UKHSA dashboard series only (section 11): admissions, beds occupied and ventilated beds by nation and NHS England region, and deaths within 28 days by date of death.
- If deaths by NHS England region are not public, regional death forecasts are dropped from the primary analysis and the gap is reported.
- Final data: downloaded once and stored with the download date.
- Real-time data: where archived vintages exist (the dashboard archive, or data snapshots kept in the forecast repos), scores are repeated against data as known at each submission date (sensitivity analysis S1).
- Target definitions changed over time. Each target is matched to the definition stated at the time, and the matching is recorded in a lookup table.

### 4.3 Explanatory variables

All are built from public data and fixed before scoring.

| Variable | Definition | Source |
|---|---|---|
| Realised change in transmission | Change in the 7-day growth rate of observed admissions between the creation date and the target date | Observed admissions |
| Epidemic phase at the target date | Growth above +5% per week, decline below -5% per week, stable or turning in between | Observed admissions |
| Variant period | Periods starting when each variant first made up more than half of sequences: wild type, Alpha, Delta, Omicron BA.1, BA.2, BA.4/5, later Omicron | Published sequence data |
| Variant emergence window | The period from first detection of a new variant until it made up more than half of sequences | Published sequence data |
| Severity ratio change | Change in the ratio of each outcome to admissions lagged by the outcome's delay, between the creation date and the target date | Observed series |
| Testing regime | Dated changes in testing policy by nation, for example the universal free lateral flow offer and the end of free testing in England. Exact dates to confirm per nation | Government announcements |
| Model period | Periods between dated changes in model or data inputs (section 9) | Repo history |
| Horizon | Days ahead, grouped in weeks 1 to 12 | Forecast files |

Realised change in transmission is measured from observed admissions, not from a retrospective R model.
That is simpler, needs no further modelling, and is close to what the model's assumption concerns.
A retrospective R estimate is used as a sensitivity analysis (S6).

## 5. Eligibility

Included:

- every submission file with `Scenario` of `Forecast` or `MTP` and a target in section 4.1
- every horizon up to the maximum submitted

Excluded:

- scenario runs
- forecasts for target dates beyond the end of the observed data
- target dates the group flagged for exclusion at the time (`data/mtp-exclusions.csv` in `covid-uk-upload`, `data/exclude/` in `covid19_uk_forecast_data`), all listed in the report
- duplicate submissions for one date: the last file committed before the submission deadline is kept, using git history

Horizon 0 is reported separately and not pooled with the forecasts.

## 6. Outcomes

Computed with `scoringutils` on the quantiles as submitted:

- Weighted interval score (WIS) on the log scale (Bosse et al. 2023) as the primary outcome, so that waves and regions of different size can be compared. The natural scale is a sensitivity analysis (S3).
- The WIS components: overprediction, underprediction and dispersion. These carry the direction of error, which the hypotheses depend on.
- Bias, as defined in `scoringutils`.
- Empirical coverage of the 50% and 90% central intervals.
- Relative WIS against each baseline (section 7).
- Absolute error of the median.

## 7. Comparators

1. Persistence: the last observed 7-day mean held flat.
2. Trend continuation: the growth rate over the last two weeks held constant. This is the fair comparator for a product described as a continuation of current trends.

Both get quantiles from the empirical distribution of their own past errors, and both use only data available at each submission date.
`epiforecasts/forecastbaselines` may provide both.

Baselines matter for the drivers question too.
Error shared with the baselines comes from the epidemic.
Error the model has and the baselines don't comes from the model.

## 8. Hypotheses and analysis

### 8.1 Hypotheses

| | Hypothesis | Prediction | Test |
|---|---|---|---|
| H1 | Admissions error follows realised change in transmission | Log WIS increases with the absolute change in growth rate over the horizon. Underprediction when growth accelerates, overprediction when it slows | Association of log WIS, and of the over- and underprediction components, with the signed change in growth rate |
| H2 | Error is concentrated at turning points | Log WIS is higher in the stable or turning phase than in sustained growth or decline, at the same horizon | Comparison by phase, within horizon |
| H3 | New variants raise error before they dominate | Higher log WIS and underprediction in variant emergence windows | Comparison of emergence windows with other weeks, within variant period |
| H4 | Error in deaths and occupancy adds a severity component to admissions error | For the same submission, outcome error exceeds admissions error by an amount that grows with the change in severity ratio. Overprediction of deaths after vaccination roll-out and with Omicron | Association of the outcome-minus-admissions log WIS difference with the change in severity ratio |
| H5 | Changes in testing raise admissions error | Step increases in admissions error after changes in testing regime, while the model used cases as a predictor | Interrupted time series of log WIS around each dated change |
| H6 | Model changes shift error | Step changes in error at dated model changes | Interrupted time series around each change in section 9 |

H1 is primary.
The others are secondary and are reported whatever the result.

### 8.2 Analysis

- Descriptive first: log WIS and its components by horizon, target, geography and period, with coverage, plotted over submission dates with variant and testing periods shaded (objective 2).
- For H1 to H4, a regression of log WIS on the explanatory variables, with horizon and target as fixed effects and geography and submission date as random effects. Analyses of the separate components use the same structure.
- For H5 and H6, segmented regression of weekly mean log WIS around each dated change, with the change dates fixed in advance.
- Scores are strongly correlated across horizons, geographies and adjacent weeks. Uncertainty comes from a block bootstrap over submission dates, and estimates are reported with intervals rather than p-values.
- Every result is also shown for the trend-continuation baseline, to separate error shared with the baseline from error specific to the model.

## 9. Model history

The model changed during the response.
A timeline of changes is built from:

- the `Model`, `ModelType` and `Version` columns
- commit messages and diffs in `covid-uk` and `covid19-uk-nowcasts-projections`, covering model code, delay distributions, predictors and data sources
- `NEWS.md` in both repos
- Seb and Sam's recollection, recorded as such and checked against the commits

Each change gets a date, a one-line description, a source commit and a category (model structure, delays, predictor, data source).
This timeline is fixed before scoring, so that H6 cannot be fitted to the error series.

## 10. Sensitivity analyses

- S1: scoring against real-time rather than final data.
- S2: excluding horizon week 1.
- S3: natural rather than log scale.
- S4: excluding periods the group flagged as unreliable at the time.
- S5: phase thresholds of ±2% and ±10% per week.
- S6: realised change in transmission measured by a retrospective R estimate instead of observed growth rate.

## 11. Data governance

- The forecasts are the group's own outputs. Two of the source repos are already public.
- Some submissions used restricted UKHSA line-list data as model input. The outputs are aggregated projections, but the data sharing terms of the time are to be checked with Seb before any result from the private repos is published.
- The evaluation uses public observed data only, so anyone can reproduce the scoring.

## 12. Outputs

1. A tidy, hub-format dataset of the LSHTM UK submissions from 2020 to 2023, with the model history timeline and a data descriptor.
2. A paper on what drove error, reporting objectives 1 to 4.
3. Input to #18 (consensus calibration) and to the COVID chapter of the history work.

## 13. Steps

### 0. Agree the protocol

Share with Seb and Sam. Agree the hypotheses, the variant and testing dates and the phase threshold. Then tag `protocol-v1`.

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

# One row per file: models, predictors, targets and size
audit <- raw |>
  summarise(
    rows = n(),
    models = paste(unique(Model), collapse = "; "),
    model_types = paste(unique(ModelType), collapse = "; "),
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
         target = ValueType, model = Model, model_type = ModelType,
         scenario = Scenario, quantile_level, predicted)
```

### 3. Get observed data and build explanatory variables

Download the public series, save them with the download date, and build the target lookup table (section 4.2) and the variables in section 4.3.

```r
# Realised change in growth rate between creation and target date
growth <- observed |>
  filter(target == "hospital_inc") |>
  arrange(geography, date) |>
  mutate(
    # 7-day rolling mean, then weekly growth rate on the log scale
    mean_7d = slider::slide_dbl(observed, mean, .before = 3, .after = 3),
    growth_rate = log(mean_7d) - lag(log(mean_7d), 7),
    .by = geography
  )

# Attach growth at creation and target dates to each forecast
forecast_drivers <- forecasts |>
  distinct(creation_date, target_date, geography) |>
  left_join(select(growth, geography, creation_date = date, growth_start = growth_rate),
            by = c("geography", "creation_date")) |>
  left_join(select(growth, geography, target_date = date, growth_end = growth_rate),
            by = c("geography", "target_date")) |>
  mutate(growth_change = growth_end - growth_start)
```

### 4. Build baselines

Build both baselines from the data available at each `creation_date` (section 7).

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

# Descriptive summary by target and horizon in weeks
scores |>
  mutate(horizon_weeks = ceiling(horizon_days / 7)) |>
  summarise_scores(by = c("target", "horizon_weeks"))
```

### 6. Fit the driver models

Regressions and segmented regressions as in section 8.2, with a block bootstrap over submission dates.

### 7. Write up

Report against the EPIFORGE checklist.

## 14. Open questions

1. Is ±5% per week the right phase threshold?
2. Which testing regime changes, by nation, go into H5? This needs a dated list agreed before scoring.
3. Did admissions projections use cases as the only predictor throughout, or did this change? The audit and Seb or Sam can answer it.
4. If the combined MTP files on the group server can be used (with DHSC or UKHSA permission), does a consensus comparison join this study as a secondary objective, or stay with #18?
5. Should the 2020 multi-model set in `covid19.forecasts.uk` be used to test H1 across models, or only as context?

## Amendments

- 2026-11-04: rescoped after discussion. The primary aim moved from overall accuracy to drivers of error. R nowcasts moved out of scope to #18. Pre-registration is by git tag only.

## Sources

- Funk S, et al. (2020). Short-term forecasts to inform the response to the Covid-19 epidemic in the UK. medRxiv. Data and code: `epiforecasts/covid19.forecasts.uk`.
- Manley H, et al. (2024). Combining models to generate consensus medium-term projections of hospital admissions, occupancy and deaths relating to COVID-19 in England. Royal Society Open Science. doi:10.1098/rsos.231832.
- Bosse NI, et al. (2023). Scoring epidemiological forecasts on transformed scales. PLoS Computational Biology.
- Pollett S, et al. (2021). Recommended reporting items for epidemic forecasting and prediction research: the EPIFORGE 2020 guidelines. PLoS Medicine.
- `epiforecasts/covid19-uk-nowcasts-projections/README.md`, for the projection assumption quoted in section 1.
- Repository contents as read on 2026-11-04: file trees of the epiforecasts repos in section 4.1, and five sampled submission files from `covid19-uk-nowcasts-projections`.
