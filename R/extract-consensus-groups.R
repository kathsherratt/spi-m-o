# Modelling groups named in the SPI-M-O and EMRG consensus statements
#
# Reads the statement text in spi-m-o-data and keeps every sentence that names
# contributing groups. One row per group per sentence, with the sentence as
# evidence, so any row can be checked by reading it.
#
# Groups are named in two places: an acknowledgements sentence in EMRG
# statements (July 2021 on), and a few SPI-M-O views in 2020 and 2021. Most
# SPI-M-O statements name none in the text; the group behind each bar in a
# figure is in the image.
#
# Usage, from the spi-m-o repo root:
#   Rscript R/extract-consensus-groups.R
# SPIMO_DATA points at spi-m-o-data/data/covid/spimo, SPIMO_OUT at the output
# directory.
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidyr)
library(tibble)
library(here)

spimo <- Sys.getenv("SPIMO_DATA",
  unset = here("..", "spi-m-o-data", "data", "covid", "spimo"))
out_dir <- Sys.getenv("SPIMO_OUT", unset = here("data", "derived"))

# A closed, hand-checked vocabulary. Anything outside it that looks like an
# institution is reported below, so a missed group is a warning, not silence.
groups <- tribble(
  ~group, ~pattern,
  "Imperial College London", "Imperial",
  "LSHTM", "LSHTM|London School of Hygiene",
  "University of Warwick", "Warwick",
  "University of Bristol", "Bristol",
  "University of Manchester", "Manchester",
  "University of Cambridge", "Cambridge",
  "University of Exeter", "Exeter",
  "Lancaster University", "Lancaster",
  "University of Oxford", "Oxford",
  "University of Liverpool", "Liverpool",
  "Dstl", "Dstl|Defence (and )?Science",
  "European Bioinformatics Institute", "European Bioinformatics",
  "Institute for Disease Modeling", "Institute (of|for) Disease Model"
)
# Named in these sentences but not counted. UKHSA and PHE convene the
# statements and take responsibility for them. The Turing Institute appears
# once, as prior work considered at SAGE.
not_counted <- "UKHSA|PHE|Public Health England|Turing"

# A sentence names contributors if it lists three or more groups, or two with
# a contribution cue, or one with an explicit acknowledgement.
cue <- regex("contribut|thank|acknowledg|drawn on|modelling (work|groups?)|estimates include|models? from|model outputs|groups? \\(", ignore_case = TRUE)
ack <- regex("acknowledges the work|contribut\\w+ model outputs", ignore_case = TRUE)

statements <- tibble(path = list.files(file.path(spimo, "txt"), "\\.txt$", full.names = TRUE)) |>
  mutate(
    statement = tools::file_path_sans_ext(basename(path)),
    type = str_extract(statement, "^(emrg|spi-m-o|spi-m)"),
    text = map_chr(path, \(p) str_squish(paste(read_lines(p), collapse = " ")))
  ) |>
  left_join(
    read_csv(file.path(spimo, "manifest.csv"), show_col_types = FALSE) |>
      select(statement = file, date),
    by = "statement"
  )

mentions <- statements |>
  mutate(sentence = str_split(text, "(?<=[.;:])\\s+(?=[A-Z])")) |>
  select(statement, type, date, sentence) |>
  unnest(sentence) |>
  cross_join(groups) |>
  filter(str_detect(sentence, pattern)) |>
  group_by(statement, sentence) |>
  filter(n() >= 3 | (n() >= 2 & str_detect(sentence, cue)) | str_detect(sentence, ack)) |>
  ungroup() |>
  transmute(date, statement, type, group, sentence = str_trunc(sentence, 500)) |>
  distinct() |>
  arrange(date, statement, group)

# Institution-like phrases in those sentences that the vocabulary does not know
institution <- "University of [A-Z][a-z]+|[A-Z][a-z]+ University|[A-Z][A-Za-z ]+ (Institute|Laboratory|Agency|Centre|College)[A-Za-z ]*"
known <- str_c(c(groups$pattern, not_counted), collapse = "|")
unknown <- mentions$sentence |>
  str_extract_all(institution) |>
  unlist() |>
  str_squish() |>
  unique() |>
  keep(\(x) !str_detect(x, known))
if (length(unknown)) {
  warning("Not in the vocabulary: ", str_c(unknown, collapse = "; "), call. = FALSE)
}

# first_emrg is the first acknowledgement; first can be an incidental mention
by_group <- mentions |>
  group_by(group) |>
  summarise(
    statements = n_distinct(statement),
    first = min(date),
    first_emrg = min(c(date[type == "emrg"], NA), na.rm = TRUE),
    last = max(date)
  ) |>
  arrange(desc(statements))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(mentions, file.path(out_dir, "statement-groups.csv"))
write_csv(by_group, file.path(out_dir, "statement-groups-summary.csv"))
message(n_distinct(mentions$statement), " of ", nrow(statements),
        " statements name groups; written to ", out_dir)
print(by_group, n = Inf)
