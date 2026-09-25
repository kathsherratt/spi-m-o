# ai-input
# Download the SPI-M-O statements and SAGE minutes from the 2009 H1N1
# pandemic and extract their text. The DH pages survive only in the UK
# Government Web Archive.
#
# Outputs, under data/h1n1/consensus/spimo/ and data/h1n1/sage/minutes/:
#   manifest.csv  one row per PDF, with SHA-256
#   pdf/          source PDFs (git-ignored; regenerate with this script)
#   txt/          extracted text, one file per PDF, pages split by \f
#
# Reruns skip PDFs already on disk, so only the index page calls repeat.
#
# SPI-M-O link labels are shifted by one meeting from 30_04 to 11_06: each
# link serves the statement from the following meeting (the "30_04 risk
# assessment" link serves the 16 May consensus statement, the 11_06 link the
# 18 June one). So 11_06 and 18_06 both carry the 18 June statement, every
# meeting from 14 May to 18 June is covered, and the 30 April risk assessment
# is not in the archive. Use doc_date, read from the PDF header, as the
# statement date; date is the label on the page.
library(dplyr)
library(purrr)
library(httr2)
library(rvest)
library(pdftools)
library(here)
Sys.setlocale("LC_TIME", "C") # English month names for dates

ua <- "spi-m-o-research (LSHTM; github.com/kathsherratt/spi-m-o)"
archive <- "https://webarchive.nationalarchives.gov.uk"

# "mp_" serves the captured page itself, not the archive's iframe wrapper
capture_url <- function(timestamp, dh_path) {
  paste0(archive, "/ukgwa/", timestamp, "mp_/http://www.dh.gov.uk/", dh_path)
}

archive_request <- function(url) {
  request(url) |>
    req_user_agent(ua) |>
    req_throttle(capacity = 1, fill_time_s = 1) |>
    req_retry(max_tries = 4)
}

# PDF links on one archived DH page, as link text and absolute URL
fetch_pdf_links <- function(url) {
  links <- archive_request(url) |>
    req_perform() |>
    resp_body_html() |>
    html_elements("a[href$='.pdf']")
  tibble(
    title = trimws(sub("\\(PDF, [0-9]+K\\)$", "", sub("^Download ", "", html_text(links)))),
    url = paste0(archive, html_attr(links, "href"))
  ) |>
    distinct(url, .keep_all = TRUE)
}

# Link text carries SPI-M-O meeting dates as dd_mm_yyyy
underscore_date <- function(x) as.Date(x, format = "%d_%m_%Y")

# Date as printed in the document header, which can be a day or two after
# the meeting date in the link
header_date <- function(text) {
  m <- regexpr("\\d{1,2}(st|nd|rd|th)? [A-Z][a-z]+ \\d{4}", text)
  out <- rep(NA_character_, length(text))
  out[m > 0] <- sub("(st|nd|rd|th) ", " ", regmatches(text, m))
  as.Date(out, format = "%d %B %Y")
}

# Download each PDF in to_get (needs file, url), extract text to txt/, and
# return to_get with doc_date, bytes, pages and sha256 added
download_extract <- function(to_get, out_dir) {
  stopifnot(!anyDuplicated(to_get$file))
  dir.create(file.path(out_dir, "pdf"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "txt"), showWarnings = FALSE)
  pdf <- file.path(out_dir, "pdf", paste0(to_get$file, ".pdf"))
  for (i in seq_along(pdf)) {
    if (!file.exists(pdf[i]) || file.size(pdf[i]) < 1000) {
      archive_request(to_get$url[i]) |> req_perform(path = pdf[i])
    }
  }
  text <- map(pdf, pdf_text)
  walk2(text, to_get$file, \(txt, stem) {
    writeLines(
      paste(txt, collapse = "\f"),
      file.path(out_dir, "txt", paste0(stem, ".txt")),
      useBytes = TRUE
    )
  })
  to_get |>
    mutate(
      doc_date = header_date(map_chr(text, 1)),
      bytes = file.size(pdf),
      pages = lengths(text),
      sha256 = map_chr(pdf, \(f) digest::digest(file = f, algo = "sha256"))
    )
}

# SPI-M-O statements --------------------------------------------------------
spimo_dir <- here("data", "h1n1", "consensus", "spimo")

spimo <- fetch_pdf_links(capture_url("20110609133736", "ab/SPI/DH_118862")) |>
  mutate(
    type = case_when(
      grepl("Consensus Statement", title) ~ "consensus",
      grepl("Interpretive Statement", title) ~ "interpretive",
      grepl("Future development", title) ~ "future-development",
      grepl("Risk Assessment", title) ~ "risk-assessment",
      grepl("Update Statement", title) ~ "update",
      grepl("Lessons Learnt", title) ~ "lessons-learnt"
    ),
    date = underscore_date(if_else(
      grepl("\\d{2}_\\d{2}_\\d{4}", title),
      sub("^.*?(\\d{2}_\\d{2}_\\d{4}).*$", "\\1", title, perl = TRUE),
      NA_character_
    )),
    revised = underscore_date(if_else(
      grepl("revised on", title),
      sub(".*revised on (\\d{2}_\\d{2}_\\d{4}).*", "\\1", title),
      NA_character_
    )),
    group = "SPI-M-O",
    file = paste0(
      "h1n1-spimo-", type,
      if_else(is.na(date), "", paste0("-", date)),
      if_else(is.na(revised), "", paste0("-revised-", revised))
    )
  )
stopifnot(!anyNA(spimo$type))

spimo <- download_extract(spimo, spimo_dir)
spimo |>
  arrange(date, type) |>
  select(file, group, type, date, revised, doc_date, title, url, bytes, pages, sha256) |>
  write.csv(file.path(spimo_dir, "manifest.csv"), row.names = FALSE)

# SAGE minutes --------------------------------------------------------------
# Redacted minutes of the 22 SAGE meetings on H1N1, May 2009 to Jan 2010
sage_dir <- here("data", "h1n1", "sage", "minutes")

sage <- fetch_pdf_links(capture_url("20120907150455", "ab/SPI/DH_120535")) |>
  mutate(
    date = as.Date(sub(".*held on (\\d{1,2} [A-Z][a-z]+ \\d{4}).*", "\\1", title), format = "%d %B %Y"),
    group = "SAGE",
    type = "minutes",
    file = paste0("h1n1-sage-minutes-", date)
  )
stopifnot(!anyNA(sage$date))

sage <- download_extract(sage, sage_dir)
sage |>
  arrange(date) |>
  select(file, group, type, date, doc_date, title, url, bytes, pages, sha256) |>
  write.csv(file.path(sage_dir, "manifest.csv"), row.names = FALSE)

# Coverage check: documents per month, to spot gaps
bind_rows(spimo, sage) |>
  count(group, type, month = format(date, "%Y-%m")) |>
  print(n = Inf)
