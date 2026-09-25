# ai-input
# Index all SPI-M-O publications on gov.uk, then download the SPI-M-O and
# EMRG consensus statements and extract their text.
#
# Outputs, under data/covid19/consensus/spimo/:
#   spimo-index.csv  every SPI-M(-O) publication in the SAGE collection
#   manifest.csv     one row per downloaded consensus PDF, with SHA-256
#   pdf/             source PDFs (git-ignored; regenerate with this script)
#   txt/             extracted text, one file per PDF, pages split by \f
#
# Reruns skip PDFs already on disk, so only the gov.uk metadata calls repeat.
library(dplyr)
library(purrr)
library(httr2)
library(pdftools)
library(here)
Sys.setlocale("LC_TIME", "C") # English month names for title dates

out_dir <- here("data", "covid19", "consensus", "spimo")
dir.create(file.path(out_dir, "pdf"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "txt"), showWarnings = FALSE)

ua <- "spi-m-o-research (LSHTM; github.com/kathsherratt/spi-m-o)"

govuk_request <- function(url) {
  request(url) |>
    req_user_agent(ua) |>
    req_throttle(capacity = 5, fill_time_s = 1) |>
    req_retry(max_tries = 4)
}

# Page through every SAGE-organisation document on the gov.uk Search API.
# Filtering by organisation, not q=, gives a finite, complete set (~1,150);
# SPI-M-O documents are then selected by title.
fetch_sage_index <- function(count = 500L) {
  fetch_page <- function(start) {
    govuk_request("https://www.gov.uk/api/search.json") |>
      req_url_query(
        filter_organisations = "scientific-advisory-group-for-emergencies",
        count = count, start = start,
        fields = c("title", "link", "public_timestamp", "format"),
        .multi = "explode"
      ) |>
      req_perform() |>
      resp_body_json()
  }
  first <- fetch_page(0L)
  starts <- seq(count, max(first$total - 1L, 0L), by = count)
  starts <- starts[starts < first$total]
  results <- c(first$results, flatten(map(starts, \(s) fetch_page(s)$results)))
  map_dfr(results, \(r) tibble(
    title = r$title,
    path = r$link,
    published = as.Date(substr(r$public_timestamp %||% NA_character_, 1, 10)),
    format = r$format %||% NA_character_
  )) |>
    distinct(path, .keep_all = TRUE)
}

# File attachments of one gov.uk publication, via the Content API. HTML
# attachments are dropped: every one found so far duplicates a PDF.
fetch_attachments <- function(path) {
  resp <- govuk_request(paste0("https://www.gov.uk/api/content", path)) |>
    req_perform() |>
    resp_body_json()
  map_dfr(resp$details$attachments %||% list(), \(a) tibble(
    path = path,
    attachment_title = a$title %||% NA_character_,
    attachment_type = a$attachment_type %||% NA_character_,
    url = a$url %||% NA_character_
  ))
}

# Statement date is the meeting date in the title, not the publication date
# (often weeks or months later)
title_date <- function(title) {
  m <- regexpr("\\d{1,2} [A-Z][a-z]+ \\d{4}", title)
  out <- rep(NA_character_, length(title))
  out[m > 0] <- regmatches(title, m)
  as.Date(out, format = "%d %B %Y")
}

# Index ---------------------------------------------------------------------
sage <- fetch_sage_index()
spimo <- sage |>
  filter(grepl("SPI-M", title, ignore.case = TRUE) | grepl("^EMRG", title)) |>
  mutate(
    group = if_else(grepl("^EMRG", title), "EMRG", "SPI-M-O"),
    date = title_date(title),
    slug = basename(path)
  ) |>
  arrange(date)

attachments <- map_dfr(spimo$path, fetch_attachments)

spimo <- spimo |>
  left_join(
    attachments |>
      summarise(
        n_pdf = sum(attachment_type == "file" & grepl("\\.pdf$", url)),
        n_html = sum(attachment_type == "html"),
        consensus_attachment = any(grepl("consensus", attachment_title, ignore.case = TRUE)),
        .by = path
      ),
    by = "path"
  ) |>
  mutate(consensus = grepl("consensus", title, ignore.case = TRUE) | consensus_attachment %in% TRUE)

write.csv(select(spimo, -consensus_attachment), file.path(out_dir, "spimo-index.csv"), row.names = FALSE)

# Download consensus PDFs ---------------------------------------------------
to_get <- attachments |>
  filter(path %in% spimo$path[spimo$consensus], attachment_type == "file", grepl("\\.pdf$", url)) |>
  left_join(select(spimo, path, slug, title, group, date, published), by = "path") |>
  mutate(file = if (n() == 1) slug else paste0(slug, "__", row_number()), .by = path)

# EMRG statements from Jul 2021 are attachments on one collection page, not
# separate publications, so the title search above finds only two of them
emrg_path <- "/government/publications/consensus-statements-on-covid-19"
emrg <- fetch_attachments(emrg_path) |>
  mutate(attachment_title = trimws(attachment_title)) |>
  filter(
    attachment_type == "file", grepl("\\.pdf$", url),
    grepl("^EMRG consensus statement", attachment_title, ignore.case = TRUE)
  ) |>
  mutate(
    group = "EMRG",
    date = title_date(attachment_title),
    title = attachment_title,
    published = as.Date(NA),
    slug = paste0("emrg-consensus-statement-on-covid-19-", date),
    file = slug
  )

# Keep the standalone publication where both exist
to_get <- bind_rows(to_get, anti_join(emrg, to_get, by = c("group", "date")))

for (i in seq_len(nrow(to_get))) {
  dest <- file.path(out_dir, "pdf", paste0(to_get$file[i], ".pdf"))
  if (!file.exists(dest) || file.size(dest) < 1000) {
    govuk_request(to_get$url[i]) |> req_perform(path = dest)
  }
}

# Extract text --------------------------------------------------------------
manifest <- to_get |>
  mutate(
    pdf = file.path(out_dir, "pdf", paste0(file, ".pdf")),
    bytes = file.size(pdf),
    sha256 = map_chr(pdf, \(f) digest::digest(file = f, algo = "sha256")),
    pages = map_int(pdf, \(f) pdf_info(f)$pages)
  )

walk2(manifest$pdf, manifest$file, \(f, stem) {
  writeLines(
    paste(pdf_text(f), collapse = "\f"),
    file.path(out_dir, "txt", paste0(stem, ".txt")),
    useBytes = TRUE
  )
})

manifest |>
  select(file, group, date, published, title, attachment_title, url, bytes, pages, sha256) |>
  write.csv(file.path(out_dir, "manifest.csv"), row.names = FALSE)

# Coverage check: statements per month, to spot gaps
manifest |>
  count(group, month = format(date, "%Y-%m")) |>
  print(n = Inf)
