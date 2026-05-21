library(dplyr)
library(tibble)
library(httr2)
library(rvest)
library(xml2)
library(here)

url <- "https://www.gov.uk/api/search.json?q=spi-m-o"
resp <- httr2::request(url) |>
  httr2::req_perform()
url_content <- url |>
  read_html() |>
  html_elements(".govuk-link")

# Scrape pdf download links containing "SPI-M-O"
example <- "https://www.gov.uk/government/publications/spi-m-o-consensus-statement-on-covid-19-28-april-2021"

scrape_pdf_link <- function(page) {
  page_links <- page |>
    read_html() |>
    html_elements(".govuk-link")

  page_content <- page_links |>
    html_attr("href")

  names(page_content) <- html_text2(page_links)

  page_pdf <- enframe(page_content) |>
    filter(grepl("pdf$", value) &
             grepl("SPI-M-O", name, ignore.case = TRUE)) |>
    deframe()

  return(page_pdf)
}

pdf_links <- scrape_pdf_link(example)
for (pdf in pdf_links) {
  dest <- here("download", paste0(names(pdf), ".pdf"))
  download.file(url = pdf, destfile = dest)
}

# ------
# Scrape PDFs from a gov.uk publication page via Content API
library(purrr)
library(rlang)

fetch_govuk_pdfs <- function(path) {
  url <- paste0("https://www.gov.uk/api/content/", path)
  resp <- httr2::request(url) |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  attachments <- resp$details$attachments
  if (is.null(attachments) || length(attachments) == 0) {
    return(tibble(title = character(), url = character()))
  }

  map_dfr(attachments, \(a) tibble(
    title = a$title %||% NA_character_,
    url = a$url %||% NA_character_,
    content_type = a$content_type %||% NA_character_
  )) |>
    filter(content_type == "application/pdf" | grepl("\\.pdf$", url))
}

pdfs <- fetch_govuk_pdfs(
  "government/publications/consensus-statements-on-covid-19"
)

# Optional download
dir.create(here("download"), showWarnings = FALSE)
walk2(pdfs$url, pdfs$title, \(u, t) {
  dest <- here("download", paste0(gsub("[^A-Za-z0-9_-]+", "_", t), ".pdf"))
  download.file(u, dest, mode = "wb")
})
