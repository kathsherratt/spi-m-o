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
