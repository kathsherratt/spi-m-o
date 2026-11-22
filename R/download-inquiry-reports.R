# Download UK Covid-19 Inquiry module reports (full reports and English
# in-brief summaries) and extract their text, one .txt per PDF with page
# markers. Skips files already on disk, so reruns only fetch new reports.
# Report index: https://covid19.public-inquiry.uk/reports/
# To add a new module, append its PDF link(s) to `reports` below.

library(here)
library(httr2)
library(pdftools)
library(purrr)
library(tibble)

base <- "https://covid19.public-inquiry.uk/wp-content/uploads/"

reports <- tribble(
  ~name, ~path,
  "module-1-full-report", "2024/07/18095012/UK-Covid-19-Inquiry-Module-1-Full-Report.pdf",
  "module-2-full-report-vol-1", "2026/02/12093057/M2-Volume-1-with-post-publication-amends-Feb-2026.pdf",
  "module-2-full-report-vol-2", "2025/11/18102920/1435-HH-E03296643_UK-Covid-19-M2-%E2%80%A2V2%E2%80%A2-Inquiry_Volume-II_Accessible.pdf",
  "module-3-full-report", "2026/03/14162519/14.5.26-E03361629_Covid-Inquiry-M3-Inquiry_accessible-w-Corrslip-2.pdf",
  "module-4-full-report", "2026/04/09151131/E03430128_Covid-Inquiry-M4-Inquiry_Accessible.pdf",
  "module-5-full-report", "2026/07/09094028/E03444965_Covid-Inquiry-M5-Inquiry_Web-Accessible.pdf",
  "module-6-full-report", "2026/10/19064857/M6_Report_Accessible.pdf",
  "module-7-full-report", "2026/11/18113244/M7_Report_Accessible.pdf",
  "module-1-in-brief", "2024/07/18095009/UK-Covid-19-Inquiry-Module-1-In-Brief-Report.pdf",
  "module-2-in-brief", "2025/11/18092508/M2-In-Brief-17.11.25-.pdf",
  "module-3-in-brief", "2026/03/14115053/5_03_25-Final-M3-In-Brief-typeset.pdf",
  "module-4-in-brief", "2026/04/09160407/M4-In-Brief-A.pdf",
  "module-5-in-brief", "2026/07/09090907/2026-07-08-M5-in-brief-English.pdf",
  "module-6-in-brief", "2026/10/19070006/26-10-08-M6-In-Brief-final.pdf",
  "module-7-in-brief", "2026/11/18091946/26-11-10-M7-In-Brief-Final.pdf"
) |>
  transform(url = paste0(base, path))

dir_pdf <- here("data", "inquiry", "module-reports", "pdf")
dir_txt <- here("data", "inquiry", "module-reports", "txt")
dir.create(dir_pdf, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_txt, recursive = TRUE, showWarnings = FALSE)

# Download one PDF; the site's CDN rejects requests without a browser user agent
download_report <- function(url, file) {
  if (file.exists(file)) return(invisible(file))
  request(url) |>
    req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7)") |>
    req_retry(max_tries = 3) |>
    req_perform(path = file)
  invisible(file)
}

# Extract text from one PDF, prefixing each page with its PDF page number
extract_text <- function(pdf, txt) {
  if (file.exists(txt)) return(invisible(txt))
  pages <- pdf_text(pdf)
  out <- paste0("=== PAGE ", seq_along(pages), " ===\n", pages)
  writeLines(out, txt, useBytes = TRUE)
  invisible(txt)
}

pwalk(reports, function(name, url, ...) {
  pdf <- file.path(dir_pdf, paste0(name, ".pdf"))
  txt <- file.path(dir_txt, paste0(name, ".txt"))
  message("Processing ", name)
  download_report(url, pdf)
  extract_text(pdf, txt)
})
