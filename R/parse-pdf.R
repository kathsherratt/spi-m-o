library(here)
library(pdftools)

file_target <- here(
    "data", "covid19", "consensus",
    "emrg", "2022-04-27.pdf"
)

download.file(
    url = "https://assets.publishing.service.gov.uk/media/627e82698fa8f53f93a4ae98/2022-04-27_EMRG_Consensus_Statement.pdf",
    destfile = file_target,
    mode = "wb"
)

txt <- pdf_text(file_target)

txt_yardstick <- purrr::map_dfr(
    tolower(names(yardstick)),
    ~ grepl(.x, tolower(txt))
)


"https://www.gov.uk/government/publications/spi-m-o-consensus-statement-on-covid-19-8-december-2021/spi-m-o-consensus-statement-on-covid-19-8-december-2021"
