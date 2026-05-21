
library(pdftools)
download.file("https://assets.publishing.service.gov.uk/media/627e82698fa8f53f93a4ae98/2022-04-27_EMRG_Consensus_Statement.pdf", 
"consensus", "emrg", "2022-04-27.pdf", mode = "wb")

txt <- pdf_text("1403.2805.pdf")

# first page text
cat(txt[1])

# second page text
cat(txt[2])