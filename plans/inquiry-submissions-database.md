# Plan: database of Covid-19 Inquiry submissions from SAGE participants

Status: plan, not yet executed. Code chunks below are untested sketches.
Written 2026-10-29.

## Goal

Build a tidy, reusable record of evidence submitted to the UK Covid-19 Inquiry by participants in SAGE and its sub-groups, tabulated by author, group and module.
Where feasible, extract the documents each person says they contributed to SAGE advice (Rule 9 question 4), to give a person-to-document network that links to the wider SPI-M evaluation work in this repo (see `notebook/pages/sources.qmd`, "Impact tracing").

## Scope

In scope (first pass):

- Rule 9 questionnaire responses from participants of SAGE and all its sub-groups:
  SAGE, SPI-M-O, SPI-B, NERVTAG, EMG, SCWG (Social Care Working Group), TFC (Children's Task and Finish Group), Ethnicity sub-group, and any other sub-group that appears in titles.
- All modules, though in practice the batch is Module 2.

Extensions (after first pass, see step 10):

- Witness statements and oral evidence from the same people in any module.
- Devolved advisory groups (Welsh Technical Advisory Group/Cell, Scottish advisory group), which have their own Rule 9 batch under Modules 2A to 2C.

## What we know already

- The Rule 9 responses were published as one batch on 18 December 2023, typed "Evidence", Module 2.
- Inquiry IDs cluster in roughly `INQ000056376` to `INQ000056608`.
- Inquiry web pages are reachable as `https://covid19.public-inquiry.uk/?p=<post id>`, and the batch sits in post IDs roughly 21494 to 22017 (observed; edges unconfirmed).
- PDFs live at `wp-content/uploads/2023/12/<timestamp>/INQ0000564xx.pdf`. The timestamp folder varies, so URLs must be read from each document page, not built.
- Each response answers a request referenced `M2/SAGE/01/<initials>` (e.g. `M2/SAGE/01/EBP`, `M2/SAGE/01/SXF`). In PDF text this can appear with stray spaces (`M 2/SAG E/01/E B P`).
- Titles follow a loose pattern with inconsistent punctuation and some typos, e.g.
  - `Rule 9 Questionnaire Response from Dr Leon Danon, Scientific Pandemic Influenza Group on Modelling (SPI-M-O); Social Care Working Group (SCWG), dated 09/11/2022.`
  - `Rule 9 Questionnaire Response from Dr Rosanna Barnard Scientific Pandemic Influenza Group on Modelling (SPI-M-O); ...` (no comma after name)
  - `... Techincal Advisory Group/Cell, undated.` (typo in source)
- The groups in the title can differ from the groups the author lists in Q2 (e.g. Brooks-Pollock's title gives SPI-B and SPI-M-O, while her Q2 also lists the Children's Task and Finish Group). Store both.

### The eight Rule 9 questions

1. Qualifications, career history, expertise and major publications
2. List of SAGE groups/sub-groups participated in, with time periods
3. Overview of involvement January 2020 to February 2022
4. Summary of documents contributed to advise SAGE and/or sub-groups, with links where possible
5. Articles, interviews and/or evidence given about the groups' work or the UK response
6. Views on whether the groups' work succeeded in its aims
7. Lessons learned, changes already made, recommendations
8. Documentation held

### Q4 format (from INQ000056404, Brooks-Pollock)

Free text with topic headings (e.g. "Vaccine rollout"), short prose under each, then numbered entries (1 to 29), each with a title, usually a date, and a URL on following lines.
Dates are sometimes year-only or missing; SAGE meeting numbers appear only in some titles or file names (e.g. "SAGE 43", `sage26`); some entries say the document is not online.
Other responses may be prose only, so expect partial structure.

## Constraints

- The Inquiry site refused connections from the Claude cloud workspace (proxy 403), so the crawl must run locally (Positron).
- Be polite: throttle to 1 request per second, cache every page and PDF to disk so the crawl is resumable and runs once.
- Personal data is redacted in the PDFs ("Personal Data" placeholders). Keep it that way; do not attempt to recover redactions.
- PDFs: ~250 files would add substantially to the repo (already ~151 MB in `data/`). Default: cache PDFs locally and gitignore them; commit only the CSVs.

## Outputs

All under `data/covid19/inquiry/rule9/`:

| File | One row per | Key columns |
|---|---|---|
| `pages.csv` | crawled post ID | `post_id`, `url`, `title`, `pdf_url`, `published`, `module`, `type` |
| `documents.csv` | Rule 9 response | `inq_id`, `post_id`, `author`, `author_key`, `date`, `undated`, `rule9_ref`, `module`, `n_pages`, `pdf_url`, `text_ok` |
| `document_groups.csv` | response x group | `inq_id`, `group`, `source` (`title` or `q2`) |
| `people.csv` | person | `author_key`, `name`, `on_participants_list`, `orcid` (later) |
| `sections.csv` | response x question | `inq_id`, `question` (1 to 8), `text`, `split_method` |
| `q4_entries.csv` | Q4 entry | `inq_id`, `entry_n`, `topic`, `title`, `date`, `date_precision`, `url`, `doi`, `sage_meeting`, `doc_group`, `parse_status` |
| `q4_matches.csv` | Q4 entry x canonical doc | `inq_id`, `entry_n`, `govuk_url`, `match_method`, `match_score` |

Cache (gitignored): `data/covid19/inquiry/rule9/cache/html/`, `.../cache/pdf/`.

## Steps

- [ ] 0. Set up
- [ ] 1. Probe the site
- [ ] 2. Crawl document pages
- [ ] 3. Filter and parse titles
- [ ] 4. Check coverage against the participants list
- [ ] 5. Download PDFs and extract text
- [ ] 6. Split responses into questions
- [ ] 7. Extract Q4 entries
- [ ] 8. Validate
- [ ] 9. Write up
- [ ] 10. Expand

### 0. Set up

Create `R/inquiry-rule9.R` (or split into one script per step) and add the cache folders to `.gitignore`.

```r
# Packages for crawling, parsing and text extraction
library(tidyverse)
library(httr2)
library(rvest)
library(pdftools)
library(here)

# Folders for outputs and the (gitignored) cache
out_dir <- here("data", "covid19", "inquiry", "rule9")
html_dir <- file.path(out_dir, "cache", "html")
pdf_dir <- file.path(out_dir, "cache", "pdf")
walk(c(html_dir, pdf_dir), \(d) dir.create(d, recursive = TRUE, showWarnings = FALSE))
```

```
# .gitignore addition
data/covid19/inquiry/rule9/cache/
```

### 1. Probe the site

First check whether the site exposes the WordPress REST API.
If it does, it is a faster and more complete route than scraping HTML (and makes the step 10 full catalogue cheap).

```r
# Check for a WordPress API; 200 means usable
probe <- request("https://covid19.public-inquiry.uk/wp-json/wp/v2/types") |>
  req_error(is_error = \(r) FALSE) |>
  req_perform()
resp_status(probe)
```

If it returns 200, inspect the types for the documents post type and page through `wp-json/wp/v2/<type>?search=Rule%209&per_page=100&page=<n>` instead of step 2.
If not, continue with step 2.

Also open one cached page (e.g. post 22017, Funk) in a browser and confirm the CSS selectors used in step 2 (title element, PDF link, published/module fields).

### 2. Crawl document pages

Crawl a padded range around the observed batch, caching raw HTML.
Padding to 21400 to 22200 is ~800 requests, about 15 minutes at 1 per second.
Check the hits at both edges; extend the range if Rule 9 responses appear near either end.

```r
# Fetch one post ID, caching raw HTML; returns final URL and status
fetch_page <- function(id) {
  path <- file.path(html_dir, paste0(id, ".html"))
  if (file.exists(path)) {
    return(tibble(post_id = id, status = 200L, path = path))
  }
  resp <- request("https://covid19.public-inquiry.uk/") |>
    req_url_query(p = id) |>
    req_user_agent("spi-m-o research project") |>
    req_throttle(rate = 1) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  status <- resp_status(resp)
  if (status == 200) writeLines(resp_body_string(resp), path)
  tibble(post_id = id, status = status, url = resp$url,
         path = if (status == 200) path else NA_character_)
}

crawl <- map(21400:22200, fetch_page, .progress = TRUE) |>
  list_rbind()
```

```r
# Read title, PDF link and metadata from a cached page (check selectors first)
parse_page <- function(path) {
  html <- read_html(path)
  meta <- html |> html_elements("li") |> html_text2()
  tibble(
    title = html |> html_element("h1") |> html_text2(),
    pdf_url = html |> html_element("a[href$='.pdf']") |> html_attr("href"),
    published = str_subset(meta, "^Published") |> first(),
    module = str_subset(meta, "^Modules?:") |> first(),
    type = str_subset(meta, "^Type") |> first()
  )
}

pages <- crawl |>
  filter(!is.na(path)) |>
  mutate(parsed = map(path, parse_page)) |>
  unnest(parsed)

write_csv(pages, file.path(out_dir, "pages.csv"))
```

### 3. Filter and parse titles

Detect groups from a lookup of loose patterns rather than splitting on punctuation, because titles are inconsistent and contain typos.

```r
# Loose patterns for each SAGE group (and devolved groups, kept for later)
group_lookup <- tribble(
  ~group,      ~pattern,
  "SAGE",      "Scientific Advisory Group [Ff]or Emergenc|\\(SAGE\\)",
  "SPI-M-O",   "Pandemic Influenza Group on Modell?ing|SPI-?M",
  "SPI-B",     "Pandemic Insights Group on Behaviou?rs|SPI-?B",
  "NERVTAG",   "New and Emerging Respiratory Virus|NERVTAG",
  "EMG",       "Environmental Modell?ing Group|\\(EMG\\)",
  "SCWG",      "Social Care Working Group|SCWG",
  "TFC",       "Children.s Task and Finish|\\(TFC\\)",
  "Ethnicity", "Ethnicity Sub-?group",
  "TAG/TAC",   "Tech[a-z]*cal Advisory (Group|Cell)"
)

all_groups <- paste(group_lookup$pattern, collapse = "|")
```

```r
# Keep Rule 9 responses and parse ID, author, date
documents <- pages |>
  filter(str_detect(title, "Rule 9 Questionnaire Response")) |>
  mutate(
    inq_id = str_extract(title, "INQ\\d{9}"),
    date = str_extract(title, "\\d{2}/\\d{2}/\\d{4}") |> dmy(),
    undated = str_detect(title, "undated"),
    author = title |>
      str_remove("^.*Questionnaire Response from\\s+") |>
      str_split_i(all_groups, 1) |>
      str_remove("[,;\\s]+$"),
    # Matching key: drop honorifics, punctuation and case
    author_key = author |>
      str_remove_all("\\b(Dr|Professor|Prof|Sir|Dame|Mr|Mrs|Ms)\\b\\.?") |>
      str_replace_all("[^A-Za-z ]", " ") |>
      str_squish() |>
      str_to_lower()
  )
```

```r
# One row per response x group, from the title
document_groups <- documents |>
  select(inq_id, title) |>
  cross_join(group_lookup) |>
  filter(str_detect(title, pattern)) |>
  transmute(inq_id, group, source = "title")
```

Note `str_split_i()` with a regex splits at the first group name, which is where the author name ends.
Check every author by eye at this stage; it is a short list.

### 4. Check coverage against the participants list

Use the GOV.UK Content API (as in `R/scrape-spimo.R`) on the SAGE participants publication:
`government/publications/scientific-advisory-group-for-emergencies-sage-coronavirus-covid-19-response-membership`.
The list is likely an HTML attachment: fetch the publication, follow the attachment, and parse names (and groups, if given) from its body.
The page was revised several times; use the latest version, and optionally earlier versions from the page history to catch people later removed.

Then compare both ways on `author_key`:

- On the list but no response found: likely not sent a request, or published elsewhere. Search by name on the Inquiry site before concluding.
- Response found but not on the list: check spelling and name forms.

Write `people.csv` with an `on_participants_list` flag.

### 5. Download PDFs and extract text

```r
# Download each PDF once into the cache
pdf_path <- \(inq_id) file.path(pdf_dir, paste0(inq_id, ".pdf"))

documents |>
  filter(!file.exists(pdf_path(inq_id))) |>
  pwalk(\(pdf_url, inq_id, ...) {
    download.file(pdf_url, pdf_path(inq_id), mode = "wb")
    Sys.sleep(1)
  })
```

```r
# Extract text per page; flag likely scans (very little text) for OCR
documents <- documents |>
  mutate(
    pages_text = map(inq_id, \(id) pdf_text(pdf_path(id))),
    n_pages = map_int(pages_text, length),
    text_ok = map_lgl(pages_text, \(p) mean(nchar(p)) > 200),
    full_text = map_chr(pages_text, \(p) paste(p, collapse = "\n")),
    # Rule 9 reference, allowing stray spaces from PDF extraction
    rule9_ref = full_text |>
      str_extract("M\\s*2\\s*/\\s*S\\s*A\\s*G\\s*E\\s*/\\s*0\\s*1\\s*/\\s*[A-Z](\\s?[A-Z])+") |>
      str_remove_all("\\s")
  )
```

For responses with `text_ok == FALSE`, use `pdftools::pdf_ocr_text()` (needs the `tesseract` package) and keep a `text_source` column (`pdf` or `ocr`).

### 6. Split responses into questions

Do not split on numbered lines alone: Q4 lists are themselves numbered 1 to 29, so a naive split on `^1.`, `^2.` breaks inside Q4.
Locate each question by a distinctive phrase from its wording, in order, and fall back to manual review.

```r
# Distinctive phrases that open each question
question_stems <- c(
  "qualifications, career history",
  "list of the groups",
  "overview of your involvement",
  "summary of any documents",
  "articles you have written",
  "succeeded in its aims",
  "lessons that can be learned",
  "description of documentation"
)

# Find each stem's position and cut the text between consecutive stems
split_sections <- function(text) {
  starts <- map_int(question_stems, \(s) {
    hit <- str_locate(str_to_lower(text), s)[1, "start"]
    if (is.na(hit)) NA_integer_ else as.integer(hit)
  })
  tibble(question = 1:8, start = starts) |>
    filter(!is.na(start)) |>
    arrange(start) |>
    mutate(
      end = lead(start, default = nchar(text) + 1L) - 1L,
      text = str_sub(text, start, end),
      split_method = "stem"
    )
}

sections <- documents |>
  select(inq_id, full_text) |>
  mutate(s = map(full_text, split_sections)) |>
  select(-full_text) |>
  unnest(s)

# Responses where some questions were not found need a look
sections |> count(inq_id) |> filter(n < 8)
```

Many respondents will have restated the question text (Brooks-Pollock did), which makes this work; others may answer under bare numbers.
For those, review by hand and record `split_method = "manual"`.
Optionally add a Q2-derived group list to `document_groups.csv` by running the group lookup over the Q2 section (`source = "q2"`).

### 7. Extract Q4 entries

Feasibility: good for numbered or bulleted lists (as in Brooks-Pollock), weak for prose answers.
Record `parse_status` so the gaps are visible: `list`, `prose`, `empty`, `manual`.

```r
# Split Q4 text into entries: a new entry starts at "12." / "12)" / a bullet;
# topic headings (short lines without a number or URL) carry forward
parse_q4 <- function(text) {
  lines <- str_split_1(text, "\n") |> str_squish()
  lines <- lines[lines != ""]
  is_entry <- str_detect(lines, "^(\\d{1,3}[.)]|[•●▪–-])\\s")
  if (sum(is_entry) == 0) {
    return(tibble(parse_status = "prose", raw = text))
  }
  tibble(line = lines, is_entry = is_entry) |>
    mutate(
      entry_id = cumsum(is_entry),
      is_heading = !is_entry & nchar(line) < 80 &
        !str_detect(line, "https?://|doi") & entry_id >= 0
    ) |>
    filter(entry_id > 0) |>
    summarise(raw = paste(line, collapse = " "), .by = entry_id) |>
    mutate(
      entry_n = str_extract(raw, "^\\d{1,3}") |> as.integer(),
      parse_status = "list"
    )
}
```

```r
# Pull structured fields out of each entry
q4_entries <- sections |>
  filter(question == 4) |>
  mutate(e = map(text, parse_q4)) |>
  select(inq_id, e) |>
  unnest(e) |>
  mutate(
    # Rejoin URLs broken across lines before extracting
    raw_joined = str_replace_all(raw, "(https?://\\S+)\\s(?=[\\w/.-]+(\\.pdf|/))", "\\1"),
    url = str_extract(raw_joined, "https?://\\S+") |> str_remove("[).,;:]+$"),
    doi = str_extract(raw_joined, "10\\.\\d{4,9}/\\S+") |> str_remove("[).,;:]+$"),
    date_text = str_extract(raw, "\\d{1,2}(st|nd|rd|th)?\\s+[A-Z][a-z]+\\s+\\d{4}|\\d{1,2}/\\d{1,2}/\\d{4}"),
    date = dmy(date_text, quiet = TRUE),
    date_precision = case_when(
      !is.na(date) ~ "day",
      str_detect(raw, "\\b(19|20)\\d{2}\\b") ~ "year",
      .default = "none"
    ),
    sage_meeting = coalesce(
      str_extract(raw, "SAGE\\s*(\\d{1,3})", group = 1),
      str_extract(url, "(?i)sage[_-]?(\\d{1,3})", group = 1)
    ) |> as.integer(),
    title = raw |>
      str_remove("^(\\d{1,3}[.)]|[•●▪–-])\\s*") |>
      str_remove("https?://.*$") |>
      str_squish()
  )
```

Then tag `doc_group` with the same `group_lookup` patterns applied to each entry title.
The topic-heading carry-forward is sketched only (the `is_heading` flag); finish it once real examples are in hand.

Linking to canonical documents (`q4_matches.csv`):

1. Build a catalogue of SAGE evidence on GOV.UK with `fetch_spimo_search()` from `R/scrape-spimo.R`, generalised by removing the SPI-M-O title filter so it returns all SAGE-organisation publications.
2. Match on URL first (normalise: lower case, drop query strings and trailing slashes; map `assets.publishing.service.gov.uk` attachment URLs to their publication pages via the Content API where possible).
3. Then fuzzy title plus date match (e.g. `stringdist` Jaro-Winkler on titles, within 14 days), keeping `match_score` and reviewing anything below a threshold.
4. DOIs point to papers rather than SAGE papers; keep them as a separate link type.

Option for `prose` responses: LLM-assisted extraction of entries into the same columns, with every extracted row checked by hand against the PDF and marked `parse_status = "manual"`.
Decide whether to use this after seeing how many responses are prose (see open questions).

### 8. Validate

Known values to test against (from manual reading on 2026-10-29):

| inq_id | Author | Date | Title groups | Rule 9 ref | Other |
|---|---|---|---|---|---|
| INQ000056404 | Ellen Brooks-Pollock | 2022-10-03 | SPI-B, SPI-M-O | M2/SAGE/01/EBP | 9 pages; Q4 has 29 entries; entry 1 "SPI-M-O Concensus Statement on public gatherings 11 March 2020" |
| INQ000056416 | Leon Danon | 2022-11-09 | SPI-M-O, SCWG | | |
| INQ000056453 | Sebastian Funk | undated | SAGE, SPI-M-O, NERVTAG | M2/SAGE/01/SXF | 4 pages |

Also:

- Counts by group and by date; any response with no group or no author.
- Each Rule 9 reference unique; initials consistent with author name.
- Random sample of 10 responses: compare `sections` and `q4_entries` against the PDF by eye, and record error rates in the write-up.

### 9. Write up

Add `notebook/pages/inquiry-submissions.qmd` (it will be picked up by `_quarto.yml`) with:

- Method summary and known gaps (missing responses, prose Q4s, OCR).
- Tables: responses per group; people with no published response.
- Person-to-document network from `q4_matches.csv` (who contributed to which SAGE papers, by group and over time).

Render it locally and commit `_freeze/` (CI has no R; see `README.md`).

### 10. Expand

In rough order of value for evaluating SPI-M:

1. **Witness statements by the same people, any module.** If the WordPress API works (step 1), pull the full document catalogue and filter titles for `author_key` names. Otherwise use the site's own search by name. Module 1 statements already in `data/covid19/inquiry/module-1/` (Keeling, Edmunds, Ferguson, Medley draft) seed this.
2. **Oral evidence.** Search the independent transcript index (<https://ukcovid19inquiry.dracos.co.uk/search/>) by name for appearances and mentions.
3. **Exhibits.** SAGE and SPI-M-O papers adduced as evidence (e.g. SPI-M-O consensus statements, which appear with their own INQ IDs) can be linked to the same canonical catalogue.
4. **Devolved advisory groups.** Rule 9 responses from the Welsh Technical Advisory Group/Cell (e.g. `?p=28934`, `?p=28991`) are a separate batch; crawl that range with the same code.

## Open questions

- Commit PDFs, or cache locally only (default here: local only)?
- Use LLM-assisted extraction for prose Q4 answers, with manual checking, or leave them as `prose`?
- Keep full text of Q6 and Q7 (views and lessons) in `sections.csv` for later analysis of recommendations? (Default: yes, it is public.)

## Sources

- Rule 9 response examples: [INQ000056404 Brooks-Pollock (PDF)](https://covid19.public-inquiry.uk/wp-content/uploads/2023/12/18145719/INQ000056404.pdf), [INQ000056416 Danon (PDF)](https://covid19.public-inquiry.uk/wp-content/uploads/2023/12/18145720/INQ000056416.pdf), [INQ000056453 Funk (PDF)](https://covid19.public-inquiry.uk/wp-content/uploads/2023/12/18145722/INQ000056453.pdf)
- Range and title-format examples: [Hill ?p=21494](https://www.covid19.public-inquiry.uk/?p=21494), [Presanis INQ000056376](https://www.covid19.public-inquiry.uk/?p=21610), [Barnard INQ000056443](https://covid19.public-inquiry.uk/?p=21931), [Pokhrel INQ000056608](https://www.covid19.public-inquiry.uk/?p=21869), [Funk ?p=22017](https://covid19.public-inquiry.uk/tl/?p=22017), [Komianos (TAC)](https://www.covid19.public-inquiry.uk/?p=28991)
- [SAGE participants publication, page history](https://govdiff.njk.onl/update/2022-01-27T17:12:00+00:00/www.gov.uk/government/publications/scientific-advisory-group-for-emergencies-sage-coronavirus-covid-19-response-membership)
- [Searchable Inquiry transcripts](https://ukcovid19inquiry.dracos.co.uk/search/)
