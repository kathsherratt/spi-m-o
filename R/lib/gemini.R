# ai-input
#' One client for Gemini.
#'
#' Copied from kathsherratt/bvd-sitreps (R/lib/gemini.R, commit c593cfb,
#' 2026-09-17), where the retry policy, schema enforcement, quota stop and
#' usage ledger were built and tested. Kept as a copy, not a dependency, so
#' this repository runs on its own. Changes from the original: this header,
#' and the default backend is `agy`.
#'
#' Every model call goes through `gemini()`. `responseSchema` (or
#' `--json-schema` on agy) turns a malformed answer into a parse error at the
#' boundary instead of a wrong row three scripts later.
#'
#' The yardstick steps are text in, structured text out, so they run on the
#' agy backend (Antigravity CLI, plan quota, no per-call cost). Set before a
#' run:
#'   GEMINI_BACKEND=agy  AGY_MODEL=gemini-3.1-pro-low
#'   GEMINI_LEDGER=~/Documents/Github/bvd-sitreps/outputs/gemini-ledger.csv
#' agy has no temperature control, and adds 20-35k tokens of agent context to
#' every call, so batch work into few, large calls.

GEMINI_ENDPOINT <- "https://generativelanguage.googleapis.com/v1beta/models"

GEMINI_MODEL_TRANSCRIBE <- Sys.getenv("GEMINI_MODEL_TRANSCRIBE",
    unset = "gemini-3.1-pro-preview")
GEMINI_MODEL_TRANSLATE <- Sys.getenv("GEMINI_MODEL_TRANSLATE",
    unset = "gemini-3.8-flash")

GEMINI_ATTEMPTS <- 8
GEMINI_TIMEOUT <- 900

gemini_key <- function() {
    key <- Sys.getenv("GOOGLE_AI_KEY")
    if (!nzchar(key)) {
        stop("GOOGLE_AI_KEY is not set. Put it in ~/.Renviron (local) or in ",
            "the repository's Actions secrets (CI).", call. = FALSE)
    }
    key
}

#' List the models this key can reach.
#'
#' Used at setup to choose what to pin, and worth rerunning when a call starts
#' failing with a 404: a model id that has been retired fails that way rather
#' than saying so.
gemini_models <- function() {
    resp <- httr2::request(GEMINI_ENDPOINT) |>
        httr2::req_url_query(key = gemini_key(), pageSize = 200) |>
        httr2::req_timeout(60) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
    vapply(resp$models, function(m) sub("^models/", "", m$name), character(1))
}

#' What kind of 429 this is.
#'
#' The API returns the same status for four different situations, and they
#' need opposite responses. A per-minute limit clears in seconds and is worth
#' waiting out. A per-day limit clears at midnight Pacific, so retrying burns
#' the next report's attempt for nothing and the run should stop. An exhausted
#' spend cap or prepaid balance will not clear on its own at all. Only the
#' body says which, in the quota ids and the message.
gemini_429_kind <- function(resp) {
    err <- tryCatch(httr2::resp_body_json(resp)$error, error = function(e) NULL)
    if (is.null(err)) return("minute")
    if (grepl("spending cap|prepayment credits|billing details",
        err$message %||% "", ignore.case = TRUE) &&
        !grepl("FreeTier", jsonlite::toJSON(err$details, auto_unbox = TRUE))) {
        return("billing")
    }
    ids <- unlist(lapply(err$details %||% list(), function(d) {
        vapply(d$violations %||% list(), function(v) v$quotaId %||% "", character(1))
    }))
    if (any(grepl("PerDay", ids))) return("daily")
    "minute"
}

#' The wait the API asks for, where it gives one.
gemini_retry_after <- function(resp) {
    err <- tryCatch(httr2::resp_body_json(resp)$error, error = function(e) NULL)
    for (d in err$details %||% list()) {
        if (!is.null(d$retryDelay)) {
            return(as.numeric(sub("s$", "", d$retryDelay)) + 1)
        }
    }
    NA_real_
}

#' Raise a condition a caller can stop a whole run on.
#'
#' Classed so that the loops in 02 and 04 can tell "no more calls today" from
#' "this one report failed", and end the run cleanly in the first case rather
#' than failing every remaining report in turn.
gemini_quota_stop <- function(kind, label, detail = NULL) {
    msg <- switch(kind,
        daily = "Daily request quota reached. It resets at midnight Pacific time; rerun then and cached reports are skipped.",
        billing = "The project behind GOOGLE_AI_KEY has no usable billing (spend cap reached or prepaid credits depleted).",
        budget = paste0("GEMINI_BUDGET_USD reached (", detail, "). Raise it to continue; cached reports are skipped."),
        agy_quota = paste0("Antigravity plan quota reached (", detail, "). It refreshes every five hours, within a weekly limit; rerun then and cached reports are skipped."))
    structure(
        class = c("gemini_quota_stop", "error", "condition"),
        list(message = paste0(msg, if (!is.na(label)) paste0(" (at ", label, ")") else ""),
            call = NULL, kind = kind)
    )
}

# ------------------------------------------------------ agy backend ----

#' Which route calls go by: `api` (the Gemini API, billed per token to the
#' project behind GOOGLE_AI_KEY) or `agy` (the Antigravity CLI in print mode,
#' drawing on a Google AI Pro or Ultra plan's quota at no per-call cost).
#'
#' `agy` exists because the API credit ran out after the corpus was built,
#' and the remaining steps are text in, structured text out, which the plan's
#' quota covers. It is a coding agent, not a bare model call, and it differs
#' from the API in three ways that matter here. There is no temperature
#' setting, so reruns are less repeatable. Every call carries about 35,000
#' tokens of agent context before the prompt, which counts against the quota.
#' And quota is not published as a number: it refreshes every five hours
#' within a weekly limit. The schema is still enforced, the output still goes
#' through the same checks, and a quota stop still ends a run cleanly.
#'
#' It handles text parts only. Transcribing a PDF stays on the API, because
#' how an agent reads a PDF is not something the pipeline can see or check.
#'
#' Before a long run, set Antigravity's "AI Credit Overages" to Never, so that
#' exhausting the plan quota stops the run rather than drawing on credits.
GEMINI_BACKEND <- Sys.getenv("GEMINI_BACKEND", unset = "agy")

agy_path <- function() {
    path <- Sys.getenv("AGY_PATH", unset = path.expand("~/.gemini/bin/agy"))
    if (!file.exists(path)) {
        stop("Antigravity CLI not found at ", path, ". Install Antigravity, or ",
            "set AGY_PATH.", call. = FALSE)
    }
    path
}

#' The Antigravity model id for an API model id.
#'
#' Antigravity names models by thinking level (`gemini-3.8-flash-high`) and
#' has no preview suffix. `thinking_level` picks the level, defaulting to high;
#' pro offers only high and low. `AGY_MODEL` overrides the mapping outright.
agy_model <- function(model, thinking_level = NULL) {
    forced <- Sys.getenv("AGY_MODEL")
    if (nzchar(forced)) return(forced)
    if (grepl("-(high|medium|low)$", model)) return(model)
    base <- sub("-preview$", "", model)
    level <- thinking_level %||% "high"
    if (grepl("pro", base) && level == "medium") level <- "high"
    paste0(base, "-", level)
}

#' What to record as the model a result came from.
#'
#' Used in cache keys and front matter, so a result from the plan route is
#' never mistaken for, or silently reused as, a result from the API.
gemini_model_label <- function(model, thinking_level = NULL) {
    if (GEMINI_BACKEND == "agy") paste0("agy:", agy_model(model, thinking_level)) else model
}

#' A Gemini API schema as standard JSON Schema.
#'
#' The API takes an OpenAPI subset with upper-case types and a
#' `propertyOrdering` key; `agy --json-schema` takes JSON Schema.
agy_schema <- function(x) {
    if (!is.list(x)) return(x)
    x$propertyOrdering <- NULL
    if (!is.null(x$type) && is.character(x$type)) x$type <- tolower(x$type)
    # I() keeps a one-item `required` an array; auto_unbox would write a string.
    if (!is.null(x$required)) x$required <- I(unlist(x$required))
    if (!is.null(x$properties)) x$properties <- lapply(x$properties, agy_schema)
    if (!is.null(x$items)) x$items <- agy_schema(x$items)
    x
}

AGY_QUOTA_PATTERN <- "quota|rate.?limit|resource.?exhausted|limit (has been )?reached|usage limit|too many requests|429"

agy_log_usage <- function(label, model_label, usage) {
    if (is.null(usage)) return(invisible(NULL))
    row <- data.frame(
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        key_tail = "agy",
        label = label,
        model = model_label,
        input_tokens = usage$input_tokens %||% 0,
        output_tokens = usage$output_tokens %||% 0,
        thinking_tokens = usage$thinking_tokens %||% 0,
        cost_usd = 0
    )
    path <- gemini_ledger_path()
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    data.table::fwrite(row, path, append = file.exists(path))
    invisible(NULL)
}

#' One structured call through `agy -p`.
#'
#' Runs from an empty temporary directory with slash commands off and an
#' instruction not to use tools, so the agent has nothing in reach to read or
#' change. One retry on a failure that is not a quota stop, because a single
#' agent run can fail for reasons a second run does not share.
gemini_agy <- function(parts, schema, model, label, thinking_level = NULL) {
    if (any(vapply(parts, function(p) !is.null(p$inline_data), logical(1)))) {
        stop("The agy backend handles text only; PDFs go through the API ",
            "(GEMINI_BACKEND=api).", call. = FALSE)
    }
    text <- paste(vapply(parts, function(p) p$text %||% "", character(1)),
        collapse = "\n\n")
    prompt <- paste0(
        "Do not use any tools. Do not read, create or change any file. ",
        "Everything you need is in this message. Answer only with the ",
        "structured output the schema describes.\n\n", text)

    wd <- tempfile("agy-")
    dir.create(wd)
    on.exit(unlink(wd, recursive = TRUE), add = TRUE)
    schema_file <- file.path(wd, "schema.json")
    jsonlite::write_json(agy_schema(schema), schema_file, auto_unbox = TRUE)

    m <- agy_model(model, thinking_level)
    args <- c("-p", prompt, "--output-format", "json",
        "--json-schema", schema_file, "--model", m,
        "--disable-slash-commands", "--print-timeout", "20m")

    for (attempt in 1:2) {
        res <- processx::run(agy_path(), args, wd = wd, error_on_status = FALSE,
            timeout = 25 * 60)
        env <- tryCatch(jsonlite::fromJSON(res$stdout, simplifyVector = FALSE),
            error = function(e) NULL)
        # Kept for diagnosis: an agent run that fails says why only here.
        raw_dir <- here::here("outputs", "logs", "agy")
        dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
        stem <- file.path(raw_dir, paste0(gsub("[^A-Za-z0-9_-]", "_", label),
            "_", format(Sys.time(), "%Y%m%d-%H%M%S"), "_attempt", attempt))
        writeLines(res$stdout, paste0(stem, ".stdout.json"))
        writeLines(res$stderr, paste0(stem, ".stderr.txt"))
        if (!is.null(env)) agy_log_usage(label, paste0("agy:", m), env$usage)

        ok <- !is.null(env) && identical(env$status, "SUCCESS")
        out <- if (ok) env$structured_output else NULL
        if (ok && is.null(out) && !is.null(env$response)) {
            out <- tryCatch(jsonlite::fromJSON(env$response, simplifyVector = FALSE),
                error = function(e) NULL)
        }
        if (!is.null(out)) return(out)

        said <- paste(c(res$stderr, env$status, env$response), collapse = " ")
        if (grepl(AGY_QUOTA_PATTERN, said, ignore.case = TRUE)) {
            stop(gemini_quota_stop("agy_quota", label,
                substr(gsub("\\s+", " ", said), 1, 200)))
        }
        if (attempt == 2) {
            stop("agy did not return structured output",
                if (!is.na(label)) paste0(" on ", label) else "",
                " (exit ", res$status, ", status ", env$status %||% "none", "): ",
                substr(gsub("\\s+", " ", said), 1, 300), call. = FALSE)
        }
    }
}

#' A single generateContent call returning parsed JSON.
#'
#' `parts` is the request's content parts, built by the callers below. The
#' retry covers per-minute 429s and 5xx, which are the transient ones, and
#' waits as long as the API asks. A daily or billing 429 is not retried and
#' raises `gemini_quota_stop`. A 400 is a bad request and retrying it just
#' wastes quota, so it is allowed to fail.
gemini <- function(parts, schema, model = GEMINI_MODEL_TRANSCRIBE,
                   temperature = 0, label = NA_character_,
                   thinking_level = NULL) {
    if (GEMINI_BACKEND == "agy") {
        return(gemini_agy(parts, schema, model, label, thinking_level))
    }
    gemini_check_budget(model, label)

    body <- list(
        contents = list(list(role = "user", parts = parts)),
        generationConfig = list(
            temperature = temperature,
            responseMimeType = "application/json",
            responseSchema = schema
        )
    )
    # Left to the model's default unless set. SitRep 008 thought for 62,914
    # tokens on pro, filled the 65,536-token output limit and returned 2,608
    # tokens of transcription, the same way on a rerun; "low" is for reports
    # like that, not a general saving.
    if (!is.null(thinking_level)) {
        body$generationConfig$thinkingConfig <- list(thinkingLevel = thinking_level)
    }

    req <- httr2::request(paste0(GEMINI_ENDPOINT, "/", model, ":generateContent")) |>
        httr2::req_url_query(key = gemini_key()) |>
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_timeout(GEMINI_TIMEOUT) |>
        httr2::req_retry(
            max_tries = GEMINI_ATTEMPTS,
            is_transient = function(r) {
                s <- httr2::resp_status(r)
                if (s == 429) return(gemini_429_kind(r) == "minute")
                s %in% c(500, 502, 503, 504)
            },
            after = gemini_retry_after,
            backoff = function(i) min(60, 5 * 2^(i - 1)),
            # A dropped connection is not a response, so is_transient never
            # sees it. Without this, a network blip failed two reports in a
            # row twice in the first full build.
            retry_on_failure = TRUE
        )

    resp <- tryCatch(
        httr2::req_perform(req),
        httr2_http_429 = function(e) {
            kind <- gemini_429_kind(e$resp)
            if (kind %in% c("daily", "billing")) stop(gemini_quota_stop(kind, label))
            stop(e)
        }
    ) |>
        httr2::resp_body_json(simplifyVector = FALSE)

    # Logged before any check below can fail: a call that stops on the output
    # limit is still billed, and leaving it out of the ledger would let the
    # budget stop undercount.
    gemini_log_usage(label, model, resp$usageMetadata)

    cand <- resp$candidates[[1]]
    reason <- cand$finishReason
    if (!is.null(reason) && !reason %in% c("STOP", "MAX_TOKENS")) {
        stop("Gemini stopped with finishReason ", reason,
            if (!is.na(label)) paste0(" on ", label) else "", call. = FALSE)
    }
    if (!is.null(reason) && reason == "MAX_TOKENS") {
        stop("Gemini hit the output token limit",
            if (!is.na(label)) paste0(" on ", label) else "",
            " (", resp$usageMetadata$candidatesTokenCount %||% "?", " output, ",
            resp$usageMetadata$thoughtsTokenCount %||% "?", " thinking tokens).",
            " Usually a repetition loop or runaway thinking rather than a long",
            " document; a rerun often succeeds.", call. = FALSE)
    }

    txt <- paste(vapply(cand$content$parts, function(p) p$text %||% "",
        character(1)), collapse = "")
    parsed <- tryCatch(
        jsonlite::fromJSON(txt, simplifyVector = FALSE),
        error = function(e) stop("Gemini returned unparseable JSON",
            if (!is.na(label)) paste0(" on ", label) else "", ": ",
            conditionMessage(e), call. = FALSE)
    )

    parsed
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ----------------------------------------------------------------- spend ----

#' Standard paid-tier prices, USD per million tokens, from
#' https://ai.google.dev/gemini-api/docs/pricing on 2026-09-17. Thinking tokens
#' are billed at the output rate. Prompts over 200k tokens cost more on pro;
#' no report comes near that. Update this table when changing a pin, or the
#' budget stop below will price calls wrongly.
GEMINI_PRICES <- list(
    "gemini-3.1-pro-preview" = c(input = 2.00, output = 12.00),
    "gemini-3.8-flash" = c(input = 0.75, output = 3.75),
    "gemini-3.5-flash-lite" = c(input = 0.30, output = 2.50),
    "gemini-3.1-flash-lite" = c(input = 0.25, output = 1.50),
    "gemini-2.5-flash-lite" = c(input = 0.10, output = 0.40)
)

#' `GEMINI_LEDGER` can point several repositories at one ledger, so that a
#' single `GEMINI_BUDGET_USD` covers everything spent on one key.
gemini_ledger_path <- function() {
    Sys.getenv("GEMINI_LEDGER", unset = here::here("outputs", "gemini-ledger.csv"))
}

#' The last four characters of the key, to attribute spend to a project.
#'
#' Spend is budgeted per key because each key's project is billed separately,
#' and a budget for this key should not count calls made on another.
gemini_key_tail <- function() {
    k <- gemini_key()
    substr(k, nchar(k) - 3L, nchar(k))
}

gemini_cost <- function(model, input_tokens, billed_output_tokens) {
    p <- GEMINI_PRICES[[model]]
    if (is.null(p)) return(NA_real_)
    (input_tokens * p[["input"]] + billed_output_tokens * p[["output"]]) / 1e6
}

#' USD spent so far on the current key, estimated from the ledger.
gemini_spent <- function() {
    path <- gemini_ledger_path()
    if (!file.exists(path)) return(0)
    led <- data.table::fread(path, colClasses = list(character = "key_tail"))
    sum(led[key_tail == gemini_key_tail()]$cost_usd, na.rm = TRUE)
}

#' Stop before a call if the budget for this key is used up.
#'
#' `GEMINI_BUDGET_USD` caps spend on the current key, estimated from token
#' counts and the price table. It is checked before each call, so a run can
#' overshoot by at most one call. A prepaid project also has Google's own hard
#' stop when credit runs out, which arrives as a "billing" 429 and stops the
#' run the same way; this check is for stopping short of that, so that some
#' budget is kept back.
gemini_check_budget <- function(model, label) {
    budget <- suppressWarnings(as.numeric(Sys.getenv("GEMINI_BUDGET_USD")))
    if (is.na(budget)) return(invisible(TRUE))
    if (is.null(GEMINI_PRICES[[model]])) {
        stop("No price for ", model, " in GEMINI_PRICES, so a budget cannot ",
            "be enforced. Add it before running with GEMINI_BUDGET_USD set.",
            call. = FALSE)
    }
    spent <- gemini_spent()
    if (spent >= budget) {
        stop(gemini_quota_stop("budget", label,
            sprintf("$%.2f of $%.2f", spent, budget)))
    }
    invisible(TRUE)
}

#' Append one row per call to outputs/gemini-ledger.csv.
#'
#' Token counts are the only honest way to answer "what did rebuilding the
#' corpus cost", and they are gone once the response is discarded. Thinking
#' tokens are recorded separately because they are billed as output but are
#' not in `candidatesTokenCount`; on pro they were twice the transcription.
gemini_log_usage <- function(label, model, usage) {
    if (is.null(usage)) return(invisible(NULL))
    input <- usage$promptTokenCount %||% 0
    output <- usage$candidatesTokenCount %||% 0
    thinking <- usage$thoughtsTokenCount %||% 0
    row <- data.frame(
        time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        key_tail = gemini_key_tail(),
        label = label,
        model = model,
        input_tokens = input,
        output_tokens = output,
        thinking_tokens = thinking,
        cost_usd = round(gemini_cost(model, input, output + thinking), 6)
    )
    path <- gemini_ledger_path()
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    data.table::fwrite(row, path, append = file.exists(path))
    invisible(NULL)
}

#' A PDF as a request part.
#'
#' Inline base64 rather than the Files API: the reports are a few megabytes,
#' which is well inside the inline limit, and it keeps the call stateless so a
#' failure leaves nothing to clean up on Google's side.
gemini_pdf_part <- function(path) {
    list(inline_data = list(
        mime_type = "application/pdf",
        data = base64enc::base64encode(path)
    ))
}

gemini_text_part <- function(text) list(text = text)
