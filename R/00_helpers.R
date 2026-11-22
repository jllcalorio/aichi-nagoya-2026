# Helpers for the Aichi-Nagoya 2026 (20th Asian Games) results API
# The public results site (results.asiangames2026.org) is a Vue app that reads
# JSON from back.results.asiangames2026.org. Responses are zlib-compressed and
# then re-encoded as UTF-8 text, so we undo the UTF-8 step and inflate.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
})

API_BASE  <- "https://back.results.asiangames2026.org/s/AG2026/en"
RAW_DIR   <- "data/raw"
UA        <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36"
MIN_DELAY <- 1.0   # seconds between live requests (polite rate limit)

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)

# Undo the "bytes -> latin1 code points -> UTF-8" step.
# All code points are < 256, so only 0xC2 / 0xC3 lead bytes occur.
undo_utf8 <- function(b) {
  i <- as.integer(b)
  lead <- which(i == 0xC2L | i == 0xC3L)
  if (length(lead)) {
    i[lead + 1L] <- i[lead + 1L] + ifelse(i[lead] == 0xC3L, 0x40L, 0L)
    i <- i[-lead]
  }
  as.raw(i)
}

decode_body <- function(b) {
  out <- tryCatch(memDecompress(undo_utf8(b), "gzip"), error = function(e) NULL)
  if (is.null(out)) out <- b               # some responses may be plain JSON
  txt <- rawToChar(out)
  Encoding(txt) <- "UTF-8"
  txt
}

.last_call <- new.env()
.last_call$t <- Sys.time() - 10

cache_path <- function(path) {
  file.path(RAW_DIR, paste0(gsub("[^A-Za-z0-9.]+", "_", path), ".json"))
}

# GET an API path, caching the decoded JSON on disk. Returns parsed JSON
# (simplifyVector = FALSE) or NULL on HTTP error.
ag_get <- function(path, refresh = FALSE, tries = 4) {
  f <- cache_path(path)
  if (!refresh && file.exists(f) && file.size(f) > 0) {
    return(fromJSON(f, simplifyVector = FALSE))
  }
  for (k in seq_len(tries)) {
    wait <- MIN_DELAY - as.numeric(difftime(Sys.time(), .last_call$t, units = "secs"))
    if (wait > 0) Sys.sleep(wait)
    .last_call$t <- Sys.time()
    resp <- tryCatch(
      request(API_BASE) |>
        req_url_path_append(path) |>
        req_user_agent(UA) |>
        req_headers(Origin = "https://results.asiangames2026.org",
                    Referer = "https://results.asiangames2026.org/") |>
        req_timeout(60) |>
        req_error(is_error = \(r) FALSE) |>
        req_perform(),
      error = function(e) NULL)
    if (is.null(resp)) { Sys.sleep(5 * k); next }
    st <- resp_status(resp)
    if (st == 200) {
      txt <- decode_body(resp_body_raw(resp))
      writeLines(txt, f, useBytes = TRUE)
      return(fromJSON(txt, simplifyVector = FALSE))
    }
    if (st %in% c(429, 500, 502, 503, 504)) { Sys.sleep(10 * k); next }
    return(NULL)                            # 404 etc.: no data for this path
  }
  NULL
}

# NULL-safe scalar extraction
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
chr <- function(x) { x <- x %||% NA_character_; if (is.list(x)) NA_character_ else as.character(x) }

# Find the value of an Extensions entry by Code (first match)
ext_val <- function(exts, code) {
  for (e in exts %||% list()) if (identical(e$Code, code)) return(chr(e$Value))
  NA_character_
}
