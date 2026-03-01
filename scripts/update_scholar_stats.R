#!/usr/bin/env Rscript

default_user_id <- "OCdG4IgAAAAJ"
user_id <- Sys.getenv("SCHOLAR_USER_ID", unset = default_user_id)
out_file <- Sys.getenv("SCHOLAR_OUTPUT_FILE", unset = "generated/scholar-stats.md")
profile_url <- sprintf("https://scholar.google.com/citations?user=%s&hl=en", user_id)
max_fetch_attempts <- suppressWarnings(as.integer(Sys.getenv("SCHOLAR_FETCH_ATTEMPTS", unset = "3")))
serpapi_key <- Sys.getenv("SERPAPI_KEY", unset = "")
if (is.na(max_fetch_attempts) || max_fetch_attempts < 1) {
  max_fetch_attempts <- 3L
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(y)
  }
  x
}

format_metric <- function(x) {
  if (is.na(x)) {
    return("NA")
  }
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

write_stats_markdown <- function(path, updated_at, since_label, metrics, note = NULL) {
  lines <- c(
    sprintf("_Last updated: %s._", updated_at),
    if (!is.null(note)) c("", sprintf("_%s_", note)) else NULL,
    "",
    sprintf("| Metric | All | %s |", since_label),
    "|---|---:|---:|",
    sprintf("| Citations | %s | %s |", format_metric(metrics$citations_all), format_metric(metrics$citations_since)),
    sprintf("| h-index | %s | %s |", format_metric(metrics$h_all), format_metric(metrics$h_since)),
    sprintf("| i10-index | %s | %s |", format_metric(metrics$i10_all), format_metric(metrics$i10_since)),
    "",
    sprintf("[View full profile](%s)", profile_url)
  )

  writeLines(lines, path, useBytes = TRUE)
}

extract_tag_text <- function(html, pattern, tag) {
  matches <- gregexpr(pattern, html, perl = TRUE)[[1]]
  if (length(matches) == 1 && matches[1] == -1) {
    return(character(0))
  }

  raw <- regmatches(html, list(matches))[[1]]
  cleaned <- gsub(sprintf("</?%s[^>]*>", tag), "", raw)
  trimws(cleaned)
}

parse_integer_metric <- function(x) {
  x <- gsub("[^0-9]", "", x)
  if (!nzchar(x)) {
    return(NA_integer_)
  }
  as.integer(x)
}

read_existing_stats <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  lines <- readLines(path, warn = FALSE)
  if (!length(lines)) {
    return(NULL)
  }

  updated_line <- grep("^_Last updated:\\s*.+\\._$", lines, value = TRUE)
  updated_at <- NA_character_
  if (length(updated_line)) {
    updated_at <- sub("^_Last updated:\\s*(.+)\\._$", "\\1", updated_line[1])
  }

  header_line <- grep("^\\|\\s*Metric\\s*\\|\\s*All\\s*\\|", lines, value = TRUE)
  since_label <- "Since recent years"
  if (length(header_line)) {
    since_label <- sub("^\\|\\s*Metric\\s*\\|\\s*All\\s*\\|\\s*(.+?)\\s*\\|\\s*$", "\\1", header_line[1], perl = TRUE)
  }

  read_metric_row <- function(metric_name) {
    escaped <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", metric_name, perl = TRUE)
    pattern <- sprintf("^\\|\\s*%s\\s*\\|\\s*([^|]+?)\\s*\\|\\s*([^|]+?)\\s*\\|\\s*$", escaped)
    match_line <- grep(pattern, lines, value = TRUE, perl = TRUE)
    if (!length(match_line)) {
      return(c(NA_integer_, NA_integer_))
    }
    captures <- regmatches(match_line[1], regexec(pattern, match_line[1], perl = TRUE))[[1]]
    c(parse_integer_metric(captures[2]), parse_integer_metric(captures[3]))
  }

  citations <- read_metric_row("Citations")
  h_index <- read_metric_row("h-index")
  i10_index <- read_metric_row("i10-index")

  metrics <- list(
    citations_all = citations[1],
    citations_since = citations[2],
    h_all = h_index[1],
    h_since = h_index[2],
    i10_all = i10_index[1],
    i10_since = i10_index[2]
  )

  if (all(is.na(unlist(metrics)))) {
    return(NULL)
  }

  list(
    updated_at = updated_at,
    since_label = since_label,
    metrics = metrics
  )
}

parse_citations_from_meta <- function(html) {
  match <- regexpr("Cited by\\s*([0-9,]+)", html, perl = TRUE, ignore.case = TRUE)
  if (length(match) == 1 && match[1] == -1) {
    return(NA_integer_)
  }
  raw <- regmatches(html, match)
  as.integer(gsub("[^0-9]", "", raw))
}

fetch_serpapi_json <- function(author_id, api_key) {
  endpoint <- sprintf(
    "https://serpapi.com/search.json?engine=google_scholar_author&author_id=%s&api_key=%s",
    URLencode(author_id, reserved = TRUE),
    URLencode(api_key, reserved = TRUE)
  )

  command <- paste(
    "curl -sSL --retry 3 --retry-delay 2 --connect-timeout 15 --max-time 45 --compressed",
    "-w", shQuote("\\n__CURL_HTTP_STATUS__:%{http_code}"),
    shQuote(endpoint),
    "2>&1"
  )

  output <- suppressWarnings(system(command, intern = TRUE, ignore.stderr = FALSE))
  status <- attr(output, "status")
  http_line <- grep("^__CURL_HTTP_STATUS__:", output, value = TRUE)
  http_status <- NA_integer_
  if (length(http_line)) {
    http_status <- suppressWarnings(as.integer(sub("^__CURL_HTTP_STATUS__:", "", http_line[length(http_line)])))
  }

  body_lines <- output[!grepl("^__CURL_HTTP_STATUS__:", output)]
  json_text <- paste(body_lines, collapse = "\n")

  if (!is.null(status) && status != 0) {
    msg <- if (length(body_lines)) {
      paste(body_lines, collapse = "\n")
    } else {
      sprintf("curl exited with status %s", status)
    }
    stop(msg)
  }

  if (!is.na(http_status) && (http_status < 200 || http_status >= 300)) {
    stop(sprintf("SerpAPI returned HTTP status %s.", http_status))
  }

  if (!nzchar(json_text)) {
    stop("SerpAPI response was empty.")
  }

  if (grepl("\"error\"\\s*:\\s*\"", json_text, perl = TRUE)) {
    err_match <- regmatches(
      json_text,
      regexec("\"error\"\\s*:\\s*\"([^\"]+)\"", json_text, perl = TRUE)
    )[[1]]
    if (length(err_match) >= 2) {
      stop(sprintf("SerpAPI error: %s", err_match[2]))
    }
    stop("SerpAPI returned an error response.")
  }

  json_text
}

parse_serpapi_metric <- function(json_text, metric_key) {
  pattern_all_first <- sprintf(
    "\"%s\"\\s*:\\s*\\{[^\\}]*\"all\"\\s*:\\s*([0-9]+)[^\\}]*\"(since_[0-9]{4})\"\\s*:\\s*([0-9]+)",
    metric_key
  )
  match <- regmatches(json_text, regexec(pattern_all_first, json_text, perl = TRUE))[[1]]
  if (length(match) >= 4) {
    return(list(
      all = as.integer(match[2]),
      since_key = match[3],
      since = as.integer(match[4])
    ))
  }

  pattern_since_first <- sprintf(
    "\"%s\"\\s*:\\s*\\{[^\\}]*\"(since_[0-9]{4})\"\\s*:\\s*([0-9]+)[^\\}]*\"all\"\\s*:\\s*([0-9]+)",
    metric_key
  )
  match <- regmatches(json_text, regexec(pattern_since_first, json_text, perl = TRUE))[[1]]
  if (length(match) >= 4) {
    return(list(
      all = as.integer(match[4]),
      since_key = match[2],
      since = as.integer(match[3])
    ))
  }

  NULL
}

parse_serpapi_metrics <- function(json_text) {
  citations <- parse_serpapi_metric(json_text, "citations")
  h_index <- parse_serpapi_metric(json_text, "h_index")
  i10_index <- parse_serpapi_metric(json_text, "i10_index")

  if (is.null(citations) || is.null(h_index) || is.null(i10_index)) {
    stop("Unable to parse metrics from SerpAPI response.")
  }

  since_key <- c(citations$since_key, h_index$since_key, i10_index$since_key)
  since_key <- since_key[!is.na(since_key) & nzchar(since_key)]
  since_label <- "Since recent years"
  if (length(since_key)) {
    since_label <- sprintf("Since %s", sub("^since_", "", since_key[1]))
  }

  list(
    since_label = since_label,
    metrics = list(
      citations_all = citations$all,
      citations_since = citations$since,
      h_all = h_index$all,
      h_since = h_index$since,
      i10_all = i10_index$all,
      i10_since = i10_index$since
    )
  )
}

fetch_profile_html <- function(url) {
  profile_urls <- unique(c(
    url,
    sprintf("%s&view_op=list_works", url),
    sprintf("%s&view_op=list_works&sortby=pubdate", url)
  ))

  user_agents <- c(
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_7_0) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.0 Safari/605.1.15"
  )

  last_error <- "Unknown error fetching Scholar profile."

  for (attempt in seq_len(max_fetch_attempts)) {
    for (target_url in profile_urls) {
      for (agent in user_agents) {
        command <- paste(
          "curl -sSL --retry 4 --retry-all-errors --retry-delay 2 --connect-timeout 15 --max-time 45 --compressed",
          "-H", shQuote("Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
          "-H", shQuote("Accept-Language: en-US,en;q=0.9"),
          "-A", shQuote(agent),
          "-w", shQuote("\\n__CURL_HTTP_STATUS__:%{http_code}"),
          shQuote(target_url),
          "2>&1"
        )

        output <- suppressWarnings(system(command, intern = TRUE, ignore.stderr = FALSE))
        status <- attr(output, "status")

        http_line <- grep("^__CURL_HTTP_STATUS__:", output, value = TRUE)
        http_status <- NA_integer_
        if (length(http_line)) {
          http_status <- suppressWarnings(as.integer(sub("^__CURL_HTTP_STATUS__:", "", http_line[length(http_line)])))
        }

        body_lines <- output[!grepl("^__CURL_HTTP_STATUS__:", output)]
        html <- paste(body_lines, collapse = "\n")

        if (!is.null(status) && status != 0) {
          last_error <- if (length(body_lines)) {
            paste(body_lines, collapse = "\n")
          } else {
            sprintf("curl exited with status %s", status)
          }
          next
        }

        if (!is.na(http_status) && (http_status < 200 || http_status >= 300)) {
          last_error <- sprintf("Scholar returned HTTP status %s.", http_status)
          next
        }

        if (grepl("unusual traffic|captcha|/sorry/|enablejs", html, ignore.case = TRUE)) {
          last_error <- sprintf(
            "Google Scholar returned an anti-bot/interstitial response%s.",
            if (!is.na(http_status)) sprintf(" (HTTP %s)", http_status) else ""
          )
          next
        }

        if (nzchar(html)) {
          return(html)
        }

        last_error <- "Received an empty response from Scholar."
      }
    }

    if (attempt < max_fetch_attempts) {
      Sys.sleep(min(20, 2^attempt))
    }
  }

  stop(last_error)
}

parse_metrics <- function(html) {
  header_values <- extract_tag_text(
    html,
    "<th[^>]*class=\"[^\"]*gsc_rsb_sth[^\"]*\"[^>]*>[^<]*</th>",
    "th"
  )
  header_values <- header_values[nzchar(header_values)]
  since_label <- if (length(header_values) >= 2) header_values[2] else "Since recent years"

  stat_values <- extract_tag_text(
    html,
    "<td[^>]*class=\"[^\"]*gsc_rsb_std[^\"]*\"[^>]*>[^<]*</td>",
    "td"
  )
  stat_numbers <- vapply(stat_values, parse_integer_metric, integer(1))

  if (length(stat_numbers) < 6 || any(is.na(stat_numbers[1:6]))) {
    stop("Unable to parse Scholar metrics from profile HTML.")
  }

  list(
    since_label = since_label,
    metrics = list(
      citations_all = stat_numbers[1],
      citations_since = stat_numbers[2],
      h_all = stat_numbers[3],
      h_since = stat_numbers[4],
      i10_all = stat_numbers[5],
      i10_since = stat_numbers[6]
    )
  )
}

existing_stats <- read_existing_stats(out_file)

result <- tryCatch({
  html <- fetch_profile_html(profile_url)
  parsed <- parse_metrics(html)
  updated <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  write_stats_markdown(out_file, updated, parsed$since_label, parsed$metrics)
  message(sprintf("Updated Scholar metrics: %s", out_file))
  list(success = TRUE, html = html, source = "scholar")
}, error = function(e) {
  message(sprintf("Scholar update failed: %s", conditionMessage(e)))
  list(success = FALSE, error = conditionMessage(e))
})

if (!result$success && nzchar(serpapi_key)) {
  result <- tryCatch({
    json_text <- fetch_serpapi_json(user_id, serpapi_key)
    parsed <- parse_serpapi_metrics(json_text)
    updated <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
    write_stats_markdown(out_file, updated, parsed$since_label, parsed$metrics)
    message(sprintf("Updated Scholar metrics via SerpAPI fallback: %s", out_file))
    list(success = TRUE, source = "serpapi")
  }, error = function(e) {
    message(sprintf("SerpAPI fallback failed: %s", conditionMessage(e)))
    result
  })
}

if (!result$success) {
  updated <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  meta_citations <- if (!is.null(result$html)) parse_citations_from_meta(result$html) else NA_integer_

  if (!is.null(existing_stats)) {
    fallback_metrics <- existing_stats$metrics
    if (!is.na(meta_citations)) {
      fallback_metrics$citations_all <- meta_citations
    }

    last_success <- existing_stats$updated_at %||% "the previous successful update"
    note <- sprintf(
      "Automatic refresh failed; showing last known Scholar metrics from %s. The next scheduled run will retry automatically.",
      last_success
    )

    write_stats_markdown(
      out_file,
      updated,
      existing_stats$since_label %||% "Since recent years",
      fallback_metrics,
      note = note
    )
    message(sprintf("Wrote fallback Scholar metrics from existing data: %s", out_file))
  } else {
    placeholder <- list(
      citations_all = NA_integer_,
      citations_since = NA_integer_,
      h_all = NA_integer_,
      h_since = NA_integer_,
      i10_all = NA_integer_,
      i10_since = NA_integer_
    )

    write_stats_markdown(
      out_file,
      updated,
      "Since recent years",
      placeholder,
      note = "Unable to refresh Google Scholar data during this build. The next scheduled run will retry automatically."
    )
    message(sprintf("Wrote placeholder Scholar metrics: %s", out_file))
  }
}
