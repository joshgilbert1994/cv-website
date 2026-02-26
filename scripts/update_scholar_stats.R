#!/usr/bin/env Rscript

default_user_id <- "OCdG4IgAAAAJ"
user_id <- Sys.getenv("SCHOLAR_USER_ID", unset = default_user_id)
out_file <- Sys.getenv("SCHOLAR_OUTPUT_FILE", unset = "generated/scholar-stats.md")
profile_url <- sprintf("https://scholar.google.com/citations?user=%s&hl=en", user_id)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

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

fetch_profile_html <- function(url) {
  command <- paste(
    "curl -fsSL --retry 3 --retry-delay 2",
    "-A", shQuote("Mozilla/5.0"),
    shQuote(url),
    "2>&1"
  )

  output <- suppressWarnings(system(command, intern = TRUE, ignore.stderr = FALSE))
  status <- attr(output, "status")

  if (!is.null(status) && status != 0) {
    msg <- if (length(output)) paste(output, collapse = "\n") else sprintf("curl exited with status %s", status)
    stop(msg)
  }

  paste(output, collapse = "\n")
}

parse_metrics <- function(html) {
  header_values <- extract_tag_text(html, "<th class=\"gsc_rsb_sth\">[^<]*</th>", "th")
  header_values <- header_values[nzchar(header_values)]
  since_label <- if (length(header_values) >= 2) header_values[2] else "Since recent years"

  stat_values <- extract_tag_text(html, "<td class=\"gsc_rsb_std\">[^<]*</td>", "td")
  stat_values <- gsub(",", "", stat_values, fixed = TRUE)
  stat_values <- trimws(stat_values)
  stat_values[stat_values %in% c("", "\u2014", "-")] <- NA_character_

  stat_numbers <- suppressWarnings(as.integer(stat_values))

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

existing_file <- file.exists(out_file)

result <- tryCatch({
  html <- fetch_profile_html(profile_url)
  parsed <- parse_metrics(html)
  updated <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  write_stats_markdown(out_file, updated, parsed$since_label, parsed$metrics)
  message(sprintf("Updated Scholar metrics: %s", out_file))
  TRUE
}, error = function(e) {
  message(sprintf("Scholar update failed: %s", conditionMessage(e)))
  FALSE
})

if (!result && !existing_file) {
  placeholder <- list(
    citations_all = NA_integer_,
    citations_since = NA_integer_,
    h_all = NA_integer_,
    h_since = NA_integer_,
    i10_all = NA_integer_,
    i10_since = NA_integer_
  )

  updated <- format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC")
  write_stats_markdown(
    out_file,
    updated,
    "Since recent years",
    placeholder,
    note = "Unable to refresh Google Scholar data during this build. The next scheduled run will retry automatically."
  )
  message(sprintf("Wrote placeholder Scholar metrics: %s", out_file))
}
