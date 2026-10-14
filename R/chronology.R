# Local clock times are compared directly: no timezone conversion is performed.
parse_consumed_at <- function(value) {
  if (!is.character(value) || anyNA(value) || any(!grepl(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2}T)?[0-9]{2}:[0-9]{2}(:[0-9]{2})?$", value)))
    stop("consumed_at requires a local time such as 13:30, or a local date/time such as 2026-10-14T13:30.")
  dated <- grepl("T", value, fixed = TRUE)
  if (any(dated) && !all(dated)) stop("Do not mix dated and time-only consumption values in one scenario.")
  clock <- sub("^.*T", "", value)
  hours <- as.numeric(substr(clock, 1, 2)); minutes <- as.numeric(substr(clock, 4, 5))
  seconds <- ifelse(nchar(clock) == 8, as.numeric(substr(clock, 7, 8)), 0)
  if (any(hours > 23 | minutes > 59 | seconds > 59)) stop("Invalid consumption clock time.")
  days <- rep(0, length(value))
  if (all(dated)) {
    dates <- substr(value, 1, 10)
    parsed <- as.Date(dates, format = "%Y-%m-%d")
    if (anyNA(parsed) || any(format(parsed, "%Y-%m-%d") != dates)) stop("Invalid consumption calendar date.")
    days <- as.numeric(parsed)
  }
  days * 86400 + hours * 3600 + minutes * 60 + seconds
}

format_local_time <- function(seconds, dated = FALSE) {
  day_seconds <- seconds %% 86400
  clock <- sprintf("%02d:%02d:%02d", as.integer(day_seconds %/% 3600),
    as.integer(day_seconds %% 3600 %/% 60), as.integer(day_seconds %% 60))
  if (dated) paste0(format(as.Date(floor(seconds / 86400), origin = "1970-01-01"), "%Y-%m-%d"), "T", clock) else clock
}

normalize_chronology <- function(events) {
  has_clock <- "consumed_at" %in% names(events) && any(!is.na(events$consumed_at) & nzchar(events$consumed_at))
  if (has_clock) {
    if (anyNA(events$consumed_at) || any(!nzchar(events$consumed_at)))
      stop("Use consumed_at for every event in a clock-time scenario; do not mix clock times and offsets.")
    seconds <- parse_consumed_at(events$consumed_at)
    # Consumption times are authoritative when offsets are also supplied.
    events$time_minutes <- (seconds - min(seconds)) / 60
    events$consumed_at_local <- format_local_time(seconds, all(grepl("T", events$consumed_at, fixed = TRUE)))
  } else {
    if (!"time_minutes" %in% names(events)) stop("Supply time_minutes or consumed_at for every event.")
    events$consumed_at <- NA_character_
    events$consumed_at_local <- NA_character_
  }
  events
}
