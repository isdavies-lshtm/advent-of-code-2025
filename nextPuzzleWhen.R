nextPuzzleWhen <- function(tz = "America/New_York",
                          now = as.POSIXlt(Sys.time(), tz = tz),
                          start = as.POSIXct("2025-12-01", tz = tz)) {

  # Determine target: Start date or next midnight
  target <- if (now < start) start else as.POSIXct(trunc(now, "days") + 86400)
  diff <- as.numeric(difftime(target, now, units = "secs"))

  # Print formatted output
  cat(sprintf(
    "\n>>> %s in: %d days, %02d hours, %02d minutes and %02d seconds. <<<\n",
    if (now < start)
      "First puzzle"
    else
      "Next puzzle",
    diff %/% 86400,
    (diff %% 86400) %/% 3600,
    (diff %% 3600) %/% 60,
    round(diff %% 60)
  ))
}

nextPuzzleWhen()
