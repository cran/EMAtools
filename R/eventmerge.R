#' Merge Mobile EMA (mEMA) event-level data into momentary data
#'
#' This allows you to merge event-level data (e.g., yes/no to an event) into momentary data, placing each event on the most recent momentary datapoint at or before the event (for the same subject). The match is based on subject and timestamp, so it does not depend on the row order of either dataset or on how the mEMA KEY is constructed.
#' @param MOMENTARY a dataframe with momentary (i.e., level-1) data exported from mEMA. It must contain a "subject_id" column and a numeric "timestamp" column (other mEMA columns such as KEY and instance_key may be present and are passed through unchanged).
#' @param EVENT a dataframe with event data (i.e., level-2). It must contain a "subject_id" column, a numeric "timestamp" column, and the event indicator in the LAST column (which can have any name). If a "local_date" column is present it is carried through as "date_event". Any other columns (e.g., respondent_id, survey_id, timezone_offset) are ignored.
#' @param eventNAME variable name for your event in the final merged dataset (does not have to match last column in EVENT dataset, but can). Defaults to "eventYN".
#' @return A dataframe equal to MOMENTARY (same rows, same order) with three columns added: the event indicator (named by \code{eventNAME}, 0 where no event maps to that momentary point), "timestamp_event" (the timestamp of the matched event, NA otherwise) and "date_event" (the event's local_date, NA otherwise). It has N rows = N rows in the momentary dataset. If more than one event maps to the same momentary datapoint, the most recent event is kept.
#' @keywords merging
#' @examples
#' MOMENTARYdata <- data.frame(
#'   subject_id = c(1, 1, 1),
#'   timestamp  = c(100, 200, 300))
#' EVENTdata <- data.frame(
#'   subject_id = c(1, 1),
#'   timestamp  = c(150, 250),
#'   eventYN    = c(1, 1))
#' newDATA <- eventmerge(MOMENTARYdata, EVENTdata, eventNAME = "eventYN")



eventmerge=function(MOMENTARY,EVENT,eventNAME="eventYN"){

  MOMENTARY <- as.data.frame(MOMENTARY)
  EVENT     <- as.data.frame(EVENT)

  if (!all(c("subject_id", "timestamp") %in% names(MOMENTARY)))
    stop("MOMENTARY must contain 'subject_id' and 'timestamp' columns.")
  if (!all(c("subject_id", "timestamp") %in% names(EVENT)))
    stop("EVENT must contain 'subject_id' and 'timestamp' columns.")

  # The event indicator is, by mEMA convention, the last column of EVENT.
  event_col   <- names(EVENT)[ncol(EVENT)]
  event_value <- EVENT[[event_col]]
  event_sid   <- EVENT[["subject_id"]]
  event_ts    <- EVENT[["timestamp"]]
  event_date  <- if ("local_date" %in% names(EVENT)) EVENT[["local_date"]] else NA

  # Start from the momentary data and add empty event columns. Momentary points
  # with no preceding event keep the default of 0 / NA.
  out <- MOMENTARY
  out[[eventNAME]]         <- 0
  out[["timestamp_event"]] <- NA
  out[["date_event"]]      <- NA

  mom_sid <- out[["subject_id"]]
  mom_ts  <- out[["timestamp"]]

  # For each event, attach it to the most recent momentary datapoint at or
  # before the event, for the same subject. This is robust to the ordering of
  # either dataset because it compares timestamps directly rather than relying
  # on row position or a constructed key.
  for (i in seq_len(nrow(EVENT))) {
    ets  <- event_ts[i]
    cand <- which(mom_sid == event_sid[i] & mom_ts <= ets)
    if (length(cand) == 0L) next                    # event precedes all momentary data
    j <- cand[which.max(mom_ts[cand])]              # most recent prompt at/before event

    # If several events map to the same momentary point, keep the most recent.
    if (is.na(out[["timestamp_event"]][j]) || ets >= out[["timestamp_event"]][j]) {
      out[[eventNAME]][j]         <- event_value[i]
      out[["timestamp_event"]][j] <- ets
      out[["date_event"]][j]      <- event_date[i]
    }
  }

  return(out)
}



