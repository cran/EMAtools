.onLoad <- function(libname = find.package("EMAtools"), pkgname = "EMAtools"){

  # CRAN Note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("respondent_id","survey_id","timestamp_event","timezone_offset"))
}
