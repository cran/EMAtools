.onLoad <- function(libname = find.package("EMAtools"), pkgname = "EMAtools"){

  # CRAN Note avoidance: these are column names referenced inside ggplot2::aes()
  # in ema.powercurve(), so they have no visible binding at check time.
  if(getRversion() >= "2.15.1")
    utils::globalVariables(c("Effect_Size", "Power", "Resp", "Response_Rate"))
}
