#' Create observation numbers in your data
#'
#' This function allows you number your observations by participant (P), by day (D) or by participant and day (PD) using a unix (numeric) timestamp
#' @param ID name of ID variable
#' @param TS name unix (numeric) timestamp (if you have a date-time object, try converting it with as.numeric(as.POSIXct()))
#' @param BY participant (P), by day (D), or by participant and day (PD). P will create a column of just 1 through n responses for each participant. D will create a day-level sequential value. PD will create an observation within each day. This might be most useful by first using the BY=D option, which will give you day number to use in tandem with observation #.
#' @return A column in your dataframe (with person-centered data)
#' @keywords Observation Numbers
#' @export
#' @examples
#' df <- data.frame(
#'   ID = c(1, 1, 1, 2, 2),
#'   TS = as.numeric(as.POSIXct(
#'     c("2020-01-01 09:00", "2020-01-01 17:00", "2020-01-02 09:00",
#'       "2020-01-01 10:00", "2020-01-02 10:00"), tz = "UTC")))
#' df$ObsNumb   <- ObsNumbs(df$ID, df$TS, BY = "P")
#' df$DayNumb   <- ObsNumbs(df$ID, df$TS, BY = "D")
#' df$ObsNumb_D <- ObsNumbs(df$ID, df$TS, BY = "PD")

ObsNumbs<-function(ID,TS,BY=c("P","D","PD")){

  if (BY == "P"){
    ObsNumb_out<- ave(TS, ID, FUN = seq_along)
  }
  if (BY == "D"){
    PromptDate<-anytime::anydate(TS)
     ObsNumb_out<- ave(as.character(PromptDate),ID,FUN=function(x) as.numeric(factor(x)))
  }
  if (BY == "PD"){
    PromptDate<-anytime::anydate(TS)
    ObsNumb_out<- ave(TS, paste(ID,PromptDate), FUN = seq_along)
  }
  return(ObsNumb_out)
}



