% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/powercurve.R
\name{ema.powercurve}
\alias{ema.powercurve}
\title{Create power curves for EMA data}
\usage{
ema.powercurve(
  NumbPart,
  NumbResp,
  days,
  respday,
  Est_ICC = 0.05,
  COL.8 = "red",
  COL.5 = "blue",
  COL.2 = "green"
)
}
\arguments{
\item{NumbPart}{Total number of participants (i.e., level-2 unit)}

\item{NumbResp}{Total max number of responses per participant (e.g., number of days * number of responses per day). You can either enter this OR enter number of days and number of responses per day manually. If all are entered, it will default to NumbResp.}

\item{days}{Maximum number of days in study.}

\item{respday}{Maximum number of responses per day.}

\item{Est_ICC}{Estimated model ICC. Defaults to .05, but you should use a priori information from prior studies.}

\item{COL.8}{Color of line for large (d=.8) effect size. Defualt is red, but you can specify colors by name or by hex code (make sure to put colors in quotation marks).}

\item{COL.5}{Color of line for medium (d=.5) effect size. Defualt is blue, but you can specify colors by name or by hex code (make sure to put colors in quotation marks).}

\item{COL.2}{Color of line for small (d=.2) effect size. Defualt is green, but you can specify colors by name or by hex code (make sure to put colors in quotation marks).}
}
\value{
A ggplot object that displays power curves at three effect sizes (d=.2,.5,.8). You can use this like any other ggplot object (e.g., by adding other ggplot objects to it)
}
\description{
This allows you to estimate power to detect an effect at three standard effect sizes (d = 0.2, 0.5, and 0.8). It uses the smpsize_lmm function from sjstats to generate data for the curves and ggplot2 to plot them.
}
\examples{
\dontrun{ema.powercurve(NumbPart=80,days=30,respday=3)}
 \dontrun{ema.powercurve(NumbPart=80,NumbResp=200)}
  \dontrun{ema.powercurve(NumbPart=80,NumbResp=200,COL.8="orange")}
  \dontrun{ema.powercurve(NumbPart=80,NumbResp=200,COL.8="orange",COL.5="#FF5733",COL.3="#8E44AD")}
}
\keyword{analysis}
\keyword{power}
