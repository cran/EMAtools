# EMAtools 0.1.6

* Added method references (with DOIs) to the DESCRIPTION, as requested by CRAN.
* Replaced all `\dontrun{}` examples: the centering and observation-numbering
  functions now have self-contained, executable examples, and the model-fitting
  and plotting functions use `\donttest{}` with runnable code.

# EMAtools 0.1.5

* Removed the dependency on the 'DataCombine' package (now archived on CRAN).
  `eventmerge()` now uses an internal base-R implementation of the lead/slide
  operation it previously relied on.
* Fixed bugs in `eventmerge()` that caused it to error on entry (stray `get()`
  calls and unquoted column-name references).
* Fixed an over-width example line in `ema.powercurve()` documentation and
  corrected a typo (`COL.3` -> `COL.2`) in that example.
