
#' Centering on grand-means
#'
#' This function allows you to center on grand-means.
#' @param var name of variable to be centered
#' @return A column in your dataframe (with grand-mean centered data)
#' @keywords centering
#' @export
#' @examples
#' df <- data.frame(var = c(1, 3, 5, 9))
#' df$centeredVAR <- gcenter(df$var)


gcenter<-function(var){
  centered<- var-(mean(var, na.rm=T))
  return(centered)
}



