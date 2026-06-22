
#' Centering on person-means
#'
#' This function allows you calculate person-level means. This will create a level-2 variable that can be used in tandem with person-centered means. This is useful if you are interested in both the within-person and between-person effects.
#' @param ID name of ID variable
#' @param var name of variable to be centered
#' @return A column in your dataframe (with person-level means)
#' @keywords centering
#' @export
#' @examples
#' df <- data.frame(ID = c(1, 1, 2, 2), var = c(1, 3, 5, 9))
#' df$personMEAN <- pmean(df$ID, df$var)


pmean<-function(ID,var){
  centered<- ave(var, ID,FUN=function(x) mean(x, na.rm=T))
  return(centered)
}



