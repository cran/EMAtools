
#' Centering on person-means
#'
#' This function allows you to center on person-means (also called "centering within clusters")
#' @param ID name of ID variable
#' @param var name of variable to be centered
#' @return A column in your dataframe (with person-centered data)
#' @keywords centering
#' @export
#' @examples
#' df <- data.frame(ID = c(1, 1, 2, 2), var = c(1, 3, 5, 9))
#' df$centeredVAR <- pcenter(df$ID, df$var)


pcenter<-function(ID,var){
  centered<- var-ave(var, ID,FUN=function(x) mean(x, na.rm=T))
  return(centered)
}



