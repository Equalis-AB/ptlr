#' Robust Mean and Standard Deviation by Q/Hampel-method
#'
#' This function calculates robust mean and standard deviation with the 
#' 'Q/Hampel method' described in ISO 13528-2015, section C.5.4
#' @param df data frame containing one row per lab, and one column per replicate.
#' @param a,b,c tuning parameters a<b<c. Greater efficiency is obtained by
#' increasing the range; improved resistance to outliers or minor modes is
#' obtained by reducing the range.
#' @return robust mean
#' @export
#' @import tidyr
#' @import dplyr
#' @examples
#' Q_Hampel(data.frame(replicate1=c(1,2,3,4,5),replicate2 = c(6,7,NA,100,NA)))

Q_Hampel <- function(df, a = 1.5, b = 3.0, c = 4) {
  robust_sd <- Q_method(df)
  robust_mean <- Hampel(df,a,b,c,robust_sd)
  return(list(robust_sd = robust_sd,
              robust_mean = robust_mean))
}
