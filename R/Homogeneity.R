#' Homogeneity by between-sample standard deviation estimate
#'
#' This function calculates measures homogeneity as described in
#' ISO 13528-2015, section B.3, page 48.
#' @param df gx2 data.frame, where g is the number of labs participating in the 
#' test. 
#' @return estimate of between-sample standard deviation
#' @export
#' @import tidyr
#' @import dplyr
#' @examples
#' Homogeneity(data.frame(replicate1=c(1,2,3,4,5),replicate2 = c(6,7,3,4,6)))

Homogeneity <- function(df) {
  sample_mean <- (df[,1]+df[,2])/2
  w <- abs(df[,1]-df[,2])
  total_mean <-mean(sample_mean)
  s_x<-sd(sample_mean)
  s_w <- sqrt(sum(w^2)/(2*dim(df)[1]))
  return(max(0,sqrt(s_x^2 -(s_w^2/2))))
}