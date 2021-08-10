#' Stability
#'
#' This function calculates measures stability as described in
#' ISO 13528-2015, section B.5.1 and B.5.2.c.
#' @param df1 first measurement data. gx2 data.frame, where g is the number of 
#' labs participating in the test. 
#' @param df2 second measurement data. gx2 data.frame, where g is the number of
#' labs participating in the test. 
#' @param var_pt assigned variance for the proficiency testing round
#' @param add_uncertainty if TRUE, use B.5.2.c as stability criteria. Else use B.5.1.
#' @return list with the difference of means between df1 and df2, and the 
#' acceptence criteria. If the  difference_means <= max_allowed then the 
#' proficiency test items are considered adequetely stable
#' \itemize{
#'   \item difference_means - difference of means between the data of the two 
#'   testing times, df1 and df2.
#'   \item max_allowed - the maximum allowed value of mean_difference (r.h.s. of
#'   formula B.18 if add_uncertainty is TRUE, else r.h.s of B.17).
#' }
#' @export
#' @import tidyr
#' @import dplyr
#' @examples
#' Stability(data.frame(replicate1=c(1,2,3,4,5),replicate2=c(6,7,3,4,6)),
#'           data.frame(replicate1=c(2,2,3,8,3),replicate2=c(3,7,3,5,6)))

Stability <- function(df1,df2,var_pt,add_uncertainty=T) {
  y_1_mean <- mean(unlist(df1))
  sd_1 <- sd(unlist(df1))
  y_2_mean <- mean(unlist(df2))
  sd_2 <- sd(unlist(df2))
  return(list(difference_means = abs(y_1_mean-y_2_mean),
              max_allowed = 0.3*var_pt+add_uncertainty*2*sqrt(sd_1^2+sd_2^2)))
}