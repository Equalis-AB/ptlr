#' Robust Mean by Hampel Estimation
#'
#' This function calculates robust mean with the 'Hampel estimator' described in
#' ISO 13528-2015, section C.5.3.3
#' @param df data frame containing one row per lab, and one column per replicate.
#' @param a,b,c tuning parameters a<b<c. Greater efficiency is obtained by
#' increasing the range; improved resistance to outliers or minor modes is
#' obtained by reducing the range.
#' @param s_star robust standard deviation used in calculations. Defaults to the
#' Q method.
#' @return robust mean
#' @export
#' @import tidyr
#' @import dplyr
#' @examples
#' Hampel(data.frame(replicate1=c(1,2,3,4,5),replicate2 = c(6,7,NA,100,NA)))

Hampel <- function(df, a = 1.5, b = 3.0, c = 4.5, s_star =  Q_method(df)) {
  # Number of labs n
  n = dim(df)[1]

  # Calculate mean of results per alb
  y <- df %>%
    rowwise() %>%
    mutate(y_mean = mean(c_across(),na.rm=T)) %>%
    pull(y_mean)

  # Solve C.25 by trying all interpolation nodes for equation C.25
  d <- rep(NA,6*n)
  for (i in 1:n) {
    tmp <- 6*(i-1)
    yi <- y[i]
    d[tmp+1] <- yi-c*s_star
    d[tmp+2] <- yi-b*s_star
    d[tmp+3] <- yi-a*s_star
    d[tmp+4] <- yi+a*s_star
    d[tmp+5] <- yi+b*s_star
    d[tmp+6] <- yi+c*s_star
  }
  d <- sort(d)

  # find solutions of types (i) and (ii) (as described in section C.5.3.3)
  p <- sapply(1:length(d),function(m) sum_psi(y,d[m],s_star,a,b,c))
  solutions <- d[which(p==0)]

  # find solutions of type (iii) (as described in section C.5.3.3)
  solutions <- solutions %>%
    c(
      sapply(
      1:(length(d)-1),
      function(m) {
          if(p[m]*p[m+1]<0){
            d[m]-p[m]*(d[m+1]-d[m])/(p[m+1]-p[m])
         } else {NA}
      }
     ) %>% na.omit()
    )
  median_y <- median(y)
  res <- min(sapply(solutions, function(x) abs(x-median_y)))
  if (length(res)!=1) {
    return(median_y)
  }
  return(res)
}

# Left hand side of equation C.25
sum_psi <- function(y,x,s_star,a,b,c) {
  data.frame(y) %>%
    transmute(psi = psi((y-x)/s_star,a,b,c)) %>%
    summarise(sum(psi)) %>%
    pull()
}

# function C.26
psi <- function(q,a,b,c) {
  case_when(q<=-c ~ 0,
            q> -c & q<=-b ~ -c-q,
            q> -b & q<=-a ~ -a,
            q> -a & q<= a ~ q,
            q>  a & q<= b ~ a,
            q>  b & q<= c ~ c-q,
            q>  c ~ 0)
}
