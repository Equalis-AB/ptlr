#' Robust Mean and Standard Deviation
#'
#' This function calculates robust SD with the 'Q method' described in
#' ISO 13528-2015, section C.5.2.2
#' @param df data frame containing one row per lab, and one column per replicate.
#' @return robust standard deviation
#' @export
#' @import tidyr
#' @import dplyr
#' @examples
#' Q_method(data.frame(replicate1=c(1,2,3,4,5),replicate2 = c(6,7,NA,100,NA)))

Q_method <- function(df) {
  #dec: maximum number of decimals points in input
  dec <-  df %>%
    pivot_longer(everything()) %>%
    mutate(num_dec =nchar(gsub("(.*)(\\.)|([0]*$)","",value))) %>%
    summarise(max(num_dec,na.rm=T)) %>%
    pull()
  #d: distances between every replicate
  d<-round(as.matrix(dist(unlist(df))),dec)
  #xx: discontinuity points of H
  xx <- sort(H_discontinuity_points(df,d,dec))
  df_res <- data.frame(H=unlist(lapply(xx,function(x) {H(x,df,d)})),x=xx)
  df_res <- df_res %>% G()
  H_0 <- ifelse(length(which(df_res$x==0))==0,0,df_res$H[which(df_res$x==0)])
  x_for_Ginv <- 0.25+0.75*H_0
  Ginv_res <-approx(unique(c(0,df_res$G)),unique(c(0,df_res$x)),x_for_Ginv)$y
  x_for_qnorm <-0.625+0.375*H_0
  q_norm<- qnorm(x_for_qnorm)
  return(Ginv_res/(sqrt(2)*q_norm))
}

H <- function(x,df,d) {
  #p: no. labs
  p <- dim(df)[1]
  #r: no. replicates
  r <- dim(df)[2]
  # difference matrix
  c1 <- 2/(p*(p-1))
  list_df <- list()
  for (i in 1:p) {
    list_df <- append(list_df,list(as.numeric(df[i,])[which(!is.na(as.numeric(df[i,])))]))
  }
  res <- 0
  for (j in 2:p) {
    n_j <- length(list_df[[j]])
    for(i in 1:(j-1)) {
      n_i <- length(list_df[[i]])
      c2 <- 1/(n_j*n_i)
      restmp <- c2*sum(d[seq(i,p*r,p),seq(j,p*r,p)]<=x,na.rm=T)
      res <- res + restmp
    }
  }
  res = c1*res
  return(res)
}

H_discontinuity_points <- function(df,d,dec) {
  n <- dim(d)[1]
  depth=dim(df)[2]-1
  nr <- dim(df)[1]
  rm_index <- list()
  for( i in depth) { rm_index <- append(rm_index, seq(nr*i+1, n*(n+1)+1-i*(nr*n+1), n+1))}
  rm_index <- unlist(rm_index)
  d[rm_index] <- NA
  return(unique(round(d[which(lower.tri(d))],dec)))
}

G <- function(df) {
  G <- 0.5*(df$H+dplyr::lag(df$H))
  G[1] <- 0.5*df$H[1]
  G[df$x==0] <-0
  df$G <- G
  return(df)
}
