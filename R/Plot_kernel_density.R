#' Kernel Density Plot
#'
#' This function returns a kernel density plot (ggplot) as described in
#' ISO 13528-2015, section 10.3
#' @param x a numerical vector of data
#' @param kernal_sd a character giving the method for determining kernel density
#' standard deviation (default: 'a' according to ISO 13528-2015, section 10.3.i.a,
#' where s* is calculated by Algorithm_A(x)) or numerical standard deviation.
#' @return ggplot object
#' @export
#' @import ggplot2
#' @import dplyr
#' @examples
#' Plot_kernel_density(c(0.2640, 0.2670, 0.2960 ,0.3110, 0.3310, 0.4246))
#' Plot_kernel_density(c(0.2640, 0.2670, 0.2960 ,0.3110, 0.3310, 0.4246),0.025)
Plot_kernel_density <- function(x,kernal_sd = "a") {
  #TODO add more variants
  if(kernal_sd == "a") {
    robust_sd <- Algorithm_A(x)$robust_sd
    kernal_sd <- (0.9*robust_sd)/(length(x)^0.2)
  }
  dens <- density(x,kernal_sd,sqrt(kernal_sd),"gaussian")
  dens_plot <- data.frame(xx=dens$x,
                          yy=dens$y) %>%
    ggplot(aes(x=xx,y=yy)) +
    geom_point(data=data.frame(x),aes(x=x, y=rep(0,length(x)))) +
    geom_area(alpha=0.5,
              fill= "#006EAB")+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  dens_plot
  return(dens_plot)
}
