#' Robust Mean and Standard Deviation
#'
#' This function calculates robust mean and sd with 'Algorithm A' described in
#' ISO 13528-2015, section C.3.1
#' @param x numerical vector
#' @return A list with robust mean and sd.
#' \itemize{
#'   \item robust_mean - the robust mean of x.
#'   \item robust_sd - the robust standard deviation of x.
#' }
#' @export
#' @examples
#' Algorithm_A(c(0.2640, 0.2670, 0.2960 ,0.3110, 0.3310, 0.4246))

Algorithm_A <- function(x) {
  if(min(is.na(x))){
    return(NA)
  }
  else if (length(x)) {
    return(list(robust_mean = x, robust_sd = 0))
  }
  stab=1
  Res<-x
  Res<-sort(Res)

  #-----------Algortim A------

  #1.Identifiera medianvardet for samtliga originalresultat.
  xx<-median(Res)

  medel_alla<-vector()
  medel_alla[1]<-mean(Res)
  sd(Res)

  #2.Berakna avvikelsen fran medianvardet i absoluta tal for varje enskilt originalresultat.
  Avv<-abs(Res-xx)

  #3.Berakna medianavvikelsen (= medianvardet for avvikelserna fran steg 2).
  MedAvv<-median(Avv)

  #4.Berakna preliminar SD (SD*) enligt: SD*= medianavvikelsen × 1,483.
  sx=1.483*MedAvv

  SD_alla<-vector()
  SD_alla[1]<-sx


  #5.Berakna gransvarden medianvardet - 1,5 × SD* och medianvardet + 1,5 × SD*.
  d=1.5*sx
  Ner<-gransvardeNer<-xx-d
  Upp<-gransvardeUpp<-xx+d

  #6.Ersatt de resultat som ligger utanfor granserna med det narmaste gransvardet.
  Res_rob<-Res
  i=1
  for (i in i:length(Res)){
    if (Res_rob[i]<gransvardeNer){
      Res_rob[i]=gransvardeNer}
    if (Res_rob[i]>gransvardeUpp){
      Res_rob[i]=gransvardeUpp}
  }

  #7.Berakna preliminart medelvarde (m*) och preliminart SD (SD*) for de nya resultaten pa vanligt satt
  xx<-mean(Res_rob)
  sx=sd(Res_rob)

  medel_alla[2]<-xx
  SD_alla[2]<-sx

  #11. Upprepa steg 8-10 tills granserna inte langre andras.
  xtra=0
  nr=0
  while (stab==1){
    #antal iterationer sparas
    sxOld<-sx
    nr=nr+1
    stab=0
    #8.Berakna nya gransvarden: m* + 1,5 × 1,134 × SD* och m* - 1,5 × 1,134 × SD*.
    sx=1.134*sx
    d=1.5*sx
    gransvardeNer<-xx - d
    gransvardeUpp<-xx + d

    #9.Ersatt de originalresultat som ligger utanfor granserna med gransvardena fran steg 8.
    i=1
    for (i in i:length(Res_rob)){
      if (Res[i]<gransvardeNer){
        Res_rob[i]=gransvardeNer
      } else if (Res[i]>gransvardeUpp){
        Res_rob[i]=gransvardeUpp
      } else {Res_rob[i]=Res[i]}
    }

    #10.Berakna nytt preliminart medelvarde (m*) och SD*
    xx<-mean(Res_rob)
    sx=sd(Res_rob)
    #gamla kriteriet <1%
    #if(sx/sxOld<0.99 | sx/sxOld>1.01){stab=1}

    #kriteriet enl standard (stabilt pa i tre sig siffror)
    if (signif(xx,3)!=signif(medel_alla[length(medel_alla)],3) | signif(sx,3)!=signif(SD_alla[length(SD_alla)],3)){stab=1}
    # if(stab==0 & xtra == 0){
    #   xtra=1
    #   stab=1
    # }
    #Kriterie med helt stabila granser
    #if(Ner!=gransvardeNer | Upp!=gransvardeUpp){stab=1}
    Ner=gransvardeNer
    Upp=gransvardeUpp

    medel_alla[length(medel_alla)+1]<-xx
    SD_alla[length(SD_alla)+1]<-sx

  }

  #11. Upprepa steg 8-10 tills granserna inte langre andras. Vanligen hogst 20 iterationer
  #(upprepningar). Slutligt medelvarde ar medelvardet (m*) fran sista iterationen och
  #slutlig SD ar 1,134 x SD* fran sista iterationen.
  sx/sxOld

  return(list(robust_mean =mean(Res_rob),
  robust_sd =1.134*sd(Res_rob)))
}

