#> iPhones<-read.table("iPhones.csv", sep=";", header=T)
#> names(iPhones)
#> attach(iPhones)


question3_1 <- function(){
  TAC <- unlist(iPhones["IMEI"])
  codeTAC <- substr(TAC, 3, 8)
  codeTAC <- replace(codeTAC, codeTAC == "161200", 1)
  codeTAC <- replace(codeTAC, codeTAC == "161300", 2)
  codeTAC <- replace(codeTAC, codeTAC == "161400", 3)
  codeTAC <- replace(codeTAC, codeTAC == "171200", 4)
  codeTAC <- replace(codeTAC, codeTAC == "171300", 5)
  codeTAC <- replace(codeTAC, codeTAC == "171400", 6)
  codeTAC <- replace(codeTAC, codeTAC == "174200", 7)
  codeTAC <- replace(codeTAC, codeTAC == "174300", 8)
  codeTAC <- replace(codeTAC, codeTAC == "174300", 9)
  codeTAC <- replace(codeTAC, codeTAC == "174400", 9)
  codeTAC <- replace(codeTAC, codeTAC == "177100", 10)
  codeTAC <- replace(codeTAC, codeTAC == "177300", 11)
  codeTAC <- replace(codeTAC, codeTAC == "177400", 12)
  codeTAC <- replace(codeTAC, codeTAC == "177500", 13)
  codeTAC <- replace(codeTAC, codeTAC == "177600", 14)
  codeTAC <- replace(codeTAC, codeTAC == "180900", 15)
  numTAC <- as.numeric(codeTAC)
  numTAC <- numTAC - 1
  numTAC <- numTAC * 10 ^ 6
  SNR <- substr(unlist(iPhones["IMEI"]), 9, 13)
  SNR <- as.numeric(SNR)
  NS = SNR
  # cat(TAC[1],"\n")
  # for(i in 1:139){
  #   NS[i] = TAC[i]+(SNR[i]*10)
  # }
  # cat(TAC[1]+SNR[1],"\n")
  NS <- numTAC+(SNR*10)
  
  semaine <- substr(unlist(iPhones["PC"]), 4, 5)
  semaine #vecteur semaine
  split(NS, rep(1:8, c(33, 39, 16, 17, 14, 8, 8, 4))) #groupe des semaines
  for( i in 1:8) {
    vec <- unlist(NS[i])
    print(max(vec))
  }
  plot(NS)
}