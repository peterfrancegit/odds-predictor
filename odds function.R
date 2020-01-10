odds <- function (home, away) {
  score <- matrix(0,nrow = 11,ncol = 11)
  
  hgs <- as.numeric(table[team(home),2]) / as.numeric(table[team(home),1])
  xhgs <- as.numeric(table[team(home),5]) / as.numeric(table[team(home),1])
  agc <- as.numeric(table[team(away),3]) / as.numeric(table[team(away),1])
  xagc <- as.numeric(table[team(away),6]) / as.numeric(table[team(away),1])
  
  ags <- as.numeric(table[team(away),2]) / as.numeric(table[team(away),1])
  xags <- as.numeric(table[team(away),5]) / as.numeric(table[team(away),1])
  hgc <- as.numeric(table[team(home),3]) / as.numeric(table[team(home),1])
  xhgc <- as.numeric(table[team(home),6]) / as.numeric(table[team(home),1])
  
  hg <- (hgs + xhgs + agc + xagc) / 4
  ag <- (ags + xags + hgc + xhgc) / 4
  
  hprob <- 0
  dprob <- 0
  aprob <- 0
  
  for (i in 1:11) {
    for (j in 1:11) {
      score[i,j] <- dpois(i-1,hg) * dpois(j-1,ag)
      if (i > j) {
        hprob <- hprob + score[i,j]
      } else if (i == j) {
        dprob <- dprob + score[i,j]
      } else {
        aprob <- aprob + score[i,j]
      }
    }
  }
  
  hodds <- 1 / hprob
  dodds <- 1 / dprob
  aodds <- 1 / aprob
  
  oddsa <- matrix(round(c(hprob,hodds,dprob,dodds,aprob,aodds),digits=4),ncol = 3, nrow = 2)
  colnames(oddsa) <- c(toupper(home), "DRAW", toupper(away))
  rownames(oddsa) <- c("PROB", "ODDS")
  odds <- as.table(oddsa)
  
  print(paste('the expected result is',toupper(home),signif(hg, digits = 3),'-',signif(ag, digits = 3),toupper(away)))
  return(odds)
}