skew <- function (home) {
  
  for (i in 1:20) {
    if (i != team(home)) {

      hgs <- as.numeric(table[team(home),2]) / as.numeric(table[team(home),1])
      xhgs <- as.numeric(table[team(home),5]) / as.numeric(table[team(home),1])
      agc <- as.numeric(table[i,3]) / as.numeric(table[i,1])
      xagc <- as.numeric(table[i,6]) / as.numeric(table[i,1])

      ags <- as.numeric(table[i,2]) / as.numeric(table[i,1])
      xags <- as.numeric(table[i,5]) / as.numeric(table[i,1])
      hgc <- as.numeric(table[team(home),3]) / as.numeric(table[team(home),1])
      xhgc <- as.numeric(table[team(home),6]) / as.numeric(table[team(home),1])

      errors <- matrix(nrow=1, ncol=6)
      for (l in 0:10) {
        for (j in 0:10) {
          for (k in 0:10) {
            if (l + j + k <= 10) {
              hg <- (l*hgs + j*xhgs + k*agc + (10 - l - j - k)*xagc) / 40
              ag <- (l*ags + j*xags + k*hgc + (10 - l - j - k)*xhgc) / 40
            
              he <- hg - as.numeric(realscore(home,teamcode[i],seas19)[1])
              ae <- ag - as.numeric(realscore(home,teamcode[i],seas19)[2])
              
              rbind(errors,c(l, j, k, 10-l-j-k, he, ae))
            }
          }
        }
      }
    }
  }
  
  return(c(errors[min(errors[,5]),], errors[min(errors[,6]),]))
}
