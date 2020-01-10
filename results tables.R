seas19 <- read.table("2018-19.txt", sep="\t", header=FALSE, fill=TRUE, colClasses=c("NULL","NULL","character","NULL","character","character"))
seas19 <- as.matrix(replace(seas19))

realscore <- function (home, away, seas) {
  a <- which(seas[,1] == home)
  b <- which(seas[,2] == away)
  for (i in 1:length(a)) {
    for (j in 1:length(b)) {
      if (a[i] == b[j]) {
        hg <- substring(seas[a[i],3],1,1)
        ag <- substring(seas[a[i],3],3,3)
      }
    }
  }
  return(c(hg,ag)) 
}