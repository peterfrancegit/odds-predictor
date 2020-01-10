dftable <- read.table("league table.txt",header=TRUE, sep="")
table <- as.matrix(dftable)

for (i in 1:20) {
  for (j in 5:7) {
    if (regexpr('+',table[i,j], fixed = TRUE)[1] > 0) {
      table[i,j] <- substring(table[i,j],1,regexpr('+',table[i,j], fixed = TRUE)[1] - 1)
    } else if (regexpr('-',table[i,j], fixed = TRUE)[1] > 0) {
      table[i,j] <- substring(table[i,j],1,regexpr('-',table[i,j], fixed = TRUE)[1] - 1)
    }
  }
}