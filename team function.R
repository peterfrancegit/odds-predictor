teamcode <- c('ars','bou','bha','bur','car','che','cry','eve','ful','hud','lei','liv','mci','mun','new','sou','tot','wat','whu','wol')
teamname <- c('Arsenal FC','AFC Bournemouth','Brighton & Hove Albion','Burnley FC','Cardiff City','Chelsea FC','Crystal Palace','Everton FC','Fulham FC','Huddersfield Town','Leicester City','Liverpool FC','Manchester City','Manchester United','Newcastle United','Southampton FC','Tottenham Hotspur','Watford FC','West Ham United','Wolverhampton Wanderers')

team <- function (x) {
  y <- which(teamcode == x)
  return(y)
}

replace <- function (table) {
  for (i in 1:20) {
    for (j in 1:nrow(table)) {
      for (k in 1:ncol(table)) {
        if (table[j,k] == teamname[i]) {
          table[j,k] <- teamcode[i]
        }
      }
    }
  }
  return(table)
}