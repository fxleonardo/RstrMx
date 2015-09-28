
year <- 2015

playerStats <- data.frame(Date=character(), Player=character(), Team=character(), Opp=character(), Away=logical(), MinPlayed=numeric(), ThreePts=numeric(),
                          FT=numeric(), TotRB=numeric(), Assist=numeric(), Steal=numeric(), Block=numeric(),
                          Turnover=numeric(), Points=numeric(), USGPct=numeric(), DKPoints=numeric())

allBoxScoresURL <- c()

for (team in Team$Id) {
  tryCatch({
    result <- GetTableBoxScores(team, year, allBoxScoresURL)
    allBoxScoresURL <- result$allBoxScoresURL
    playerStats <- rbind(playerStats, result$playerStats)
  }, error = function(e) {
    message(paste0('Error retrieving team: ', team, ' ', e))
  })
}

save(playerStats, file='Data/playerStats.RData')
