year <- 2015

playerInfo <- data.frame(Player=character(), Team=character(), Pos=character(), Exp=numeric())

allBoxScoresURL <- c()

for (team in Team$Id) {
  tryCatch({
    result <- GetTableTeam(team, year)
    playerInfo <- rbind(playerInfo, result %>% mutate(Team=team) %>%
                          select(Player=roster.Player, Team, Pos=roster.Pos, Exp=roster.Exp))
  }, error = function(e) {
    message(e)
    message(paste0('Error retrieving team: ', team))
  })
}

playerInfo['Player'] <- lapply(playerInfo['Player'], as.character)

playerInfo[playerInfo$Player == 'J.J. Barea', ]$Player <- 'Jose Barea'
playerInfo[playerInfo$Player == 'John Lucas III', ]$Player <- 'John Lucas'
playerInfo[playerInfo$Player == 'Glenn Robinson III', ]$Player <- 'Glenn Robinson'

save(playerInfo, file='Data/playerInfo.RData')
