isNumeric <- function(x) !is.na(as.numeric(as.character(x)))
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, isNumeric)[1, ]], asNumeric))

year <- 2015

playerAvgStats <- dplyr::data_frame(Player=character(), Pos=character(), Salary=numeric(), Points=numeric(), ThreePts=numeric(), TotRB=numeric(),
                                    Assist=numeric(), Steal=numeric(), Block=numeric(), Turnover=numeric())
for (team in Team$Id) {
  tryCatch({
    playerBio <- GetTableRoster(team, year, 'roster')
    basicStat <- GetTableRoster(team, year, 'per_minute')
    advStat <- GetTableRoster(team, year, 'advanced')

    allStats <- inner_join(inner_join(playerBio, basicStat, by=c('roster.Player'='per_minute.Player')), advStat, by=c('roster.Player'='advanced.Player'))
    playerAvgStats <- rbind(playerAvgStats, allStats %>% mutate(Salary = 0) %>% mutate(Team=team) %>% select(Player=roster.Player, Team, Pos=roster.Pos, Salary, Points=per_minute.PTS,
                                                                                                             ThreePts=per_minute.3P, TotRB=per_minute.TRB,
                                                                                                             Assist=per_minute.AST, Steal=per_minute.STL,
                                                                                                             Block=per_minute.BLK, Turnover=per_minute.TOV))
  }, error = function(e) {
    message(e)
    message(paste0('Error retrieving team: ', team))
  })
}

# clean up
rm(advStat, allStats, basicStat, playerBio)

playerAvgStats <- factorsNumeric(playerAvgStats)

playerAvgStats['Player'] <- lapply(playerAvgStats['Player'], as.character)

playerAvgStats[playerAvgStats$Player == 'J.J. Barea', ]$Player <- 'Jose Barea'
playerAvgStats[playerAvgStats$Player == 'John Lucas III', ]$Player <- 'John Lucas'
playerAvgStats[playerAvgStats$Player == 'Glenn Robinson III', ]$Player <- 'Glenn Robinson'

save(playerAvgStats, file='Data/playerAvgStats.RData')
