isNumeric <- function(x) !is.na(as.numeric(as.character(x)))
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, isNumeric)[1, ]], asNumeric))

year <- 2014

playerStats <- dplyr::data_frame(Name=character(), Position=character(), Salary=numeric(), Point=numeric(), Made3Pt=numeric(), Rebound=numeric(),
                                 Assist=numeric(), Steal=numeric(), Block=numeric(), Turnover=numeric())
for (team in Team$Id) {
  tryCatch({
    playerBio <- GetTableRoster(team, year, 'roster')
    basicStat <- GetTableRoster(team, year, 'per_minute')
    advStat <- GetTableRoster(team, year, 'advanced')

    allStats <- inner_join(inner_join(playerBio, basicStat, by=c('roster.Player'='per_minute.Player')), advStat, by=c('roster.Player'='advanced.Player'))
    playerStats <- rbind(playerStats, allStats %>% mutate(Salary = 0) %>% select(Name=roster.Player, Position=roster.Pos, Salary, Point=per_minute.FG,
                                                                                 Made3Pt=per_minute.3P, Rebound=per_minute.TRB,
                                                                                 Assist=per_minute.AST, Steal=per_minute.STL,
                                                                                 Block=per_minute.BLK, Turnover=per_minute.TOV))
  }, error = function(e) {
    message(e)
    message(paste0('Error retrieving team: ', team))
  })
}

playerStats <- factorsNumeric(playerStats)
