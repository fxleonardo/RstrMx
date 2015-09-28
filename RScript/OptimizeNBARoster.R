library(RstrMx)

# currentLineups <- playerStats[sample.int(nrow(playerStats), 10), ]
# collect stats from the season to the average

summaryPlayerStats <- playerStats %>% group_by(Player) %>%
  summarise(Points.SD = sd(Points), ThreePts.SD=sd(ThreePts), FT.SD=sd(FT),
            TotRB.SD=sd(TotRB), Assist.SD=sd(Assist), Steal.SD=sd(Steal),
            Block.SD=sd(Block), Turnover.SD=sd(Turnover), GamesPlayed=n())

currentPlayerStats <- left_join(playerAvgStats, summaryPlayerStats, by=(c('Name'='Player'))) %>%
  filter(GamesPlayed > 1)

setdiff(playerAvgStats$Name, summaryPlayerStats$Player)

currentPlayerStats <- inner_join(currentPlayerStats, playerInfo, by=(c('Name'='Player')))

# playerStats %>% filter(Player=='A.J. Price') %>% arrange(Date)

optRoster <- rosterOpt(currentPlayerStats)
currentPlayerStats[ifelse(optRoster == 1, TRUE, FALSE), ]

optRoster <- rosterOpt(currentPlayerStats, type='Max SR')
currentPlayerStats[ifelse(optRoster == 1, TRUE, FALSE), ]
