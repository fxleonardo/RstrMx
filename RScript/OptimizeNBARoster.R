currentLineups <- playerStats[sample.int(nrow(playerStats), 10), ]
optRoster <- rosterOpt(currentLineups)
currentLineups[ifelse(optRoster == 1, TRUE, FALSE), ]
