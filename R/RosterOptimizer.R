'http://www.basketball-reference.com/boxscores/200511020TOR.html'
'http://www.basketball-reference.com/boxscores/201412080TOR.html'
'http://www.basketball-reference.com/boxscores/200511050DET.html'
'http://www.basketball-reference.com/boxscores/201504180TOR.html'
'http://www.basketball-reference.com/boxscores/201504260WAS.html'

# table team
"http://www.basketball-reference.com/teams/TOR/2006_games.html"

# table roster
"http://www.basketball-reference.com/teams/TOR/2010.html"

# DF <- data.frame(Player=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M'),
#                  Role=c('T', 'M', 'B', 'J', 'S', 'T', 'M', 'T', 'B', 'M', 'J', 'S', 'B'),
#                  AvgPts=c(30,19,30,25,20,21,26,22,20,18,14,22,10),
#                  AvgReb=c(20,9,20,15,10,11,16,12,10,8,4,12,0),
#                  AvgAssist=c(10,9,10,5,10,1,6,2,10,8,4,2,0),
#                  AvgBlock=c(1,4,5,2,5,1,3,1,5,4,2,1,0),
#                  AvgSteal=c(3,3,3,1,3,1,2,2,3,3,1,2,0),
#                  Salary=c(930,900,1300,970,910,920,980,900,920,930,910,900,800))

rosterOpt <- function(DF) {
  lprec <- make.lp(0, nrow(DF))

  set.objfn(lprec, DF$Point + 0.5 * DF$Made3Pt + 1.25 * DF$Rebound + 1.5 * DF$Assist + 2 * DF$Steal + 2 * DF$Block -
                   0.5 * DF$Turnover)

#   dd_tt <- function(key, DF) {
#     otherObj <- DF[, !colnames(DF) %in% key]
#     rowSums(apply(otherObj >= 10, 2, as.integer))
#   }
  # objKeys <- c('AvgPts', 'AvgReb', 'AvgAssist', 'AvgBlock', 'AvgSteal')
  # set.objfn(lprec, DF$AvgPts + 1.25 * DF$AvgReb + 1.5 * DF$AvgAssist + 2 * DF$AvgSteal + 2 * DF$AvgBlock +
  #             ifelse(DF$AvgPts >= 10 & dd_tt('AvgPts', DF[, objKeys]) >= 1,
  #                    ifelse(dd_tt('AvgPts', DF[, objKeys]) >= 2, 3, 1.5), 0) * DF$AvgPts +
  #             ifelse(DF$AvgReb >= 10 & dd_tt('AvgReb', DF[, objKeys]) >= 1,
  #                    ifelse(dd_tt('AvgReb', DF[, objKeys]) >= 2, 3, 1.5), 0) * DF$AvgReb +
  #             ifelse(DF$AvgAssist >= 10 & dd_tt('AvgAssist', DF[, objKeys]) >= 1,
  #                    ifelse(dd_tt('AvgAssist', DF[, objKeys]) >= 2, 3, 1.5), 0) * DF$AvgAssist +
  #             ifelse(DF$AvgBlock >= 10 & dd_tt('AvgBlock', DF[, objKeys]) >= 1,
  #                    ifelse(dd_tt('AvgBlock', DF[, objKeys]) >= 2, 3, 1.5), 0) * DF$AvgBlock +
  #             ifelse(DF$AvgSteal >= 10 & dd_tt('AvgSteal', DF[, objKeys]) >= 1,
  #                    ifelse(dd_tt('AvgSteal', DF[, objKeys]) >= 2, 3, 1.5), 0) * DF$AvgBlock)

  set.type(lprec, 1:nrow(DF), 'binary')
  add.constraint(lprec, rep(1, nrow(DF)), '=', 8)
  add.constraint(lprec, DF$Salary, "<=", 50000)

  # one player for each role
  add.constraint(lprec, ifelse(DF$Position == 'PG', 1, 0), '<=', 2)
  add.constraint(lprec, ifelse(DF$Position == 'SG', 1, 0), '<=', 2)
  add.constraint(lprec, ifelse(DF$Position == 'PF', 1, 0), '<=', 2)
  add.constraint(lprec, ifelse(DF$Position == 'C', 1, 0), '<=', 2)
  add.constraint(lprec, ifelse(DF$Position == 'SF', 1, 0), '<=', 2)
  add.constraint(lprec, ifelse(DF$Position == 'G', 1, 0), '<=', 2)

  set.bounds(lprec, lower=rep(0, nrow(DF)), upper=rep(1, nrow(DF)))

  #set objective direction
  lp.control(lprec,sense='max')
  write.lp(lprec,'model.lp',type='lp')

  solve(lprec)
  get.objective(lprec)
  get.variables(lprec)
}
