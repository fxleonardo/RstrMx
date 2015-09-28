########
#Function: get_table_team(team_id, yearlist)
########

#team_id: team name as three letter string (e.g., "CHI")
#yearlist: which years to fetch data from
#returns dataframe with all the tables

#EXAMPLE: chi <- get_table_team("chi", 2007:2013)
GetTableBoxScores <- function(team_id, yearlist, allBoxScoresURL=c()) {
  playerStats <- data.frame(Date=character(), Player=character(), Team=character(), Opp=character(), Away=logical(), MinPlayed=numeric(), ThreePts=numeric(),
                             FT=numeric(), TotRB=numeric(), Assist=numeric(), Steal=numeric(), Block=numeric(),
                             Turnover=numeric(), Points=numeric(), USGPct=numeric(), DKPoints=numeric())
  for(year in yearlist) {
    fileurl <- paste("http://www.basketball-reference.com/teams/",toupper(team_id),"/",year,"_games.html", sep = "")
    doc <- htmlTreeParse(fileurl, useInternal = TRUE) #parse html
    html_tables <- readHTMLTable(doc) #read html tables: contains Regular Season Table and Playoffs Table
    regseason <- html_tables[[1]][, c(2, 6, 7, 10)] #Get all info from Regular Season table
    regseason <- regseason[regseason$Date != "Date",] #Remove extra repeated rows containing headers
    awayGames <- which(regseason[, 2] == '@')
    regseason$Date <- as.Date(regseason$Date, format='%a, %b %d, %Y')

    for (i in seq_along(regseason$Date)) {
      score <- regseason[i, ]
      opp_id <- (Team %>% filter(stringr::str_detect(score$Opponent, Name)))$Id
      gameLoc <- ifelse(i %in% awayGames, opp_id, team_id)
      boxScoresUrl <- paste0('http://www.basketball-reference.com/boxscores/',
                             format(score$Date, '%Y%m%d0'), gameLoc, '.html')
      if (boxScoresUrl %in% allBoxScoresURL)
        next

      allBoxScoresURL <- c(allBoxScoresURL, boxScoresUrl)

      tryCatch({
        bsDoc <- htmlTreeParse(boxScoresUrl, useInternal = TRUE) #parse html
        bs_tables <- readHTMLTable(bsDoc) #read html tables: contains Regular Season Table and Playoffs Table

        for (team in c(team_id, opp_id)) {
          basicStat <- bs_tables[[paste0(team, '_basic')]]
          advStat <- bs_tables[[paste0(team, '_advanced')]]
          allStat <- inner_join(basicStat, advStat, by=('Starters'))
          allStat <- allStat[allStat$Starters != 'Reserves', ]
          allStat$MP.x <- sub("(.*)\\:.*", "\\1", allStat$MP.x)
          allStat <- allStat %>% filter(!MP.x %in% c('Did Not Play', 'Player Suspended', 'Inactive')) %>%
            mutate(Date=score$Date, Team=team, Opp=setdiff(c(team_id, opp_id), team), Away=team != gameLoc) %>%
            select(Date, Player=Starters, Team, Opp, Away, MinPlayed=MP.x, ThreePts=`3P`, FT, TotRB=TRB, Assist=AST, Steal=STL,
                   Block=BLK, Turnover=TOV, Points=PTS, USGPct=`USG%`)

          allStat <- factorsNumeric(allStat)

          allStat <- allStat %>% rowwise() %>% mutate(DKPoints=Points + 0.5 * ThreePts + 1.25 * TotRB + 1.5 * Assist + 2 * Steal + 2 * Block - 0.5 * Turnover +
                                          ifelse(sum(c(Points >= 10, TotRB >= 10, Assist >= 10, Block >= 10, Steal >= 10)) >= 3, 3,
                                                 ifelse(sum(c(Points >= 10, TotRB >= 10, Assist >= 10, Block >= 10, Steal >= 10)) == 2, 1.5, 0)))

          allStat['Player'] <- lapply(allStat['Player'], as.character)

          playerStats <- rbind(playerStats, allStat)
        }
      }, error = function(e) {
        message(e)
        message(paste0('Error retrieving team: ', boxScoresUrl))
      })
    }
  }
  return(list(playerStats=playerStats, allBoxScoresURL=allBoxScoresURL))
}

isNumeric <- function(x) !is.na(as.numeric(as.character(x)))
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, isNumeric)[1, ]], asNumeric))

