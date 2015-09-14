########
#Function: get_table_team(team_id, yearlist)
########

#team_id: team name as three letter string (e.g., "CHI")
#yearlist: which years to fetch data from
#returns dataframe with all the tables

#EXAMPLE: chi <- get_table_team("chi", 2007:2013)
GetTableBoxScores <- function(team_id, yearlist) {
  results <- c()
  for(year in yearlist) {
    fileurl <- paste("http://www.basketball-reference.com/teams/",toupper(team_id),"/",year,"_games.html", sep = "")
    doc <- htmlTreeParse(fileurl, useInternal = TRUE) #parse html
    html_tables <- readHTMLTable(doc) #read html tables: contains Regular Season Table and Playoffs Table
    regseason <- html_tables[[1]][,c("Date", "Opponent", "Tm", "Opp", "W", "L", "Streak")] #Get all info from Regular Season table
    regseason <- regseason[regseason$Date != "Date",] #Remove extra repeated rows containing headers
    awayGames <- which(regseason[, 6] == '@')
    regseason$Date <- as.Date(regseason$Date, format='%a, %b %d, %Y')

    for (i in seq_along(regseason$Date)) {
      score <- regseason[i, ]
      opp_id <- (Team %>% filter(stringr::str_detect(score$Opponent, Name)))$Id
      boxScoresUrl <- paste0('http://www.basketball-reference.com/boxscores/',
                             format(regseason$Date[1], '%Y%m%d0'),
                             ifelse(i %in% awayGames, opp_id, team_id),
                             '.html')
      bsDoc <- htmlTreeParse(boxScoresUrl, useInternal = TRUE) #parse html
      bs_tables <- readHTMLTable(bsDoc) #read html tables: contains Regular Season Table and Playoffs Table
      stats <- outer(c(team_id, opp_id), c('_basic', '_advanced'), 'paste0')
      for (j in seq_along(stats)) {
        players <- bs_tables[[stats[[j]]]]
        players <- players[players$Starters != 'Reserves', ]
      }
    }

    Season <- rep(as.numeric(year), nrow(regseason)) #add column for year

    regseason <- cbind(regseason, Season)
    results <- rbind(results, regseason)
  }
  return(results)
}
