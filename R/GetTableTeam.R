########
#Function: get_table_roster(team_id, yearlist)
########

#team_id: team name as three letter string (e.g., "CHI")
#yearlist: which years to fetch data from
#tablename: if unspecified, you will be shown list of tables you can choose from, otherwise specify name of table
#tablename possibilities: "injury", "roster", "team_stats", "team_misc", "totals", "per_game", "per_minute", "advanced", "shooting", "salaries"
#returns dataframe with all the tables of the players on the team

#EXAMPLE: get_table_roster("TOR",2014, "per_minute")

GetTableTeam <- function(team_id, yearlist, tablename = 'roster') {
  inputlist <- c("injury", "roster", "team_stats", "team_misc", "totals", "per_game", "per_minute", "advanced", "shooting", "salaries")
  if(tablename %in% inputlist == FALSE) {
    stop("invalid table name")
  }
  results <- c()
  for(year in yearlist) {
    fileurl <- paste("http://www.basketball-reference.com/teams/",toupper(team_id),"/",year,".html", sep = "")
    doc <- htmlTreeParse(fileurl, useInternal = TRUE) #parse html
    html_tables <- readHTMLTable(doc) #read html tables: contains Regular Season Table and Playoffs Table
    perminute <- data.frame(html_tables[tablename]) #[,c("Date", "Opponent", "Tm", "Opp", "W", "L")] #Get all info from Regular Season table
    #regseason <- regseason[regseason$Date != "Date",] #Remove extra repeated rows containing headers
    perminute <- cbind(as.character(team_id), year, perminute) #teamname col at beginning
    results <- rbind(results, perminute)
  }
  return(results)
}
