context("GetTableRoster")

test_that("Get table roster for toronto", {
  tor <- GetTableRoster("TOR",2010:2012, 'advanced') #gets the 2014 data on the Toronto Raptors player stats for the "per minute" table
  head(tor)
})
