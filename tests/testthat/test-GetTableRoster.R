context("GetTableRoster")

test_that("Get advanced table roster for toronto", {
  tor <- GetTableRoster("TOR",2010:2012, 'advanced') #gets the 2014 data on the Toronto Raptors player stats for the "per minute" table
  head(tor)
})

test_that("Get per_game table roster for toronto", {
  tor <- GetTableRoster("TOR",2014, 'per_game') #gets the 2014 data on the Toronto Raptors player stats for the "per minute" table
  head(tor)
})
