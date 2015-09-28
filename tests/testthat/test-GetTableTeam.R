context("GetTableTeam")

test_that("Get table team for toronto", {
  tor <- GetTableTeam("TOR", 2006:2014)
  head(tor)
})

test_that("Get table team for toronto", {
  tor <- GetTableTeam("TOR", 2015)
  head(tor)
})
