context("GetTableBoxScores")

test_that("Get table box scores for toronto", {
  tor <- GetTableBoxScores("ATL", 2015)
  head(tor)
})
