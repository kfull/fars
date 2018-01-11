library(testthat)
library(fars)

# test function make_filename(year)
test_that("make_filename(year) works correctly", {
  expect_is(make_filename("2015"), "character")
  is_identical_to(make_filename("2015"), "accident_2015.csv.bz2")
})