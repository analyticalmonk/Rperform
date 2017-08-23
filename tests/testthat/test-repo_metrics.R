library(Rperform)
library(testthat)

context("Check if repo metric functionalities work properly")

if(!dir.exists(paths = "./stringr")){
  git2r::clone(url = "https://github.com/hadley/stringr", local_path = "./stringr")
}
setwd("./stringr")

test_that("Wrong parameter type results in error", {
  expect_error(time_compare(test_path = "tests/testthat/test-dup.r", num_commits = "5"))
  expect_error(mem_compare(test_path = "tests/testthat/test-dup.r", num_commits = "5"))
})

test_that("To pass a wrong path as a parameter gives error", {
  expect_error(time_compare(test_path = tests/testthat/test-dup.r, num_commits = 10))
  expect_error(mem_compare(test_path = tests/testthat/test-dup.r, num_commits = 10))
})

test_that("Data frames returned by metric functions are of the correct dimensions",{
  expect_equal((ncol(time_compare(test_path = "tests/testthat/test-count.r", 2))), 7)
  expect_equal(ncol(mem_compare(test_path = "tests/testthat/test-count.r", 2)), 7)
})


setwd("./../")
unlink(x = "./stringr", recursive = T, force = T)

