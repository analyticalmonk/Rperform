context("Check if repo metric functionalities work properly")

if(!dir.exists(paths = "./stringr")){
  git2r::clone(url = "https://github.com/hadley/stringr", local_path = "./stringr")
  }
setwd("./stringr")

test_that("Wrong parameter type results in error", {
  expect_error(time_compare(test_path = "tests/testthat/test-dup.r", num_commits = "5"))
  
})

setwd("./../")
unlink(x = "./stringr", recursive = T, force = T)