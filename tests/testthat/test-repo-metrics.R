library(Rperform)
library(testthat)

context("Check if repo metric functionalities work properly")

setwd("./RperformTestPackage")

test_that("Wrong parameter type results in error", {
  expect_error(time_compare(test_path = "tests/testthat/test-dup.r", num_commits = "5"))
  expect_error(mem_compare(test_path = "tests/testthat/test-dup.r", num_commits = "5"))
})

test_that("list_commits is working properly", {
  expect_equal(nrow(list_commits(path = "./", num_commits = 8)), 8)
  expect_equal(ncol(list_commits(path = "./", num_commits = 8)), 3)
  expect_equal(names(list_commits(path = "./", num_commits = 8)),  
               c("msg_list", "date_list", "sha_list"))
})

temp <- invisible(capture_output(code = Rperform_times <- time_compare(test_path = "./tests/testthat/testTime.R", 
                                                      num_commits = 4)))
test_that("time_compare is working properly", {
  expect_equal(nrow(Rperform_times), 36)
  expect_equal(ncol(Rperform_times), 7)
  expect_equal(names(Rperform_times), c("test_name", "metric_name", "status", "metric_val", "message",
                                        "sha", "date_time"))
})
rm(Rperform_times, temp)

setwd("./../")
