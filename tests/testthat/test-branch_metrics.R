library(Rperform)
library(testthat)

context("check if the branch-metric.R works properly")

if(!dir.exists(paths = "./stringr")){
  git2r::clone(url = "https://github.com/hadley/stringr", local_path = "./stringr")
}
setwd("./stringr")


test_that("To checck if the parameters of 'time_branch' are passed right",

	{	
		expect_error(time_branch(test_path=tests/testthat/test-dup.r, branch = "master", num_commits = 5))
		expect_error(time_branch(test_path="tests/testthat/test-dup.r", branch = master, num_commits = 5))
		expect_error(time_branch(test_path=tests/testthat/test-dup.r, branch = "master", num_commits = "5"))
		}
)

test_that("To checck if the parameters of 'compare_branchm' are passed right",

	{	
		expect_error(time_branch(test_path=tests/testthat/test-dup.r, branch = "master", num_commits = 5))
		expect_error(time_branch(test_path="tests/testthat/test-dup.r", branch = master, num_commits = 5))
		expect_error(time_branch(test_path=tests/testthat/test-dup.r, branch = "master", num_commits = "5"))
		}
)
