library(Rperform)
library(testthat)
context("check if the plot metrics work properly")

if(!dir.exists(paths = "./stringr")){
  git2r::clone(url = "https://github.com/hadley/stringr", local_path = "./stringr")
}
setwd("./stringr")

test_that("To checck if the paramters passed are right",
   {
	expect_error(plot_metric(test_path=tests/testthat/test-dup.r, metric, num_commits = 5, 
		save_data = FALSE, save_plots = FALSE, interactive = FALSE))
	
	expect_error(plot_metric(test_path, metric, num_commits = "5", 
		save_data = FALSE, save_plots = FALSE, interactive = FALSE))
    
    expect_error(plot_metric(test_path, metric = "blah", num_commits = 5, 
		save_data = FALSE, save_plots = FALSE, interactive = FALSE))
    }
)








