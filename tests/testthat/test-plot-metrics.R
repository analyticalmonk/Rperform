library(Rperform)
library(testthat)

context("check if the plot metrics work properly")
#TEST BLOCK 1 (To set the path of the tests)
curr_path <- getwd()
print(curr_path)

test_path <- ("./git2r")

if(curr_path != test_path)
{
		setwd(test_path)
}
test_that("if the current path and the test path are both equal")
{
	expect_equal(test_path, curr_path)
}

#TEST BLOCK 2
test_that("if the plot metric function working properly",
{
	expect_error(plot_metrics(test_path=test/graph.R, metric="memory", num_commits, save_data, save_plots))
	expect_error(plot_metrics(test_path=test/graph.R, metric="time", num_commits, save_data, save_plots)
	plot_metrics(plot_metrics(test_path=test/graph.R, metric="memory", num_commits, save_data, save_plots))
)



#TEST BLOCK 3
test_that("",
    expect_error(plot_branchmetrics( ))
	expect_error(plot_webpage( ))
	expect_error(plot_directory( ))


}
)



