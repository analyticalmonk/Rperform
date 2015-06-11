## The plot_time function, given a test-file path, plots the time taken by
## individual testthat blocks against the corresponding sha_values for the given
## number of commits.

plot_time <- function(test_path, num_commits) {
  time_frame <- get_times(test_path, num_commits)
#   Will replace sha_val with the dates after figuring out how to obtain them.
  ggplot2::qplot(sha_val, seconds, data = time_frame, color = test_name)
}