#' Plot run-time across versions.
#' 
#' Given a test-file path, plot the run-time of individual testthat blocks
#' against the date-time values of the specified number of commits (with the
#' commits' date-time values on the y-axis) in the current git repository.
#' 
#' @param test_path File-path of the test-file which is to be used for run-time
#'   comparisons.
#' @param num_commits Number of commits (versions) against which the file is to
#'   be tested, with default being 20.
#'   
#' @examples
#' ## Example-1
#' 
#' # Specify the test-file path
#' t_path <- "Path/to/file"
#' 
#' # Pass the parameters and obtain the run-time details against 10 commits
#' library(Rperform)
#' plot_time(test_path = t_path, n_commits = 10)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}}
#' @seealso \code{\link[testthat]}  

## The plot_time function, given a test-file path, plots the time taken by
## individual testthat blocks against the corresponding sha_values for the given
## number of commits.

plot_time <- function(test_path, num_commits) {
  time_frame <- get_times(test_path, num_commits)
#   Will replace sha_val with the dates after figuring out how to obtain them.
  ggplot2::qplot(date_time, seconds, data = time_frame, color = test_name)
}