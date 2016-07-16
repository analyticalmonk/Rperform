compare_PR <- function(test_path, metric = "time") {
  
  # Obtain remote
  targetdir_1 <- list.dirs(path = ".")[2]
  remote <- git2r::remote_url(repo = git2r::repository(path = targetdir_1))[1]
  
  # Clone master branch as a directory
  git2r::clone(url = remote, local_path = "./master", branch = "master")
  targetdir_2 <- "master"
  
  # Compare the two directories
  dir_df <- compare_dir(targetdir_1, targetdir_2, test_path, metric)
  
  unlink(x = "master/", recursive = T, force = T)
  
  dir_df
}

##  -----------------------------------------------------------------------------------------

#' Compare details across directories/repositories.
#' 
#' Given a test-file, two directories and their corresponding branches, returns 
#' the run-time details of the file against the first commit till the latest 
#' common commit in branch1 of dir1, and against the latest commit in branch2 of
#' dir2.
#' 
#' @param dir1 Path to the first directory/repository.
#' @param test_path1 File-path, relative to the first directory, for the test
#'   file to be tested.
#' @param branch1 Branch in the first repository against whose commits the test
#'   file is to be tested.
#' @param dir2 Path to the second directory/repository.
#' @param test_path2 File-path, relative to the second directory, for the test
#'   file to be tested.
#' @param branch2 Branch in the second directory against whose commits the test
#'   file is to be tested.
#'   
#' @examples
#' 
#' \dontrun{
#' # Set the current directory to the parent directory of the concerned repositories.
#' setwd("./Path/to/parent/directory")
#' 
#' # Set the directory paths
#' d_path1 <- "Path/to/first/directory"
#' d_path2 <- "Path/to/second/directory"
#' 
#' # Set the file-paths
#' t_path1 <- "First/path/to/file"
#' t_path2 <- "Second/path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_dirt(d_path1, t_path1, branch1 = "master",
#'              d_path2, t_path2, branch2 = "patch")
#' }
#' 
#' @section Value:
#' compare_brancht returns an object of class "data.frame".
#' The data-frame consists of the following columns:
#' \code{test_name}
#' \code{metric_name}
#' \code{status}
#' \code{metric_val}
#' \code{message}
#' \code{date_time}
#' \code{branch}
#' \code{directory}
#' 
#' @section Warning:
#'   Function assumes the current directory to be the parent directory of both 
#'   the repositories being tested. That means both the repositories should be
#'   inside the same directory.
#'

compare_dir <- function(dir1, dir2, test_path, metric = "time") {
  
  # Obtain information about the latest common commit.
  same_commit <- .common_commit(dir1, dir2)
  #                  same_commit
  # ---------------------------------------------
  #      common_datetime, cnum_b1, cnum_b2
  
  print("Printing same commit")
  print(same_commit)
  
  curr_dir <- "../."
  
  setwd(dir1)
  dir1_df <- time_compare(test_path = test_path, num = same_commit$cnum_b1)
  dir1_df$directory <- rep(dir1, times = nrow(dir1_df))
  setwd(curr_dir)
  
  setwd(dir2)
  dir2_df <- time_compare(test_path = test_path, num_commits = same_commit$cnum_b2)
  dir2_df$directory <- rep(dir2, times = nrow(dir2_df))
  setwd(curr_dir)
  
  print(head(rbind(dir1_df, dir2_df)), 2)
  print(tail(rbind(dir1_df, dir2_df)), 2)
  
  rbind(dir1_df, dir2_df)
}