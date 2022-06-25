##  -----------------------------------------------------------------------------------------
##                                TIME CHUNK BEGINS
##  -----------------------------------------------------------------------------------------

#' Run-times of a file on the given branch.
#' 
#' Given a test-file and branch, returns the run-time details of the file over
#' the given number of commits on the branch.
#' 
#' @param test_path File-path for the test file to be tested.
#' @param branch Branch against whose commits the test file is to be 
#'   tested (with master being the default).   
#' @param num_commits Number of commits on the branch against which the test
#'   file is to be tested.
#'   
#' @examples
#' 
#' \dontrun{
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' time_branch(test_path = t_path, branch_name = "helper", num_commits = 10)
#' }
#' @section Value:
#' time_branch returns an object of class "data.frame".
#' The data-frame consists of the following columns:
#' \code{test_name}
#' \code{metric_name}
#' \code{status}
#' \code{metric_val}
#' \code{message}
#' \code{date_time}
#' \code{branch}
#' 
#' @section Warning:
#'   Function assumes the current directory to be the root directory of the
#'   package being tested.
#'   

# Given a test and branch, time_branch returns the run-time details of the test
# over the given number of commits on the specified branch.

time_branch <- function(test_path, branch = "master", num_commits = 5) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch))
  stopifnot(length(branch) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  num_commits <- floor(num_commits)
  
  # Git operations
  target <- git2r::repository("./")
  origin_state <- git2r::repository_head(target)
  git2r::checkout(target, branch)
  on.exit(expr = git2r::checkout(origin_state))
  
  # We use time_compare() from R/repo_metrics.R to obtain the required
  # results.
  test_results_df <- time_compare(test_path, num_commits)
  test_results_df$branch <- branch
  ## -----------------------------------------------------------------------
  
  test_results_df
}

##  -----------------------------------------------------------------------------------------
#' Run-time details across branches.
#' 
#' Given a test-file and two branches, returns the run-time details of the file
#' against the first commit till the latest common commit in branch1, and
#' against the latest commit in branch2.
#' 
#' @param test_path File-path for the test file to be tested.
#' @param branch1 Branch against whose commits the test file is to be 
#'   tested.
#' @param branch2 Branch into which branch1 is supposedly to be merged.  
#'   
#' @examples
#' 
#' \dontrun{
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_brancht(test_path = t_path, branch1 = "helper", branch2 = "master")
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
#' 
#' @section Warning:
#'   Function assumes the current directory to be the root directory of the
#'   package being tested.
#' 

compare_brancht <- function(test_path, branch1, branch2 = "master") {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch1))
  stopifnot(length(branch1) == 1)
  stopifnot(is.character(branch2))
  stopifnot(length(branch2) == 1)
  
  same_commit <- .common_commit(branch1 = branch1, branch2 = branch2)
  #                  same_commit
  # ---------------------------------------------
  #      common_datetime, cnum_b1, cnum_b2
  
  if (same_commit$cnum_b2 == 1) {
    branch1_df <- time_branch(test_path = test_path, branch = branch1,
                              num_commits = (same_commit$cnum_b1 - 1))
  }
  else {
    branch1_df <- time_branch(test_path = test_path, branch = branch1,
                              num_commits = same_commit$cnum_b1)
  }
  branch2_df <- time_branch(test_path = test_path, branch = branch2,
                            num_commits = 1)
  
  rbind(branch1_df, branch2_df)
}


##  -----------------------------------------------------------------------------------------
##                                MEMORY CHUNK BEGINS
##  -----------------------------------------------------------------------------------------

#' Memory metrics across branches.
#' 
#' Given a test-file and two branches, returns the memory metrics of the file 
#' against the first commit till the latest common commit in branch1, and 
#' against the latest commit in branch2. Memory metrics returned are the memory 
#' leaked and maximum memory utilized during its execution.
#' 
#' @param test_path File-path for the test file to be tested.
#' @param branch1 Branch against whose commits the test file is to be 
#'   tested.
#' @param branch2 Branch into which branch1 is supposedly to be merged.  
#'   
#' @examples
#' 
#' \dontrun{
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_branchm(test_path = t_path, branch1 = "helper", branch2 = "master")
#' }
#' 
#' @section Value:
#' compare_branchm returns an object of class "data.frame".
#' The data-frame consists of the following columns:
#' \code{test_name}
#' \code{metric_name}
#' \code{status}
#' \code{metric_val}
#' \code{message}
#' \code{date_time}
#' \code{time_branch} 
#' 
#' @section Warning:
#'   Function assumes the current directory to be the root directory of the
#'   package being tested.
#'

compare_branchm <- function(test_path, branch1, branch2 = "master") {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch1))
  stopifnot(length(branch1) == 1)
  stopifnot(is.character(branch2))
  stopifnot(length(branch2) == 1)
  
  target <- git2r::repository("./")
  original_state <- git2r::repository_head(target)
  same_commit <- .common_commit(branch1 = branch1, branch2 = branch2)
  #                  same_commit
  # ---------------------------------------------
  #      common_datetime, cnum_b1, cnum_b2
  
  # For branch1
  git2r::checkout(target, branch1)
  if (same_commit$cnum_b2 == 1) {
    branch1_df <- mem_compare(test_path = test_path, num_commits = (same_commit$cnum_b1 - 1))
  }
  else {
    branch1_df <- mem_compare(test_path = test_path, num_commits = same_commit$cnum_b1)
  }
  branch1_df$branch <- rep(branch1, times = nrow(branch1_df))
  git2r::checkout(original_state)
  
  # For branch2
  git2r::checkout(target, branch2)
  branch2_df <- mem_compare(test_path = test_path, num_commits = 1)
  branch2_df$branch <- "master"
  git2r::checkout(original_state)
  
  rbind(branch1_df, branch2_df)
}

##  -----------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------
## Function to find the latest common commit given two branches of a repository
## ----------------------------------------------------------------------------  

.common_commit <- function(dir1 = NULL, dir2 = NULL, branch1 = NULL, branch2 = NULL,
                           PR = F) {
  
  curr_dir <- file.path("./../")
  
  ## Git operations
  # Change into the first target directory
  if (!is.null(dir1)) {
    setwd(dir1)
  }
  target1 <- git2r::repository(file.path("./"))
  # If branch1 is specified, check out to it and obtain commit list
  if (!is.null(branch1)) {
    original_state1 <- git2r::repository_head(target1)
    git2r::checkout(object = target1, branch = branch1)
  }
  commitlist1 <- git2r::commits(target1)
  # Revert to the original state if checked out to branch1 before
  if (!is.null(branch1)) {
    git2r::checkout(original_state1) 
  }
  
  # Change into the second target directory
  if (!is.null(dir2)) {
    setwd(curr_dir)
    setwd(dir2)
  }
  target2 <- git2r::repository(file.path("./"))
  # If branch2 is specified, check out to it and obtain commit list
  if (!is.null(branch2)) {
    original_state2 <- git2r::repository_head(target2)
    git2r::checkout(object = target2, branch = branch2) 
  }
  commitlist2 <- git2r::commits(target2)
  # Revert to the original state if checked out to branch2 before
  if (!is.null(branch2)) {
    git2r::checkout(original_state2)
  }
  # Change to original directory if changed into directory 2
  if (!is.null(dir2)) {
    setwd(curr_dir)
  }
  
  dtime_list1 <- lapply(commitlist1, FUN = get_datetime)
  dtime_list2 <- lapply(commitlist2, FUN = get_datetime)
  
  for (c1 in seq(dtime_list1)) {
    if (PR == T) {
      if (c1 == 1) {
        next
      }
    }
    search_result <- .b_search(dtime_list2, dtime_list1[[c1]],
                               1, length(dtime_list2))
    if (search_result$status) {
      commit1 <- c1
      commit2 <- search_result$sequence
      break
    }
  }
  
  info_df <- data.frame(common_datetime = dtime_list1[[commit1]],
                        common_message = get_msg(commitlist1[[commit1]]),
                        cnum_b1 = commit1, cnum_b2 = commit2)
  info_df
}

##  -----------------------------------------------------------------------------------------

##  -----------------------------------------------------------------------------------------
## Customized (recursive) implementation of Binary Search
## ------------------------------------------------------

.b_search <- function(search_list, search_target, start, end) {
  mid_val <- as.integer((end + start) / 2)
  if (mid_val < 1) {
    info_df <- data.frame(status= F, sequence = mid_val)
  } else if (search_list[[mid_val]] == search_target) {
    info_df <- data.frame(status = T, sequence = mid_val)
    return(info_df)
  } else if (search_target < search_list[[mid_val]]) {
    return(.b_search(search_list, search_target, mid_val + 1, end))
  } else {
    return(.b_search(search_list, search_target, start, mid_val - 1))
  }
}

##  -----------------------------------------------------------------------------------------
