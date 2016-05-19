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
  
  ## TO-DO --------------------------------------------------------
  
  # Git operations
  target <- git2r::repository("./")
  origin_state <- git2r::head(target)
  git2r::checkout(target, branch)
  on.exit(expr = git2r::checkout(origin_state))
  
  # Creating the tempfiles
  t_lines <- readLines(test_path)
  q_lines <- sub("test_that(", "testthatQuantity(", t_lines, fixed = TRUE)
  temp_file_original <- tempfile()
  temp_file_subbed <- tempfile()
  writeLines(t_lines, temp_file_original)
  writeLines(q_lines, temp_file_subbed)
  
  suppressPackageStartupMessages(devtools::load_all(file.path("./")))
  test_results <- list()
  commit_list <- git2r::commits(repo = target, n = num_commits)

  for (commit_i in seq(commit_list)) {
    
#     sha_val <- get_sha(commit_list[[commit_i]])
    commit_msg <- get_msg(commit_val = commit_list[[commit_i]])
    commit_dtime <- get_datetime(commit_val = commit_list[[commit_i]])
      
    # Code block measuring the run-time for the test file as a whole
    # --------------------------------------------------------------
    
    require(testthat)
    file_status = "pass"
    seconds_file <- tryCatch(expr = {
      if(requireNamespace(microbenchmark)){
        times <- microbenchmark::microbenchmark(test = {
          base::source(temp_file_original, local = T)
        }, times = 3)
        times$time/1e9
      } else {
        replicate(3, {
          time_vec <- system.time( {
            source(temp_file_original, local = T)
          } )
          time_vec[["elapsed"]]
        })
      }
    },
    error = function(e){
      file_status = "fail"
      NA
    }
    )
    
    # ---------------------------------------------------------------
    
    # Code block measuring the run-time of the testthat code blocks (if present)
    # --------------------------------------------------------------------------
    
    testthatQuantity <- function(test_name, code){
      e <- parent.frame()
      code_subs <- substitute(code)
      run <- function(){
        testthat:::test_code(test_name, code_subs, env=e)
      }
      status = "pass"
      seconds <- tryCatch(expr = {
        if(requireNamespace(microbenchmark)){
          times <- microbenchmark::microbenchmark(test = {
            run()
          }, times = 3)
          times$time/1e9
        } else {
          replicate(3, {
            time_vec <- system.time( {
              run()
            } )
            time_vec[["elapsed"]]
          })
        }
      },
      error = function(e){
        status = "fail"
        NA
      }
      )

      time_df <- data.frame(test_name, metric_name = "seconds", status, 
                            metric_val = seconds, message = commit_msg, 
                            date_time = commit_dtime, branch = branch)
      test_results[[test_name]] <<- time_df
    }
    
    
    source(file = temp_file_subbed, local = T)
#     seconds_file2 <- {
#       times <- microbenchmark(times = 1, source(file = temp_file_subbed, local = T))
#       times$time/1e9
#     }
    
    # Formatting the output
    # --------------------------------------------------------------------------
    
    test_df <- do.call(rbind, test_results)
    #   test_results_df["file runtime"] <- seconds_file
    if (exists("test_results_df")) {
      test_results_df <- rbind(test_results_df, test_df)
    } else {
      test_results_df <- test_df
    }
    test_results_df <- 
      rbind(test_results_df, data.frame(test_name = basename(test_path), 
                                        metric_name = "seconds", status = file_status,
                                        metric_val = seconds_file, message = commit_msg, 
                                        date_time = commit_dtime,  branch = branch))
    rownames(test_results_df) <- NULL
  }
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
#' @param branch_1 Branch against whose commits the test file is to be 
#'   tested.
#' @param branch_2 Branch into which branch1 is supposedly to be merged.  
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
  
  branch1_df <- time_branch(test_path = test_path, branch = branch1,
                            num_commits = same_commit$cnum_b1)
  branch2_df <- time_branch(test_path = test_path, branch = branch2,
                            num_commits = 1)

  rbind(branch1_df, branch2_df)
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Run-time details across directories/repositories.
#' 
#' Given a test-file and two branches, returns the run-time details of the file
#' against the first commit till the latest common commit in branch1, and
#' against the latest commit in branch2.
#' 
#' @param dir1 Path to the first directory/repository.
#' @param test_path1 File-path, relative to the first directory, for the test
#'   file to be tested.
#' @param branch_1 Branch in the first repository against whose commits the test
#'   file is to be tested.
#' @param dir2 Path to the second directory/repository.
#' @param test_path2 File-path, relative to the second directory, for the test
#'   file to be tested.
#' @param branch_2 Branch in the second directory against whose commits the test
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
compare_dirt <- function(dir1, test_path1, branch1 = "master", 
                         dir2, test_path2, branch2 = "master") {
  
  same_commit <- .common_commit(dir_one, dir_two, branch1, branch2)
  curr_dir <- file.path("./")
  
  setwd(dir_one)
  dir1_df <- time_branch(test_path1, branch1, num_commits = same_commit$cnum_b1)
  dir1_df$directory <- rep(dir1, times = rnum(dir1_df))
  setwd(curr_dir)
  
  setwd(dir_two)
  dir2_df <- time_branch(test_path2, branch2, num_commits = same_commit$cnum_b2)
  dir2_df$directory <- rep(dir2, times = rnum(dir2_df))
  setwd(curr_dir)
  
  rbind(dir1_df, dir2_df)
}


##  -----------------------------------------------------------------------------------------
##                                MEMORY CHUNK BEGINS
##  -----------------------------------------------------------------------------------------

#' Memory metrics across branches.
#' 
#' Given a test-file and two branches, returns the memory metrics of the file 
#' against the first commit till the latest common commit in branch1, and 
#' against the latest commit in branch2. Memory metrics returned are the memory 
#' leaked and maximum meory swapped during its execution.
#' 
#' @param test_path File-path for the test file to be tested.
#' @param branch_1 Branch against whose commits the test file is to be 
#'   tested.
#' @param branch_2 Branch into which branch1 is supposedly to be merged.  
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
  original_state <- git2r::head(target)
  same_commit <- .common_commit(branch1 = branch1, branch2 = branch2)
  #                  same_commit
  # ---------------------------------------------
  #      common_datetime, cnum_b1, cnum_b2
  
  # For branch1
  git2r::checkout(target, branch1)
  branch1_df <- mem_compare(test_path = test_path, num_commits = same_commit$cnum_b1)
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

.common_commit <- function(dir1 = NULL, dir2 = NULL, branch1, branch2) {
  stopifnot(is.character(branch1))
  stopifnot(length(branch1) == 1)
  stopifnot(is.character(branch2))
  stopifnot(length(branch2) == 1)
  
  curr_dir <- file.path("./../")
  
  # Git operations
  if (!is.null(dir1)) {
    setwd(dir1)
  }
  target1 <- git2r::repository(file.path("./"))
  original_state1 <- git2r::head(target1)
  git2r::checkout(object = target1, branch = branch1)
  commitlist1 <- git2r::commits(target1)
  git2r::checkout(original_state1)
  
  if (!is.null(dir2)) {
    setwd(curr_dir)
    setwd(dir2)
  }
  target2 <- git2r::repository(file.path("./"))
  original_state2 <- git2r::head(target2)
  git2r::checkout(object = target2, branch = branch2)
  commitlist2 <- git2r::commits(target2)
  git2r::checkout(original_state2)
  if (!is.null(dir2)) {
    setwd(curr_dir)
  }
  
  dtime_list1 <- lapply(commitlist1, FUN = get_datetime)
  dtime_list2 <- lapply(commitlist2, FUN = get_datetime)
  
  for (c1 in seq(dtime_list1)) {
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
## Customized implementation of Binary Search
## ------------------------------------------

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
