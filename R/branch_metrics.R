##  -----------------------------------------------------------------------------------------
##                                TIME CHUNK BEGINS
##  -----------------------------------------------------------------------------------------

#' Run-times of a file on the given branch.
#' 
#' Given a test-file and branch, returns the run-time of the file over the given
#' number of commits on the branch.
#' 
#' @param test_path File-path for the test file to be tested.
#' @param branch First Branch against whose commits the test file is to be 
#'   tested (with master being the default).   
#' @param num_commits Number of commits on the first branch against which the test
#'   file is to be tested.
#'   
#' @examples
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' time_branch(test_path = t_path, branch_name = "helper", num_commits = 10)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#'   

# Given a test and branch, compare_branch returns the run-time of the test over
# the given number of commits on the branch and the latest commit on master.

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
  temp_file1 <- tempfile()
  temp_file2 <- tempfile()
  writeLines(t_lines, temp_file1)
  writeLines(q_lines, temp_file2)
  
  devtools::load_all(file.path("./"))
  test_results <- list()
  commit_list <- git2r::commits(repo = target, n = num_commits)

  for (commit_i in seq(commit_list)) {
    
#     sha_val <- get_sha(commit_list[[commit_i]])
    commit_msg <- get_msg(commit_val = commit_list[[commit_i]])
    commit_dtime <- get_datetime(commit_val = commit_list[[commit_i]])
      
    # Code block measuring the run-time for the test file as a whole
    # --------------------------------------------------------------
    
    require(testthat)
    seconds_file <- if(require(microbenchmark)){
      times <- microbenchmark(test = {
        source(temp_file1, local = T)
      }, times = 3)
      times$time/1e9
    } else {
      replicate(3, {
        time_vec <- system.time( {
          source(temp_file1, local = T)
        } )
        time_vec[["elapsed"]]
      })
    }
    
    # ---------------------------------------------------------------
    
    # Code block measuring the run-time of the testthat code blocks (if present)
    # --------------------------------------------------------------------------
    
    testthatQuantity <- function(test_name, code){
      e <- parent.frame()
      code_subs <- substitute(code)
      run <- function(){
        testthat:::test_code(test_name, code_subs, env=e)
      }
      seconds <- if(require(microbenchmark)){
        times <- microbenchmark(test = {
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
      status <- "pass"
      time_df <- data.frame(test_name, seconds, status, branch = branch,
                            message = commit_msg, date_time = commit_dtime)
      test_results[[test_name]] <<- time_df
    }
    
    
    source(file = temp_file2, local = T)
#     seconds_file2 <- {
#       times <- microbenchmark(times = 1, source(file = temp_file2, local = T))
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
                                        seconds = seconds_file, status = "pass",
                                        branch = branch, message = commit_msg, 
                                        date_time = commit_dtime))
    rownames(test_results_df) <- NULL
  }
  ## -----------------------------------------------------------------------
  
  test_results_df
}

##  -----------------------------------------------------------------------------------------
#' Run-times across branches.
#' 
#' Given a test-file and two branches, returns the run-times of the file against
#' the first commit till the latest common commit in branch1, and against the
#' latest commit in branch2.
#' 
#' @param test_path File-path for the test file to be tested.
#' @param branch_1 Branch against whose commits the test file is to be 
#'   tested.
#' @param branch_2 Branch into which branch1 is supposedly to be merged.  
#'   
#' @examples
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_brancht(test_path = t_path, branch1 = "helper", branch2 = "master")
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 

compare_brancht <- function(test_path, branch1, branch2 = "master") {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch1))
  stopifnot(length(branch1) == 1)
  stopifnot(is.character(branch2))
  stopifnot(length(branch2) == 1)
  
  same_commit <- .common_commit(branch1, branch2)
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
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_branchm(test_path = t_path, branch1 = "helper", branch2 = "master")
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
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
  same_commit <- .common_commit(branch1, branch2)
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
  
  system("rm *RSS*")
  system("rm mem_result.RData")
  rbind(branch1_df, branch2_df)
}

##  -----------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------
## Function to find the latest common commit given two branches of a repository
## ----------------------------------------------------------------------------  

.common_commit <- function(branch1, branch2) {
  stopifnot(is.character(branch1))
  stopifnot(length(branch1) == 1)
  stopifnot(is.character(branch2))
  stopifnot(length(branch2) == 1)
  
  # Git operations
  target <- git2r::repository(file.path("./"))
  original_state <- git2r::head(target)
  on.exit(expr = git2r::checkout(original_state))
  git2r::checkout(object = target, branch = branch1)
  commitlist1 <- git2r::commits(target)
  git2r::checkout(object = target, branch = branch2)
  commitlist2 <- git2r::commits(target)
  
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
