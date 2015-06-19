##  -----------------------------------------------------------------------------------------

#' Commits' details.
#' 
#' Given a repository path and number of commits (n), returns a data frame containing
#' the SHA1 values and summary of the last n commits in the repo.
#' 
#' @param path File-path to the git repository whose commits are to be summarized.
#' @param num_commits Number of commits to be summarized. The default is 20.
#' 
#' @examples
#' 
#' ## Example-1
#' 
#' # Changed to the directory containing the git repository
#' setwd(path)
#' 
#' # Obtained details of the last 10 commits in the repository
#' list_commits(num_commits = 10)
#' 
#' ## Example-2
#' 
#' # Obtained the details of the last 20 (default value) commits in the repository
#' # specified by path.
#' list_commits(path)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}}

# The list_commits function, given a repository path and number of commits (n), returns
# a data frame containing the SHA1 values and summary of the last n commits in the repo.

list_commits <- function(path = "./", num_commits = 20){
  stopifnot(is.character(path))
  stopifnot(length(path) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  num_commits <- floor(num_commits)

  target <- git2r::repository(path)
  
  commit_list <- git2r::commits(target, n = num_commits)
  sha_list  <- list()
  msg_list  <- list()
  date_list <- list()
  
  for (i in 1:num_commits) {
    com <- attr(commit_list[[i]], which = "sha")
    msg <- attr(commit_list[[i]], which = "summary")
    com_date <- as(commit_list[[i]]@committer@when, "character")
    sha_list[i] <- com
    msg_list[i] <- msg
    date_list[i] <- com_date
  }
  
  as.data.frame(cbind(msg_list, date_list, sha_list), stringsAsFactors = F)
}

##  -----------------------------------------------------------------------------------------
                                   ## TIME CHUNK BEGINS ##
##  -----------------------------------------------------------------------------------------

#' Test file's run-time.
#' 
#' Given a test-file's path, checks its run-time against the commit specified by the 
#' commit \code{object} passed as a parameter.
#' 
#' @param test_path File-path for the test file whose run-time is to be checked.
#' @param test_commit git2r commit \code{object} corresponding to which the
#'   run-time is to be checked.
#'    
#' @examples
#' ## Example-1
#' # Obtain the commit object
#' commit_list <- git2r::commits()
#' t_commit <- commit_list[[1]]
#' 
#' # Specify the test-file path
#' t_path <- "Path/to/file"
#' 
#' # Pass the parameters and obtain the run-time details
#' library(Rperform)
#' time_commit(t_path, t_commit)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}}

# The time_commit function, given a test-file path, checks its run-time details
# against the specified commit in the current git repository.

time_commit <- function(test_path, test_commit) {
  
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(!is.null(test_commit))
  stopifnot(git2r::is_commit(test_commit))

  # Get the meta-information from the commit
  sha_val <- get_sha(test_commit)
  commit_dtime <- get_datetime(test_commit)
  # Create the tempfiles
  t_lines <- readLines(test_path)
  q_lines <- sub("test_that(", "testthatQuantity(", t_lines, fixed=TRUE)
  temp_file1 <- tempfile()
  temp_file2 <- tempfile()
  writeLines(t_lines, temp_file1)
  writeLines(q_lines, temp_file2)
  
  target <- git2r::repository("./")
# Reverting to the current branch on exit from the function
######################################################################  
  original_state <- git2r::head(target)
  git2r::checkout(test_commit)
  on.exit(expr = git2r::checkout(original_state))
######################################################################
  test_results <- list()
  
# Code block measuring the run-time for the test file as a whole
# --------------------------------------------------------------
  
  require(testthat)
  seconds_file2 <- if(require(microbenchmark)){
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
    time_df <- data.frame(test_name, seconds, status, sha_val,
                          date_time = commit_dtime)
    test_results[[test_name]] <<- time_df
  }

# --------------------------------------------------------------------------

# Code block measuring the run-time of test file as a whole
# --------------------------------------------------------------------------

  seconds_file <- (microbenchmark(source(temp_file2, local = T)
                                  , times = 1))$time/1e9

# --------------------------------------------------------------------------


# Formatting the output
# --------------------------------------------------------------------------

test_results_df <- do.call(rbind, test_results)
#   test_results_df["file runtime"] <- seconds_file
#   test_results_df["file runtime-2"] <- seconds_file2
  test_results_df <- rbind(test_results_df, data.frame(test_name = basename(test_path), 
                                       seconds = seconds_file2, status = "pass",
                                       sha_val = sha_val, date_time = commit_dtime))
  rownames(test_results_df) <- NULL
  test_results_df
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------


#' Run-time across versions.
#' 
#' Given a test-file path, checks its run-time against the specified number of commits 
#' in the current git repository and returns a data-frame comprised of the test name, 
#' status of test run, time (if successful) and SHA1 value corresponding to the commit
#' the value is for.
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
#' time_commit(test_path = t_path, n_commits = 10)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}} 

# The get_time function, given a test-file path, checks its run-time against the
# specified number of commits in the current git repository and returns a
# data-frame comprised of the test name, status of test run, time (if
# successful) and SHA1 value corresponding to the commit the value is for.

get_times <- function(test_path, num_commits = 20) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  num_commits <- floor(num_commits)
  
  target <- git2r::repository("./")
  commit_list <- git2r::commits(target, n = num_commits)
  result_list <- list()
  # Loads the functions from the repository for the package to be tested
  devtools::load_all(file.path("./"))

  for(commit_i in seq_along(commit_list)){
    one_commit <- commit_list[[commit_i]]
    result_list[[commit_i]] <- time_commit(test_path, one_commit)
  } 
  
  test_results <- do.call(rbind, result_list)
  test_results
}

##  -----------------------------------------------------------------------------------------

#' Run-times across branches.
#' 
#' Given a test-file and branch, returns the run-time of the file over the given
#' number of commits on the branch and the latest commit on master.
#' 
#' @param test_path File-path for the test file to be tested.
#' @param branch_1 First Branch against whose commits the test file is to be 
#'   tested.
#' @param branch_2 Second Branch against whose commits the test file is to be 
#'   tested.   
#' @param num_commits Number of commits on the first branch against which the test
#'   file is to be tested.
#'   
#' @examples
#' # Set the file-path
#' t_path <- "Path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_branch(test_path = t_path, branch_name = "helper", num_commits = 10)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}}

# Given a test and branch, compare_branch returns the run-time of the test over the given
# number of commits on the branch and the latest commit on master.

compare_branch <- function(test_path, branch_1, branch_2 = "master",
                           num_commits) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch_1))
  stopifnot(length(branch_1) == 1)
  stopifnot(is.character(branch_2))
  stopifnot(length(branch_2) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  num_commits <- floor(num_commits)
  
## TO-DO --------------------------------------------------------
  
#   target <- git2r::repository("./")
#   origin_state <- head(target)
#   git2r::checkout(target, branch_1)
#   on.exit(expr = git2r::checkout(origin_state))
# 
#   test_results <- get_times(test_path, num_commits)
#   test_results["branch_name"] <- branch_1
#   git2r::checkout(target, branch_2)
#   master_result <- get_times(test_path, 1)
#   master_result["branch_name"] <- branch_2
#   
#   rbind(test_results, master_result)

## TO-DO --------------------------------------------------------
}


##  -----------------------------------------------------------------------------------------
                                  ## MEMORY CHUNK BEGINS ##
##  -----------------------------------------------------------------------------------------

#' Test-file's memory statistics.
#' 
#' Given a test-file's path, checks its memory metrics against the commit
#' specified by the commit \code{object} passed as a parameter. Memory 
#' metrics returned are the memory leaked and maximum meory swapped during
#' its execution.
#' 
#' @param test_path File-path for the test file which is to be checked.
#' @param test_commit git2r commit \code{object} corresponding to which the
#'   memory stats are to be checked.
#'    
#' @examples
#' ## Example-1
#' # Obtain the commit object
#' commit_list <- git2r::commits()
#' t_commit <- commit_list[[1]]
#' 
#' # Specify the test-file path
#' t_path <- "Path/to/file"
#' 
#' # Pass the parameters and obtain the memory stats
#' library(Rperform)
#' mem_commit(t_path, t_commit)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}}

# The mem_commit function, given a test-file path, checks its memory details, 
# more specifically the memory leaked and maximum memory swapped during its
# execution. It does so against the specified commit in the current git
# repository.

mem_commit <- function(test_path, test_commit) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(!is.null(test_commit))
  stopifnot(git2r::is_commit(test_commit))
  
  target <- git2r::repository("./")
  sha_val <- get_sha(test_commit)
  commit_dtime <- get_datetime(test_commit)
  test_name <- basename(test_path)
  t_lines <- readLines(test_path)
  temp_file <- tempfile()
  writeLines(t_lines, temp_file)
  
  target <- git2r::repository("./")
  original_state <- git2r::head(target)
  git2r::checkout(test_commit)
  on.exit(expr = git2r::checkout(original_state))
  test_results <- list()
  rss_list <- list()
  
# --------------------------------------------------------------------------  
  
  require(testthat)
  .rss.profile.start(paste(test_name, ".RSS", sep = ""))
  source(temp_file, local = TRUE)
  rss_list <- .rss.profile.stop(paste(test_name, ".RSS", sep = ""))

  data.frame(test_name, swap_mb = rss_list$swap/1000, leak_mb = rss_list$leak/1000,
             date_time = commit_dtime, stringsAsFactors = FALSE)
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

                       ## TO GET MEMORY DETAILS FOR MULTIPLE COMMITS ##

#' Test-file's memory statistics.
#' 
#' Given a test-file's path, checks its memory metrics against the commit 
#' specified by the commit number passed as a parameter. Memory metrics returned
#' are the memory leaked and maximum meory swapped during its execution. A
#' commit number,n, would correspond to the nth commit in the commit log of the
#' current git repository.
#' 
#' @param test_path File-path for the test file which is to be checked.
#' @param commit_num commit number in the git log for the current git repository
#'   against which the memory stats are to be checked, with the commit number
#'   for the most recent commit being 1.
#'    
#' @examples
#' ## Example-1
#' 
#' # Specify the test-file path
#' t_path <- "Path/to/file"
#' 
#' # Pass the parameters and obtain the memory stats
#' library(Rperform)
#' get_mem(t_path, 3)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}}

# The get_mem function, given a test-file path, checks its memory details, more
# specifically the memory leaked and maximum memory swapped during its 
# execution. It does so against the nth commit in the current git repository, n
# being the commit_num parameter (with default value equal to 1).

get_mem <- function(test_path, commit_num = 1) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.numeric(commit_num))
  stopifnot(length(commit_num) == 1)
  commit_num <- floor(commit_num)
  
  target <- git2r::repository("./")
  target_commit <- git2r::commits(target)[[commit_num]]
  result_list <- list()
  # Loads the functions from the repository for the package to be tested
  devtools::load_all(file.path("./"))
  
#   for(commit_i in seq_along(commit_list)){
#     one_commit <- commit_list[[commit_i]]
#     result_list[[commit_i]] <- mem_commit(test_path, one_commit)
#   } 
  
  test_results <- mem_commit(test_path, target_commit)
  test_results
}

##  -----------------------------------------------------------------------------------------

##  -----------------------------------------------------------------------------------------

#' Test-file's memory statistics for multiple commits.
#' 
#' Given a test-file's path, checks its memory metrics against the number of 
#' commits specified by the parameter num_commits. Memory metrics returned are
#' the memory leaked and maximum meory swapped during its execution.
#' 
#' @param test_path File-path for the test file which is to be checked.
#' @param num_commits number of commits against all of which the memory stats
#'   are to be checked starting from the most recent one.
#'   
#' @examples
#' ## Example-1
#' 
#' # Specify the test-file path
#' t_path <- "Path/to/file"
#' 
#' # Pass the parameters and obtain the run-time details
#' library(Rperform)
#' mem_commit(t_path, 5)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}}

# The mem_compare function, given a test-file path, returns its memory details. 
# Specifically it obtains the values for memory leaked and maximum memory
# swapped during the file's execution. It does so against the specified number
# of commits from the git log in the current git repository.

mem_compare <- function(test_path, num_commits = 5) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  num_commits <- floor(num_commits)
  
  script.R <- system.file("exec", "get_mem.R", package="Rperform")
  Rscript <- file.path(R.home("bin"), "Rscript")
  result_list <- list()
  
  for (commit_i in 1:num_commits) {
    cmd <- paste(Rscript, script.R, test_path,
                 as.character(commit_i))
    for (test_t in 1:3) {
      system(cmd)
      load("mem_result.RData")
      result_list[[paste0(commit_i, as.character(test_t))]] <- mem_result 
    }
  }
  
  do.call(what = rbind, args = result_list)
  
}

##  -----------------------------------------------------------------------------------------