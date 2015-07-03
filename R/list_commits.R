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
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
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
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
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
  msg_val <- get_msg(test_commit)
  commit_dtime <- get_datetime(test_commit)
  # Create the tempfiles
  t_lines <- readLines(test_path)
  q_lines <- sub("test_that(", "testthatQuantity(", t_lines, fixed=TRUE)
  temp_file_original <- tempfile()
  temp_file_subbed <- tempfile()
  writeLines(t_lines, temp_file_original)
  writeLines(q_lines, temp_file_subbed)
  
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
  seconds_file <- tryCatch(expr = {
      if(require(microbenchmark)){
        times <- microbenchmark(test = {
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
      NA
    }
  )
  if(is.na(seconds_file)){
    file_status <- "fail"
  } else {
    file_status <- "pass"
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
    seconds <- tryCatch(expr = {
        if(require(microbenchmark)){
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
      },
      error = function(e){
        NA
      }
    )
    if(is.na(seconds)){
      status <- "fail"
    } else {
      status <- "pass"
    }
    time_df <- data.frame(test_name, seconds, status, msg_val,
                          date_time = commit_dtime)
    test_results[[test_name]] <<- time_df
  }

# --------------------------------------------------------------------------

# Code block measuring the run-time of test file as a whole
# --------------------------------------------------------------------------
# 
  source(temp_file_subbed, local = T)

# --------------------------------------------------------------------------


# Formatting the output
# --------------------------------------------------------------------------

  test_results_df <- do.call(rbind, test_results)
#   test_results_df["file runtime"] <- seconds_file
#   test_results_df["file runtime-2"] <- seconds_file2
  test_results_df <- rbind(test_results_df, data.frame(test_name = basename(test_path), 
                                       seconds = seconds_file, status = file_status,
                                       msg_val = msg_val, date_time = commit_dtime))
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
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
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
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
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
  
  ## Creating the tempfiles
  target <- git2r::repository("./")
  sha_val <- get_sha(test_commit)
  msg_val <- get_msg(test_commit)
  commit_dtime <- get_datetime(test_commit)
  t_lines <- readLines(test_path)
  q_lines <- sub("test_that(", "testthatQuantity(", t_lines, fixed=TRUE)
  temp_file_original <- tempfile()
  temp_file_subbed   <- tempfile()
  writeLines(t_lines, temp_file_original)
  writeLines(q_lines, temp_file_subbed)
  
  ## Git operations
  target <- git2r::repository("./")
  original_state <- git2r::head(target)
  git2r::checkout(test_commit)
  on.exit(expr = git2r::checkout(original_state))
  test_results <- list()
  testthat_rss_list <- list()
  rss_list <- list()
  
  ## Function for obtaining the memory metrics for testthat blocks
  require(testthat)
  testthatQuantity <- function(test_name, code){
    e <- parent.frame()
    code_subs <- substitute(code)
    run <- function(){
      testthat:::test_code(test_name, code_subs, env=e)
    }
    new_name <- gsub(pattern = " ", replacement = "", x = test_name)
    testthat_rss_list <- {
        .rss.profile.start(paste0(new_name, ".RSS"))
        run()
        .rss.profile.stop(paste0(new_name, ".RSS"))
    }
    testthat_mem_df <- data.frame(test_name, swap_mb = testthat_rss_list$swap/1000, 
                          leak_mb = testthat_rss_list$leak/1000,
                          msg_val = msg_val, date_time = commit_dtime)
    test_results[[test_name]] <<- testthat_mem_df
  }
  
#   source(temp_file_subbed, local = TRUE)
  
  ## Obtaining the memory metrics for the file 
  file_name <- basename(test_path)
  .rss.profile.start(paste0(file_name, ".RSS"))
  source(temp_file_subbed, local = TRUE)
  rss_list <- .rss.profile.stop(paste0(file_name, ".RSS"))
  # Check /R/mem_experiment.R for source code for the above functions
  
  #Formatting the result dataframe
  testthat_df <- do.call(rbind, test_results)
  mem_df <- rbind(testthat_df, data.frame(test_name = file_name, swap_mb = rss_list$swap/1000, 
                                     leak_mb = rss_list$leak/1000, msg_val = msg_val, 
                                     date_time = commit_dtime))
  rownames(mem_df) <- NULL
  mem_df
#   data.frame(file_name, swap_mb = rss_list$swap/1000, leak_mb = rss_list$leak/1000,
#              msg_val = msg_val, date_time = commit_dtime)
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
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
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
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
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
  # Check out the code for above script in /exec/get_mem.R
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
  
  system("rm *RSS*")
  system("rm mem_result.RData")
  do.call(what = rbind, args = result_list)
  
}

##  -----------------------------------------------------------------------------------------