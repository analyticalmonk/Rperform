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
#   if (!is.null(test_path)) {
#     stopifnot(is.character(test_path))
#     stopifnot(length(test_path) == 1)
#   }
  target <- git2r::repository(path)
#   if (!is.null(test_path)) {
#     test_lines <- readLines(test_path)
#     qlines <- sub("test_that(", "testthatQuantity(", test_lines, fixed=TRUE)
#     qfile <- tempfile()
#     writeLines(qlines, qfile)
#   }
  
  commit_list <- git2r::commits(target, n = num_commits, reverse = T)
  sha_vec  <- character(num_commits)
  msg_vec  <- character(num_commits)
  
  for (i in 1:num_commits) {
    com <- attr(commit_list[[i]], which = "sha")
    msg <- attr(commit_list[[i]], which = "summary")
    sha_vec[i] <- com
    msg_vec[i] <- msg
  }
  
  git2r::checkout(target, "master")
  
  data.frame(SHA1 = sha_vec, Summary = msg_vec, stringsAsFactors = F)
}

##  -----------------------------------------------------------------------------------------
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

# The commit_time function, given a test-file path, checks its run-time details
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
# Need to work out this part
######################################################################  
#   curr_version <- git2r::commits()[[1]]
  git2r::checkout(test_commit)
  on.exit(expr = git2r::checkout(target, "master"))
######################################################################
  test_results <- list()
  
# Code block measuring the run-time for the test file as a whole
# --------------------------------------------------------------
  
  require(testthat)
  file_time <- if(require(microbenchmark)){
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
  seconds_file2 <- mean(file_time)

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
      }, times = 1)
      times$time/1e9
    } else {
      replicate(1, {
        time_vec <- system.time( {
          run()
        } )
        time_vec[["elapsed"]]
      })
    }
    seconds <- mean(seconds)
    status <- "pass"
    time_df <- data.frame(test_name, seconds, status, sha_val,
                          date_time = commit_dtime)
    test_results[[test_name]] <<- time_df
  }

# --------------------------------------------------------------------------

  seconds_file <- (microbenchmark(source(temp_file2, local = T)
                                  , times = 1))$time/1e9
  test_results_df <- do.call(rbind, test_results)
  test_results_df["file runtime"] <- seconds_file
  test_results_df["file runtime-2"] <- seconds_file2
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
#' @param branch_name Branch name against whose commits the test file is to be 
#'   tested.
#' @param num_commits Number of commits on the branch against which the test
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

compare_branch <- function(test_path, branch_name, num_commits) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch_name))
  stopifnot(length(branch_name) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  num_commits <- floor(num_commits)
  
  target <- git2r::repository("./")
  git2r::checkout(target, branch_name)
  test_results <- get_times(test_path, num_commits)
  test_results["branch_name"] <- branch_name
  git2r::checkout(target, "master")
  master_result <- get_times(test_path, 1)
  master_result["branch_name"] <- "master"
  
  rbind(test_results, master_result)
}


##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

mem_commit <- function(test_path, test_commit) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(!is.null(test_commit))
  stopifnot(git2r::is_commit(test_commit))
  
  sha_val <- get_sha(test_commit)
  t_lines <- readLines(test_path)
  t_lines <- sub("test_that(", "testthatQuantity(", t_lines, fixed=TRUE)
  temp_file <- tempfile()
  writeLines(t_lines, temp_file)
  target <- git2r::repository("./")
  git2r::checkout(test_commit)
  on.exit(expr = git2r::checkout(target, "master"))
  test_results <- list()
  
  
  testthatQuantity <- function(test_name, code){
    e <- parent.frame()
    code_subs <- substitute(code)
    run <- function(){
      testthat:::test_code(test_name, code_subs, env=e)
    }
    gc()
    memory <- as.numeric(pryr::mem_change(run()))
    gc()
    status <- "pass"
    time_df <- data.frame(test_name, memory, status, sha_val)
    test_results[[test_name]] <<- memory
  }
  
  devtools::load_all("./")
#   require(testthat)
#   pryr::mem_change(source(temp_file, local = T))
  test_results <- do.call(rbind, test_results)
  source(temp_file, local = T)
  rownames(test_results) <- NULL
  test_results
}

##  -----------------------------------------------------------------------------------------