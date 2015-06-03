# The latest_sha function, given a git repository path returns a character vector which is the
# SHA1 value for the latest commit of the repository.

latest_sha <- function(path = "./"){
  stopifnot(is.character(path))
  stopifnot(length(path) == 1)
  
  target <- git2r::repository(path)
  attr((git2r::commits(target, n = 1))[[1]], which = "sha")  
}

##  -----------------------------------------------------------------------------------------

# The list_commits function, given a repository path and number of commits (n), returns
# a data frame containing the SHA1 values and summary of the last n commits in the repo.

list_commits <- function(path = "./", num_commits = 20, test_path = NULL){
  stopifnot(is.character(path))
  stopifnot(length(path) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  num_commits <- floor(num_commits)
  if (!is.null(test_path)) {
    stopifnot(is.character(test_path))
    stopifnot(length(test_path) == 1)
  }
  target <- git2r::repository(path)
  if (!is.null(test_path)) {
    test_lines <- readLines(test_path)
    qlines <- sub("test_that(", "testthatQuantity(", test_lines, fixed=TRUE)
    qfile <- tempfile()
    writeLines(qlines, qfile)
  }
  
  commit_list <- git2r::commits(target, n = num_commits, reverse = T)
  sha_vec  <- character(num_commits)
  msg_vec  <- character(num_commits)
#   time_vec <- numeric(num_commits)
  for (i in 1:num_commits) {
    com <- attr(commit_list[[i]], which = "sha")
    msg <- attr(commit_list[[i]], which = "summary")
    sha_vec[i] <- com
    msg_vec[i] <- msg
#     git2r::checkout(commit_list[[i]])
#     time_vec[i] <- get_time(qfile, 3, test_path) 
  }
  
  git2r::checkout(target, "master")
  
  data.frame(SHA1 = sha_vec, Summary = msg_vec, stringsAsFactors = F)
}

##  -----------------------------------------------------------------------------------------

# The get_time function, given a test-file path, checks its run-time against the specified
# commit in the current git repository.

get_time <- function(test_path, test_commit) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(!is.null(test_commit))
  stopifnot(git2r::is_commit(test_commit))

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
    time_df <- data.frame(test_name, seconds, status)
    test_results[[test_name]] <<- time_df
  }
  
  source(temp_file, local = T)
  do.call(rbind, test_results)
}

##  -----------------------------------------------------------------------------------------