# head_sha <- function(path = "./"){
#   stopifnot(is.character(path))
#   stopifnot(length(path) == 1)
#   
#   target <- git2r::repository(path)
#   
# }

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
  time_vec <- numeric(num_commits)
  for (i in 1:num_commits) {
    com <- attr(commit_list[[i]], which = "sha")
    msg <- attr(commit_list[[i]], which = "summary")
    sha_vec[i] <- com
    msg_vec[i] <- msg
    git2r::checkout(commit_list[[i]])
    time_vec[i] <- get_time(qfile, 3, test_path) 
  }
  
  git2r::checkout(target, "master")
  
  data.frame(SHA1 = sha_vec, Summary = msg_vec, stringsAsFactors = F)
}


get_time <- function(test_file, num_repetitions = 3, test_path) {
  old_wd <- setwd(dirname(test_path))
  on.exit({
    setwd(old_wd)
  })
  
  test_results <- list()
  testthatQuantity <- function(test_name, code){
    e <- parent.frame()
    code_subs <- substitute(code)
    run <- function(){
      testthat:::test_code(test_name, code_subs, env=e)
    }
    seconds <- if(require(microbenchmark)){
      times <- microbenchmark(test={
        run()
      }
      , times = num_repetitions)
      times$time/1e9
    }else{
      replicate(num_repetitions, {
        time.vec <- system.time({
          run()
        })
        time.vec[["elapsed"]]
      })
    }
    time_df <- data.frame(test.name, seconds)
    test_results[[test.name]] <<- time.df
  }
  source(qfile, local = TRUE)
  do.call(rbind, test_results)
}