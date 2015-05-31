list_commits <- function(path = "./", num_commits = 20){
  stopifnot(is.character(path))
  stopifnot(length(path) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  num_commits <- floor(num_commits)
  
  target <- git2r::repository(path)
  commit_list <- git2r::commits(target, n = num_commits)
  sha_list <- list()
  for (c in commit_list) {
    c <- attr(c, which = "sha")
    sha_list[[c]] <- c
  }  
  as.data.frame(sha_list)
}