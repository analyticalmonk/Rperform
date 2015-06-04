# The get_sha function, given a git commit object returns a character vector which is the
# SHA1 value for the given commit.

get_sha <- function(commit_val){
  stopifnot(git2r::is_commit(commit_val))
  
  attr(commit_val, which = "sha")  
}

##  -----------------------------------------------------------------------------------------
