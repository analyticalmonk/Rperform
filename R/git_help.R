#' SHA1 value of a git commit object.
#' 
#' \code{get_sha(commit_val = )} returns the SHA1 value for the git commit object provided
#' as the parameter.
#' 
#' @param commit_val git commit object, as returned by git2r::commits()
#' 
#' @seealso \code{\link[git2r]{commits}}

# The get_sha function, given a git commit object returns a character vector which is the
# SHA1 value for the given commit.

get_sha <- function(commit_val) {
  stopifnot(git2r::is_commit(commit_val))
  
  attr(commit_val, which = "sha")  
}

##  -----------------------------------------------------------------------------------------
#' DateTime value of a git commit object.
#' 
#' \code{get_sha(commit_val = )} returns the date-time value for the git commit
#' object provided as the parameter.
#' 
#' @param commit_val git commit object, as returned by git2r::commits()
#' 
#' @seealso \code{\link[git2r]{commits}}

get_datetime <- function(commit_val) {
  stopifnot(git2r::is_commit(commit_val))
  
  as.POSIXct(git2r::when(commit_val$author$when))
}

##  -----------------------------------------------------------------------------------------

#' Message summary of a git commit object.
#' 
#' \code{get_sha(commit_val = )} returns the summary of the message for the git
#' commit object provided as the parameter.
#' 
#' @param commit_val git commit object, as returned by git2r::commits()
#' 
#' @seealso \code{\link[git2r]{commits}}

# The get_sha function, given a git commit object returns a character vector which is the
# message's summary for the given commit.

get_msg <- function(commit_val) {
  stopifnot(git2r::is_commit(commit_val))
  
  base::substr(commit_val$summary, start = 1, stop = 15)  
}

##  -----------------------------------------------------------------------------------------

#' Current branch name of a git repository.
#' 
#' \code{get_branch} returns the current branch name for the git repository passed in as
#' parameter (default being the current repository).
#' 
#' @param dir_path Path of the git repository.
#' 
#' @seealso \code{\link[git2r]{repository}}

get_branch <- function(dir_path = "./") {
  repo <- git2r::repository(dir_path)
  git2r::repository_head(repo)$name
}

##  -----------------------------------------------------------------------------------------

