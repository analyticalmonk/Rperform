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
#' @seealso \code{\link[POSIXct]}

get_datetime <- function(commit_val) {
  stopifnot(git2r::is_commit(commit_val))
  
  methods::as((commit_val@committer@when), "POSIXct")
}

##  -----------------------------------------------------------------------------------------

## Return the current git branch

get_branch <- function() {
  repo <- repository(file.path("./"))
  git2r::head(repo)@name
#   b_list <- system(command = "git branch", intern = TRUE)
#   b_logical <- grepl(pattern = '\\* ', x = b_list)
#   gsub(pattern = "\\* ",replacement = "", x =  b_list[b_logical])
}

##  -----------------------------------------------------------------------------------------
