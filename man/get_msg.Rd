% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/git_help.R
\name{get_msg}
\alias{get_msg}
\title{Message summary of a git commit object.}
\usage{
get_msg(commit_val)
}
\arguments{
\item{commit_val}{git commit object, as returned by git2r::commits()}
}
\description{
\code{get_sha(commit_val = )} returns the summary of the message for the git
commit object provided as the parameter.
}
\seealso{
\code{\link[git2r]{commits}}
}

