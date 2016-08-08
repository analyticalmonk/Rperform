#' Compare performance across directories/repositories.
#' 
#' Given a test-file, two directories and the required metric, returns the 
#' metric details of the file against the first commit till the latest common 
#' commit for dir1, and against the latest commit for dir2. It also returns
#' information regarding the latest common commit for the two directories.
#' 
#' @param dir1 Path to the first directory/repository.
#' @param dir2 Path to the second directory/repository.
#' @param test_path File-path, relative to the directories, for the test file to
#'   be tested.
#' @param metric The metric (runtime or memory) for which the file is to be 
#'   tested.
#' @param PR When set to True, it performs the analysis in accordance with the setup
#'   on Travis-CI environment for testing Pull Requests.
#'   
#' @examples
#' 
#' \dontrun{
#' # Set the current directory to the parent directory of the concerned repositories.
#' setwd("./Path/to/parent/directory")
#' 
#' # Set the directory paths
#' d_path1 <- "Path/to/first/directory"
#' d_path2 <- "Path/to/second/directory"
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#' 
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_dir(d_path1, d_path2, t_path, metric = "time")
#' }
#' 
#' @section Value: compare_brancht returns an object of class "list" containing
#'   two members.
#' 
#' The first member is a data-frame and consists of the following columns:
#' \code{test_name}
#' \code{metric_name}
#' \code{status}
#' \code{metric_val}
#' \code{message}
#' \code{sha}
#' \code{date_time}
#' \code{directory}
#' 
#' The second member is a data-frame and consists of the following columns:
#' \code{common_datetime}
#' \code{common_message}
#' \code{cnum_b1}
#' \code{cnum_b2}
#' 
#' @section Warning:
#'   Function assumes the current directory to be the parent directory of both 
#'   the repositories being tested. That means both the repositories should be
#'   inside the same directory.

compare_dir <- function(dir1, dir2, test_path, metric = "time", PR = F) {
  
  # Obtain information about the latest common commit.
  same_commit <- .common_commit(dir1, dir2, PR)
  #                  same_commit
  # ---------------------------------------------
  #      common_datetime, cnum_b1, cnum_b2
  
  #   print("Printing same commit")
  #   print(same_commit)
  
  curr_dir <- "../."
  
  setwd(dir1)
  if (metric == "time") {
    if (same_commit$cnum_b2 == 1 & same_commit$cnum_b1 != 1) {
      dir1_df <- time_compare(test_path = test_path, num_commits = same_commit$cnum_b1 - 1)
    } 
    else {
      dir1_df <- time_compare(test_path = test_path, num_commits = same_commit$cnum_b1)
    } 
  }
  else if (metric == "memory") {
    if (same_commit$cnum_b2 == 1 & same_commit$cnum_b1 != 1) {
      dir1_df <- mem_compare(test_path = test_path, num_commits = same_commit$cnum_b1 - 1)
    } 
    else {
      dir1_df <- mem_compare(test_path = test_path, num_commits = same_commit$cnum_b1)
    }     
  }
  dir1_df$directory <- rep(basename(dir1), times = nrow(dir1_df))
  setwd(curr_dir)
  
  setwd(dir2)
  if (metric == "time") {
    dir2_df <- time_compare(test_path = test_path, num_commits = 1) 
  }
  else if (metric == "memory") {
    dir2_df <- mem_compare(test_path = test_path, num_commits = 1)
  }
  dir2_df$directory <- rep(basename(dir2), times = nrow(dir2_df))
  setwd(curr_dir)
  
  dir_df <- rbind(dir1_df, dir2_df)
  dir_list <- list(dir_df, same_commit)
  
}

##  -----------------------------------------------------------------------------------------
##  FUNCTIONS DESIGNED FOR TRAVIS PRs
##  -----------------------------------------------------------------------------------------

#' Generate a webpage containing a visualization detailing PR's impact on
#' performance without haveing to merge.
#' 
#' The function must be called from a directory containing only a single git
#' repository checked out to the branch which is meant to be tested against the
#' master branch of the repository's remote repo. This function is designed keeping
#' in mind the PR testing methodlogy of Travis-CI.
#' Given a test-file path and the required metric, it creates a webpage
#' visualizing the metric details of the file against the first commit till the
#' latest common commit for the git repo, and against the latest commit for
#' master branch of the repo's remote.
#' 
#' @param test_path File-path, relative to the git repo, for the test file to
#'   be tested.
#' @param metric The metric (runtime or memory) for which the file is to be 
#'   tested.
#'   
#' @examples
#' 
#' \dontrun{
#' # Set the current directory to the parent directory of the concerned repository.
#' setwd("./Path/to/parent/directory")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#' 
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' plot_PR_webpage(t_path, metric = "time")
#' }
#' 
#' @section Value: None
#' 
#' @section Warning:
#'   Function assumes the current directory to be the parent directory of the 
#'   the repository being tested.

plot_PR_webpage <- function(test_path, metric = "time") {
  
  out_file <- paste0("PR", ".Rmd")
  
  line_p1 <- "---\ntitle: \"plot\"\noutput: html_document\n---\n\n```{r}\nRperform::plot_PR(\""
  line_p3 <- "\", metric = \""
  line_p5 <- "\")\n```"
  file_lines <- paste0(line_p1, test_path, line_p3, metric, line_p5)
  writeLines(file_lines, con = out_file)
  rmarkdown::render(input = out_file, output_format = "html_document", 
                    output_file = paste0("RperformTest", ".html"))
}

##  -----------------------------------------------------------------------------------------

## FUNCTION TO VISUALIZE METRIC DETAILS FOR A PR ON TRAVIS-CI

#' Visualize PR's impact on performance without having to merge.
#' 
#' The function must be called from a directory containing only a single git
#' repository checked out to the branch which is meant to be tested against the
#' master branch of the repository's remote repo. This function is designed keeping
#' in mind the PR testing methodlogy of Travis-CI.
#' Given a test-file path and the required metric, it plots the metric details
#' of the file against the first commit till the latest common commit for the
#' git repo, and against the latest commit for master branch of the repo's
#' remote.
#' 
#' @param test_path File-path, relative to the git repo, for the test file to
#'   be tested.
#' @param metric The metric (runtime or memory) for which the file is to be 
#'   tested.
#'   
#' @examples
#' 
#' \dontrun{
#' # Set the current directory to the parent directory of the concerned repository.
#' setwd("./Path/to/parent/directory")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#' 
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' plot_PR(t_path, metric = "time")
#' }
#' 
#' @section Value: None
#' 
#' @section Warning:
#'   Function assumes the current directory to be the parent directory of the 
#'   the repository being tested.

plot_PR <- function(test_path, metric = "time") {  
  
  dir_list <- compare_PR(test_path)
  same_commit <- dir_list[[2]]
  PR_data <- dir_list[[1]]
  dir1 <- unique(PR_data$directory)[1]
  dir2 <- unique(PR_data$directory)[2]
  
  curr_name <- gsub(pattern = " ", replacement = "_", x = basename(test_path))
  curr_name <- gsub(pattern = "*.[rR]$", replacement = paste0("_", dir1, "_", dir2),
                    x = curr_name)
  
  # Trying to find the min and max vals for each test
  ###################################################
  
  extremes_frame <- .find_midvals(data = PR_data)  
  ##                        extremes_frame
  ## test_name   |      max_val     |    min_val    |   mid_val
  ## ----------------------------------------------------------
  
  ###################################################
  
  # Plot the dires' metric data
  tryCatch(expr = {test_plot <- 
                     ggplot2::ggplot(data = PR_data, mapping = ggplot2::aes(message, metric_val)) +
                     ggplot2::geom_point(color = "blue") +
                     ggplot2::facet_grid(test_name ~ metric_name, scales = "free") +
                     ggplot2::geom_text(data = extremes_frame, 
                                        mapping = ggplot2::aes(x = same_commit$cnum_b2 + 0.3,
                                                               y = mid_val,
                                                               label = dir2, angle = 90)) +
                     ggplot2::geom_text(data = extremes_frame, 
                                        mapping = ggplot2::aes(x = same_commit$cnum_b2 + 0.7,
                                                               y = mid_val,
                                                               label = dir1, angle = -90)) +                     
                     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90),
                                    strip.text.x = ggplot2::element_text(size = 10, face = "bold")) +
                     ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = same_commit$cnum_b2 + 0.5)) +
                     ggplot2::scale_x_discrete(limits = rev(levels(PR_data$message))) +
                     # In the above 8 lines of code, the first line creates
                     # the basic plot. The sixth and eigth lines display the
                     # x-axis labels at 90 degrees to the horizontal and
                     # correct the order of message labels on the x -axis, 
                     # respectively. The seventh line plots a vertical seperator between
                     # the commit from dir2 and the commits from dir1.
                     ggplot2::xlab(label = "Commit messages") +
                     ggplot2::ylab(label = "Memory (in Mb)") +
                     ggplot2::ggtitle(label = paste0("Variation in ", metric,  " metrics across branches ",
                                                     dir2, " and ", dir1))
                   
                   print(test_plot)
                   
  },
  error = function(e){
    print("Encountered an error!")
  })
  
}

##  -----------------------------------------------------------------------------------------

## FUNCTION TO OBTAIN METRIC DETAILS FOR THE COMMITS FROM A PR ON TRAVIS-CI

#' Analyze PR's impact on performance without having to merge.
#' 
#' The function must be called from a directory containing only a single git
#' repository checked out to the branch which is meant to be tested against the
#' master branch of the repository's remote repo. This function is designed keeping
#' in mind the PR testing methodlogy of Travis-CI.
#' Given a test-file path and the required metric, it returns the metric details
#' of the file against the first commit till the latest common commit for the
#' git repo, and against the latest commit for master branch of the repo's
#' remote. It also returns information regarding the latest common commit for
#' the two directories.
#' 
#' @param test_path File-path, relative to the git repo, for the test file to
#'   be tested.
#' @param metric The metric (runtime or memory) for which the file is to be 
#'   tested.
#'   
#' @examples
#' 
#' \dontrun{
#' # Set the current directory to the parent directory of the concerned repository.
#' setwd("./Path/to/parent/directory")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#' 
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_PR(t_path, metric = "time")
#' }
#' 
#' @section Value: compare_brancht returns an object of class "list" containing
#'   two members.
#' 
#' The first member is a data-frame and consists of the following columns:
#' \code{test_name}
#' \code{metric_name}
#' \code{status}
#' \code{metric_val}
#' \code{message}
#' \code{sha}
#' \code{date_time}
#' \code{directory}
#' 
#' The second member is a data-frame and consists of the following columns:
#' \code{common_datetime}
#' \code{common_message}
#' \code{cnum_b1}
#' \code{cnum_b2}
#' 
#' @section Warning:
#'   Function assumes the current directory to be the parent directory of the 
#'   the repository being tested.

compare_PR <- function(test_path, metric = "time") {
  
  # Obtain remote
  targetdir_1 <- "Rperform_copy"
  remote <- git2r::remote_url(repo = git2r::repository(path = targetdir_1))[1]
  
  # Clone master branch as a directory
  git2r::clone(url = remote, local_path = "./master", branch = "master")
  targetdir_2 <- "master"
  
  # Compare the two directories
  dir_list <- compare_dir(targetdir_1, targetdir_2, test_path, metric, PR=T)
  
  unlink(x = "master/", recursive = T, force = T)
  
  dir_list
}
