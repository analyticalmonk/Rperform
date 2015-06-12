#' Plot run-time across versions.
#' 
#' Given a test-file path, plot the run-time of individual testthat blocks
#' against the date-time values of the specified number of commits (with the
#' commits' date-time values on the y-axis) in the current git repository.
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
#' plot_time(test_path = t_path, n_commits = 10)
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
#' @seealso \code{\link[git2r]{commits}}
#' @seealso \code{\link[testthat]}  

## The plot_time function, given a test-file path, plots the time taken by 
## individual testthat blocks against the corresponding date_time values for the
## given number of commits.

plot_time <- function(test_path, num_commits) {
  time_frame <- get_times(test_path, num_commits)
#   Will replace sha_val with the dates after figuring out how to obtain them.
  ggplot2::qplot(date_time, seconds, data = time_frame, color = test_name)
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

## The plot_directory function, given a test-directory path, plots the time 
## taken by all the test files present inside the directory (including those of 
## the individual testthat blocks) against the corresponding sha_values for the 
## given number of commits. It stores the plots as png files in the root
## repository directory.

plot_directory <- function(test_dir, num_commits = 5) {
#   # Multiple plot function
#   #
#   # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#   # - cols:   Number of columns in layout
#   # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#   #
#   # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#   # then plot 1 will go in the upper left, 2 will go in the upper right, and
#   # 3 will go all the way across the bottom.
#   #
#   multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#     library(grid)
#     
#     # Make a list from the ... arguments and plotlist
#     plots <- c(list(...), plotlist)
#     
#     numPlots = length(plots)
#     
#     # If layout is NULL, then use 'cols' to determine layout
#     if (is.null(layout)) {
#       # Make the panel
#       # ncol: Number of columns of plots
#       # nrow: Number of rows needed, calculated from # of cols
#       layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                        ncol = cols, nrow = ceiling(numPlots/cols))
#     }
#     
#     if (numPlots==1) {
#       print(plots[[1]])
#       
#     } else {
#       # Set up the page
#       grid.newpage()
#       pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#       
#       # Make each plot, in the correct location
#       for (i in 1:numPlots) {
#         # Get the i,j matrix positions of the regions that contain this subplot
#         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#         
#         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                         layout.pos.col = matchidx$col))
#       }
#     }
#   }
  
  file_names <- list.files(test_dir)
  for (file_i in seq_along(file_names)) {
    png(file = sub(x = file_names[[file_i]], pattern = "*.[rR]", 
                   replacement = ""))
    print(plot_time(test_path = file.path(test_dir, file_names[[file_i]])
                                      , num_commits = 5))
    dev.off()
  }
  
#   multiplot(plotlist = file_plots, col = 3)
}

##  -----------------------------------------------------------------------------------------
