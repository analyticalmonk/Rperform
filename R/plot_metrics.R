#' Plot run-time across versions.
#' 
#' Given a test-file path, plot the run-time of entire file and individual 
#' testthat blocks against the commit message summaries of the specified number 
#' of commits in the current git repository.
#' 
#' @param test_path File-path of the test-file which is to be used for run-time
#'   comparisons.
#' @param num_commits Number of commits (versions) against which the file is to
#'   be tested, with default being 5.
#'   
#' @examples
#' ## Example-1
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Specify the test-file path
#' t_path <- "Path/to/file"
#' 
#' # Pass the parameters and obtain the run-time details against 10 commits
#' library(Rperform)
#' plot_time(test_path = t_path, n_commits = 10)
#' 
#' @section WARNING:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 

## The plot_time function, given a test-file path, plots the time taken by 
## individual testthat blocks against the corresponding commit message values
## for the given number of commits.

plot_time <- function(test_path, num_commits = 5) {
  time_frame <- get_times(test_path, num_commits)
  ggplot2::qplot(msg_val, seconds, data = time_frame, color = test_name) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
    ggplot2::scale_x_discrete(limits = rev(levels(time_frame$msg_val)))
  # In the above 3 lines code, the first line creates the basic qplot. The 
  # second and third lines display the x-axis labels at 90 degrees to the 
  # horizontal and correct the order of message labels on the x -axis,
  # respectively.
  
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Plot memory statistics across versions.
#' 
#' Given a test-file path, plot the memory usage statistics of entire file 
#' against the commit message summaries of the specified number of commits in
#' the current git repository. The statistics plotted are the memory leaked and
#' memory swapped during the run of a test file.
#' 
#' @param test_path File-path of the test-file which is to be used for run-time
#'   comparisons.
#' @param num_commits Number of commits (versions) against which the file is to
#'   be tested, with default being 5.
#'   
#' @examples
#' ## Example-1
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Specify the test-file path
#' t_path <- "Path/to/file"
#' 
#' # Pass the parameters and obtain the memory usage details against 10 commits
#' library(Rperform)
#' plot_mem(test_path = t_path, n_commits = 10)
#' 
#' @section WARNING:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 

# The plot_mem function, given a test-file path, plots the memory usage 
# statistics (memory leaked and memory swapped) of entire test file and
# individual testthat blocks against the corresponding commit message values for
# the given number of commits.

plot_mem <- function(test_path, num_commits = 5) {
    
  ## Multiple plot function
  #--- Source code obtained courtesy Winston Chang from:
  # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/ ---
  #---------------------------------------------------------------------------------
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  #---------------------------------------------------------------------------------
  
  mem_frame <- mem_compare(test_path, num_commits)
  
  swap_plot <- ggplot2::qplot(msg_val, swap_mb, data = mem_frame) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
    ggplot2::scale_x_discrete(limits = rev(levels(mem_frame$msg)))
  # In the above 3 lines code, the first line creates the basic qplot. The 
  # second and third lines display the x-axis labels at 90 degrees to the 
  # horizontal and correct the order of message labels on the x -axis,
  # respectively.
  
  leak_plot <- ggplot2::qplot(msg_val, leak_mb, data = mem_frame) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
    ggplot2::scale_x_discrete(limits = rev(levels(mem_frame$msg)))
  # Same explanation as above.
  
  file_plots <- list(swap_plot, leak_plot)
  multiplot(plotlist = file_plots)
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------
#' Plot run-times across branches.
#' 
#' Given a test-file and two branches, plots the run-times of the file against 
#' the first commit till the latest common commit in branch1, and against the 
#' latest commit in branch2. The vertical line divides the commits from the two
#' branches with the ones from branch1 on the left side.
#' 
#' @param test_path File-path for the test file to be tested.
#' @param branch_1 Branch against whose commits the test file is to be 
#'   tested.
#' @param branch_2 Branch into which branch1 is supposedly to be merged.  
#'   
#' @examples
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Set the file-path
#' t_path <- "Path/to/file"
#'
#' # Load the library and pass the parameters to the function
#' library(Rperform)
#' compare_brancht(test_path = t_path, branch1 = "helper", branch2 = "master")
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
plot_btimes <- function(test_path, branch1, branch2 = "master") {
  btimes_df <- compare_brancht(test_path = test_path, branch1 = branch1,
                               branch2 = branch2)
  common_commitdf <- (.common_commit(branch1, branch2))
  
  ggplot2::qplot(x = message, y = seconds, data = btimes_df, color = test_name) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))+
    ggplot2::scale_x_discrete(limits = rev(levels(btimes_df$msg_val))) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = common_commitdf$cnum_b1 + 0.5))
  # In the above 4 lines code, the first line creates the basic qplot. The 
  # second and third lines display the x-axis labels at 90 degrees to the 
  # horizontal and correct the order of message labels on the x -axis, 
  # respectively. The fourth line of code divides the commits from the two
  # different branches by a vertical line.
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

## The plot_directory function, given a test-directory path, plots the time 
## taken by all the test files present inside the directory (including those of 
## the individual testthat blocks) against the corresponding sha_values for the 
## given number of commits. It stores the plots as png files in the root
## repository directory.

.plot_directory <- function(test_dir, num_commits = 5) {
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
