
plot_metrics <- function(test_path, metric = "time", num_commits = 5, save_data = FALSE) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(metric))
  stopifnot(length(metric) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  stopifnot(is.logical(save_data))
  stopifnot(length(save_data) == 1)
  
  if (metric == "time") {
    .plot_time(test_path, num_commits, save_data)
  }
  else if (metric == "mem") {
    .plot_mem(test_path, num_commits, save_data)
  }
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Plot run-time across versions.
#' 
#' Given a test-file path, plot the run-time of entire file and individual 
#' testthat blocks against the commit message summaries of the specified number 
#' of commits in the current git repository. If the parameter save_data is set 
#' to true, it also stores the corresponding data frame in an RData file in a 
#' folder 'Rperform_Data' in the current directory.
#' 
#' @param test_path File-path of the test-file which is to be used for run-time
#'   comparisons.
#' @param num_commits Number of commits (versions) against which the file is to
#'   be tested, with default being 5.
#' @param save_data If set to TRUE, the data frame containing the time metrics
#'   information is stored.
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
#' plot_time(test_path = t_path, n_commits = 10, save_data = F)
#' 
#' @section WARNING:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 

## The plot_time function, given a test-file path, plots the time taken by 
## individual testthat blocks against the corresponding commit message values 
## for the given number of commits. If the parameter save_data is set to true,
## it also stores the corresponding data frame in an RData file in a folder 
## 'Rperform_Data' in the current directory.

.plot_time <- function(test_path, num_commits = 5, save_data = FALSE) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.numeric(num_commits))
  num_commits <- floor(num_commits)
  
  # Obtain the metrics data
  suppressMessages(time_frame <- time_compare(test_path, num_commits))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){  
    
    # Store the metric data
    .save_data(time_frame, pattern = "*.[rR]$", replacement = "_time.RData",
               replace_string = basename(test_path))
  }
  
  # Plot the metric data
  ggplot2::qplot(message, metric_val, data = time_frame) +
    ggplot2::facet_grid(facets =  test_name ~ ., scales = "free") +
    ggplot2::geom_point(color = "blue") + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
    ggplot2::scale_x_discrete(limits = rev(levels(time_frame$message))) +
  # In the above 3 lines code, the first line creates the basic qplot. The 
  # second and third lines display the x-axis labels at 90 degrees to the 
  # horizontal and correct the order of message labels on the x -axis,
  # respectively.
    ggplot2::xlab("Commit message") +
    ggplot2::ylab("Time (in seconds)") +
    ggplot2::ggtitle(label = "Variation in runtime across Git versions")
  
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
#' @param save_data If set to TRUE, the data frame containing the memory metrics
#'   is stored.
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

.plot_mem <- function(test_path, num_commits = 5, save_data = FALSE) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.numeric(num_commits))
  num_commits <- floor(num_commits)
  
  # Obtain the metrics data
  suppressMessages(mem_frame <- mem_compare(test_path, num_commits))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){  
    
    # Store the metric data
    .save_data(mem_frame, pattern = "*.[rR]$", replacement = "_mem.RData",
               replace_string = basename(test_path))
  }  
  
  ggplot2::qplot(message, metric_val, data = mem_frame, color = metric_name) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90),
                   strip.text.x = ggplot2::element_text(size = 10, face = "bold")) +
    ggplot2::scale_x_discrete(limits = rev(levels(mem_frame$message))) + 
    ggplot2::facet_grid(test_name ~ metric_name, scales = "free") +
  # In the above 4 lines of code, the first line creates the basic qplot. The 
  # second and third lines display the x-axis labels at 90 degrees to the 
  # horizontal and correct the order of message labels on the x -axis,
  # respectively. The fourth line creates a facet grid so as to seperate
  # the plots for the swap and leak memory metrics.
    ggplot2::ylab(label = "Memory (in mb)") +
    ggplot2::xlab(label = "Commit messages") +
    ggplot2::ggtitle(label = "Variation in memory metrics acros Git versions")
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Plot time and memory metrics of all test files on a webpage
#' 
#' It plots the time and memory metrics for all the tests present in the specified
#' directory of the current git repository on a webpage.
#' 
#' @param test_directory Path of the directory containing the test files.
#' @param out_file Name of the output webpage.
#' 
#' @examples
#' ## Example-1
#' 
#' # Set to the git repository in consideration.
#' setwd("path/to/repo")
#' 
#' # Load the package and run the function
#' library(Rperform)
#' plot_webpage(test_directory = "path/to/tests", out_file = "output.Rmd")
#' 
#' @section WARNING:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#'   

plot_webpage <- function(test_directory = "tests/testthat", output_name = "index"){
  stopifnot(is.character(test_directory))
  stopifnot(is.character(output_name))
  stopifnot(length(test_directory) == 1)
  stopifnot(length(output_name) == 1)
  
  out_file <- paste0(output_name, ".Rmd")
  
  if(!file.exists(out_file)){
    file.create(out_file)
  }
  
  line_p1 <- "---\ntitle: \"plot\"\noutput: html_document\n---\n\n```{r}\nRperform::plot_directory(\""
  line_p3 <- "\")\n```"
  file_lines <- paste0(line_p1, test_directory, line_p3)
  writeLines( file_lines, con = out_file)
  knitr::knit2html(input = out_file, output = paste0(output_name, ".html"))
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
#' plot_btimes(test_path = t_path, branch1 = "helper", branch2 = "master")
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
plot_btimes <- function(test_path, branch1, branch2 = "master") {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch1))
  stopifnot(length(branch1) == 1)
  stopifnot(is.character(branch2))
  stopifnot(length(branch2) == 1)
  
  btimes_df <- compare_brancht(test_path = test_path, branch1 = branch1,
                               branch2 = branch2)
  common_commitdf <- (.common_commit(branch1, branch2))
  
  ggplot2::qplot(x = message, y = metric_val, data = btimes_df, color = test_name) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))+
    ggplot2::scale_x_discrete(limits = rev(levels(btimes_df$message))) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = common_commitdf$cnum_b1 + 0.5, size = 2)) +
  # In the above 4 lines code, the first line creates the basic qplot. The 
  # second and third lines display the x-axis labels at 90 degrees to the 
  # horizontal and correct the order of message labels on the x -axis, 
  # respectively. The fourth line of code divides the commits from the two
  # different branches by a vertical line.
    ggplot2::ylab(label = "Time (in seconds)") +
    ggplot2::xlab(label = "Commit messages") +
    ggplot2::ggtitle(label = "Variation in time metrics acros Git branches")
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Plot memory metrics across branches.
#' 
#' Given a test-file and two branches, plots the memory metrics of the file 
#' against the first commit till the latest common commit in branch1, and 
#' against the latest commit in branch2. The memory metrics plotted are the 
#' memory leaked and maximum swapped memory during execution. The vertical line 
#' divides the commits from the two branches with the ones from branch1 on the 
#' left side.
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
#' plot_bmemory(test_path = t_path, branch1 = "helper", branch2 = "master")
#' 
#' @section Warning:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 
plot_bmemory <- function(test_path, branch1, branch2 = "master") {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch1))
  stopifnot(length(branch1) == 1)
  stopifnot(is.character(branch2))
  stopifnot(length(branch2) == 1)
  
  bmem_df <- compare_branchm(test_path = test_path, branch1 = branch1,
                               branch2 = branch2)
  common_commitdf <- (.common_commit(branch1, branch2))
  
  ggplot2::qplot(message, metric_val, data = bmem_df, color = test_name) +
    ggplot2::facet_grid(. ~ metric_name) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
    ggplot2::scale_x_discrete(limits = rev(levels(bmem_df$message))) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = common_commitdf$cnum_b1 + 0.5, size = 2)) +
  # In the above 4 lines code, the first line creates the basic qplot. The 
  # second and third lines display the x-axis labels at 90 degrees to the 
  # horizontal and correct the order of message labels on the x -axis, 
  # respectively. The fourth line of code divides the commits from the two
  # different branches by a vertical line.
    ggplot2::ylab(label = "Memory (in mb)") +
    ggplot2::xlab(label = "Commit messages") +
    ggplot2::ggtitle(label = "Variation in memory metrics acros Git branches")
  
#   file_plots <- list(swap_plot, leak_plot)
#   # To hide the warning messages from being displayed
#   suppressWarnings(.multiplot(plotlist = file_plots))
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Plot memory and time statistics across versions for all files in a given directory.
#' 
#' Given a directory path, plot the memory and time usage statistics of all files 
#' in the directory against the commit message summaries of the specified number 
#' of commits in the current git repository. 
#'
#' 
#' @param test_dir Directory containing the test-files which are to be used.
#' @param num_commits Number of commits (versions) against which the files are to
#'   be tested, with default being 5.
#' @param save_data If set to TRUE, the metrics data is saved in a folder 'Rperform_Data'
#'   in the current directory.
#'
#' @examples
#' ## Example-1
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Specify the directory containing the test files
#' d_path <- "Path/to/directory"
#' 
#' # Pass the parameters and obtain the memory usage details against 10 commits
#' library(Rperform)
#' plot_directory(test_dir = d_path, n_commits = 10)
#' 
#' @section WARNING:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 

## The plot_directory function, given a test-directory path, plots the time 
## taken by all the test files present inside the directory (including those of 
## the individual testthat blocks) against the corresponding commit messages for
## the given number of commits. 

plot_directory <- function(test_dir, num_commits = 5, save_data = FALSE) {
  
  file_names <- list.files(test_dir)
  
  # For each file, plots for both time and space metrics are plotted and stored
  # in the folder Rperform_Graphs in png format
  for (file_i in seq_along(file_names)) {
    
    # Obtain the metrics data
    suppressWarnings(mem_frame <- mem_compare(test_path = file.path(test_dir, file_names[[file_i]]),
                                              num_commits = num_commits))
    
    suppressWarnings(time_frame <- time_compare(test_path = file.path(test_dir, file_names[[file_i]]),
                                                num_commits = num_commits))
    
    metric_frame <- rbind(time_frame, mem_frame)
    
    # Print and plot the file
    plot_file <- ggplot2::qplot(data = metric_frame, x = message, y = metric_val, color = test_name) +
      ggplot2::facet_grid(metric_name ~ ., scales = "free") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
      ggplot2::scale_x_discrete(limits = rev(levels(metric_frame$message))) +
      ggplot2::xlab(label = "Commit messages") + 
      ggplot2::ggtitle("Variation in metrics across Git versions")
    
    print(plot_file)
    
    # Store the metrics data if save_data is TRUE
    if (save_data){  
      # Store the metric data
      
      .save_data(time_frame, pattern = "*.[rR]$", replacement = "_time.RData",
                 replace_string = basename(test_path))
      
      .save_data(mem_frame, pattern = "*.[rR]$", replacement = "_mem.RData",
                 replace_string = basename(test_path))
    }      
  }
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

.save_data <- function(metric_frame, pattern = "*.[rR]$", replacement, replace_string = test_path) {
  
  # Create a directory for storing the metric data
  if (!dir.exists("./Rperform_Data")){
    dir.create(path = "./Rperform_Data")
  }
  
  if(grepl(pattern = "time", x = replacement) > 0) {
    time_frame <- metric_frame
    save(time_frame, file = file.path("Rperform_Data", sub(pattern = pattern,
                                                             replacement = replacement,
                                                             x = basename(replace_string))))
  }
  else if(grepl(pattern = "mem", x = replacement) > 0){
    mem_frame <- metric_frame
    save(mem_frame, file = file.path("Rperform_Data", sub(pattern = pattern,
                                                             replacement = replacement,
                                                             x = basename(replace_string))))
  }
}

##  -----------------------------------------------------------------------------------------
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
.multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
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

