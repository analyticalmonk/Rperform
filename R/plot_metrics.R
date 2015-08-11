#' Plot test-file metrics across versions.
#' 
#' Given a test-file path, plot the metrics of entire file and individual 
#' testthat blocks against the commit message summaries of the specified number 
#' of commits in the current git repository. If the parameter save_data is set 
#' to true, it also stores the corresponding data-frames in an RData file in a 
#' folder 'Rperform_Data' in the current directory.The metrics plotted are in
#' accordance with those specified using the parameter metric.
#' 
#' @param test_path File-path of the test-file which is to be used for run-time
#'   comparisons.
#' @param metric Type of plot(s) desired. (See examples below for more details)
#' @param num_commits Number of commits (versions) against which the file is to
#'   be tested, with default being 5.
#' @param save_data If set to TRUE, the data frame containing the time metrics 
#'   information is stored in the 'Rperform_Data' directory in the root of the
#'   repo.
#' @param save_plots If set to TRUE, the plots generated are stored in the
#'   'Rperform_plots' directory in the root of the repo rather than being
#'   printed.
#'   
#' @examples
#' 
#' # Set the current directory to the git repository concerned.
#' setwd("./Path/to/repository")
#' 
#' # Specify the test-file path
#' t_path <- "Path/to/file"
#' 
#' # Load the library
#' library(Rperform)
#' 
#' ## Example-1
#' 
#' # Pass the parameters and obtain the run-time followed by memory details against 10 commits
#' plot_metrics(test_path = t_path, metric = "time", n_commits = 10, save_data = F)
#' plot_metrics(test_path = t_path, metric = "memory", n_commits = 10, save_data = F)
#' 
#' ## Example-2
#' 
#' # Obtain both memory and time metrics for each individual testthat block
#' # inside a file and the file itself. The plots get stored in a directory
#' # 'Rperform_Graphs' in the repo's root directory.
#' plot_metrics(test_path = t_path, metric = "testMetrics", n_commits = 5, save_data = F)
#' 
#' @section WARNING:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 

plot_metrics <- function(test_path, metric, num_commits = 5, save_data = FALSE, save_plots = TRUE) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(metric))
  stopifnot(length(metric) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  stopifnot(is.logical(save_data))
  stopifnot(length(save_data) == 1)
  
  if (metric == "time") {
    .plot_time(test_path, num_commits, save_data, save_plots)
  }
  else if (metric == "memory") {
    .plot_mem(test_path, num_commits, save_data, save_plots)
  }
  else if (metric == "memtime") {
    .plot_time(test_path, num_commits, save_data, save_plots)
    .plot_mem(test_path, num_commits, save_data, save_plots)
  }
  else if (metric == "testMetrics") {
    .plot_testMetrics(test_path, num_commits, save_data, save_plots)
  }
}

##  -----------------------------------------------------------------------------------------

.plot_testMetrics <- function(test_path, num_commits = 5, save_data = FALSE, save_plots) {
  mem_data <- mem_compare(test_path, num_commits)
  suppressMessages(time_data <- time_compare(test_path, num_commits))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){  
    
    # Store the metric data
    .save_data(time_data, pattern = "*.[rR]$", replacement = "_time.RData",
               replace_string = basename(test_path))
    .save_data(mem_data, pattern = "*.[rR]$", replacement = "_mem.RData",
               replace_string = basename(test_path))
  }
  
  metric_data <- rbind(time_data, mem_data)
  t_names <- levels(metric_data$test_name)
  
  for (num in seq(t_names)) {
    test_frame <- metric_data[metric_data$test_name == t_names[num],]
    
    test_plot <- ggplot2::qplot(data = test_frame, x = message, y = metric_val) +
      ggplot2::facet_grid(facets = metric_name ~ ., scales = "free") + 
      ggplot2::geom_point(color = "blue") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
      ggplot2::scale_x_discrete(limits = rev(levels(test_frame$message))) +
      # In the above 3 lines code, the first line creates the basic qplot. The 
      # second and third lines display the x-axis labels at 90 degrees to the 
      # horizontal and correct the order of message labels on the x -axis,
      # respectively.
      ggplot2::xlab("Commit message") +
      ggplot2::ylab("Metric value") +
      ggplot2::ggtitle(label = paste0("Variation in metrics for ", t_names[num]))
    
    
    if (save_plots == TRUE) {
      if (!dir.exists("./Rperform_testMetrics")){
        dir.create(path = "./Rperform_testMetrics")
      }
      
      curr_name <- gsub(pattern = " ", replacement = "_", x = t_names[num])
      curr_name <- gsub(pattern = ".[rR]$", replacement = "", x = curr_name)
      png.file <- file.path("Rperform_testMetrics", paste0("Test_", curr_name, ".png"))
      png(filename = png.file, width = 1024, height = 768, units = "px")
      print(test_plot)
      dev.off()
    }
    else {
      print(test_plot)
    }
  
  }
}

##  -----------------------------------------------------------------------------------------

.plot_time <- function(test_path, num_commits = 5, save_data = FALSE, save_plots) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.numeric(num_commits))
  num_commits <- floor(num_commits)
  
  # Obtain the metrics data
  suppressMessages(time_data <- time_compare(test_path, num_commits))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){  
    
    # Store the metric data
    .save_data(time_data, pattern = "*.[rR]$", replacement = "_time.RData",
               replace_string = basename(test_path))
  }
  
  curr_name <- gsub(pattern = " ", replacement = "_", x = basename(test_path))
  curr_name <- gsub(pattern = ".[rR]$", replacement = "", x = curr_name)
  
  # Plot the metric data
  test_plot <- ggplot2::qplot(message, metric_val, data = time_data) +
    ggplot2::facet_grid(facets =  test_name ~ ., scales = "free") +
    ggplot2::geom_point(color = "blue") + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
    ggplot2::scale_x_discrete(limits = rev(levels(time_data$message))) +
    # In the above 3 lines code, the first line creates the basic qplot. The 
    # second and third lines display the x-axis labels at 90 degrees to the 
    # horizontal and correct the order of message labels on the x -axis,
    # respectively.
    ggplot2::xlab("Commit message") +
    ggplot2::ylab("Time (in seconds)") +
    ggplot2::ggtitle(label = paste0("Variation in time metrics for ", curr_name))
                     
  if (save_plots == TRUE) {
    if (!dir.exists("./Rperform_timeMetrics")){
      dir.create(path = "./Rperform_timeMetrics")
    }
    
    png.file <- file.path("Rperform_timeMetrics", paste0("Test_", curr_name, ".png"))
    png(filename = png.file, width = 1024, height = 768, units = "px")
    print(test_plot)
    dev.off()
  }
  else {
    print(test_plot)
  }
  
}

##  -----------------------------------------------------------------------------------------

.plot_mem <- function(test_path, num_commits = 5, save_data = FALSE, save_plots) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.numeric(num_commits))
  num_commits <- floor(num_commits)
  
  # Obtain the metrics data
  suppressMessages(mem_data <- mem_compare(test_path, num_commits))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){  
    
    # Store the metric data
    .save_data(mem_data, pattern = "*.[rR]$", replacement = "_mem.RData",
               replace_string = basename(test_path))
  }  
  
  curr_name <- gsub(pattern = " ", replacement = "_", x = basename(test_path))
  curr_name <- gsub(pattern = ".[rR]$", replacement = "", x = curr_name)
  
  test_plot <- ggplot2::qplot(message, metric_val, data = mem_data) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90),
                   strip.text.x = ggplot2::element_text(size = 10, face = "bold")) +
    ggplot2::scale_x_discrete(limits = rev(levels(mem_data$message))) + 
    ggplot2::facet_grid(test_name ~ metric_name, scales = "free") +
    # In the above 4 lines of code, the first line creates the basic qplot. The 
    # second and third lines display the x-axis labels at 90 degrees to the 
    # horizontal and correct the order of message labels on the x -axis,
    # respectively. The fourth line creates a facet grid so as to seperate
    # the plots for the swap and leak memory metrics.
    ggplot2::geom_point(color = "blue") +
    ggplot2::ylab(label = "Memory (in mb)") +
    ggplot2::xlab(label = "Commit messages") +
    ggplot2::ggtitle(label = paste0("Variation in memory metrics for ", curr_name))
  
  if (save_plots == TRUE) {
    if (!dir.exists("./Rperform_memoryMetrics")){
      dir.create(path = "./Rperform_memoryMetrics")
    }
    
    curr_name <- gsub(pattern = " ", replacement = "_", x = t_names[num])
    curr_name <- gsub(pattern = ".[rR]$", replacement = "", x = curr_name)
    png.file <- file.path("Rperform_memoryMetrics", paste0("Test_", curr_name, ".png"))
    png(filename = png.file, width = 1024, height = 768, units = "px")
    print(test_plot)
    dev.off()
  }
  else {
    print(test_plot)
  }
  
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Plot the specified metrics of all test files in a specified directory on a
#' webpage.
#' 
#' It plots specified metrics for all the tests present in the specified
#' directory of the current git repository on a webpage.
#' 
#' @param test_directory Path of the directory containing the test files.
#' @param metric Type of plot(s) desired. (See examples below for more details)
#' @param out_file Name of the output .html file.
#' 
#' @examples
#' 
#' # Set to the git repository in consideration.
#' setwd("path/to/repo")
#' d_path <- "path/to/tests"
#' 
#' # Load the library
#' library(Rperform)
#' 
#' ## Example-1
#' 
#' # Pass the parameters and obtain the run-time followed by memory details against 10 commits
#' # on two seperate webpages (html files).
#' plot_webpage(test_directory = d_path, metric = "time", output_name = "timePage")
#' plot_metrics(test_directory = d_path, metric = "memory", output_name = "memPage")
#' 
#' ## Example-2
#' 
#' # Obtain both memory and time metrics for each individual testthat block
#' # inside a file and the file itself.
#' plot_webpage(d_path, metric = "testMetrics", output_name = "testMetricsPage")
#' 
#' @section WARNING:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#'   

plot_webpage <- function(test_directory = "tests/testthat", metric = "testMetrics",
                         output_name = "index"){
  stopifnot(is.character(test_directory))
  stopifnot(is.character(output_name))
  stopifnot(is.character(metric))
  stopifnot(length(test_directory) == 1)
  stopifnot(length(output_name) == 1)
  stopifnot(length(metric) == 1)
  
  out_file <- paste0(output_name, ".Rmd")
  
  if(!file.exists(out_file)){
    file.create(out_file)
  }
  
  line_p1 <- "---\ntitle: \"plot\"\noutput: html_document\n---\n\n```{r}\nRperform::plot_directory(\""
  line_p3 <- "\", metric = \"" 
  line_p5 <- "\", save_plots = FALSE)\n```"
  file_lines <- paste0(line_p1, test_directory, line_p3, metric, line_p5)
  writeLines(file_lines, con = out_file)
  knitr::knit2html(input = out_file, output = paste0(output_name, ".html"))
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Plot metrics across versions for all files in a given directory.
#' 
#' Given a directory path, plot the memory and time usage statistics of all files 
#' in the directory against the commit message summaries of the specified number 
#' of commits in the current git repository. 
#'
#' 
#' @param test_dir Directory containing the test-files which are to be used.
#' @param metric Type of plot(s) desired. (See examples below for more details)
#' @param num_commits Number of commits (versions) against which the files are to
#'   be tested, with default being 5.
#' @param save_data If set to TRUE, the metrics data is saved in a folder 'Rperform_Data'
#'   in the current directory.
#' @param save_plots If set to TRUE, the plots generated are stored in the
#'   'Rperform_plots' directory in the root of the repo rather than being
#'   printed.   
#'
#' @examples
#' # Set to the git repository in consideration.
#' setwd("path/to/repo")
#' d_path <- "path/to/tests"
#' 
#' # Load the library
#' library(Rperform)
#' 
#' ## Example-1
#' 
#' # Pass the parameters and obtain the run-time followed by memory details against 10 commits.
#' plot_directory(test_directory = d_path, metric = "time", num_commits = 10,
#'                save_data = F, save_plots = T) 
#' plot_directory(test_directory = d_path, metric = "memory", num_commits = 10,
#'                save_data = F, save_plots = T)
#' 
#' ## Example-2
#' 
#' # Obtain both memory and time metrics for each individual testthat block
#' # inside a file and the file itself ,and save the resulting plot as well as
#' # data.
#' plot_directory(d_path, metric = "testMetrics", num_commits = 5, save_data = F,
#'                save_plots = T)
#' 
#' @section WARNING:
#'   Library assumes the current directory to be the root directory of the
#'   package being tested.
#' 

## The plot_directory function, given a test-directory path, plots the time 
## taken by all the test files present inside the directory (including those of 
## the individual testthat blocks) against the corresponding commit messages for
## the given number of commits. 

plot_directory <- function(test_directory, metric = "testMetrics", num_commits = 5, save_data = FALSE, 
                           save_plots = TRUE) {
  stopifnot(is.character(test_directory))
  stopifnot(is.character(metric))
  stopifnot(is.numeric(num_commits))
  stopifnot(is.logical(save_data))
  stopifnot(is.logical(save_plots))
  stopifnot(length(test_directory) == 1)
  stopifnot(length(metric) == 1)
  stopifnot(length(save_data) == 1)
  stopifnot(length(save_plots) == 1)
  
  file_names <- list.files(test_directory)
  
  # For each file, plots for both time and space metrics are plotted and stored
  # in the folder Rperform_Graphs in png format
  for (file_i in seq_along(file_names)) {
    
    # Print the plots as per the metric parameter.
    plot_metrics(test_path = file.path(test_directory, file_names[file_i]), 
                 metric = metric, num_commits = num_commits,
                 save_data = save_data, save_plots = save_plots)
    
  }
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

.save_data <- function(metric_frame, pattern = "*.[rR]$", replacement, replace_string) {
  
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

