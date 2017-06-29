## The below function call results in the listed variables being treated
## as global variables when the 'check' tool is applied. Here, this results
## in reduction of NOTEs being returned when R CMD CHECK is applied to the
## package.
## Sample NOTE:
## plot_bmemory: no visible binding for global variable ‘metric_val’
utils::globalVariables(c("metric_val", "test_name"))

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
#' @param metric Type of plot(s) desired. This can be set to \code{time},
#'   \code{memory}, \code{memtime} or \code{testMetrics}. (See examples below
#'   for more details)
#' @param num_commits Number of commits (versions) against which the file is to
#'   be tested, with default being 5.
#' @param save_data If set to TRUE, the data frame containing the metrics
#'   information is stored in the 'Rperform_Data' directory in the root of the
#'   repo. (default set to FALSE)
#' @param save_plots If set to TRUE, the plots generated are stored in the
#'   'Rperform_plots' directory in the root of the repo rather than being
#'   printed. (default set to TRUE)
#' @param interactive If set to TRUE, the plots generated are interactive. The
#'   resulting plot is rendered in the default browser.
#'
#' @examples
#'
#' \dontrun{
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
#' # 'Rperform_testMetrics' in the repo's root directory.
#' plot_metrics(test_path = t_path, metric = "testMetrics", n_commits = 5, save_data = F)
#' }
#'
#' @section WARNING:
#'   Function assumes the current directory to be the root directory of the
#'   repository/package being tested.
#'

plot_metrics <- function(test_path, metric, num_commits = 5, save_data = FALSE, save_plots = FALSE,
                         interactive = FALSE) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(metric))
  stopifnot(length(metric) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  stopifnot(is.logical(save_data))
  stopifnot(length(save_data) == 1)
  stopifnot(is.logical(save_plots))
  stopifnot(length(save_plots) == 1)
  floor(num_commits)
  
  if (metric == "time") {
    if (interactive) {
	#dev store r ouput to a file,  a capture.output
      temp_out <- capture.output(.plot_interactive_time(test_path, num_commits, save_data, save_plots))
    } else {
      temp_out <- capture.output(.plot_time(test_path, num_commits, save_data, save_plots))
    }
  }
  else if (metric == "memory") {
    if (interactive) {
      temp_out <- capture.output(.plot_interactive_mem(test_path, num_commits, save_data, save_plots))
    } else {
      temp_out <- capture.output(.plot_mem(test_path, num_commits, save_data, save_plots))      
    }
  }
  else if (metric == "memtime") {
    if (interactive) {
      temp_out <- capture.output(.plot_interactive_time(test_path, num_commits, save_data, save_plots))
      temp_out <- capture.output(.plot_interactive_mem(test_path, num_commits, save_data, save_plots))      
    } else {
      temp_out <- capture.output(.plot_time(test_path, num_commits, save_data, save_plots))
      temp_out <- capture.output(.plot_mem(test_path, num_commits, save_data, save_plots))
    }
  }
  else if (metric == "testMetrics") {
    if (interactive) {
      cat("Interactive mode not available for this metric!\nPrinting static plots instead.")
    }
    temp_out <- capture.output(.plot_testMetrics(test_path, num_commits, save_data, save_plots))
  }
  else {
    temp_out <- NULL
    print("Input a valid metric parameter!")
  }
  remove(temp_out)
}

##  -----------------------------------------------------------------------------------------

.plot_testMetrics <- function(test_path, num_commits, save_data, save_plots) {
  suppressMessages(mem_data <- mem_compare(test_path, num_commits))
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
    
    tryCatch(expr = {test_plot <- 
                       ggplot2::ggplot(data = test_frame, mapping = ggplot2::aes(message, metric_val)) +
                       ggplot2::geom_point(color = "blue") + 
                       ggplot2::facet_grid(facets = metric_name ~ ., scales = "free") +
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
                       ggplot2::scale_x_discrete(limits = rev(levels(test_frame$message))) +
                       # In the above 5 lines of code, the first line creates the basic qplot. The
                       # fourth and fifth lines display the x-axis labels at 90 degrees to the
                       # horizontal and correct the order of message labels on the x -axis,
                       # respectively.
                       ggplot2::xlab("Commit message") +
                       ggplot2::ylab("Metric value") +
                       ggplot2::ggtitle(label = paste0("Variation in metrics for ", t_names[num]))
                     
                     
                     if (save_plots == TRUE) {
                       .save_plots(test_plot = test_plot, test_name = t_names[num], metric = "testMetrics")
                       print(test_plot)
                     }
                     else {
                       print(test_plot)
                     }
    },
    error = function(e) {
      print("Encountered an error!")
    })
  }
}

##  -----------------------------------------------------------------------------------------

.plot_interactive_time <- function(test_path, num_commits, save_data, save_plots) {
  
  # Obtain the metrics data
  suppressMessages(time_data <- time_compare(test_path, num_commits))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){
    
    # Store the metric data
    .save_data(time_data, pattern = "*.[rR]$", replacement = "_time.RData",
               replace_string = basename(test_path))
  }
  
  # Add links to the github page for each commit to data
  remoteUrl <- git2r::remote_url(repo = git2r::repository(path = "./"))
  remoteUrl <- (paste0(remoteUrl, "/commit/"))
  time_data$remoteUrl <- paste0(remoteUrl, time_data$sha)
  
  levels(time_data$test_name) <- paste0(substr(levels(time_data$test_name), start = 0, stop = 4),
                                        "...",
                                        substr(levels(time_data$test_name), 
                                               start = nchar(levels(time_data$test_name)) - 4,
                                               stop = nchar(levels(time_data$test_name))))

  test_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = message, y = metric_val,
                                               href = remoteUrl),
                        color = "blue",
                        data = time_data) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::facet_grid(facets = test_name~., scales = "free") +
    ggplot2::scale_x_discrete(limits = rev(levels(time_data$message))) +  
    ggplot2::xlab("Commit message") +
    ggplot2::ylab("Runtime value") +
    ggplot2::ggtitle(label = paste0("Variation in runtime for ", basename(test_path)))
  
  if (length(levels(time_data$test_name)) > 6) {
    test_plot <- test_plot + 
      animint::theme_animint(height = 700)
  }
  else if (length(levels(time_data$test_name)) > 3) {
    test_plot <- test_plot +
      animint::theme_animint(height = 650)
  } 
  
  viz.list <- list(timeplot = test_plot)

  print("Loaded animint")
  animint::animint2dir(plot.list = viz.list, out.dir = paste0(basename(getwd()), "_", "time_animint"))
  unlink(x = paste0(basename(getwd()), "_", "time_animint"), recursive = T, force = T)
}

##  -----------------------------------------------------------------------------------------


.plot_time <- function(test_path, num_commits, save_data, save_plots) {
  
  # Obtain the metrics data
  suppressMessages(time_data <- time_compare(test_path, num_commits))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){
    
    # Store the metric data
    .save_data(time_data, pattern = "*.[rR]$", replacement = "_time.RData",
               replace_string = basename(test_path))
  }
  #dev gsub is uded to replace something using regular expressions
  curr_name <- gsub(pattern = " ", replacement = "_", x = basename(test_path))
  curr_name <- gsub(pattern = ".[rR]$", replacement = "", x = curr_name)
  
  # Plot the metric data
  tryCatch(expr =   {test_plot <- 
                       ggplot2::ggplot() +
                       ggplot2::geom_point(mapping = ggplot2::aes(message, metric_val), 
                                           data = time_data, color = "blue") +
                       ggplot2::facet_grid(facets =  test_name ~ ., scales = "free") +
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
                       ggplot2::scale_x_discrete(limits = rev(levels(time_data$message))) +
                       # In the above 5 lines of code, the first line creates the basic qplot. The
                       # fourth and fifth lines display the x-axis labels at 90 degrees to the
                       # horizontal and correct the order of message labels on the x -axis,
                       # respectively.
                       ggplot2::xlab("Commit message") +
                       ggplot2::ylab("Time (in seconds)") +
                       ggplot2::ggtitle(label = paste0("Variation in time metrics for ", curr_name))
                     
                     if (save_plots == TRUE) {
                       .save_plots(test_plot = test_plot, test_name = curr_name, metric = "time",
                                   width = 1600, height = 1200)
                       print(test_plot)
                     }
                     else {
                       print(test_plot)
                     }
  },
  error = function(e){
    print("Encountered an error!")
  })
  
}

##  -----------------------------------------------------------------------------------------

.plot_interactive_mem <- function(test_path, num_commits, save_data, save_plots) {
  
  # Obtain the metrics data
  suppressMessages(mem_data <- mem_compare(test_path, num_commits))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){
    
    # Store the metric data
    .save_data(mem_data, pattern = "*.[rR]$", replacement = "_mem.RData",
               replace_string = basename(test_path))
  }
  
  # Add links to the github page for each commit to data
  remoteUrl <- git2r::remote_url(repo = git2r::repository(path = "./"))
  remoteUrl <- (paste0(remoteUrl, "/commit/"))
  mem_data$remoteUrl <- paste0(remoteUrl, mem_data$sha)
  
  levels(mem_data$test_name) <- paste0(substr(levels(mem_data$test_name), start = 0, stop = 4),
                                        "...",
                                        substr(levels(mem_data$test_name), 
                                               start = nchar(levels(mem_data$test_name)) - 4,
                                               stop = nchar(levels(mem_data$test_name))))
  
  test_plot <- ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x = message, y = metric_val,
                                               href = remoteUrl),
                        color = "blue",
                        data = mem_data) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank()) +
    ggplot2::facet_grid(facets = test_name~metric_name, scales = "free") +
    ggplot2::scale_x_discrete(limits = rev(levels(mem_data$message))) +  
    ggplot2::xlab("Commit message") +
    ggplot2::ylab("Memory usage (in mb)") +
    ggplot2::ggtitle(label = paste0("Variation in memory usage for ", basename(test_path)))
  
  if (length(levels(mem_data$test_name)) > 6) {
    test_plot <- test_plot + 
      animint::theme_animint(height = 700)
  }
  else if (length(levels(mem_data$test_name)) > 3) {
    test_plot <- test_plot +
      animint::theme_animint(height = 650)
  } 
  
  viz.list <- list(memplot = test_plot)
  
  print("Loaded animint")
  animint::animint2dir(plot.list = viz.list, out.dir = paste0(basename(getwd()), "_", "mem_animint"))
  unlink(x = paste0(basename(getwd()), "_", "mem_animint"), recursive = T, force = T)
}

##  -----------------------------------------------------------------------------------------

.plot_mem <- function(test_path, num_commits, save_data, save_plots) {
  
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
  
  tryCatch(expr = {test_plot <- 
                     ggplot2::ggplot(data = mem_data, mapping = ggplot2::aes(message, metric_val)) +
                     ggplot2::geom_point(color = "blue") +
                     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90),
                                    strip.text.x = ggplot2::element_text(size = 10, face = "bold")) +
                     ggplot2::scale_x_discrete(limits = rev(levels(mem_data$message))) +
                     ggplot2::facet_grid(test_name ~ metric_name, scales = "free") +
                     # In the above 5 lines of code, the first line creates the basic qplot. The
                     # third and fourth lines display the x-axis labels at 90 degrees to the
                     # horizontal and correct the order of message labels on the x -axis,
                     # respectively. The fourth line creates a facet grid so as to seperate
                     # the plots for the max memory and leak memory metrics.
                     ggplot2::ylab(label = "Memory (in Mb)") +
                     ggplot2::xlab(label = "Commit messages") +
                     ggplot2::ggtitle(label = paste0("Variation in memory metrics for ", curr_name))
                   
                   if (save_plots == TRUE) {
                     .save_plots(test_plot = test_plot, test_name = curr_name, metric = "memory")
                     print(test_plot)
                   }
                   else {
                     print(test_plot)
                   }
  },
  error = function(e) {
    print("Encountered an error!")
  })
  
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
#' @param metric Type of plot(s) desired. This can be set to \code{time},
#'   \code{memory}, \code{memtime} or \code{testMetrics}. (See examples below
#'   for more details)
#' @param output_name Name of the output .html file.
#'
#' @examples
#'
#' \dontrun{
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
#' }
#'
#' @section WARNING:
#'   Function assumes the current directory to be the root directory of the
#'   repository being tested.
#'

plot_webpage <- function(test_directory = "tests/testthat", metric = "testMetrics",
                         output_name = "RperformTest.html"){
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
  rmarkdown::render(input = out_file, output_format = "html_document", output_file = output_name)
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
#' @param test_directory Directory containing the test-files which are to be used.
#' @param metric Type of plot(s) desired. This can be set to \code{time},
#'   \code{memory}, \code{memtime} or \code{testMetrics}. (See examples below
#'   for more details)
#' @param num_commits Number of commits (versions) against which the files are to
#'   be tested, with default being 5.
#' @param save_data If set to TRUE, the metrics data is saved in a folder 'Rperform_Data'
#'   in the current directory.
#' @param save_plots If set to TRUE, the plots generated are stored in the
#'   'Rperform_plots' directory in the root of the repo rather than being
#'   printed.
#'
#' @examples
#'
#' \dontrun{
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
#' }
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
  floor(num_commits)
  
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
##  BRANCH FUNCTIONS
##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

#' Plot and compare different versions of test files across two branches.
#'
#' Given a test-file path and two branches, plot and compare the metrics of the
#' file across the two branches. The chosen metric values are plotted against the
#' commit message summaries. For the first branch, metrics are plotted upto the
#' latest common commit of both the branches. For the second branch, metrics for
#' only the latest commit are plotted, which may or may not be the latest common 
#' commit. If the parameter save_data is set
#' to true, it also stores the corresponding data-frames in an RData file in a
#' folder 'Rperform_Data' in the current directory.The metrics plotted are in
#' accordance with those specified using the parameter metric.
#' 
#'
#' @param test_path File-path of the test-file which is to be used for run-time
#'   comparisons.
#' @param metric Type of plot(s) desired. This can be set to \code{time},
#'   \code{memory}, \code{memtime} or \code{testMetrics}. (See examples below
#'   for more details)
#' @param branch1 Name of the first branch whose commits are to be analyzed.
#' @param branch2 Name of the second branch whose commits are to be analyzed.
#'    This is supposedly the branch into which branch1 is to be merged and its
#'    default value is set to 'master'.
#' @param save_data If set to TRUE, the data frame containing the metrics
#'   information is stored in the 'Rperform_Data' directory in the root of the
#'   repo. (default set to FALSE)
#' @param save_plots If set to TRUE, the plots generated are stored in the
#'   'Rperform_plots' directory in the root of the repo rather than being
#'   printed. (default set to TRUE)
#'   
#' @section NOTE:
#'    This function can be helpful when analyzing how would merging a branch into 
#'    the master branch would affect performance. 'branch2' is assumed to be the
#'    branch into which 'branch1' is to be merged.
#'    This function is useful only when branch1 had branched off from branch2 at some
#'    point, that is they have at least one common commit.
#'
#' @examples
#'
#' \dontrun{
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
#' # Pass the parameters and obtain the run-time details for branches, 'experiment' and
#' 'master'.
#' # Since branch2 is not specified in this case, it's assumed to be 'master'.
#' plot_branchmetrics(test_path = t_path, metric = "time", branch1 = 'experiment', save_data = F)
#' 
#' # Pass the parameters and obtain the memory details.
#' plot_branchmetrics(test_path = t_path, metric = 'memory', 
#'                    branch1 = 'experiment_1', branch2 = 'experiment_2')
#'
#' ## Example-2
#'
#' # Obtain both memory and time metrics for each individual testthat block
#' # inside a file, and those for the file itself. Same as the other metric
#' # cases, metrics both the commits are plotted. The plots get stored in a 
#' # directory 'Rperform_testMetrics' in the repo's root directory.
#' plot_metrics(test_path = t_path, metric = "testMetrics", branch1 = "experiment")
#' }
#'
#' @section WARNING:
#'   Function assumes the current directory to be the root directory of the
#'   repository/package being tested.
#'


plot_branchmetrics <- function(test_path, metric, branch1, branch2 = "master",
                               save_data = FALSE, save_plots = TRUE) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.character(branch1))
  stopifnot(length(branch1) == 1)
  stopifnot(is.character(branch2))
  stopifnot(length(branch2) == 1)
  stopifnot(is.character(metric))
  stopifnot(length(metric) == 1)
  stopifnot(is.logical(save_data))
  stopifnot(length(save_data) == 1)
  stopifnot(is.logical(save_plots))
  stopifnot(length(save_plots) == 1)
  
  if (metric == "time") {
    temp_out <- capture.output(.plot_btime(test_path, branch1, branch2, save_data, save_plots))
  }
  else if (metric == "memory") {
    temp_out <- capture.output(.plot_bmem(test_path, branch1, branch2, save_data, save_plots))
  }
  else if (metric == "memtime") {
    temp_out <- capture.output(.plot_btime(test_path, branch1, branch2, save_data, save_plots))
    temp_out <- capture.output(.plot_bmem(test_path, branch1, branch2, save_data, save_plots))
  }
  else if (metric == "testMetrics") {
    temp_out <- capture.output(.plot_btestMetrics(test_path, branch1, branch2, save_data, save_plots))
  }
  else {
    temp_out <- NULL
    print("Enter valid metric!")
  }
  remove(temp_out)
}

##  -----------------------------------------------------------------------------------------

.plot_btestMetrics <- function(test_path, branch1, branch2, save_data, save_plots) {
  
  suppressMessages(time_data <- compare_brancht(test_path = test_path,
                                                branch1 = branch1, branch2 = branch2))
  suppressMessages(mem_data <- compare_branchm(test_path = test_path,
                                               branch1 = branch1, branch2 = branch2))
  suppressMessages(same_commit <- .common_commit(branch1 = branch1, branch2 = branch2))
  
  # Store the metrics data if save_data is TRUE
  if (save_data){
    
    # Store the metric data
    .save_data(time_data, pattern = "*.[rR]$", 
               replacement = paste0("_", branch1, "_", branch2, "_time.RData"),
               replace_string = basename(test_path))
    .save_data(mem_data, pattern = "*.[rR]$", 
               replacement = paste0("_", branch1, "_", branch2, "_mem.RData"),
               replace_string = basename(test_path))
  }
  
  metric_data <- rbind(time_data, mem_data)
  t_names <- levels(metric_data$test_name)
  
  for(num in seq(t_names)) {
    test_frame <- metric_data[metric_data$test_name == t_names[num],]
    extremes_frame <- .find_midvals(data = test_frame)
    
    tryCatch(expr = {test_plot <- 
                       ggplot2::ggplot(data = test_frame, mapping = ggplot2::aes(message, metric_val)) +
                       ggplot2::geom_point(color = "blue") +
                       ggplot2::facet_grid(facets = metric_name ~ ., scales = "free") +
                       ggplot2::geom_text(data = extremes_frame, 
                                          mapping = ggplot2::aes(x = same_commit$cnum_b2 + 0.3, 
                                                                 y = mid_val,
                                                                 label = branch2, angle = 90)) +
                       ggplot2::geom_text(data = extremes_frame, 
                                          mapping = ggplot2::aes(x = same_commit$cnum_b2 + 0.7, 
                                                                 y = mid_val,
                                                                 label = branch1, angle = -90)) +
                       ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
                       ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = same_commit$cnum_b2 + 0.5)) +
                       ggplot2::scale_x_discrete(limits = rev(levels(test_frame$message))) +
                       # In the above 6 lines of code, the first line creates
                       # the basic qplot. The fourth and sixth lines display the
                       # x-axis labels at 90 degrees to the horizontal and
                       # correct the order of message labels on the x -axis, 
                       # respectively. The fifth line plots a vertical seperator between
                       # the commit from branch2 and the commits from branch1.
                       ggplot2::xlab("Commit message") +
                       ggplot2::ylab("Metric value") +
                       ggplot2::ggtitle(label = paste0("Variation in metrics for ", t_names[num]))
                     
                     curr_name <- paste0(branch1, "_", branch2, "_", t_names[num])
                     
                     if (save_plots == TRUE) {
                       .save_plots(test_plot = test_plot, test_name = t_names[num],
                                   metric = "testMetrics")
                       print(test_plot)
                     } else {
                       print(test_plot)
                     }
    },
    error = function(e){
      print("Encountered an error!")
    })
  }
  
}

##  -----------------------------------------------------------------------------------------


.plot_btime <- function(test_path, branch1, branch2, save_data, save_plots) {
  
  suppressMessages(time_data <- compare_brancht(test_path = test_path,
                                                branch1 = branch1, branch2 = branch2))
  suppressMessages(same_commit <- .common_commit(branch1 = branch1, branch2 = branch2))
  #                  same_commit
  # ---------------------------------------------
  #      common_datetime, cnum_b1, cnum_b2
  
  # Store the metrics data if save_data is TRUE
  if (save_data) {
    .save_data(metric_frame = time_data, pattern = "*.[rR]$",
               replacement = paste0("_", branch1, "_", branch2, "_time.RData"),
               replace_string = basename(test_path))
  }
  
  curr_name <- gsub(pattern = " ", replacement = "_", x = basename(test_path))
  curr_name <- gsub(pattern = "*.[rR]$", replacement = paste0("_", branch1, "_", branch2),
                    x = curr_name)
  
  # Trying to find the min and max vals for each test
  ###################################################
  
  extremes_frame <- .find_midvals(data = time_data)  
  ##                        extremes_frame
  ## test_name   |      max_val     |    min_val    |   mid_val
  ## ----------------------------------------------------------
  
  ###################################################
  
  # Plot the branches' metric data
  tryCatch(expr = {test_plot <- 
                     ggplot2::ggplot(data = time_data, mapping = ggplot2::aes(message, metric_val)) +
                     ggplot2::geom_point(color = "blue") +
                     ggplot2::facet_grid(test_name ~ ., scales = "free") +
                     ggplot2::geom_text(data = extremes_frame, 
                                        mapping = ggplot2::aes(x = same_commit$cnum_b2 + 0.3,
                                                               y = mid_val,
                                                               label = branch2, angle = 90)) +
                     ggplot2::geom_text(data = extremes_frame, 
                                        mapping = ggplot2::aes(x = same_commit$cnum_b2 + 0.7,
                                                               y = mid_val,
                                                               label = branch1, angle = -90)) +
                     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
                     ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = same_commit$cnum_b2 + 0.5)) +
                     ggplot2::scale_x_discrete(limits = rev(levels(time_data$message))) +
                     # In the above 8 lines of code, the first line creates
                     # the basic plot. The sixth and eigth lines display the
                     # x-axis labels at 90 degrees to the horizontal and
                     # correct the order of message labels on the x -axis, 
                     # respectively. The seventh line plots a vertical seperator between
                     # the commit from branch2 and the commits from branch1.
                     ggplot2::xlab(label = "Commit messages") +
                     ggplot2::ylab(label = "Time (in seconds)") +
                     ggplot2::ggtitle(label = paste0("Variation in time metrics across branches ",
                                                     branch2, " and ", branch1))
                   
                   if (save_plots == TRUE) {
                     .save_plots(test_plot = test_plot, test_name = curr_name, metric = "time",
                                 width = 1600, height = 900)
                     print(test_plot)
                   }
                   else {
                     print(test_plot)
                   }
  },
  error = function(e){
    print("Encountered an error!")
  })
  
}

##  -----------------------------------------------------------------------------------------

.plot_bmem <- function(test_path, branch1, branch2, save_data, save_plots) {
  
  suppressMessages(mem_data <- compare_branchm(test_path = test_path,
                                               branch1 = branch1, branch2 = branch2))
  suppressMessages(same_commit <- .common_commit(branch1 = branch1, branch2 = branch2))
  #                  same_commit
  # ---------------------------------------------
  #      common_datetime, cnum_b1, cnum_b2
  
  # Store the metrics data if save_data is TRUE
  if (save_data) {
    .save_data(metric_frame = mem_data, pattern = "*.[rR]$",
               replacement = paste0("_", branch1, "_", branch2, "_mem.RData"),
               replace_string = basename(test_path))
  }
  
  curr_name <- gsub(pattern = " ", replacement = "_", x = basename(test_path))
  curr_name <- gsub(pattern = "*.[rR]$", replacement = paste0("_", branch1, "_", branch2),
                    x = curr_name)
  
  # Trying to find the min and max vals for each test
  ###################################################
  
  extremes_frame <- .find_midvals(data = mem_data)  
  ##                        extremes_frame
  ## test_name   |      max_val     |    min_val    |   mid_val
  ## ----------------------------------------------------------
  
  ###################################################
  
  # Plot the branches' metric data
  tryCatch(expr = {test_plot <- 
                     ggplot2::ggplot(data = mem_data, mapping = ggplot2::aes(message, metric_val)) +
                     ggplot2::geom_point(color = "blue") +
                     ggplot2::facet_grid(test_name ~ metric_name, scales = "free") +
                     ggplot2::geom_text(data = extremes_frame, 
                                        mapping = ggplot2::aes(x = same_commit$cnum_b2 + 0.3,
                                                               y = mid_val,
                                                               label = branch2, angle = 90)) +
                     ggplot2::geom_text(data = extremes_frame, 
                                        mapping = ggplot2::aes(x = same_commit$cnum_b2 + 0.7,
                                                               y = mid_val,
                                                               label = branch1, angle = -90)) +                     
                     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90),
                                    strip.text.x = ggplot2::element_text(size = 10, face = "bold")) +
                     ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = same_commit$cnum_b2 + 0.5)) +
                     ggplot2::scale_x_discrete(limits = rev(levels(mem_data$message))) +
                     # In the above 8 lines of code, the first line creates
                     # the basic plot. The sixth and eigth lines display the
                     # x-axis labels at 90 degrees to the horizontal and
                     # correct the order of message labels on the x -axis, 
                     # respectively. The seventh line plots a vertical seperator between
                     # the commit from branch2 and the commits from branch1.
                     ggplot2::xlab(label = "Commit messages") +
                     ggplot2::ylab(label = "Memory (in Mb") +
                     ggplot2::ggtitle(label = paste0("Variation in memory metrics across branches ",
                                                     branch2, " and ", branch1))
                   
                   if (save_plots == TRUE) {
                     .save_plots(test_plot = test_plot, test_name = curr_name, metric = "memory")
                     print(test_plot)
                   }
                   else {
                     print(test_plot)
                   }
  },
  error = function(e){
    print("Encountered an error!")
  })
  
}

##  -----------------------------------------------------------------------------------------


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

.save_plots <- function(test_plot, test_name, metric, width = 1024, height = 768, units = "px") {
  
  if (metric == "time") {
    if(!dir.exists(paths = "./Rperform_timeMetrics")) {
      dir.create(path = "./Rperform_timeMetrics")
    }
    target_dir <- "Rperform_timeMetrics"
  }
  else if (metric == "memory") {
    if(!dir.exists(paths = "./Rperform_memoryMetrics")) {
      dir.create(path = "./Rperform_memoryMetrics")
    }
    target_dir <- "Rperform_memoryMetrics"
  }
  else if (metric == "testMetrics") {
    if(!dir.exists(paths = "./Rperform_testMetrics")) {
      dir.create(path = "./Rperform_testMetrics")
    }
    target_dir <- "Rperform_testMetrics"
  }
  
  curr_name <- gsub(pattern = " ", replacement = "_", x = test_name)
  curr_name <- gsub(pattern = ".[rR]$", replacement = "", curr_name)
  png.file <- file.path(target_dir, paste0("Test_", curr_name, ".png"))
  grDevices::png(filename = png.file, width = width, height = height, units = units)
  print(test_plot)
  grDevices::dev.off()
}

##  -----------------------------------------------------------------------------------------

.find_midvals <- function(data) {
  extremes_list <- list()
  t_names <- as.character(unique(data$test_name))
  m_names <- as.character(unique(data$metric_name))
  for (test_num in seq(t_names)) {
    for (metric_num in seq(m_names)) {
      max_val <- max(data[data$test_name == t_names[test_num] & data$metric_name == m_names[metric_num],
                          "metric_val"])
      min_val <- min(data[data$test_name == t_names[test_num] & data$metric_name == m_names[metric_num],
                          "metric_val"])
      mid_val <- (max_val + min_val) / 2
      extremes_list[[paste0(t_names[test_num], m_names[metric_num])]] <- 
        (data.frame(test_name = t_names[test_num], 
                    metric_name = m_names[metric_num], 
                    mid_val = mid_val))
    }
  }
  
  extremes_frame <- do.call(rbind, extremes_list)
  row.names(extremes_frame) <- NULL
  
  extremes_frame
}
