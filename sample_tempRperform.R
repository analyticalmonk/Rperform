#####################################################################################
# Make sure that you have correctly set the test directory in the below R
# command. The directory is set relative to the root of your git repository.
# The parameter output_name specifies the name of the output html file which
# will eventually be pushed to the gh-pages branch of your git repository. The
# default results in a file named index.html

Rperform::plot_webpage(test_directory = "./tests/testthat/", metric = "testMetrics",
                       output_name = "index")

#####################################################################################
# Alternatively, you can use other Rperform commands too. For example,
# Rperform::plot_metrics(test_path = , metric = , num_commits = , save_plots = FALSE)

#####################################################################################
# If you want to use the metrics data to make custom plots, follow the
# instructions below and uncomment the required lines. Comment the above
# plot_webpage() command in case you don't require it.

# To obtain time and memory data for specific tests,
# time_data <- Rperform::time_compare(test_path = , num_commits = )
# memory_data <- Rperform::mem_compare(test_path = , num_commits = )

# Given below is basic code to plot the time data. Make modifications as per
# your purpose.
# time_plot <- ggplot2::qplot(message, metric_val, data = time_data) +
#   ggplot2::facet_grid(facets =  test_name ~ ., scales = "free") +
#   ggplot2::geom_point(color = "blue") + 
#   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90)) +
#   ggplot2::scale_x_discrete(limits = rev(levels(time_data$message))) +
#   # In the above 3 lines code, the first line creates the basic qplot. The 
#   # second and third lines display the x-axis labels at 90 degrees to the 
#   # horizontal and correct the order of message labels on the x -axis,
#   # respectively.
#   ggplot2::xlab("Commit message") +
#   ggplot2::ylab("Time (in seconds)") +
#   ggplot2::ggtitle(label = paste0("Variation in time metrics for ", curr_name))
# 
# 
# output_name <- "index"
# out_file <- paste0(output_name, ".Rmd")
# 
# if(!file.exists(out_file)){
#   file.create(out_file)
# }
# 
# line_p1 <- "---\ntitle: \"plot\"\noutput: html_document\n---\n\n```{r}\nprint(\""
# line_p2 <- "time_plot\")\n```"
# file_lines <- paste0(line_p1, line_p2)
# writeLines(file_lines, con = out_file)
# knitr::knit2html(input = out_file, output = paste0(output_name, ".html"))

#####################################################################################