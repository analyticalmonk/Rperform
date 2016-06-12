plot_bottlenecks <- function(test_path, num_commits, benchmark, metric = 'time', 
                             threshold = 0.1, use_optimal = FALSE) {
  metric_frame <- find_bottlenecks(test_path = test_path, num_commits = num_commits, 
                                   benchmark = benchmark, metric = metric, 
                                   threshold = threshold, use_optimal = use_optimal)
  mean_frame <- unique(metric_frame[, c("test_name", "mean_metric_val")])
  
  tryCatch(expr = { bottleneck_plot <- 
                      ggplot2::ggplot(data = metric_frame, ggplot2::aes(message, metric_val)) +
                      ggplot2::geom_point(ggplot2::aes(color = bottleneck)) +
                      ggplot2::facet_grid(facets = test_name ~ metric_name, scales = "free") +
                      ggplot2::geom_hline(data = mean_frame, mapping = ggplot2::aes(yintercept = mean_metric_val)) + 
                      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90))  +
#                       ggplot2::scale_x_discrete(limits = rev(levels(metric_data$message))) +
                      ggplot2::xlab(label = "Commit Message") +
                      ggplot2::ylab(label = "Metric") +
                      ggplot2::ggtitle(label = paste0("Variation in ", metric, " for ", basename(test_path)) )
  },
  error = function(e) {
    print("Encountered an error!")
  })
  
  print(bottleneck_plot)

#   return(metric_frame)
}
