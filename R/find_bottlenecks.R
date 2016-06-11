find_bottlenecks <- function(test_path, num_commits, benchmark, metric = 'time', 
                             threshold = 0.1, use_optimal = FALSE) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(is.numeric(num_commits))
  stopifnot(length(num_commits) == 1)
  stopifnot(is.numeric(benchmark))
  stopifnot(length(benchmark) == 1)
  stopifnot(is.character(metric))
  stopifnot(length(metric) == 1)
  stopifnot(is.numeric(threshold))
  stopifnot(length(threshold) == 1)
  stopifnot(is.logical(use_optimal))
  stopifnot(length(use_optimal) == 1)
  
  if (metric == "time") {
   metric_frame <- time_compare(test_path = test_path, num_commits = num_commits)
  } 
  else if (metric == "memory") {
    metric_frame <- mem_compare(test_path = test_path, num_commits = num_commits)
  }
  
  metric_names <- levels(metric_frame$metric_name)
  test_names <- levels(metric_frame$test_name)
  # messages <- levels(metric_frame$message)
  
  # Extracting the benchmark commits
  commitgroup_len <- ( (nrow(metric_frame) / num_commits) )
  benchmark_begin <- (commitgroup_len * (benchmark - 1)) + 1
  benchmark_end <- (benchmark_begin - 1) + commitgroup_len
  # print(benchmark_begin)
  # print(benchmark_end)
  benchmark_frame <- metric_frame[benchmark_begin:benchmark_end, ]
  # print(metric_frame)
  # print(benchmark_frame)
  
  # return(benchmark_frame)
  
  temp_frame1 <- data.frame(test_name = unique(benchmark_frame$test_name))
  temp_frame2 <- data.frame(metric_name = unique(benchmark_frame$metric_name))
  benchmark_metrics <- merge(temp_frame1, temp_frame2)
  benchmark_metrics$mean_metric_val <- 0
  # print(benchmark_metrics)
  for (metric in seq(metric_names)) {
    for (test_name in seq(test_names)) {
      {benchmark_metrics[(benchmark_metrics$test_name == test_names[test_name]) 
                         & (benchmark_metrics$metric_name == metric_names[metric]),
                         "mean_metric_val"] <- 
         mean(benchmark_frame[(benchmark_frame$test_name == test_names[test_name]) 
                              & (benchmark_frame$metric_name == metric_names[metric]), 
                              "metric_val"])}
    }
  }
  
  # return(benchmark_metrics)
  
  benchmark_metrics$mean_metric_val <- (1+threshold) * benchmark_metrics$mean_metric_val
  metric_frame$bottleneck <- FALSE
  
  metric_frame <- merge(metric_frame, benchmark_metrics, by = c("test_name", "metric_name"))
  metric_frame$bottleneck <- (metric_frame$metric_val > metric_frame$mean_metric_val)
  metric_frame <- metric_frame[order(metric_frame$date_time), ]
  d_time <- metric_frame$date_time
  metric_frame$date_time <- NULL
  metric_frame$date_time <- d_time
  
  return(metric_frame)
}