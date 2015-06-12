memory.usage <- function(ps.parameter=paste("-p", Sys.getpid())){
  cmd <- sprintf("ps %s -o pid,cmd,rss", ps.parameter)
  ps.lines <- system(cmd, intern=TRUE)
  stopifnot(length(ps.lines) > 1)
  ps.table <- read.table(text=ps.lines, header=TRUE)
  ps.table$megabytes <- ps.table$RSS/1024
  ps.table$megabytes
}

##  -----------------------------------------------------------------------------------------
##  -----------------------------------------------------------------------------------------

mem_commit2 <- function(test_path, test_commit) {
  stopifnot(is.character(test_path))
  stopifnot(length(test_path) == 1)
  stopifnot(!is.null(test_commit))
  stopifnot(git2r::is_commit(test_commit))
  
  sha_val <- get_sha(test_commit)
  t_lines <- readLines(test_path)
#   t_lines <- sub("test_that(", "testthatQuantity(", t_lines, fixed=TRUE)
  temp_file <- tempfile()
  writeLines(t_lines, temp_file)
  target <- git2r::repository("./")
  git2r::checkout(test_commit)
  on.exit(expr = git2r::checkout(target, "master"))
  test_results <- list()
  
  devtools::load_all("./")
  #   require(testthat)
  #   pryr::mem_change(source(temp_file, local = T))
  # test_results <- do.call(rbind, test_results)
  require(testthat)
  gc(reset = T)
  source(temp_file, local = T)
  memory <- sum(gc()[,6])
#   rownames(test_results) <- NULL
#   test_results
  memory
}

##  -----------------------------------------------------------------------------------------