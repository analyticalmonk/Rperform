# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------

.rss.profile.start <- function(rss.file){
  stopifnot(is.character(rss.file))
  stopifnot(length(rss.file) == 1)
  sh.file <- system.file("exec", "rss.sh", package="Rperform")
  cmd <- paste("bash", sh.file, rss.file, Sys.getpid())
  gc(reset = T)
  system(cmd, wait=FALSE)
  ## while({
  ##   rss.size <- file.info(rss.file)$size
  ##   is.na(rss.size) || rss.size == 0
  ## }){
  ##   ## wait for the system(cmd) to start writing to rss.file.
  ##   cat("Waiting for rss.sh to start writing to ", rss.file, "\n")
  ## }
  Sys.sleep(1)
}

.rss.profile.stop <- function(rss.file){
  stopifnot(is.character(rss.file))
  stopifnot(length(rss.file) == 1)
  DONE.file <- paste0(rss.file, ".DONE")
  gc()
  Sys.sleep(1)
  cat("", file=DONE.file)
  kilobytes <- scan(rss.file, what=integer(), quiet=TRUE)
  list(kilobytes.over.time=kilobytes,
       swap=max(kilobytes) - kilobytes[1],
       leak=kilobytes[length(kilobytes)]-kilobytes[1])
}

# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Script for obtaining memory usage using "exec/get_mem.R"

# script.R <- system.file("exec", "get_mem.R", package="Rperform")
# Rscript <- file.path(R.home("bin"), "Rscript")
# cmd <- paste(Rscript, script.R, "./tests/testthat/test-detect.r")
# result.list <- list()
# for(test.i in 1:3){
#   system(cmd)
#   load("mem_result.RData")
#   result.list[[test.i]] <- mem_result
# }

# ----------------------------------------------------------------------------

## Use the *nix ps program to get the memory usage of this R process.
# 
# .memory.usage <- function(ps.parameter=paste("-p", Sys.getpid())){
#   cmd <- sprintf("ps %s -o pid,cmd,rss", ps.parameter)
#   ps.lines <- system(cmd, intern=TRUE)
#   stopifnot(length(ps.lines) > 1)
#   ps.table <- read.table(text=ps.lines, header=TRUE)
#   ps.table$megabytes <- ps.table$RSS/1024
#   ps.table
# }
