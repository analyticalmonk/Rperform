argv <- commandArgs(trailingOnly=TRUE)
test_path <- argv[1]
library(Rperform)
mem_result <- get_mem(test_path = test_path, num_commits = 1)
save(mem_result, file="mem_result.RData")