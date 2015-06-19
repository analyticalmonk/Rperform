argv <- commandArgs(trailingOnly = TRUE)
test_path <- argv[1]
commit_num <- as.numeric(argv[2])
mem_result <- Rperform::get_mem(test_path = test_path,
                                commit_num = commit_num)
save(mem_result, file="mem_result.RData")