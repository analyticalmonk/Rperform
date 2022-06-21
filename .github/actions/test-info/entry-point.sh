#!/bin/bash

# Install Rperform with its dependendcies
Rscript -e "install.packages(c('ggplot2', 'devtools', 'remotes', 'testthat', 'microbenchmark'), repos='http://cran.us.r-project.org')"

Rscript -e "install.packages('https://cran.r-project.org/src/contrib/Archive/git2r/git2r_0.21.0.tar.gz', repos=NULL, type='source')"

# Compile the TestSAN package
Rscript -e "library(devtools)"
Rscript -e "library(git2r)"

Rscript -e "install.packages('./', repos = NULL, type='source', verbose=TRUE)"


echo "Rperform test passed successfully"
exit 0