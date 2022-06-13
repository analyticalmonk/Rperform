# RPERFORM 
# Introduction

`Rperform` is a package that allows R developers to track quantitative performance metrics of their code. It helps to provide changes in a package’s performance metrics related to runtime and memory. It is used to access performance metrics over different git versions and across git branches. It can also be integrated with `Travis-CI` for performance testing during Travis builds by making changes to the repo's `.travis.yml` file. It can prove to be particularly useful while measuring the possible changes which can be introduced by a pull request (PR).

_The project was initiated as a part of the [Google Summer of Code 2015](https://github.com/rstats-gsoc/gsoc2015/wiki/Test-timings-on-Travis) program and the [Google Summer of Code 2016](https://github.com/rstats-gsoc/gsoc2016/wiki/Rperform:-Performance-analysis-of-R-package-code) program. It was accepted into the [Google Summer of Code 2022](https://github.com/rstats-gsoc/gsoc2022/wiki/Rperform) program_ 



# Installation

- You can install the package from github using :

``` r
library(devtools)
install_github("analyticalmonk/Rperform")
```

or

```r
devtools::install_github("analyticalmonk/Rperform")
```

# Basic examples

**NOTE**: The Rperform package requires you to set the current directory to the concerned git repository before using the functions.

```r
> setwd(dir = "Path/to/repo")
```

- The following example illustrates the use of the `Rperform::plot_metrics()` function on the git repository of the package [stringr](https://github.com/tdhock/stringr).

```r
> setwd("./stringr")
> library(Rperform)
> plot_metrics(test_path = "tests/testthat/test-join.r", metric = "time", num_commits = 10, save_data = FALSE, save_plots = FALSE)
```
![time plot](https://raw.githubusercontent.com/EngineerDanny/GSOC22-RPerform-Blog/master/images/Rplot_time.jpeg "Time Plot")

- The following example illustrates the use of the `Rperform::plot_branchmetrics()` function on the git repository of the package [stringr](https://github.com/tdhock/stringr).

```r
> setwd("./stringr")
> library(Rperform)
> plot_branchmetrics(test_path = "tests/testthat/test-interp.r", metric = "memory", branch1 = "rperform_test", branch2 = "master", save_data = F, save_plots = F)
```

![memory plot](https://raw.githubusercontent.com/EngineerDanny/GSOC22-RPerform-Blog/master/images/Rplot_branchmem.jpeg "Memory Plot")



- The following example illustrates the use of the `Rperform::time_compare()` and `Rperform::mem_compare()` functions on the git repository of the package [stringr](https://github.com/tdhock/stringr).

```r
> setwd("./stringr")
> library(Rperform)
> time_compare(test_path = "./tests/testthat/test-dup.r", num_commits = 2)
                          test_name metric_name status  metric_val         msg_val           date_time
1           basic duplication works        time   pass 0.001797014 Can now use CRA 2015-01-08 14:09:43
2           basic duplication works        time   pass 0.001539050 Can now use CRA 2015-01-08 14:09:43
3           basic duplication works        time   pass 0.001545034 Can now use CRA 2015-01-08 14:09:43
4  0 duplicates equals empty string        time   pass 0.001019430 Can now use CRA 2015-01-08 14:09:43
5  0 duplicates equals empty string        time   pass 0.000784386 Can now use CRA 2015-01-08 14:09:43
6  0 duplicates equals empty string        time   pass 0.000766279 Can now use CRA 2015-01-08 14:09:43
7                        test-dup.r        time   pass 0.003555478 Can now use CRA 2015-01-08 14:09:43
8                        test-dup.r        time   pass 0.003118946 Can now use CRA 2015-01-08 14:09:43
9                        test-dup.r        time   pass 0.003106017 Can now use CRA 2015-01-08 14:09:43
10          basic duplication works        time   pass 0.001780849 Require latest  2015-01-08 14:03:37
11          basic duplication works        time   pass 0.001545568 Require latest  2015-01-08 14:03:37
12          basic duplication works        time   pass 0.001517300 Require latest  2015-01-08 14:03:37
13 0 duplicates equals empty string        time   pass 0.001028882 Require latest  2015-01-08 14:03:37
14 0 duplicates equals empty string        time   pass 0.000783847 Require latest  2015-01-08 14:03:37
15 0 duplicates equals empty string        time   pass 0.000771520 Require latest  2015-01-08 14:03:37
16                       test-dup.r        time   pass 0.003436051 Require latest  2015-01-08 14:03:37
17                       test-dup.r        time   pass 0.003130397 Require latest  2015-01-08 14:03:37
18                       test-dup.r        time   pass 0.003082713 Require latest  2015-01-08 14:03:37
> 
```

```r
> Rperform::mem_compare(test_path = "./tests/testthat/test-join.r", num_commits = 1)
             test_name metric_name status metric_val         msg_val           date_time
11.1  basic case works     max_mem_mb   pass      0.040 Can now use CRA 2015-01-08 14:09:43
11.2  basic case works     leak_mb      pass      0.040 Can now use CRA 2015-01-08 14:09:43
11.3 NULLs are dropped     max_mem_mb   pass      0.044 Can now use CRA 2015-01-08 14:09:43
11.4 NULLs are dropped     leak_mb      pass      0.044 Can now use CRA 2015-01-08 14:09:43
11.5       test-join.r     max_mem_mb   pass      0.148 Can now use CRA 2015-01-08 14:09:43
11.6       test-join.r     leak_mb      pass      0.148 Can now use CRA 2015-01-08 14:09:43
12.1  basic case works     max_mem_mb   pass      0.040 Can now use CRA 2015-01-08 14:09:43
12.2  basic case works     leak_mb      pass      0.040 Can now use CRA 2015-01-08 14:09:43
12.3 NULLs are dropped     max_mem_mb   pass      0.044 Can now use CRA 2015-01-08 14:09:43
12.4 NULLs are dropped     leak_mb      pass      0.044 Can now use CRA 2015-01-08 14:09:43
12.5       test-join.r     max_mem_mb   pass      0.144 Can now use CRA 2015-01-08 14:09:43
12.6       test-join.r     leak_mb      pass      0.144 Can now use CRA 2015-01-08 14:09:43
13.1  basic case works     max_mem_mb   pass      0.040 Can now use CRA 2015-01-08 14:09:43
13.2  basic case works     leak_mb      pass      0.040 Can now use CRA 2015-01-08 14:09:43
13.3 NULLs are dropped     max_mem_mb   pass      0.048 Can now use CRA 2015-01-08 14:09:43
13.4 NULLs are dropped     leak_mb      pass      0.048 Can now use CRA 2015-01-08 14:09:43
13.5       test-join.r     max_mem_mb   pass      0.148 Can now use CRA 2015-01-08 14:09:43
13.6       test-join.r     leak_mb      pass      0.148 Can now use CRA 2015-01-08 14:09:43
```
