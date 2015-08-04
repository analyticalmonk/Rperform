# Make sure that you have correctly set the test directory in the below R
# command. The directory is set relative to the root of your git repository.
# The parameter output_name specifies the name of the output html file which
# will eventually be pushed to the gh-pages branch of your git repository. The
# default results in a file named index.html

Rperform::plot_webpage(test_directory = "./tests/testthat/", output_name = "index")

# Alternatively, you can use other Rperform commands too. For example,
# Rperform::plot_time(test_path = , num_commits = )