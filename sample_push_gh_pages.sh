if [ "$TRAVIS_PULL_REQUEST" == "false" ]; then
  echo -e "Starting to update gh-pages\n"

  #go to home and setup git
  cd $HOME
  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "Travis"

  #using token clone gh-pages branch
  git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/"{your_repo_here}"  gh-pages > /dev/null
  git clone --quiet --branch=master https://${GH_TOKEN}@github.com/"{your_repo_here}"  master > /dev/null

  #go into master directory and run the Rperform functions
  cd master
  if [! -f temp_Rperform.R]
  then
    touch temp_Rperform.R
    echo "Rperform::plot_webpage(test_directory = \"./tests/\")" >> temp_Rperform.R
  fi
  Rscript temp_Rperform.R
  rm temp_Rperform.R

  cd ../gh-pages
  cp -Rf $HOME/master/index.html index.html

  #add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
  git push -fq origin gh-pages > /dev/null

  echo -e "Done magic with Rperform\n"
fi

if [ "$TRAVIS_PULL_REQUEST" != "false" ]; then
  echo -e "Starting to update gh-pages\n"

  # Set ORIGIN_BRANCH environment variable
  touch pre_Rperform.txt
  echo -n "export TEMP_ORIGIN_BRANCH=" > pre_Rperform.txt
  git describe --all --contain HEAD >> pre_Rperform.txt
  source ./pre_Rperform.txt
  echo -n "export ORIGIN_BRANCH=" > pre_Rperform.txt
  re="[^~]+"
  if [[ $TEMP_ORIGIN_BRANCH =~ $re ]]; then echo ${BASH_REMATCH} >> pre_Rperform.txt; fi
  source ./pre_Rperform.txt
  rm ./pre_Rperform.txt

  #go to home and setup git
  cd $HOME
  git config --global user.email "travis@travis-ci.org"
  git config --global user.name "Travis"

  #using token clone gh-pages branch
  git clone --quiet --branch=gh-pages https://${GH_TOKEN}@github.com/"{your_repo_here}"  gh-pages > /dev/null
  git clone --quiet --branch=master https://${GH_TOKEN}@github.com/"{your_repo_here}"  master > /dev/null
  git clone --quiet --branch=$ORIGIN_BRANCH https://${GH_TOKEN}@github.com/"{your_repo_here}"  origin > /dev/null

  #Run the Rperform functions
  if [! -f temp_Rperform.R]
  then
    touch temp_Rperform.R
    echo "Rperform::compare_dirt(test_directory = \"./tests/\")" >> temp_Rperform.R
  fi
  Rscript ./master/temp_Rperform.R

  cd ../gh-pages
  cp -Rf $HOME/master/index.html index.html

  #add, commit and push files
  git add -f .
  git commit -m "Travis build $TRAVIS_BUILD_NUMBER pushed to gh-pages"
  git push -fq origin gh-pages > /dev/null

  echo -e "Done magic with Rperform\n"
fi
