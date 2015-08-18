# Set ORIGIN_BRANCH environment variable

echo -e "TRAVIS PULL REQUEST: $TRAVIS_PULL_REQUEST"

if [ "$TRAVIS_PULL_REQUEST" != "false" ]; then
  touch pre_Rperform.txt
  echo -n "export TEMP_ORIGIN_BRANCH=" > pre_Rperform.txt
  git describe --all --contain FETCH_HEAD >> pre_Rperform.txt
  source ./pre_Rperform.txt
  echo -n "export ORIGIN_BRANCH=" > pre_Rperform.txt
  re="[^~]+"
  if [[ $TEMP_ORIGIN_BRANCH =~ $re ]]; then echo ${BASH_REMATCH} >> pre_Rperform.txt; fi
  source ./pre_Rperform.txt
  rm ./pre_Rperform.txt
fi
