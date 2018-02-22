#!/usr/bin/env bash

CACHE_DIR="node_modules"

function check_error {
  EXIT_CODE="$@"
  if [ $EXIT_CODE -ne 0 ]; then
    echo "Exit check failed: $EXIT_CODE"
    exit $EXIT_CODE
  fi
}


function do_webpack {
  # sysconfcpus workaround: https://github.com/elm-lang/elm-compiler/issues/1473
  if $HOME/bin/sysconfcpus -n 1 webpack "$@" --progress 2>&1 ; then
    check_error $?
    echo "sysconfcpus -n 1 build succeeeded"
  else
    echo "sysconfcpus failed, falling back to regular build"
    webpack "$@" --progress 2>&1
    check_error $?
  fi
}

if [ $INSTALL_WEBPACK ]; then
  echo "##> Installing webpack..."
  npm install -g webpack
  echo "##> Installed webpack."
fi

# detect netlify (or other ci)
if [ $REPOSITORY_URL ]; then
  # prep for netlify cache stuff
  echo "Restoring .cache at $CACHE_DIR/.cache: \n`ls $CACHE_DIR/.cache`\n"
  cp -a $CACHE_DIR/.cache .cache || true

  # get cached elm stuff
  mkdir -p .cache
  cp -a .cache/elm-stuff elm-stuff || true

  # print cache directory
  echo "Cache Directory before build:\n"
  ls -al .cache
  echo ""
fi


# prepping build by precompiling purs and elm
echo "Compiling purescirpt"
# using yarn run bc we got a weird error from netlify about not finding `purs` once
yarn run pulp build -j 1 # build all deps we've downloaded
check_error $?

# echo "Compiling elm"
# yarn run elm-make web/src/SecureVote/SPAs/SwarmMVP/Main.elm  --output temp-32489734985.html 2>&1 # compile elm
# check_error $?

echo "Manually installing elm - yarn seems to miss it..."
npm install elm
echo "Elm installed"

ls node_modules/elm/
ls node_modules/elm/Elm-Platform/
ls node_modules/elm/Elm-Platform/0.18.0/
ls node_modules/elm/Elm-Platform/0.18.0/.cabal-sandbox/
ls node_modules/elm/Elm-Platform/0.18.0/.cabal-sandbox/bin/


echo "Trying to get a version of sysconfcpus..."
git clone https://github.com/obmarg/libsysconfcpus.git
cd libsysconfcpus
./configure --prefix=$HOME
echo "prefix used: $HOME"
make install
cd ..
echo "ls $HOME\n"
ls $HOME
echo "ls $HOME/bin\n"
ls $HOME/bin
echo "done ls $HOME\n"
echo "\"Installed\" sysconfcpus"

# do build
echo "Building now..."
time do_webpack "$@"
WEBPACK_RET=$?
check_error $WEBPACK_RET
echo "Build Complete"


if [ $REPOSITORY_URL ]; then
  # save in cache
  echo "Caching elm stuff"
  rm -rf .cache/elm-stuff || true
  cp -a elm-stuff .cache/elm-stuff

  # print cache directory
  echo "Cache Directory after build: (ls -al .cache)\n"
  ls -al .cache
  echo ""

  # save netlify cache
  cp -a .cache $CACHE_DIR/.cache
  echo "Saved .cache to $CACHE_DIR/.cache: \n`ls $CACHE_DIR/.cache`\n"
fi
