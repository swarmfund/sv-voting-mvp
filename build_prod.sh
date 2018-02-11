#!/usr/bin/env bash

if [ $INSTALL_WEBPACK ]; then
  echo "##> Installing webpack..."
  npm install -g webpack
  echo "##> Installed webpack."
fi

webpack "$@"
