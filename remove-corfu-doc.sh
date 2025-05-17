#!/bin/bash
# Script to remove corfu-doc package from straight.el

# Remove the build directory
rm -rf ~/.emacs.d/straight/build/corfu-doc

# Remove the repo directory if it exists
rm -rf ~/.emacs.d/straight/repos/corfu-doc

# Remove package from straight's modified recipes if it exists
if [ -f ~/.emacs.d/straight/modified/corfu-doc ]; then
  rm -f ~/.emacs.d/straight/modified/corfu-doc
fi
