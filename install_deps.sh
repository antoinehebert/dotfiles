#!/bin/bash

# https://github.com/rupa/z
# # z, oh how i love you
if [ ! -d ~/bin ]; then
  mkdir ~/bin
fi
cd ~/bin
git clone https://github.com/rupa/z.git
chmod +x ~/bin/z/z.sh

# for the c alias (syntax highlighted cat)
sudo easy_install Pygments
