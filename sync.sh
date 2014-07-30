 #!/bin/bash
cd "$(dirname "$0")"
git pull
function doIt() {
   rsync --exclude ".git/" --exclude ".DS_Store" --exclude "sync.sh" --exclude "README.md" --exclude "install_deps.sh" -av . ~
}
if [ "$1" == "--force" -o "$1" == "-f" ]; then
   doIt
else
   read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
   echo
   if [[ $REPLY =~ ^[Yy]$ ]]; then
      doIt
   fi
fi
unset doIt

if [ $(egrep "(source|\.) ~/.bash_profile" ~/.bashrc | wc -l) -eq 0 ]; then
   echo [ -n "$PS1" ] && source ~/.bash_profile >> ~/.bashrc
fi

source ~/.bash_profile
