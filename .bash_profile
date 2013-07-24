# Load ~/.extra, ~/.bash_prompt, ~/.exports, ~/.aliases and ~/.functions
# ~/.extra can be used for settings you don’t want to commit
for file in ~/.{extra,bash_prompt,exports,aliases,functions}; do
   [ -r "$file" ] && source "$file"
done
unset file

# node, nvm & npm
if [ -r ~/.nvm/nvm.sh ]; then
  source ~/.nvm/nvm.sh
  source ${NVM_DIR}/bash_completion
  . <(npm completion)
fi

# rbenv setup
if [ -r $HOME/.rbenv/bin ]; then
  export PATH="$HOME/.rbenv/bin:$PATH"
  eval "$(rbenv init -)"
fi

# make z command available
. ~/bin/z/z.sh
