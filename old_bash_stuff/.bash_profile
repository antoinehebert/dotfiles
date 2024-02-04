# Load ~/.extra, ~/.bash_prompt, ~/.exports, ~/.aliases and ~/.functions
# ~/.extra can be used for settings you don’t want to commit
for file in ~/.{extra,bash_prompt,exports,aliases,functions}; do
    [ -r "$file" ] && source "$file"
done
unset file

# node, nvm & npm
if [ -r $HOME/.nvm/nvm.sh ]; then
    source $HOME/.nvm/nvm.sh
    source ${NVM_DIR}/bash_completion
    . <(npm completion)
fi

# rbenv setup
if [ -r $HOME/.rbenv/bin -o -r /usr/local/bin/rbenv ]; then
    export PATH="$PATH:$HOME/.rbenv/bin"
    eval "$(rbenv init -)"
fi

# Rust
if [ -r $HOME/.cargo/bin/ ]; then
    export PATH="$PATH:$HOME/.cargo/bin"
fi

# make z command available
. ~/bin/z/z.sh

# Enable globbing
shopt -s globstar

if [ "$(uname)" == "Darwin" ]; then
    if [ -f `brew --prefix`/etc/bash_completion ]; then
        . `brew --prefix`/etc/bash_completion
    fi
fi

source ~/git-completion.bash
