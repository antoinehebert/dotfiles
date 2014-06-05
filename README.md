# dotfiles

My .files. Heavily inspired by
[@paulirish](https://github.com/paulirish/dotfiles), I'm just a copycat.

## Basic dependencies setup

Use `install_deps.sh` to install [z](https://github.com/rupa/z), pygments, etc.
install:
- gvim
- emacs
- git

## Installation

```bash
git clone https://github.com/antoinehebert/dotfiles.git
cd dotfiles
./sync.sh
```

To update later on, just run `sync.sh` again.

## Troubleshooting
If this is not working you might have to source the `.bash_profile` in `.bashrc`.