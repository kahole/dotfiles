# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/Users/kristianhole/.oh-my-zsh

ZSH_THEME="robbyrussell"
# ZSH_THEME="imajes"
# ZSH_THEME="lambda"
# ZSH_THEME="sorin"

DEFAULT_USER="kristianhole"

#plugins=(git osx zsh-autosuggestions)
# plugins=(osx zsh-syntax-highlighting vi-mode zsh-autosuggestions)
plugins=(osx zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# User configuration

# keybind autosuggest: accept suggestion: CTRL SPACE
bindkey '^ ' autosuggest-accept

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=238'

# Aliases

alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
