# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=/Users/kristianhole/.oh-my-zsh

ZSH_THEME="robbyrussell"
# ZSH_THEME="imajes"
# ZSH_THEME="lambda"
# ZSH_THEME="sorin"

DEFAULT_USER="kristianhole"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
#plugins=(git osx zsh-autosuggestions)
# plugins=(osx zsh-syntax-highlighting vi-mode zsh-autosuggestions)
plugins=(osx zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# User configuration

# keybind autosuggest: accept suggestion: CTRL SPACE
bindkey '^ ' autosuggest-accept

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=238'

# Aliases

alias code="'/Applications/Sublime Text.app/Contents/SharedSupport/bin/subl'"
alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
