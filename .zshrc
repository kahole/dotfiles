# Path to your oh-my-zsh installation.
export ZSH=/Users/kristianhole/.oh-my-zsh

# ZSH_THEME="robbyrussell"
# ZSH_THEME="imajes"
ZSH_THEME="lambda"
# ZSH_THEME="sorin"
# ZSH_THEME="agnoster"

DEFAULT_USER="kristianhole"

#plugins=(git osx zsh-autosuggestions)
# plugins=(osx zsh-syntax-highlighting vi-mode zsh-autosuggestions)
# plugins=(osx zsh-autosuggestions)
plugins=(zsh-syntax-highlighting zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

# don't insert tab when 0 chars to left of cursor
# zstyle ':completion:*' insert-tab false

# keybind autosuggest: accept suggestion: CTRL SPACE
# bindkey '^ ' autosuggest-accept
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=238'

alias vimrc="vim ~/.vimrc"
alias zshrc="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
