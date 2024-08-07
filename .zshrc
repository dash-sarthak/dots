# Set dir for Zinit
ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"

# Download Zinit if not present
if [ ! -d "$ZINIT_HOME" ]; then
    mkdir -p "$(dirname $ZINIT_HOME)"
    git clone https://github.com/zdharma-continuum/zinit.git --depth 1 "$ZINIT_HOME"
fi

# Source/Load Zinit
source "${ZINIT_HOME}/zinit.zsh"

# Extensions
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-history-substring-search

# Snippets
zinit snippet OMZP::git
zinit snippet OMZP::sudo
zinit snippet OMZP::command-not-found

# Load completions
autoload -U compinit && compinit

zinit cdreplay -q

# Oh-My-Posh
eval "$(oh-my-posh init zsh --config $HOME/.config/ohmyposh/zen.toml)"

# Keybindings
bindkey -e
bindkey '^k' history-search-backward
# bindkey '^j' history-search-forward
bindkey '^[[2~' overwrite-mode                                  
bindkey '^[[3~' delete-char                                     
bindkey '^[[C'  forward-char                                    
bindkey '^[[D'  backward-char                                   
bindkey '^[[5~' history-beginning-search-backward
bindkey '^[[6~' history-beginning-search-forward
bindkey '^[Oc' forward-word
bindkey '^[Od' backward-word                                    
bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word
bindkey '^H' backward-kill-word
bindkey '^[[Z' undo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down
bindkey '^[[A' history-substring-search-up			
bindkey '^[[B' history-substring-search-down


# History
HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=$HISTSIZE
HISTDUP=erease

# Options
setopt appendhistory
setopt sharehistory
setopt hist_ignore_space
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups
setopt nobeep
setopt autocd
setopt correct

# Completion styling
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Aliases
# alias sudo="sudo "
alias cp="cp -i"
alias df='df -h'
alias free='free -m'
alias gitu='git add . && git commit && git push'
alias vi="nvim"
alias zshrc="nvim ~/.zshrc"
alias zupdate="source ~/.zshrc"
alias ls='eza --icons --color=always --group-directories-first'
alias ll='eza -alF --icons --color=always --group-directories-first'
alias la='eza -a --icons --color=always --group-directories-first'
alias l='eza -F --icons --color=always --group-directories-first'
alias l.='eza -a | egrep "^\."'
alias fetch='macchina'
alias bl='bluetuith'
alias install='yay -S --needed'
alias update='yay -Syu'
alias remove='yay -Runs'

# Color man pages
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-R

# Environment variables
export GOROOT=/usr/local/go
export GOBIN=/home/sarthak/go/bin
export VIRTUAL_ENV_DISABLE_PROMPT=0
export PYENV_VIRTUALENV_DISABLE_PROMPT=0
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/sarthak/.config/retroarch/cores/dolphin_libretro.so
export PYENV_ROOT="$HOME/.pyenv"

# Path
path=(
    $PYENV_ROOT/bin/
    $path
    $GOROOT
    $GOROOT/bin/
    $GOBIN
    /home/sarthak/.config/emacs/doom/bin
    /home/sarthak/scripts
    /home/sarthak/scripts/md2html/
    /home/sarthak/.local/bin
)

export PATH

# Pyenv
eval "$(pyenv init -)"
