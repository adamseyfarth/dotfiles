source $HOME/dotfiles/antigen/antigen.zsh
antigen use oh-my-zsh
antigen theme tjkirch

antigen bundle zsh-users/zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
ZSH_HIGHLIGHT_STYLES[default]=none
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=red,bold
ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=green
ZSH_HIGHLIGHT_STYLES[alias]=none
ZSH_HIGHLIGHT_STYLES[builtin]=none
ZSH_HIGHLIGHT_STYLES[function]=none
ZSH_HIGHLIGHT_STYLES[command]=none
ZSH_HIGHLIGHT_STYLES[precommand]=none
ZSH_HIGHLIGHT_STYLES[commandseparator]=none
ZSH_HIGHLIGHT_STYLES[hashed-command]=none
ZSH_HIGHLIGHT_STYLES[path]=none
# ZSH_HIGHLIGHT_STYLES[globbing]=none
ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=blue
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=none
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=none
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=yellow
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=yellow
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=cyan
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=cyan
ZSH_HIGHLIGHT_STYLES[assign]=none

export PATH="$HOME/bin:$HOME/.local/bin:/usr/local/krb5/bin:/usr/local/ossh/bin:$PATH"
export EDITOR="emacsclient"
export LESS=-X
export KRB5_CONFIG=/usr/local/krb5/etc/krb5.conf

alias ls="ls --color"
alias en="emacsclient -n"