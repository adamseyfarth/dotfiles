function load () {
    test -f "$1" && source "$1"
}
load /etc/zprofile
load /etc/profile.d/autojump.zsh

source $HOME/dotfiles/antigen/antigen.zsh
antigen use oh-my-zsh
antigen theme $HOME/dotfiles/zsh aseyfarth

antigen bundles <<EOBUNDLES
    command-not-found
    zsh-users/zsh-syntax-highlighting
    zsh-users/zsh-autosuggestions
    zsh-users/zsh-completions
EOBUNDLES

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets)
ZSH_HIGHLIGHT_STYLES[default]=none
ZSH_HIGHLIGHT_STYLES[unknown-token]=fg=red,bold
ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=green
ZSH_HIGHLIGHT_STYLES[alias]=fg=green
ZSH_HIGHLIGHT_STYLES[builtin]=fg=green
ZSH_HIGHLIGHT_STYLES[function]=fg=green
ZSH_HIGHLIGHT_STYLES[command]=fg=green
ZSH_HIGHLIGHT_STYLES[precommand]=none
ZSH_HIGHLIGHT_STYLES[commandseparator]=none
ZSH_HIGHLIGHT_STYLES[hashed-command]=none
ZSH_HIGHLIGHT_STYLES[path]=none
# ZSH_HIGHLIGHT_STYLES[globbing]=none
ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=blue
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]=none
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]=none
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]=none
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]=fg=magenta
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]=fg=magenta
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=cyan
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=cyan
ZSH_HIGHLIGHT_STYLES[assign]=none

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE=fg=black,bold

alias ls="ls -F --color"
alias en="emacsclient -n"
alias vordune="fortune | tr stpfkSTPFKhHqQ zdbvgZDBVG''kK"

function ls_on_chdir() {
    ls
}
add-zsh-hook chpwd ls_on_chdir

export NVM_DIR="/home/aseyfarth@margeo.nrlssc.navy.mil/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
