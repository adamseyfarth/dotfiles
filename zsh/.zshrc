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

# alias ls='ls --color -G'
alias ls="ls -GF"
alias en="emacsclient -n"
alias vordune="fortune | tr stpfkSTPFKhHqQ zdbvgZDBVG''kK"
alias ag="ag --color-path 1\;33"

function ls_on_chdir() {
    ls
}
add-zsh-hook chpwd ls_on_chdir

source $HOME/.benchling-dotfiles/.bashrc.benchling

ssh-add -A

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

export VIRTUAL_ENV_DISABLE_PROMPT=yes
source ~/.envs/aurelia/bin/activate
export LSCOLORS=exfxcxdxbxegedabagacad

if [ -z "$INSIDE_EMACS" -a -e "${HOME}/.iterm2_shell_integration.zsh" ]; then
    source "${HOME}/.iterm2_shell_integration.zsh"
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

setopt no_share_history
