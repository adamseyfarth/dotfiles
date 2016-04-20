# -*- mode: shell-script; -*-

color='%{$fg_bold[black]%}'
ul='╭'
ll='╰'
bar='│'
dash='─'

if [ $TERM = linux ]; then
    color=''
    ul='l'
    ll='m'
    bar='x'
    dash='q'
fi

ZSH_THEME_GIT_PROMPT_PREFIX=" ${color}("
ZSH_THEME_GIT_PROMPT_SUFFIX="${color})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}⚡%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function prompt_char {
    if [ $UID -eq 0 ]; then
        echo "%{$fg[red]%}#%{$reset_color%}"
    else
        echo ${color}"$%{$reset_color%}"
    fi
}

function timestamp() {
    date +'[%Y-%m-%d %H:%M:%S.%3N%:z]'
}

function preexec_timestamp() {
    echo ${color}${bar} $(timestamp)${reset_color}
}

prompt_tail=\
'%(?,,'${color}${bar}' %{$fg[red]%}FAIL: $?%{$reset_color%}
)'${color}${ll}${dash}'$(timestamp)%{$reset_color%}$(git_prompt_info)'

prompt_head=\
${color}${ul}${dash}'%n@%m:%{$fg_bold[blue]%}%~/%{$reset_color%}$(prompt_char)
'

PROMPT=\
"$prompt_tail

$prompt_head"

PROMPT2=${color}'%_> %{$reset_color%}'
PROMPT3=${color}'?# %{$reset_color%}'
PROMPT4=${color}'+%N:%i> %{$reset_color%}'

RPROMPT=

autoload -U add-zsh-hook
add-zsh-hook preexec preexec_timestamp
