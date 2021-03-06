# -*- mode: shell-script; -*-

echocolor="$fg_bold[black]"
color="%{${echocolor}%}"
ul='╭'
ll='╰'
bar='│'
dash='─'

if [ $TERM = linux ]; then
    color=''
    echocolor=''
    ul='l'
    ll='m'
    bar='x'
    dash='q'
fi

ZSH_THEME_GIT_PROMPT_PREFIX=" ${color}(git:"
ZSH_THEME_GIT_PROMPT_SUFFIX="${color})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}𝚫%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function prompt_char {
    if [ $UID -eq 0 ]; then
        echo "%{$fg[red]%}#%{$reset_color%}"
    else
        echo ${color}"$%{$reset_color%}"
    fi
}

function timestamp() {
    date +'[%Y-%m-%d %H:%M:%S]'
}

function preexec_timestamp() {
    echo ${echocolor}${bar} $(timestamp)${reset_color}
}

function venv_tag() {
    if [ -z $VIRTUAL_ENV ]; then
        return
    fi
    echo " ${echocolor}(venv:$(basename $VIRTUAL_ENV))${reset_color}"
}

prompt_tail=\
'%(?,,'${color}${bar}' %{$fg[red]%}exit status: $?%{$reset_color%}
)'${color}${ll}${dash}'$(timestamp)%{$reset_color%}$(git_prompt_info)$(venv_tag)'

prompt_head=\
${color}${ul}${dash}'%n@%m:%{$fg_bold[blue]%}%~/%{$reset_color%}$(prompt_char)'

PROMPT=\
"$prompt_tail

$prompt_head
"

PROMPT2=${color}'%_> %{$reset_color%}'
PROMPT3=${color}'?# %{$reset_color%}'
PROMPT4=${color}'+%N:%i> %{$reset_color%}'

RPROMPT=

autoload -U add-zsh-hook
add-zsh-hook preexec preexec_timestamp
