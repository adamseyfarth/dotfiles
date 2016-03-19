# -*- mode: shell-script; -*-

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg_bold[black]%}("
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$fg_bold[black]%})%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}⚡"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function prompt_char {
    if [ $UID -eq 0 ]; then
        echo "%{$fg[red]%}#%{$reset_color%}"
    else
        echo "%{$fg_bold[black]%}$%{$reset_color%}"
    fi
}

function timestamp() {
    date +'[%Y-%m-%d %H:%M:%S.%3N%:z]'
}

function preexec_timestamp() {
    echo ${fg_bold[black]}│ $(timestamp)${fg_no_bold[white]}
}

prompt_tail=\
'%(?,,%{$fg_bold[black]%}│%{$fg[red]%} FAIL: $?%{$reset_color%}
)%{$fg_bold[black]%}╰─$(timestamp)%{$reset_color%}$(git_prompt_info)'

prompt_head=\
'%{$fg_bold[black]%}╭─%n@%m:%{$fg_bold[blue]%}%~/%{$reset_color%}$(prompt_char)
'

PROMPT=\
"$prompt_tail

$prompt_head"

PROMPT2='%{$fg_bold[black]%}%_> %{$reset_color%}'
PROMPT3='%{$fg_bold[black]%}?# %{$reset_color%}'
PROMPT4='%{$fg_bold[black]%}+%N:%i> %{$reset_color%}'

autoload -U add-zsh-hook
add-zsh-hook preexec preexec_timestamp
