# -*- mode: shell-script; -*-

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg_bold[black]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[red]%}⚡"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function prompt_char {
    if [ $UID -eq 0 ]; then
        echo "%{$fg[red]%}#%{$reset_color%}"
    else
        echo "%{$fg_bold[black]%}$%{$reset_color%}"
    fi
}

PROMPT='%(?,,%{$fg_bold[black]%}│%{$fg[red]%} FAIL: $?%{$reset_color%}
)%{$fg_bold[black]%}╰─[%D{%Y-%m-%d %H:%M:%S}]%{$reset_color%}

%{$fg_bold[black]%}╭─%n@%m: %{$fg[blue]%}%~%{$reset_color%}$(prompt_char)$(git_prompt_info)
  %{$fg_bold[black]%}%_%{$reset_color%}'
