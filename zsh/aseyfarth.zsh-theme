# -*- mode: shell-script; -*-

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[black]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" ⚡"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function prompt_char {
    if [ $UID -eq 0 ]; then
        echo "%{$fg[red]%}#%{$reset_color%}"
    else
        echo "%{$fg[black]%}$%{$reset_color%}"
    fi
}

PROMPT='%(?,,%{$fg[black]%}│%{$fg[red]%} FAIL: $?%{$reset_color%}
)%{$fg[black]%}╰─[%D{%Y-%m-%d %H:%M:%S}]%{$reset_color%}

%{$fg[black]%}╭─%n@%m: %{$fg[blue]%}%~%{$reset_color%}$(prompt_char)$(git_prompt_info)
  %{$fg[black]%}%_%{$reset_color%}'
