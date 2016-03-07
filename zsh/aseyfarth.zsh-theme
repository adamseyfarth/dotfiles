# -*- mode: shell-script; -*-

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{$fg[yellow]%}⚡"
ZSH_THEME_GIT_PROMPT_CLEAN=""

function prompt_char {
	if [ $UID -eq 0 ]; then echo "%{$fg[red]%}#%{$reset_color%}"; else echo $; fi
}

PROMPT='%(?,,%{$fg[red]%}FAIL: $?%{$reset_color%}
)%{$fg[yellow]%}[%D{%Y-%m-%d} %*]%{$reset_color%}

%{$fg[magenta]%}%n%{$reset_color%}@%{$fg[magenta]%}%m%{$reset_color%}: %{$fg_bold[blue]%}%4~%{$reset_color%}$(prompt_char)$(git_prompt_info)
%_'
