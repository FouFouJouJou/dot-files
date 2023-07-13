# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -F --color=auto'
alias ssh-init='eval $(ssh-agent -s); ssh-add ~/.ssh/github_firasjaballiftw_void;'
PS1='\u@\h:\w % '
