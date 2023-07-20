# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls -F --color=auto'
alias grep='grep --color'
PS1='\u@\h:\w % '
