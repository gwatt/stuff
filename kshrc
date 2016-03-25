
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

. /etc/ksh.kshrc

export HISTFILE=$HOME/.kshhist/`basename $(tty)`
export PS1=$'${USER}@`hostname`: ${PWD/#$HOME/\\~}\n! \$ '

alias ls='/bin/ls -F'
set -o vi
