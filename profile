#
# ~/.profile
#

[[ -f ~/.kshrc ]] && . ~/.kshrc

export PATH=$HOME/bin:$PATH
export PAGER=less
export LESS='-FSRXMix4'

export EDITOR=gvim
export GOPATH=$HOME/go

export ENV=$HOME/.kshrc
