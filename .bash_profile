NO_COLOUR="\[\033[0m\]"                                                                                                                    
GREY="\[\033[1;30m\]"                                                                                                                      
RED="\[\033[1;31m\]"                                                                                                                       
YELLOW="\[\033[1;33m\]"

export PS1="$GREY\w$NO_COLOUR \$(exit=\$?; if [[ \$exit == 0 ]]; then echo \"$YELLOW⚡\"; else echo \"$RED⚡\"; fi)$NO_COLOUR "              


# Emacs stuff
export ALTERNATE_EDITOR=""
alias em='emacsclient -nw -t'
