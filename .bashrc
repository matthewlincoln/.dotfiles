# Mimic macOS default prompt for zsh:
PS1="\u@\h \W \$ "

# Add ~/bin to path:
export PATH="$PATH:~/bin"

# Manage legacy Python 2.7 through pyenv on macOS:
if [[ "$OSTYPE" == "darwin"* ]]; then
  export PYENV_ROOT="$HOME/.pyenv"
  command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
  eval "$(pyenv init -)"
  # Use system python by default:
  pyenv global system
  ### pyenv global 2.7.18 to switch to local instance of python 2.7
fi

# Add anaconda to path on macOS:
if [[ "$OSTYPE" == "darwin"* ]]; then
  path+=/opt/homebrew/anaconda3/bin
fi

# Set aliases:
alias ll="ls -FGlAhp"
alias llr="ls -FGlAhprt"
alias c="cat"
alias lc="wc -l"
alias zcat="gzcat"

# Alias to configure dotfiles using a bare git repository:
alias dotfiles='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# Always list directory contents on cd:
cd () { builtin cd "$@"; ll; }

# Create a new project directory and git repo:
np () {
  mkdir -p $1/data
  mkdir -p $1/src
  mkdir -p $1/results
  mkdir -p $1/doc

  touch $1/.gitignore
  touch $1/README.md

  git init $1
}

# Enable shell to send information to vterm:
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Enable vterm-clear-scrollback in vterm:
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi
