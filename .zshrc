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
