# Always list directory contents on cd:
cd() { builtin cd "$@"; ll; }

# Set aliases:
alias ll="ls -FGlAhp"
alias llr="ls -FGlAhprt"
alias c="cat"
alias lc="wc -l"
alias zcat="gzcat"

# Alias to configure dotfiles using a bare git repository:
alias dotfiles='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
