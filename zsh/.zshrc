# 1. ENVIRONMENT VARIABLES & PATH CONSOLIDATION
# Set paths once at the start to avoid redundant shell lookups
ZSH_DISABLE_COMPFIX="true"
export ZSH="$HOME/.oh-my-zsh"

# Optimized Completion Loading (Fixed for first-time run)
autoload -Uz compinit

export NVM_DIR="$HOME/.nvm"
export SDKMAN_DIR="$HOME/.sdkman"
export BUN_INSTALL="$HOME/.bun"
export PNPM_HOME="/Users/beckmanl/Library/pnpm"
export KUBECONFIG=~/.kube/config:~/.kube/config.calypr-prod:~/.kube/config.calypr-dev
export EDITOR=lvim
export GPG_TTY=$(tty)

# Minimalistic Path Setup
typeset -U path  # Ensures PATH only contains unique entries
path=(
    $HOME/bin
    /usr/local/bin
    /opt/homebrew/bin
    /opt/homebrew/sbin
    $HOME/.local/bin
    $HOME/.rd/bin
    $HOME/.jenv/bin
    $HOME/.hishtory
    $HOME/.gen3
    $HOME/.opencode/bin
    $HOME/.antigravity/antigravity/bin
    $PNPM_HOME
    $BUN_INSTALL/bin
    /opt/homebrew/opt/postgresql@17/bin
    /opt/homebrew/opt/go@1.24/bin
    /opt/homebrew/opt/dart@2.19/bin
    $HOME/go/bin
    $path
)

# 2. OH-MY-ZSH CONFIGURATION
ZSH_THEME="robbyrussell"
# Disable untracked files check to speed up prompt in large git repos
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(fzf git kubectl python)

source $ZSH/oh-my-zsh.sh

# 3. HISTORY SETTINGS (Reduced for speed)
export HISTSIZE=50000
export SAVEHIST=50000
setopt EXTENDED_HISTORY

# Atuin init
eval "$(atuin init zsh --disable-up-arrow)"

# 6. ALIASES & KEYBINDINGS
alias dc=docker-compose    
alias gc="git commit"
alias vim=lvim
alias kc=kubectl
alias rmt=trash
alias cat=bat
alias nf=nextflow
alias kx=kubectx
alias ls='ls --color=auto'
alias kubectl="kubecolor"

bindkey -e
bindkey '^[^[[C' forward-word
bindkey '^[^[[D' backward-word


# opencode
export PATH=/Users/beckmanl/.opencode/bin:$PATH

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/beckmanl/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/beckmanl/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/beckmanl/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/beckmanl/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

# bun completions
[ -s "/Users/beckmanl/.bun/_bun" ] && source "/Users/beckmanl/.bun/_bun"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# pnpm
export PNPM_HOME="/Users/beckmanl/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME/bin:"*) ;;
  *) export PATH="$PNPM_HOME/bin:$PATH" ;;
esac
# pnpm end
