# - - - - - - - - - - - #
# Settings
# - - - - - - - - - - - #

setopt NO_GLOBAL_RCS
setopt prompt_subst
autoload colors
colors

# Support for Emacs Tramp.
# https://www.emacswiki.org/emacs/TrampMode
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

autoload -Uz compinit && compinit
# partial completion suggestions
zstyle ':completion:*' list-suffixes
zstyle ':completion:*' expand prefix suffix
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=60'

export LANG=en_US.UTF-8
export TERM="xterm-256color"
export PATH=/Users/beckmanl/.cargo/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Users/beckmanl/.cargo/bin
export QT_PLUGIN_PATH=/usr/local/Cellar/qt/5.14.1/plugins:/usr/local/lib/qt5/plugins
export PATH="$PATH:/usr/local/Cellar/mysql@5.7/5.7.27_1/bin/"
export PATH="$PATH:$HOME/.local/bin"

# - - - - - - - - - - - #
# Fuzzy finder
# - - - - - - - - - - - #

fuzzy_bindings="/usr/share/doc/fzf/key-bindings.zsh"
fuzzy_completions="/usr/share/doc/fzf/completion.zsh"
source $fuzzy_bindings
source $fuzzy_completions

# - - - - - - - - - - - #
# Aliases
# - - - - - - - - - - - #

alias -g ...='../..'
alias em='emacsclient -c -n -a "" -e "(startup)"'
alias gc="git commit"
alias gaa="git add --all"
alias gco="git checkout"
alias gg="git branch -a | tr -d \* | sed '/->/d' | xargs git grep"
alias gst="git status"
alias ls="exa"
alias l="exa -al"
alias kdeconnect="kcmshell5 kcm_kdeconnect"
alias magit='emacsclient -c -n -a "" -e "(progn (magit-status) (delete-other-windows))"'
alias rmt='mkdir -p $HOME/trash; mv --backup=t -t $HOME/trash'
alias jshell=/Users/beckmanl/dev/openjdk-13/jdk-13.0.1+9/Contents/Home/bin/jshell
alias vim=vim-huge

setxkbmap -option caps:escape

# - - - - - - - - - - - #
# Functions
# - - - - - - - - - - - #

gpom() {
    git add --all;
    git commit;
    git push origin $(git branch | grep \* | cut -d ' ' -f2)
}

glom() {
    git pull origin $(git branch | grep \* | cut -d ' ' -f2)
}

# https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/git.zsh
git_prompt_info () {
    local ref
    ref=$(command git symbolic-ref HEAD 2> /dev/null)  || ref=$(command git rev-parse --short HEAD 2> /dev/null)  || return 0
    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

# Checks if working tree is dirty
parse_git_dirty() {
    local STATUS
    local -a FLAGS
    FLAGS=('--porcelain')
    if [[ "$DISABLE_UNTRACKED_FILES_DIRTY" == "true" ]]; then
        FLAGS+='--untracked-files=no'
    fi
    case "$GIT_STATUS_IGNORE_SUBMODULES" in
        git)
            # let git decide (this respects per-repo config in .gitmodules)
            ;;
        *)
            # if unset: ignore dirty submodules
            # other values are passed to --ignore-submodules
            FLAGS+="--ignore-submodules=${GIT_STATUS_IGNORE_SUBMODULES:-dirty}"
            ;;
    esac
    STATUS=$(command git status ${FLAGS} 2> /dev/null | tail -n1)
    if [[ -n $STATUS ]]; then
        echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
    else
        echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
    fi
}

# - - - - - - - - - - - #
# Prompt
# - - - - - - - - - - - #

# Adapted from robbyrussell and alanpeadbody themes.
# https://zshthem.es/all/
# https://wiki.archlinux.org/index.php/Zsh#Colors
ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}x"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

# ME::HOST → DIRECTORY [GIT STATUS]
ME="%{$fg_bold[magenta]%}%n%{$fg_bold[blue]%}::%{$fg_bold[magenta]%}%m%{$reset_color%}"
RET_STATUS="%(?:%{$fg_bold[green]%}>:%{$fg_bold[red]%}>)"
DIR="%{$fg[cyan]%}%c%{$reset_color%}"
DATE="%{$fg_bold[blue]%}%D{%F %R:%S}%{$reset_color%}"
PROMPT='${ME} ${RET_STATUS} ${DIR} $(git_prompt_info)'
RPROMPT='${DATE}'
USER="$(whoami)"
USER=" $USER"

# - - - - - - - - - - - #
# Path
# - - - - - - - - - - - #

PATH="$HOME/.local/bin/:/home/liam/.gem/ruby/2.6.0/bin:/home/liam/.cargo/bin:/home/liam/bin:/home/liam/go/bin:/usr/bin:/usr/share/bin:/usr/local/bin"
export PATH="$PATH:/opt/texlive/2020/bin/x86_64-linux/"

QT_PLUGIN_PATH=/usr/local/Cellar/qt/5.14.2/plugins:/usr/local/lib/qt5/plugins
alias cat=bat
export BAT_THEME=Dracula
if [ -e /Users/beckmanl/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/beckmanl/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
PATH=$PATH:/Users/beckmanl/Library/Python/3.7/bin
PATH=/usr/local/texlive/2019/bin/x86_64-darwin/:$PATH
alias gp=gopass
alias rn=ranger

# VOID
alias open=xdg-open

# Doom Emacs
PATH=$PATH:$HOME/.emacs.d/bin/
