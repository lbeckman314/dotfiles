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
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=60'

export LANG=en_US.UTF-8
export TERM="xterm-256color"

case $(uname -s) in
    Linux*)
        setxkbmap -option caps:escape
        ;;
esac

# - - - - - - - - - - - #
# Fuzzy finder
# - - - - - - - - - - - #

case $(uname -s) in
    Linux*)
        fuzzy_bindings="/usr/share/doc/fzf/key-bindings.zsh"
        fuzzy_completions="/usr/share/doc/fzf/completion.zsh"
        ;;
    Darwin*)
        fuzzy_bindings="/usr/local/opt/fzf/shell/key-bindings.zsh"
        fuzzy_completions="/usr/local/opt/fzf/shell/completion.zsh"
        ;;
esac

source $fuzzy_bindings
source $fuzzy_completions

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

# ME::HOST > DIRECTORY [GIT STATUS] ... [DATE/TIME]
ME="%{$fg_bold[magenta]%}%n%{$fg_bold[blue]%}::%{$fg_bold[magenta]%}%m%{$reset_color%}"
RET_STATUS="%(?:%{$fg_bold[green]%}>:%{$fg_bold[red]%}>)"
DIR="%{$fg[cyan]%}%c%{$reset_color%}"
DATE="%{$fg_bold[blue]%}%D{%F %R:%S}%{$reset_color%}"
PROMPT='${ME} ${RET_STATUS} ${DIR} $(git_prompt_info)'
RPROMPT='${DATE}'

# - - - - - - - - - - - #
# Aliases
# - - - - - - - - - - - #

alias -g ...='../..'
alias cat=bat
alias em='emacsclient -c -n -a "" -e "(startup)"'
alias gaa="git add --all"
alias gc="git commit"
alias gco="git checkout"
alias gg="git branch -a | tr -d \* | sed '/->/d' | xargs git grep"
alias gp=gopass
alias gst="git status"
alias kdeconnect="kcmshell5 kcm_kdeconnect"
alias l="exa -al"
alias ls="exa"
alias magit='emacsclient -c -n -a "" -e "(progn (magit-status) (delete-other-windows))"'
alias rmt='mkdir -p $HOME/trash; mv --backup=t -t $HOME/trash'
alias rn=ranger
export BAT_THEME=Dracula

case $(uname -s) in
    Linux*)
        alias vim=vim-huge
        alias open=xdg-open
        ;;
    Darwin*)
        alias jshell=/Users/beckmanl/dev/openjdk-13/jdk-13.0.1+9/Contents/Home/bin/jshell
        alias vim=/usr/local/Cellar/vim/8.2.0654/bin/vim
        ;;
esac

# - - - - - - - - - - - #
# Path
# - - - - - - - - - - - #

case $(uname -s) in
    Linux*)
        export PATH=\
"$HOME"/.cargo/bin:\
"$HOME"/.gem/ruby/2.6.0/bin:\
"$HOME"/.local/bin:\
"$HOME"/bin:\
"$HOME"/go/bin:\
/opt/texlive/2020/bin/x86_64-linux:\
/usr/bin:\
/usr/local/bin:\
/usr/share/bin
        ;;
    Darwin*)
        export PATH=\
"$HOME"/.cargo/bin:\
"$HOME"/.local/bin:\
"$HOME"/Library/Python/3.7/bin:\
/bin:\
/sbin:\
/usr/bin:\
/usr/local/Cellar/mysql@5.7/5.7.27_1/bin:\
/usr/local/Cellar/python@3.8/3.8.4/Frameworks/Python.framework/Versions/3.8/bin:\
/usr/local/bin:\
/usr/local/texlive/2019/bin/x86_64-darwin:\
/usr/sbin \
        export QT_PLUGIN_PATH="/usr/local/Cellar/qt/5.14.1/plugins:/usr/local/lib/qt5/plugins"
        ;;
esac

