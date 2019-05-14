# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# https://www.emacswiki.org/emacs/TrampMode
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME=robbyrussell

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export TERM="xterm-256color"

# adapted from robbyrussell and alanpeadbody themes
# https://zshthem.es/all/
# https://wiki.archlinux.org/index.php/Zsh#Colors
local user="%{$fg_bold[magenta]%}%n::%{$fg_bold[magenta]%}%m%{$reset_color%}"

local ret_status="%(?:%{$fg_bold[green]%}➜:%{$fg_bold[red]%}➜)"
PROMPT='${user} ${ret_status} %{$fg[cyan]%}%c%{$reset_color%} $(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

if type gio &> /dev/null; then
    alias rmt='gio trash'
else
    alias rmt='gvfs-trash'
fi

alias acp='git add --all; git commit; git push origin master'

# https://stackoverflow.com/questions/7522712/how-to-check-if-command-exists-in-a-shell-script
if type vim.gtk &> /dev/null; then
  # install foobar here
  alias vim='vim.gtk'
fi
alias vi='vim'

# https://wiki.archlinux.org/index.php/Ruby
PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"

# https://github.com/Intoli/exodus#installation
export PATH="~/.local/bin/:${PATH}"

# https://www.reddit.com/r/archlinux/comments/7oa1h7/pacman_is_great_but_should_we_really_handle_aur/ds8dakw/
#installs single package (i.e. aur-in spotify)
ain() {

    # https://stackoverflow.com/questions/4651437/how-to-set-a-variable-to-the-output-from-a-command-in-bash#4651495
   CURRENT_DIR="$(pwd)"

   # control-c will terminate script execution and remove temporary files
   trap "echo; echo 'SIGINT received: Deleting temp files then exiting!'; cd $CURRENT_DIR; return 1" INT


   mkdir -p $HOME/Downloads/pkgs
   cd $HOME/Downloads/pkgs
   git clone https://aur.archlinux.org/${1}.git
   cd ${1}
   vim PKGBUILD
   echo "Install package? [y / n]"
   read response
   case "$response" in
       [yY])
           makepkg -si
           ;;
       *)
           echo "not installing package."
           ;;
   esac
   cd ..

   echo "remove aur downloads? [y / n]"
   read response
   case "$response" in
       [yY])
           for pkg in $HOME/Downloads/pkgs/
               rm -rfI "$pkg"
               ;;
        *)
            echo "ok. not removing."
            ;;
    esac

    cd "$CURRENT_DIR"
}


#updates packages
aup() {
   aursync -u
    # https://stackoverflow.com/questions/4651437/how-to-set-a-variable-to-the-output-from-a-command-in-bash#4651495
#   CURRENT_DIR="$(pwd)"

#   cd ~/Downloads/pkgs
#   cower -vduf
#   for pkg in ~/Downloads/pkgs
#   do
#       echo "Update package(s)? [y / n]"
#       read response
#       case "$response" in
#       [yY])
#           cd $pkg
#           makepkg -si
#           cd ..
#           ;;
#       *)
#           echo "not updating package(s)."
#           ;;
#       esac
#       # aurbuild -d custom
#       # repo-add /var/cache/pacman/custom/custom.db.tar *.pkg.tar.xz
#       # makepkg -si
#   done
#
#   echo "remove aur downloads? [y / n]"
#   read response
#   case "$response" in
#       [yY])
#           for pkg in ~/Downloads/pkgs
#               rmt "$pkg"
#               ;;
#        *)
#            echo "ok. not removing."
#            ;;
#    esac
#
#    cd "$CURRENT_DIR"

}

# http://exercism.io/clients/cli/linux
export PATH=$HOME/.local/bin:$PATH

if [ -f ~/.config/exercism/exercism_completion.zsh ]; then
  . ~/.config/exercism/exercism_completion.zsh
fi

# https://askubuntu.com/questions/758496/how-to-make-a-permanent-alias-in-oh-my-zsh
hash -d s=~/Documents/code/osu/2019spring/
hash -d f=~/Documents/code/osu/2018fall/
hash -d i=~/Documents/code/osu/2019winter/
hash -d fin=~/Documents/personal/finances/ledger/
hash -d w=~/Documents/writings/

# https://old.reddit.com/r/emacs/comments/9b1bhs/emacsshell_protip_alias_magit/
alias magit='emacsclient -c -n -a "" -e "(progn (magit-status) (delete-other-windows))"'

alias em='(emacsclient -c -a "" &)'

# https://github.com/rust-lang/rustup.rs#toolchain-specification
fpath+=~/.zfunc

export APP="cs467-map-server"
export DATABASE_FILE=database.sql
export DATABASE_LOCAL=map
export DATABASE_URL=postgres://$(whoami)@localhost/$DATABASE_LOCAL

alias startx='startx -- vt$(tty | sed -e "s:/dev/tty::")'
