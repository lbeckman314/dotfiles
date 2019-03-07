declare -A args=(
[dotfiles]=0
[programs]=0
[personal]=0
[keys]=0
)

# Parse any args
fancy=0
POSITIONAL=()

while [ $# -gt 0 ]
do
    case $1 in
        "--dotfiles"|"-d")
            # output fancy message
            args[dotfiles]=1
            shift
            ;;
        "--programs"|"-p")
            # output fancy message
            args[programs]=1
            shift
            ;;
        "--personal"|"-e")
            # output fancy message
            args[personal]=1
            shift
            ;;
        "--keys"|"-k")
            # output fancy message
            args[keys]=1
            shift
            ;;
        *)
            # save it in an array for later
            POSITIONAL+=("$1")

            # shift past argument
            shift
            ;;
    esac

done

# restore positional parameters
set -- "${POSITIONAL[@]}"


# https://stackoverflow.com/questions/17336915/return-value-in-a-bash-function
distro() {
    if [ -f /etc/os-release ]; then
        # freedesktop.org and systemd
        . /etc/os-release
        OS=$NAME

    elif type lsb_release >/dev/null 2>&1; then
        # linuxbase.org
        OS=$(lsb_release -si)

    elif [ -f /etc/lsb-release ]; then
        # For some versions of Debian/Ubuntu without lsb_release command
        . /etc/lsb-release
        OS=$DISTRIB_ID

    elif [ -f /etc/debian_version ]; then
        # Older Debian/Ubuntu/etc.
        OS=debian

    else
        # Fall back to uname, e.g. "Linux <version>", also works for BSD, etc.
        OS=$(uname -s)
    fi

    echo $OS
}


# programs
programs() {
    ROOT="sudo"
    PROGRAMS="programs.txt"

    OS=$(echo $(distro))
    OS_LOWER=$(echo $OS | tr '[:upper:]' '[:lower:]')
    OS_SHORT=$(echo $OS_LOWER | awk '{print $1;}')

    declare -A pkgman=(
    [arch]="pacman -Syu --needed - "
    [void]="xbps-install -Su"
    [debian]="apt-get update; apt-get install"
    [gentoo]="emerge --ask --verbose --update --deep --newuse @world"
    )

    for pm in ${!pkgman[@]}
    do
        if [[ $OS_SHORT == $pm ]]
        then
            echo "$OS based operating system detected."
            INSTALL="${pkgman[$pm]}"
        fi
    done


    if [ ! -z "$INSTALL" ]
    then
        echo "Running '$INSTALL < $PROGRAMS'"
        $ROOT $INSTALL < $PROGRAMS
    fi
}


keys() {
    KEY="https://git.liambeckman.com/cgit/private"
    git clone $KEY
    gpg --output private/.ssh.tar.gz --decrypt private/.ssh.tar.gz.gpg
    tar -zxvf private/.ssh.tar.gz -C private
    cp -r private/.ssh $HOME
}


# dotfiles
dotfiles() {
    PATH=$HOME/Documents/code/dotfiles
    REPO="git@liambeckman.com:/srv/git/dotfiles.git"
    BACKUP=".backup"

    declare -A configs=(
    [bspwm]=$HOME/.config/bspwm
    [emacs]=$HOME
    [gtk]=$HOME/.config/gtk
    [nvim]=$HOME
    [openbox]=$HOME/.config/openbox
    [polybar]=$HOME/.config/polybar
    [sxhkd]=$HOME/.config/sxhkd
    [tmux]=$HOME
    [vim]=$HOME
    [zsh]=$HOME
    )

    mkdir -p $PATH

    git clone $REPO $PATH

    for pkg in "${!configs[@]}"
    do
        for file in $pkg
        do
            file_count=$(find "${configs[$pkg]}" -name $file | wc -l)
            if [[ $file_count -gt 0 ]]
            then
                mv "${configs[$pkg]}/$file" "${configs[$pkg]}/$file$BACKUP"
            fi
        done

        stow -t ${configs[$pkg]} "$PATH/$pkg"
    done
}


personal() {
    xdg-user-dirs-update
    echo "exec sxhkd &" >> $HOME/.xinitrc
    echo "exec bspwm &" >> $HOME/.xinitrc
}


# ./up.sh --programs --dotfiles --personal --keys
main() {
    if [ ${args[programs]} -eq 1 ]
    then
        programs
    fi

    if [ ${args[dotfiles]} -eq 1 ]
    then
        dotfiles
    fi

    if [ ${args[personal]} -eq 1 ]
    then
        personal
    fi

    if [ ${args[keys]} -eq 1 ]
    then
        keys
    fi
}


main
