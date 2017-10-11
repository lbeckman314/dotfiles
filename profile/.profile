# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# https://wiki.lineageos.org/adb_fastboot_guide.html#setting-up-adb
if [ -d "$HOME/Documents/code/mobile/adb-fastboot/platform-tools" ] ; then
    export PATH="$HOME/Documents/code/mobile/adb-fastboot/platform-tools:$PATH"
fi

# helps display themes and colors correctly
export TERM=xterm-256color
