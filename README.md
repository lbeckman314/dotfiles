Dotfiles for vim, emacs, and others. Hack away like it's 1998!

# to add dotfiles (get started with vim for example):

```shell

# make a backup of your original config(s) just in case
mv ~/.vimrc ~/.vimrc.back
mv ~/.vim ~/.vim.back

# clone the repo
git clone https://www.github.com/lbeckman314/dotfiles.git

cd dotfiles

# gnu stow method (use if you have stow installed)
stow -t $HOME vim

# traditional method
ln -s `pwd`/vim/.vimrc ~/.vimrc
ln -s `pwd`/vim/.vim ~/.vim

```

<br />
<br />

# to remove dotfiles:

```shell

# restore backups
mv ~/.vimrc.back ~/.vimrc
mv ~/.vim.back ~/.vim

rm -rI dotfiles

```
