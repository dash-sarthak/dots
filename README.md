```text
 ________  ________  _________  ________ ___  ___       _______   ________      
|\   ___ \|\   __  \|\___   ___\\  _____\\  \|\  \     |\  ___ \ |\   ____\     
\ \  \_|\ \ \  \|\  \|___ \  \_\ \  \__/\ \  \ \  \    \ \   __/|\ \  \___|_    
 \ \  \ \\ \ \  \\\  \   \ \  \ \ \   __\\ \  \ \  \    \ \  \_|/_\ \_____  \   
  \ \  \_\\ \ \  \\\  \   \ \  \ \ \  \_| \ \  \ \  \____\ \  \_|\ \|____|\  \  
   \ \_______\ \_______\   \ \__\ \ \__\   \ \__\ \_______\ \_______\____\_\  \ 
    \|_______|\|_______|    \|__|  \|__|    \|__|\|_______|\|_______|\_________\
                                                                    \|_________|
 ```


# Dotfiles

This directory contains the dotfiles for my system

## Requirements

A few prerequisites prerequisites

| Program      | Fedora                                              | Arch                                                        |
| ------------ | --------------------------------------------------- | ----------------------------------------------------------- |
| git          | `sudo dnf install git`                              | `sudo pacman -S git`                                        |
| stow         | `sudo dnf install stow`                             | `sudo pacman -S stow`                                       |
| zsh          | `sudo dnf install zsh`                              | `sudo pacman -S zsh`                                        |
| oh-my-posh   | `curl -s https://ohmyposh.dev/install.sh | bash -s` | `yay -S oh-my-posh` (setup go before installing oh-my-posh) |
| neovim       | `sudo dnf install neovim`                           | `sudo pacman -S neovim`                                     |
| hyprland     | `sudo dnf install hyprland`                         | `sudo pacman -S hyprland`                                   |
| waybar       | `sudo dnf install waybar`                           | `sudo pacman -S waybar`                                     |
| rofi-wayland | `sudo dnf install rofi-wayland`                     | `sudo pacman -S rofi-wayland`                               |
| wlogout      | `sudo dnf install wlogout`                          | `yay -S wlogout`                                            |

## Installation

First, check out the dotfiles repo in your $HOME directory using git

```sh
$ git clone git@github.com/dash-sarthak/dots.git --depth 1 ~/dotfiles
$ cd dotfiles
```
Then use GNU stow to create symlinks

```sh
$ stow .
```

