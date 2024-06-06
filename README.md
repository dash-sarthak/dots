[//]: # ( ________  ________  _________  ________ ___  ___       _______   ________      )
[//]: # (|\   ___ \|\   __  \|\___   ___\\  _____\\  \|\  \     |\  ___ \ |\   ____\     )
[//]: # (\ \  \_|\ \ \  \|\  \|___ \  \_\ \  \__/\ \  \ \  \    \ \   __/|\ \  \___|_    )
[//]: # ( \ \  \ \\ \ \  \\\  \   \ \  \ \ \   __\\ \  \ \  \    \ \  \_|/_\ \_____  \   )
[//]: # (  \ \  \_\\ \ \  \\\  \   \ \  \ \ \  \_| \ \  \ \  \____\ \  \_|\ \|____|\  \  )
[//]: # (   \ \_______\ \_______\   \ \__\ \ \__\   \ \__\ \_______\ \_______\____\_\  \ )
[//]: # (    \|_______|\|_______|    \|__|  \|__|    \|__|\|_______|\|_______|\_________\)
[//]: # (                                                                    \|_________|)
[//]: # (                                                                                )
                                                                                )


# Dotfiles

This directory contains the dotfiles for my system

## Requirements

A few prerequisites prerequisites

| Program | Fedora                  | Arch                  |
| ------- | ----------------------- | --------------------- |
| git     | `sudo dnf install git`  | `sudo pacman -S git`  |
| stow    | `sudo dnf install stow` | `sudo pacman -S stow` |

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

