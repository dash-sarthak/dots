#! /bin/bash

IMG_PATH="/home/sarthak/Pictures/Screenshots/Screenshot from $(date +'%Y-%m-%d_%H_%M_%S').png"
grimshot --notify save $1 "$IMG_PATH" && wl-copy < "$IMG_PATH"
