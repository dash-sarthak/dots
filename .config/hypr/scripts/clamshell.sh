#!/bin/sh

LAPTOP_OUTPUT="eDP-1"
LID_STATE_FILE="/proc/acpi/button/lid/LID0/state"

read -r LS < "$LID_STATE_FILE"
echo $LS
case "$LS" in
    *open)   hyprctl keyword monitor "eDP-1, 2160x1440@60, 0x0, 1" ;;
    *closed) hyprctl keyword monitor "eDP-1, disable";;
    *)       echo "Could not get lid state" >&2 ; exit 1 ;;
esac
