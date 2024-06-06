#!/bin/bash
muted=$(pactl get-sink-mute @DEFAULT_SINK@ | awk '{print $2}')

if [ "$muted" = "yes" ]; then
    notify-send 'Audio Muted'
else
    notify-send 'Audio Unmuted'
fi
