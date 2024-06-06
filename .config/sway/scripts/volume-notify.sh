#!/bin/bash
notify-send "Volume: `pactl get-sink-volume @DEFAULT_SINK@ | awk '{ print $5}'`"
