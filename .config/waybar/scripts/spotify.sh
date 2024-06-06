#!/bin/bash

# Get the status of the player
status=$(playerctl --player=spotify status 2> /dev/null)
icon=""
# Check if playerctl command was successful
if [ $? -eq 0 ]; then
  # If something is playing
  if [ "$status" = "Playing" ] || [ "$status" = "Paused" ]; then
    artist=$(playerctl --player=spotify metadata artist)
    title=$(playerctl --player=spotify metadata title)
    if [ "$status" = "Paused" ]; then
        state=" [PAUSED]"
    else
        state=""
    fi
    echo "$icon  $artist - $title$state"
  else
    echo ""
  fi
else
  echo ""
fi

