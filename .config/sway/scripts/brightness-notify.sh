#!/bin/bash
notify-send Brightness: `light | awk '{print int(($1))}'`%
