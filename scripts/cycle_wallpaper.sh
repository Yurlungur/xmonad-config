#!/usr/bin/env sh

# cycle_wallpaper.sh
# Author: Yurlungur 
# This script will cycle your wallpaper every time T
# amount of time passes. T is given in command line and the syntax is:
# 2 = 2 seconds, 1m = 1 minute, 2h = 2 hours
# Call the script as a daemon. For instance:
# cycle_wallpaper.sh 1h &

# The script we call to rotate wallpapers
ROTATOR=/home/$(whoami)/myscripts/randomwallpaper.py

# Cycle the wallpaper once before waiting for the next rotation
python3 $ROTATOR

# The never ending while loop which cycles the wallpaper
while true; do
    sleep $@
    python3 $ROTATOR
done
