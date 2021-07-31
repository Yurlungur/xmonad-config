#!/bin/bash

# Author: Matthew Lawson
# Time-stamp: <2014-01-20 17:44:30 (jonah)>

#This script toggles on and off an external monitor
#----------------------------------------------------------------------

# Change IN and out to the inputs given if you type "xrandr" with no
# arguments into the shell.
IN="LVDS1"
EXT="VGA1"

if (xrandr | grep "$EXT" | grep "+"); then
    xrandr --output $EXT --off --output $IN --auto
else
if (xrandr | grep "$EXT" | grep " connected"); then
    xrandr --output $IN --off --output $EXT --auto
fi
fi
pkill trayer
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 14 --transparent true --tint 0x000000 --alpha 0 --height 16 &
/home/jonah/myscripts/randomwallpaper.py