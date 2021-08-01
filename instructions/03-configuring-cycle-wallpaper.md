The Wallpaper Slideshow Script
===

## Prerequisites

These scripts require:
- `python3`: standard on most systems now, but you could also install via, e.g., [anaconda](https://www.anaconda.com/products/individual).
- `nitrogen`: a simple wallpaper setting program

### The scripts

The wallpaper cycling machinery consists of two scripts:

- `randomwallpaper.py` is a little python script which selects a
  wallpaper at random from a directory specified by the user without
  replacement. When all wallpapers have been selected, it starts over
  in a new random order.

- `cycle_wallpaper.sh` is a simple bash script that, while running,
  calls `random_wallpaper.py` every specified time period. For
  example, calling `cycle_wallpaper.sh 1h` will reset the wallpaper
  every hour.

### Configuring `randomwallpaper.py`

First copy `random_wallpaper.py` into the directory you want it. For
example:
```bash
mkdir -p ~/myscripts
cp scripts/randomwallpaper.py ~/myscripts
```

At the top of the script there are a few settings you may wish to change:
```python
# Inportant GLobal Values
#--------------------------------------------------------------------------
USERNAME = getpass.getuser()
# This is the directory where you store your wallpapers. Obviously choose a
# a path that works for your system.
WALLPAPER_DIRECTORY = '/home/{}/Pictures/wallpapers/'.format(USERNAME)

# This is the set command that we pass to the os to set the wallpaper
SETCOMMAND = '/usr/bin/nitrogen --set-zoom-fill'

# This is the name of file that contains used wallpaper names
CONFIGURATION_FILE = '/home/{}/.usedwallpapers.dat'.format(USERNAME)
#--------------------------------------------------------------------------
```

- `WALLPAPER_DIRECTORY` is the directory containing the images you'd
like to use as wallpapers.

- `SETCOMMAND` is the program used to set the wallpapers. If you use
  `nitrogen` as I suggest, leave this alone. However, other programs
  work too.

- `CONFIGURATION_FILE` is the file containing which wallpapers have
  already been selected. You can change this path if you want to, but
  it should work as is.


### Configuring `cycle_wallpaper.sh`

Likewise for `cycle_wallpaper.sh`, copy it to where you want it:
```bash
mkdir -p ~/myscripts
cp scripts/cycle_wallpaper.sh ~/myscripts
```
and open the file.

The main thing to change in the `cycle_wallpaper.sh` is
```bash
# The script we call to rotate wallpapers
ROTATOR=/home/$(whoami)/myscripts/randomwallpaper.py
```
set this to the path where `randomwallpaper.py` lives.

### Running

Just run, e.g.,
```bash
cycle_wallpaper.sh 1h &
```
from somewhere. In [07-configuring-systemd](07-configuring-systemd.md)
I will show how to register this program via `systemd`.
