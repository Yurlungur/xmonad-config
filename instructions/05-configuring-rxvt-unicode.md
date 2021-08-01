Configuring the Terminal Emulator
===

I use `urxvt-unicode` which you can install (on Ubuntu) via
```bash
sudo apt install urxvt-unicode
```

`rxvt-unicode` is very extensible and there's some nice extensions 
in a git repo (not mine) that you can get via:
```bash
git clone https://github.com/muennich/urxvt-perls.git
```
these then need to be copied into a configuration folder to be
used. From the `urxvt-perls`
[readme](https://github.com/muennich/urxvt-perls/blob/master/README.md):

Simply place the scripts you want to install in `/usr/lib/urxvt/perl/`
for system-wide availability or in `~/.urxvt/ext/` for user-only
availability. You can also put them in a folder of your choice, but
then you have to add this line to your `.Xdefaults`/`.Xresources`:
```
URxvt.perl-lib: /your/folder/
```

## My configuration:

You can get my configuration from the `scripts` directory:
```bash
cp scripts/home.Xresources ~/.Xresources
```

Most details in the file are just things like colors (I like dark
backgrounds). Some relevant lines are:
```
Xft.dpi: 120
```
which sets the DPI for the whole system, and
```
URxvt.font: xft:DejaVuSansMono:size=12
URxvt.italicFont: xft:DejaVuSansMono:size=12
```
which set the fonts.
