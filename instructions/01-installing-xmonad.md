Installing Xmonad
===

Much of this is taken from the [xmonad
tutorial](https://github.com/xmonad/xmonad/blob/master/TUTORIAL.md). You
may also wish to look at the [guided
tour](https://xmonad.org/tour.html) of xmonad.

## The basics of the basics

To install `xmonad`, run
```bash
sudo apt install xmonad
```

I also like to install all extras via
```bash
sudo apt install libghc-xmonad-*
```

which gives you all modules and extensions for `xmonad`. When you next
log in to your computer, you will be able to select between different
window managers, and choose `xmonad` as an option.

## The essetials

Unfortunately, this will not entirely produce a workable `xmonad`
environment. `xmonad` does not by default come with a way to launch
programs or a way to display statuses, such as what time it is or what
programs are open. It also doesn't come with a compositor, that lets
you have transparent windows. There are thus a few usefu programs to
install to get that all to work:

First, install `xmobar`, which is the status bar for `xmonad` via:
```bash
sudo apt install xmobar
```

You will also want a system tray for applets like your network
status. We will use `trayer`
```bash
sudo apt install trayer
```

and you probably want a network status applet, so install `nm-tray` via
```bash
sudo apt install nm-tray
```

Importantly, you also need a way to launch programs. We will use the
`dmenu` program found in
[suckless-tools](https://tools.suckless.org/):
```bash
sudo apt install suckless-tools
```

For transparencies, we use the `compton` compositor:
```bash
sudo apt install compton
```

For a screensaver, there are a few options. In the past, I used
[xscreensaver](https://www.jwz.org/xscreensaver/), which can be installed
```bash
sudo apt install xscreensaver
```

But I experienced a bug where the screensaver crashed and left my
computer unlocked. I therefore now prefer `slock` and `xautolock`,
which are (conveniently) [provided by
suckless-tools](https://tools.suckless.org/slock/).

## Extras

If you want to take screen shots in `xmonad`, I suggest `scrot` via
```bash
sudo apt install scrot
```

If you want the transparent terminal wallpaper, you will need `terminator`
```bash
sudo apt install terminator
```
