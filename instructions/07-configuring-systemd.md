Configuring Systemd
===

[Systemd](https://en.wikipedia.org/wiki/Systemd) is the (rather
controversial) replacement for initscripts and upstart. If you are
using it, you can have it start custom user services when your desktop
starts up, including things like the emacs server discussed in
[06-configuring-emacs](06-configuring-emacs.md).

As always, the [Arch Wiki](https://wiki.archlinux.org/title/systemd)
has a great description of `systemd` and how to use it.

Also credit to the [dex](https://github.com/jceb/dex) project
for pointing me to this machinery and describing its use
in its `README`.

## The configuration files

Your user-specific `systemd` configuration lives in your home
directory in the hidden `.config` directory. If it doesn't exist,
create it with:
```bash
mkdir -p ~/.config/systemd/user
```

You will want to first populate that directory with a file that tells
`systemd` to group our custom services together. This file looks like this:

```
[Unit]
Description=Current graphical user session
Documentation=man:systemd.special(7)
RefuseManualStart=no
StopWhenUnneeded=no
```

and it is provided in the `dotfiles` directory. you can get it with:

```bash
cp dotfiles/home.config.systemd.user.autostart.target ~/.config/systemd/user/autostart.target
```

Next create a `service` file for each of the programs you want to
start automatically. For example, you might create a file that looks
like this:

```
[Unit]
Description=Cycle the wallpaper regularly

[Service]
ExecStart=/path/to/cycle_wallpaper.sh 1h
```

As you can tell, the basic structure here is that each file is of the form `~/.config/systemd/user/<service name>.service`, with the following content:

```
[Unit]
Description=<service description>

[Service]
ExecStart=<path to the executable> [<parameters>]
```
Don't let the process fork. (I.e., don't use `&` or use parameters that cause that to happen.) 
Otherwise `systemd` can't check when the program dies and restart it if needed.

to start the wallpaper machinery described in
[03-configuring-cycle-wallpaper](03-configuring-cycle-wallpaper.md),
where of course the path needs to be set.

This file, and my other systemd service files are in `dotfiles`. You can get this one via, e.g.,
```bash
cp dotfiles/home.config.systemd.user.cycle_wallpaper.service ~/.config/systemd/user/cycle_wallpaper.service
```

## Telling Systemd To Run Them

First, register each service with `systemd`. For example:
```bash
systemctl --user add-wants autostart.target cycle_wallpaper.service
```
and in general:
```bash
systemctl --user add-wants autostart.target <service name>.service
```
You can unregister via:
```bash
systemctl --user disable <service name>.service
```
and you can list active services with:
```bash
systemctl --user list-units
```
`systemd` will automatically start all services when you log in. But you must tell it to start things manually once. To do so:
```bash
systemctl --user start autostart.target
```

### General Systemd Management

The following commands are generic to `systemd` both systemwide and in
your user configuration. To manage your user configuration, add the
`--user` flag to each of these commands. For systemwide
configurations, you may need `sudo`.

This restarts all services after you've made a change to a config file:
```bash
systemctl daemon-reload
```

This starts a service
```bash
systemctl start <service name>.service
```

This stops a service
```bash
systemctl status <service name>.service
```

This checks the status a service
```bash
systemctl status <service name>.service
```
