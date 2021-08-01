Configuring Bash
===

If you want my bash configuration, copy it into your bashrc
```bash
cp dotfiles/home.bashrc ~/.bashrc
```
A few highlights:

### lesspipe:
```bash
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
```

### unlimited bash history
```bash
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000
```

### A hello message (requires cowsay)
```bash
#Makes a dragon tell a cow the time at the top of a new shell
startup_message=yes
if [ "$startup_message" = yes ]; then
    # Cowsay
    cowsay -f dragon-and-cow "Hello, the time is $(date "+%k:%M, on %A, %d of %B, %Y. ")"
    # A helpful startup message
    echo "Aliases: poweroff=poff, reboot=rebo, suspend=susp. Have fun!"
fi
```

### Segregate ssh and bash aliases into separate files
```bash
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.my.sshaliases ]; then
. ~/.my.sshaliases
fi
```

### Use `ssh-agent` to manage ssh keys
```bash
# Adds ssh agent
USE_SSH_AGENT=yes
SSHAGENT=/usr/bin/ssh-agent
SSHADD=/usr/bin/ssh-add
SSHAGENTARGS="-s"
SSH_ENV="$HOME/.ssh/environment"

function start_agent {
    echo "Initialising new SSH agent..."
    $SSHAGENT | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    eval $SSHADD
}
# Source the SSH settings, if applicable
if [ "$USE_SSH_AGENT" = yes ]; then
    if [ -f "${SSH_ENV}" ]; then
	. "${SSH_ENV}" > /dev/null
	ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
	    start_agent;
	}
    else
	start_agent;
    fi
fi
```

### Set the default text editor to emacs in client-server mode
```bash
# Default applications
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"
export BROWSER="firefox"
```
