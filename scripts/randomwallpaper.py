#!/usr/bin/env python3

"""
author: Yurlungur


This program will randomly cycle through the wallpapers in a selected folder. 
"""


# Import modules
#--------------------------------------------------------------------------
import os                   # For operating system/file system commands
import getpass
from subprocess import call # For calling subprocesses
from random import sample   # To select a random wallpaper
#--------------------------------------------------------------------------


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


# Function definitions
#--------------------------------------------------------------------------
# This function reads names of wallpapers out of the configuration file.
def read_file(filename):
    """ 
    This function reads the names of wallpapers out of the configuration
    file, filename. Assumed data:
    """
    with open(filename, 'r') as f:
        file_lines = f.read().split('\n')
    if file_lines.count('') != 0:
        file_lines.remove('')
    return file_lines
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# This function compares two lists, and takes their set-theoretical difference:
# dependent - independent 
def set_difference(dependent,independent):
    """
    This function compares two lists, and takes their set-theoretical 
    difference: 
    dependent - independent
    arguments: dependent, independent
    """
    return list(set(dependent)-set(independent))
#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
# This function writes a file by outputting, line by line, the
# elements of a list.
def write_file(filename, data):
    """
    Writes to a file by outputting, line by line, the elements of a list.
    arguments: filename, list of strings.
    """
    with open(filename,'w') as f:
        print(*data,sep='\n',file=f)
# --------------------------------------------------------------------------


#--------------------------------------------------------------------------
# Main
def set_wallpaper():
    """Sets a random wallpaper. Takes no arguments."""
    # Generate a list of wallpapers that have already been used. If a
    # configuration file containing this information exists, read from
    # it. Otherwise use an empty list.
    if os.access(CONFIGURATION_FILE,os.R_OK):
        used_wallpapers = read_file(CONFIGURATION_FILE)
    else:
        used_wallpapers = []
    
    # Generates a list of available wallpapers in the wallpaper directory
    available_wallpapers = os.listdir(WALLPAPER_DIRECTORY)
    # Removes the './' symbolic link if it exits
    if available_wallpapers.count('.directory') != 0:
        available_wallpapers.remove('.directory')

    # Generates a list of available walpapers that have not already
    # been used. Takes the set-theoretical difference:
    novel_wallpapers = set_difference(available_wallpapers,
                                      used_wallpapers) 

    # Now we do different things depending on whether there are any
    # novel wallpapers at all. If there are, we choose one, use it,
    # and append it to our list of used wallpapers. If there are no
    # novel wallpapers at all, we choose a wallpaper from available
    # wallpapers in the wallpaper directory, and set it as the only
    # element of the used wallpapers list.
    if len(novel_wallpapers) != 0:
        chosen_wallpaper = sample(novel_wallpapers,1)[0]
        used_wallpapers.append(chosen_wallpaper)
    else:
        chosen_wallpaper = sample(available_wallpapers,1)[0]
        used_wallpapers = [chosen_wallpaper]

    # Of course, we have to use our chosen wallpaper. We need to send
    # a command to our wallpaper editor through the shell. We can also
    # use error management here to tell the terminal something went
    # wrong if we couldn't run nitrogen.
    didntwork = call(SETCOMMAND.split(' ')+
                      [WALLPAPER_DIRECTORY+chosen_wallpaper])
    if didntwork:
        print("ERROR: I couldn't set your wallpaper.")
        return 1
    
    # Assuming nothing went wrong when setting our wallpaper, we just
    # need to output which wallpapers we've used recently.
    write_file(CONFIGURATION_FILE, used_wallpapers)
    return 0
# --------------------------------------------------------------------------


# Executes the main function
if __name__ == "__main__":
    set_wallpaper()
