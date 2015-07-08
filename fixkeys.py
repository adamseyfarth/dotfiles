#!/usr/bin/env python3

"""fixkeys.py: Change the keyboard layout to my preferences"""

import argparse
import subprocess
from distutils.util import strtobool

__author__ = "Adam Seyfarth"
__copyright__ = "Copyright 2015, Adam Seyfarth"
__email__ = "adam@seyfarth.name"
__license__ = "Apache License"


def main():
    """Delegate whole program to other functions
    """
    args = command_line()
    if args.environment == "console":
        console_layout(args.layout)
    elif args.environment == "x":
        x_layout(args.layout)


def command_line():
    """Use argparse to parse and return command-line arguments
    """
    kbmap_locs = ["/usr/share/keymaps/i386/",
                  "/usr/share/kbd/keymaps/i386/",
                  "/usr/share/X11/xkb/symols/"]
    parser = argparse.ArgumentParser()
    parser.add_argument("environment", choices=["console", "x"],
                        default="x")
    parser.add_argument("layout", type=str, nargs="?",
                        help=("For x, look for choices in one of: {}.  "
                              "For console, use a file like from dumpkeys"
                              .format("\n".join(kbmap_locs))))
    return parser.parse_args()


def console_layout(layout):
    """Change the layout for a virtual console
    """
    command = ["loadkeys"]
    if layout is None:
        command.append("-d")
    else:
        command.append(layout)
    confirm(command)


def x_layout(layout):
    """Change the layout for X windows
    """
    command = ["setxkbmap"]
    if layout is not None:
        command.append(layout)
    command += ["-option", "ctrl:nocaps"]
    confirm(command)


def confirm(command):
    print('Command will be "{}"\nRun? (yes/no)'
          .format(" ".join(command)),
          end=" ")
    if strtobool(input()):
        subprocess.call(command)



if __name__ == "__main__":
    main()
