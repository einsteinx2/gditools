#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
    Program that outputs the volume label of the iso9660 filesystem of
    a GDI dump. Designed to be used in a GUI SiZiOUS is writing.
    
    FamilyGuy 2014


    GuiHelper is released under the GNU General Public License 
    (version 3), a copy of which (GNU_GPL_v3.txt) is provided in the 
    license folder.
"""

import os, sys
sys.path.append('..')
sys.path.append('.')
from gditools import GDIfile


if __name__ == '__main__':
    with GDIfile(sys.argv[1]) as gdifile:
        tmp = gdifile._sorted_records()[0]['name']
        if tmp[0] == '/':
            tmp = tmp[1:]
        print(tmp)
        print(gdifile.get_pvd()['volume_identifier'])
