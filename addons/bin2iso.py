#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
    bin2iso, blindly read a file as a 2352 bytes/sector cd-rom image
    and outputs the corresponding 2048 bytes/sector image file.

    This is an example of a simple program that uses gditools.py as a 
    base library to handle gdi files in a meaningful manner.

    FamilyGuy 2014


    bin2iso.py is released under the GNU General Public License 
    (version 3), a copy of which (GNU_GPL_v3.txt) is provided in the 
    license folder.
"""

import os, sys
sys.path.append('..')
sys.path.append('.')
from gditools import CdImage, _copy_buffered


def bin2iso(ifile, ofile='{dirname}/{basename}.iso', length = None):
    binfile = CdImage(ifile, mode = 2352)
    ofile = ofile.format(dirname = os.path.dirname(ifile),
                         basename = os.path.splitext(os.path.basename(ifile))[0])
    print('Reading: {} \nWriting: {}'.format(ifile, ofile))
    with open(ofile, 'wb') as of:
        _copy_buffered(binfile, of, length=length)

def main(argv):
    if len(argv) > 1 and os.path.isfile(argv[1]):
        bin2iso(*argv[1:])
    else:
        print('bin2iso, converts a bin file into an iso, BLINDLY.\n')
        print('Usage: bin2iso.py file.bin [file.iso]')
        print('\nFamilyGuy 2014')

if __name__ == '__main__':
    main(sys.argv)
