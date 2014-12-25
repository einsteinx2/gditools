#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
    gdifix, like isofix.exe but much more simple.

    This is an example of a simple program that uses gditools.py as a 
    base library to handle gdi files in a meaningful manner.

    FamilyGuy 2014


    gdifix.py is released under the GNU General Public License 
    (version 3), a copy of which (GNU_GPL_v3.txt) is provided in the 
    license folder.
"""

import os, sys
sys.path.append('..')
sys.path.append('.')
from gditools import GDIfile, _copy_buffered


def gdifix(ifile, ofile='{dirname}/fixed.iso'):
    gdifile = GDIfile(ifile, verbose = True)._gdifile
    gdifile.seek(0,0)
    ofile = ofile.format(dirname = os.path.dirname(ifile))
    print('Reading: {} \nWriting: {}'.format(ifile,ofile))
    with open(ofile,'wb') as of:
        _copy_buffered(gdifile,of)

def main(argv):
    if len(argv) > 1 and os.path.isfile(argv[1]):
        gdifix(*argv[1:])
    else:
        print('gdifix, converts a gdi dump into a valid iso file\n')
        print('Usage: gdifix.py disc.gdi [fixed.iso]')
        print('\nFamilyGuy 2014')

if __name__ == '__main__':
    main(sys.argv)
