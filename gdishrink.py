import sys
from gditools import gdishrink

def main(argv):
    inputfile = argv[1]
    outputpath = argv[2] if len(argv) > 2 else None
    gdishrink(inputfile, outputpath, True, True)
        
def _printUsage(pname='gditools.py'):
    print('Usage: {} input_gdi output_path\n'.format(pname))
    print('If no output_path is supplied, the image is shrunk in place')
        
if __name__ == '__main__':
    if len(sys.argv) > 1:
        main(sys.argv)
    else:
        _printUsage(sys.argv[0])