#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
from iso9660 import ISO9660 as _ISO9660_orig
try:
    from cStringIO import StringIO
except ImportError:
    from StringIO import StringIO


# TODO TODO TODO
#
#   - Create a class to read a 2352/2048 images as 2048 ones; make OffsetedFile inherit from it.
#   - Create an 'AppendedFile' class, OffsettedFile and WormHoleFile seem to work well now.
#      (That'd be used to append an OffsettedFile to a WormHoleFile to dump 5+ tracks GDI)
#
# TODO TODO TODO


class ISO9660(_ISO9660_orig):
    """
    Modification to iso9660.py to easily build sorttxt files and dump GDI files. (eventually) 
    
    FamilyGuy 2014
    
    Original iso9660.py by Barney Gale : github.com/barneygale/iso9660
    Some fixes were applied to the original code via method overriding.
    See the comments in the code for more details.
    """
    
    ### Overriding Functions of original class in this section

    def __init__(self, *args, **kwargs):
        # We obviously override the __init__ to add support for WormHoleFile
        if kwargs.has_key('offset'):
            self._offset = kwargs.pop('offset')
        else:
            self._offset = 0

        if kwargs.has_key('wormhole'):
            self._wormhole = kwargs.pop('wormhole')
        else:
            self._wormhole = [0, 0, 0]

        _ISO9660_orig.__init__(self, *args, **kwargs)

    
    ### Overriding this function allows to parse iso files as faked by WormHoleFile
    
    def _get_sector_file(self, sector, length):
        with WormHoleFile(self._url, 'rb', offset =  self._offset, wormhole = self._wormhole) as f:
            f.seek(sector*2048)
            self._buff = StringIO(f.read(length))


    ### Overriding this function in iso9660 because it did not worked for 
    ### directory record longer than a sector if the elements of the record
    ### were padded not to overlap two sectors.

    def _unpack_dir_children(self, d):
        #Assuming d is a directory record, this generator yields its children
        read = 0
        self._get_sector(d['ex_loc'], d['ex_len'])
        while read < d['ex_len']: #Iterate over files in the directory
            data, e = self._unpack_record()
            read += data

            if data == 1: #end of directory listing or padding until next sector
                self._unpack_raw( 2048 - (self._buff.tell() % 2048) )
                read = self._buff.tell()
            elif e['name'] not in '\x00\x01':
                yield e


    ### Overriding this function as _dir_record_by_table was not fully implemented.
    ### It only retured files in its first sector.

    def get_file(self, path):
        path = path.upper().strip('/').split('/')
        path, filename = path[:-1], path[-1]

        if len(path)==0:
            parent_dir = self._root
        else:
            parent_dir = self._dir_record_by_root(path)

        f = self._search_dir_children(parent_dir, filename)

        self._get_sector(f['ex_loc'], f['ex_len'])
        return self._unpack_raw(f['ex_len'])



    ### NEW FUNCTIONS FOLLOW ###

    def get_record(self, path):
        path = path.upper().strip('/').split('/')
        path, filename = path[:-1], path[-1]

        if len(path)==0:
            parent_dir = self._root
        else:
            parent_dir = self._dir_record_by_root(path)

        f = self._search_dir_children(parent_dir, filename)
        return f

    
    # Not that useful anymore
    #def get_info(self, path, info):
    #    return self.get_record(path)[info]


    def gen_records(self, get_files = True):
        gen = self._tree_nodes_records(self._root)
        for i in gen:
            if get_files:
                yield i
            elif i['flags'] == 2:
                yield i 


    def _tree_nodes_records(self, node):
        spacer = lambda s: dict({j:s[j] for j in [i for i in s if i != 'name']}.items(), \
                                    name = "%s/%s" % (node['name'].lstrip('\x00\x01'), s['name']))
        for c in list(self._unpack_dir_children(node)):
            yield spacer(c)
            if c['flags'] & 2:
                for d in self._tree_nodes_records(c):
                    yield spacer(d)


    def get_bootsector(self, lba = 45000):
        self._get_sector(lba, 16*2048)
        return self._unpack_raw(16*2048)


    def get_file_by_record(self, filerec):
        self._get_sector(filerec['ex_loc'], filerec['ex_len'])
        return self._unpack_raw(filerec['ex_len'])


    def get_sorttxt(self, crit='ex_loc', prefix='data', add_dummy=True, dummyname='0.0'):
        # TODO: Idea -> Add an option for a list of important files to be at outer part of disc, used for 1st_read.bin 
        """
        *crit* (criterion) can be any file record entry, examples: 

        'ex_loc' or 'EX_LOC'    ->    Sorted by LBA value.
        'name' or 'NAME'        ->    Sorted by file name.
        'ex_len' or 'EX_LEN'    ->    Sorted by file size.
        
        If the first letter of *criterion* is uppercase, order is reversed.

        Note: First file in sorttxt represents the last one on disc.


        e.g.

        - To get a sorttxt that'd yield the same file order as the source iso:
            self.get_sorted(crit='ex_loc')

        - To get a sorttxt with BIGGEST files at the outer part of disc:
            self.get_sorted(crit='ex_len')

        - To get a sorttxt with SMALLEST files at the outer part of disc:
            self.get_sorted(criterion='EX_LEN')
        """
        #   *** THAT WAS SO FREAKING SLOW, IT TOOK LIKE, ... MINUTES! ***
        #          (Keeping it as an example of a false good idea)
        #
        #        nodes = [i for i in self.tree()]
        #        paths = [i for i in self.tree(get_files = False)]
        #        for i in paths:
        #            nodes.pop(nodes.index(i))
        #        files_info = [[i,self.get_file_loc(i),self.get_file_len(i)] for i in nodes]
        file_records = [i for i in self.gen_records()]
        for i in [i for i in self.gen_records(get_files = False)]:
            file_records.pop(file_records.index(i))  # Strips directories
        reverse = crit[0].islower()
        crit = crit.lower()
        ordered_records = sorted(file_records, key=lambda k: k[crit], reverse = reverse)

        # Building the sorttxt file string
        sorttxt=''
        newline = '{prefix}{filename} {importance}\r\n'
        for i,f in enumerate(ordered_records):
            sorttxt = sorttxt + newline.format(prefix=prefix, filename=f['name'], importance = i+1)

        if add_dummy:
            if not dummyname[0] == '/': 
                dummyname = '/' + dummyname
            sorttxt = sorttxt + newline.format(prefix=prefix, filename=dummyname,\
                    importance=len(file_records)+1)

        return sorttxt


    def dump_sorttxt(self, filename='sorttxt.txt', verbose = False, **kwargs):
        with open(filename, 'wb') as f:
            if verbose: print('Dumping sorttxt to {filename}'.format(filename = filename))
            f.write(self.get_sorttxt(**kwargs))

    def dump_bootsector(self, filename='ip.bin', verbose = False):
        with open(filename, 'wb') as f:
            if verbose: print('Dumping bootsector to {filename}'.format(filename = filename))
            f.write(self.get_bootsector())

    def dump_file_by_record(self, rec, target = '.', verbose = False, **kwargs):
        if not target[-1] == '/': target += '/'
        if kwargs.has_key('filename'):
            filename = target + kwargs['filename'].strip('/') # User provided filename overrides records's subfolders & name
        else: filename = target + rec['name'].strip('/')

        if rec['flags'] == 2:
            filename += '/' # This way os.path.isdirname reports the good value for records representing directories
        
        path = os.path.dirname(filename)
        if not os.path.exists(path):
            os.makedirs(path)   # Creates the target directories required by the record; this will create empty directories too
            if verbose: UpdateLine('Created directory: {dirname}'.format(dirname = path))

        if rec['flags'] != 2:   # Unless the record represents a directory we dump the file to disc.
            with open(filename, 'wb') as f:
                if verbose: UpdateLine('Dumping file {source} to {target}    ({loc}, {_len})'.format(source = rec['name'],\
                                    target = filename, loc = rec['ex_loc'], _len = rec['ex_len'] ))
                f.write(self.get_file_by_record(rec))



    def dump_file(self, name, **kwargs):
        self.dump_file_by_record(self.get_record(name), **kwargs)


    def dump_all_files(self, target, **kwargs): # kwarg *target* is a required argument to avoid filling dev folder with files
        # Listing all files
        file_records = [i for i in self.gen_records()]
        # Sorting according to LBA to avoid too much skipping on HDDs, hopefully ...
        ordered_records = sorted(file_records, key=lambda k: k['ex_loc'], reverse = False)
        for i in ordered_records:
            self.dump_file_by_record(i, target = target, **kwargs) # DUMPING FILE

        if kwargs.has_key('verbose'):
            if kwargs['verbose']: UpdateLine('Dumping all files done')




class OffsetedFile(file):
    """
    Like a file, but offsetted! Padding is made of 0x00.

    READ ONLY: trying to open a file in write mode will raise a NotImplementedError
    """
    def __init__(self, filename, *args, **kwargs):

        if kwargs.has_key('offset'):
            self.offset = kwargs.pop('offset')
        else:
            self.offset = 0

        if (len(args) > 0) and (args[0] not in ['r','rb']):
            raise NotImplementedError('Only read mode is implemented.')

        file.__init__(self, filename, 'rb')
        
        file.seek(self,0,2)
        self.length = file.tell(self)
        file.seek(self,0,0)

        self.seek(0)


    def seek(self, a, b=0):
        if b == 0:
            self.pointer = a
        if b == 1:
            self.pointer += a
        if b == 2:
            self.pointer = self.length + self.offset - a

        if self.pointer > self.offset:
            file.seek(self, self.pointer - self.offset)
        else:
            file.seek(self, 0)


    def read(self, length=None):
        if length == None:
            length = self.offset + self.length - self.pointer
        tmp = self.pointer
        FutureOffset = self.pointer + length
        if tmp >= self.offset:
            #print 'AFTER OFFSET'
            self.seek(tmp)
            data = file.read(self, length)
        elif FutureOffset < self.offset:
            #print 'BEFORE OFFSET'
            data = '\x00'*length
        else:
            #print 'CROSSING OFFSET'
            preData = '\x00'*(self.offset - tmp)
            self.seek(self.offset)
            postData = file.read(self, FutureOffset - self.offset)
            data = preData + postData
        self.seek(FutureOffset)
        return data


    def tell(self):
        return self.pointer




class WormHoleFile(OffsetedFile):
    """
    Redirects an offset-range to another offset in a file. Because everbody likes wormholes.
    """
    def __init__(self, *args, **kwargs):

        # *wormhole* should be [target_offset, source_offset, wormlen]
        # target_offset + wormlen < source_offset
        
        if kwargs.has_key('wormhole'):
            self.target, self.source, self.wormlen = kwargs.pop('wormhole')
        else:
            self.target, self.source, self.wormlen = [0,0,0]

        OffsetedFile.__init__(self, *args, **kwargs)


    def read(self, length=None):

        if length == None:
            length = self.offset + self.length - self.pointer
        tmp = self.pointer
        FutureOffset = self.pointer + length

        # If we start after the wormhole or if we don't reach it, everything is fine
        if (tmp >= self.target + self.wormlen) or (FutureOffset < self.target):
            # print 'OUT OF WORMHOLE'
            data = OffsetedFile.read(self, length)

        # If we start inside the wormhole, it's trickier        
        elif tmp >= self.target:
            # print 'START INSIDE'
            self.seek(tmp - self.target + self.source)  # Through the wormhole to the source

            # If we don't exit the wormhole, it's somewhat simple
            if FutureOffset < self.target + self.wormlen: 
                # print 'DON\'T EXIT IT'
                data = OffsetedFile.read(self, length)    # Read in the source

            # If we exit the wormhole midway, it's even trickier
            else:   
                # print 'EXIT IT'
                inWorm_len = self.target + self.wormlen - tmp
                outWorm_len = FutureOffset - self.target - self.wormlen
                inWorm = OffsetedFile.read(self, inWorm_len)
                self.seek(self.target + self.wormlen)
                outWorm = OffsetedFile.read(self, outWorm_len)
                data = inWorm + outWorm

        # If we start before the wormhole then hop inside, it's also kinda trickier
        elif FutureOffset < self.target + self.wormlen: 
            # print 'START BEFORE, ENTER IT'
            preWorm_len = self.target - tmp
            inWorm_len = FutureOffset - self.target
            preWorm = OffsetedFile.read(self, preWorm_len)
            self.seek(self.source)
            inWorm = OffsetedFile.read(self, inWorm_len)
            data = preWorm + inWorm

        # Now if we start before the wormhole and jump over it, it's the trickiest
        elif FutureOffset > self.target + self.wormlen:
            # print 'START BEFORE, END AFTER'
            preWorm_len = self.target - tmp
            inWorm_len = self.wormlen
            postWorm_len = FutureOffset - self.target - self.wormlen

            preWorm = OffsetedFile.read(preWorm_len)
            self.seek(self.source)
            inWorm = OffsetedFile.read(inWorm_len)
            self.seek(self.target + inWorm_len)
            postWorm = OffsetedFile.read(postWorm_len)

            data = preWorm + inWorm + postWorm
        

        # Pretend we're still where we should, in case we went where we shouldn't!
        self.seek(FutureOffset)     
        return data

def UpdateLine(text):
    """
    Allows to print suiccessive messages over the last line. Problem occurs if last line is over 100 char long.
    """
    import sys
    if text[-1] == '\r':
        text = text[:-1]
    text += ' '*(100-len(text))+'\r'
    sys.stdout.write(text)
    sys.stdout.flush()



def _copy_buffered(f1, f2, bufsize = 1*1024*1024, closeOut = True):
    f1.seek(0,2)
    length = f1.tell()
    f1.seek(0,0)
    f2.seek(0,0)

    while length:
        chunk = min(length, bufsize)
        length = length - chunk
        data = f1.read(chunk)
        f2.write(data)

    if closeOut:
        f2.close()

        

def bin2iso(src, dest=None, bufsize=1024*2048, outmode='wb', length_override=False):
    """
    A VERY stupid bin2iso implementation. Not dumb-proof AT ALL.
    
    It basically reads (blindly) a file in 2352 chunks and spits 2048 ones
    ditching the first 16 and last 288 bytes of each input chunk. It should
    be enough to provide a parseable iso and extract files/files-infos.

    bufsize is floored to SECTORS, as it'd be stupid/complex to do otherwise.

    bufsize of 1024 sectors was optimized on a SSD. Might be tuned for HDD.
    Options outmode and length_override are meant to be used by isofix.
    """
    # TODO: Cuesheet support to skip audio tracks
    bufsize=bufsize/2048

    if dest == None:
        if not src.find('.bin') == -1:
            dest = src.replace('.bin','.iso')
        elif not src.find('.BIN') == -1:
            dest = src.replace('.BIN','.ISO')
        else:
            dest = src+'.iso'
    elif dest.find('.iso') == -1:
        dest=dest+'.iso'

    with open(src,'rb') as BIN, open(dest,outmode) as ISO:
        BIN.seek(0,2) 
        if length_override:
            length = length_override
        else:
            length = BIN.tell()/2352 # Get lenght of bin file, in sectors.
        BIN.seek(16,0)      # Seeking here skips the 1st 16 bytes of sector 0
        while length:
            chunk = min(bufsize,length)
            length = length-chunk
            data=''
            while chunk:
                data+=BIN.read(2048)
                BIN.seek(304,1) # Skips 288 last of current and 16 first of next sector.
                chunk=chunk-1
            ISO.write(data)


def isofix(src, dest=None, LBA=45000, bufsize=1024*2048, source='iso'):
    """
    Bin files are converted to iso on the fly in the copy process.
    """
    # TODO: AutoDetect .bin via name. Autodetect .bin LBA via header
    if not source.lower() in ['iso','bin']:
        return -1

    # This next line is ugly enough not to deserve explanation; it works though.
    hasExtension=bool(src.find('.iso') + src.find('.ISO') + src.find('.bin') + src.find('.BIN') + 4)

    if dest == None:
        if (hasExtension):
            dest = src[:-4]+'_fixed.iso'
        else:
            dest = src+'_fixed.iso'


    # 1 - Writes bootsector (0-15), PVD (16) and SVD (17) to beginning of fixed iso
    if source == 'iso':
        with open(src,'rb') as old, open(dest,'wb') as fix:
            fix.write(old.read(18*2048))
    if source == 'bin':
        bin2iso(src, dest, bufsize=bufsize, length_override=18)
    
    # 2 - Appends 0x00 to the file until LBA is reached
    with open(src,'rb') as old, open(dest,'ab') as fix:
        # Padding with zeros until LBA to fixed to
        length = (LBA-18)*2048
        while length:
            chunk = min(length,bufsize)
            length=length-chunk
            fix.write('\x00'*chunk)

    # 3 - Appends the source iso to the dest iso
    if source == 'iso':
        with open(src,'rb') as old, open(dest,'ab') as fix:
            # Get length of source iso
            old.seek(0,2)
            length=old.tell()
            old.seek(0,0) # Return to beginning ready to be all read

            # Append source iso to end of prepared file
            while length:
                chunk = min(length,bufsize)
                length=length-chunk
                fix.write(old.read(chunk))
    if source == 'bin':
        bin2iso(src, dest, bufsize=bufsize, outmode='ab')







