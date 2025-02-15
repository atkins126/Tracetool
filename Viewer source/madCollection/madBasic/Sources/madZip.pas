// ***************************************************************
//  madZip.pas                version: 0.2.1  �  date: 2013-12-03
//  -------------------------------------------------------------
//  compression stuff
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2013 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2013-12-03 0.2.1 added workaround for Delphi x64 compiler bug
// 2012-04-03 0.2.0 added x64 support
// 2009-11-09 0.1k  added unicode file functions
// 2009-09-30 0.1j  fixed: file routines didn't always report errors properly
// 2009-08-02 0.1i  fixed: (de)compression of >4GB files didn't work properly
// 2009-06-18 0.1h  fixed: (de)compression of >2GB files didn't work properly
// 2009-02-09 0.1g  (1) Delphi 2009 support
//                  (2) "Zip" supports unicode file names now
// 2006-04-24 0.1f  file share mode in "Zip" changed to read + write
// 2005-05-13 0.1e  "Zip" compresses some files into a zip compatible archive
// 2004-06-30 0.1d  (1) new file logic (see 0.1c) called UnmapViewOfFile twice
//                  (2) got rid of Math.pas, which imported SysUtils.pas
// 2004-04-09 0.1c  file routines failed when dealing with big (>= 500 MB) files
// 2003-06-09 0.1b  (1) Uncompress (file) optionally sets file attributes
//                  (2) GetCompressedFileInfo + GetUncompressedFileInfo added
//                  (3) some more internally exception catching
// 2002-09-04 0.1a  exceptions caught internally
// 2001-08-26 0.1   based on Mike Lischke's zlib Delphi translation

// Mike Lischke
// public@lischke-online.de
// www.lischke-online.de
// www.delphi-unicode.net

// Original copyright of the creators:
//
// zlib.H -- interface of the 'zlib' general purpose compression library version 1.1.0, Feb 24th, 1998
//
// Copyright (C) 1995-1998 Jean-loup Gailly and Mark Adler
//
// This software is provided 'as-is', without any express or implied warranty.  In no event will the authors be held
// liable for any damages arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter
// it and redistribute it freely, subject to the following restrictions:
// 1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software.
//    If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is
//    not required.
// 2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
// 3. This notice may not be removed or altered from any Source distribution.
//
// Jean-loup Gailly        Mark Adler
// jloup@gzip.org          madler@alumni.caltech.edu
//
// The data format used by the zlib library is described by RFCs (Request for Comments) 1950 to 1952 in the files
// ftp://deststate.internic.net/rfc/rfc1950.txt (zlib format), rfc1951.txt (Deflate format) and rfc1952.txt (gzip format).
//
// patch 112 from the zlib home page is implicitly applied here
//
// Delphi translation: (C) 2000 by Dipl. Ing. Mike Lischke

unit madZip;

{$I mad.inc}

interface

uses Windows, madTypes;

const CMapSize = 1024 * 1024;

function   Compress (src, dst: pointer; srcLen, dstLen: integer) : integer; overload;
function Uncompress (src, dst: pointer; srcLen, dstLen: integer) : integer; overload;

function   Compress (const data: AnsiString; failIfGrow: boolean = false) : AnsiString; overload;
function Uncompress (const data: AnsiString                             ) : AnsiString; overload;

function   Compress (const srcFile, dstFile: UnicodeString; failIfGrow: boolean = false              ) : boolean; overload;
function Uncompress (const srcFile, dstFile: UnicodeString; lastWriteTime: int64 = 0; attr: dword = 0) : boolean; overload;

function GetCompressedFileInfo   (const   comprFile: UnicodeString; var size: int64; var crc32: dword) : boolean;
function GetUncompressedFileInfo (const uncomprFile: UnicodeString; var size: int64; var crc32: dword) : boolean;
function IsCompressedFileEqual   (const uncomprFile, comprFile: UnicodeString) : boolean;

function UpdateCrc32 (crc32: cardinal; const inBuf; inLen: integer) : cardinal;

function Zip (const zip: UnicodeString; const files: array of UnicodeString; const zipAs: TDAUnicodeString) : boolean;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses madStrings;

//----------------- general library stuff ------------------------------------------------------------------------------

const
  CMemLevel   = 8;
  CWindowBits = 15;

type
  PInflateHuft = ^TInflateHuft;
  TInflateHuft = record
    Exop,           // number of extra bits or operation
    Bits: byte;     // number of bits in this code or subcode
    Base: cardinal; // literal, Length base, or distance base or table offset
  end;

  THuftField = array [0..(MaxInt div SizeOf(TInflateHuft)) - 1] of TInflateHuft;
  PHuftField = ^THuftField;
  PPInflateHuft = ^PInflateHuft;

  TInflateCodesMode = ( // waiting for "I:"=input, "O:"=output, "X:"=nothing
    icmStart,    // X: set up for Len
    icmLen,      // I: get length/literal/eob next
    icmLenNext,  // I: getting length extra (have base)
    icmDistance, // I: get distance next
    icmDistExt,  // I: getting distance extra
    icmCopy,     // O: copying bytes in window, waiting for space
    icmLit,      // O: got literal, waiting for output space
    icmWash,     // O: got eob, possibly still output waiting
    icmZEnd,     // X: got eob and all data flushed
    icmBadCode   // X: got error
  );

  // inflate codes private state 
  PInflateCodesState = ^TInflateCodesState;
  TInflateCodesState = record
    Mode : TInflateCodesMode;         // current inflate codes mode
    // mode dependent information
    Len  : cardinal;
    Sub  : record case byte of        // submode
             0: (Code : record                      // if Len or Distance, where in tree
                          Tree     : PInflateHuft;  // pointer into tree
                          need     : cardinal;      // bits needed
                        end);
             1: (lit  : cardinal);                  // if icmLit, literal
             2: (copy : record                      // if EXT or icmCopy, where and how much
                          get      : cardinal;      // bits to get for extra
                          Distance : cardinal;      // distance back to copy from
                        end);
           end;
    // mode independent information
    LiteralTreeBits  : byte;          // LiteralTree bits decoded per branch
    DistanceTreeBits : byte;          // DistanceTree bits decoder per branch
    LiteralTree      : PInflateHuft;  // literal/length/eob tree
    DistanceTree     : PInflateHuft;  // distance tree
  end;

  TInflateBlockMode = (
    ibmZType,      // get type bits (3, including end bit)
    ibmLens,       // get lengths for stored
    ibmStored,     // processing stored block
    ibmTable,      // get table lengths
    ibmBitTree,    // get bit lengths tree for a dynamic block
    ibmDistTree,   // get length, distance trees for a dynamic block
    ibmCodes,      // processing fixed or dynamic block
    ibmDry,        // output remaining window bytes
    ibmBlockDone,  // finished last block, done
    ibmBlockBad    // got a data error -> stuck here
  );

  // inflate blocks semi-private state
  PInflateBlocksState = ^TInflateBlocksState;
  TInflateBlocksState = record
    Mode   : TInflateBlockMode;   // current inflate block mode
    // mode dependent information
    Sub    : record case byte of  // submode
               0: (left   : cardinal);               // if ibmStored, bytes left to copy
               1: (Trees  : record                   // if DistanceTree, decoding info for trees
                              Table : cardinal;      // table lengths (14 Bits)
                              Index : cardinal;      // index into blens (or BitOrder)
                              blens : TPACardinal;   // bit lengths of codes
                              BB    : cardinal;      // bit length tree depth
                              TB    : PInflateHuft;  // bit length decoding tree
                            end);
               2: (decode : record                   // if ibmCodes, current state
                              TL    : PInflateHuft;
                              TD    : PInflateHuft;  // trees to free
                              codes : PInflateCodesState;
                            end);
             end;
    Last   : boolean;             // True if this block is the last block
    // mode independent information
    bitk   : cardinal;            // bits in bit buffer
    bitb   : cardinal;            // bit buffer
    hufts  : PHuftField;          // single allocation for tree space
    window : PByte;               // sliding window
    zend   : PByte;               // one byte after sliding window
    read   : PByte;               // window read pointer
    write  : PByte;               // window write pointer
  end;

  // The application must update NextInput and AvailableInput when AvailableInput has dropped to zero. It must update
  // NextOutput and AvailableOutput when AvailableOutput has dropped to zero. All other fields are set by the
  // compression library and must not be updated by the application.

  // The fields TotalInput and TotalOutput can be used for statistics or progress reports. After compression, TotalInput
  // holds the total size of the uncompressed data and may be saved for use in the decompressor
  // (particularly if the decompressor wants to decompress everything in a single step).

  PZState = ^TZState;
  TZState = record
    NextInput       : PByte;                // next input byte
    AvailableInput  : cardinal;             // number of bytes available at NextInput
    TotalInput      : int64;                // total number of input bytes read so far
    NextOutput      : PByte;                // next output byte should be put there
    AvailableOutput : cardinal;             // remaining free space at NextOutput
    TotalOutput     : int64;                // total number of bytes output so far
    State           : PInflateBlocksState;  // not visible by applications
  end;

const
  // Return codes for the compression/decompression functions. Negative
  // values are errors, positive values are used for special but normal events.
  Z_OK           = 0;
  Z_STREAM_END   = 1;
  Z_STREAM_ERROR = -2;
  Z_DATA_ERROR   = -3;
  Z_MEM_ERROR    = -4;
  Z_BUF_ERROR    = -5;

  // three kinds of block type
  STORED_BLOCK = 0;
  STATIC_TREES = 1;
  DYN_TREES    = 2;

  // minimum and maximum match lengths
  MIN_MATCH = 3;
  MAX_MATCH = 258;

//----------------- deflation support ----------------------------------------------------------------------------------

const
  LENGTH_CODES = 29;                             // number of length codes, not counting the special END_BLOCK code
  LITERALS     = 256;                            // number of literal bytes 0..255
  L_CODES      = (LITERALS + 1 + LENGTH_CODES);  // number of literal or length codes, including the END_BLOCK code
  D_CODES      = 30;                             // number of distance codes
  BL_CODES     = 19;                             // number of codes used to transfer the bit lengths
  HEAP_SIZE    = (2 * L_CODES + 1);              // maximum heap size
  MAX_BITS     = 15;                             // all codes must not exceed MAX_BITS bits

type
  // data structure describing a single value and its code string
  PTreeEntry = ^TTreeEntry;
  TTreeEntry = record
    fc: record case byte of
          0: (Frequency : word);  // frequency count
          1: (Code      : word);  // bit string
        end;
    dl: record case byte of
          0: (dad       : word);  // father node in Huffman tree
          1: (Len       : word);  // length of bit string
        end;
  end;

  TLiteralTree  = array [0..HEAP_SIZE - 1] of TTreeEntry;  // literal and length tree
  TDistanceTree = array [0..2 * D_CODES  ] of TTreeEntry;  // distance tree
  THuffmanTree  = array [0..2 * BL_CODES ] of TTreeEntry;  // Huffman tree for bit lengths

  PTree = ^TTree;
  TTree = array [0..(MaxInt div SizeOf(TTreeEntry)) - 1] of TTreeEntry;  // generic tree type

  PStaticTreeDescriptor = ^TStaticTreeDescriptor;
  TStaticTreeDescriptor = record
    StaticTree : PTree;       // static tree or nil
    ExtraBits  : TPAInteger;  // extra bits for each code or nil
    ExtraBase  : integer;     // base index for ExtraBits
    Elements   : integer;     // max number of elements in the tree
    MaxLength  : integer;     // max bit length for the codes
  end;
  
  PTreeDescriptor = ^TTreeDescriptor;
  TTreeDescriptor = record
    DynamicTree      : PTree;
    MaxCode          : integer;                // largest code with non zero frequency
    StaticDescriptor : PStaticTreeDescriptor;  // the corresponding static tree
  end;

  PDeflateState = ^TDeflateState;
  TDeflateState = record
    ZState            : PZState;   // pointer back to this zlib stream
    PendingBuffer     : TPAByte;   // output still pending
    PendingBufferSize : integer;
    PendingOutput     : PByte;     // next pending byte to output to the stream
    Pending           : integer;   // nb of bytes in the pending buffer
    WindowSize        : cardinal;  // LZ77 window size (32K by default)
    WindowBits        : cardinal;  // log2(WindowSize) (8..16)
    WindowMask        : cardinal;  // WindowSize - 1

    // Sliding window. Input bytes are read into the second half of the window,
    // and move to the first half later to keep a dictionary of at least WSize
    // bytes. With this organization, matches are limited to a distance of
    // WSize - MAX_MATCH bytes, but this ensures that IO is always
    // performed with a length multiple of the block Size. Also, it limits
    // the window Size to 64K, which is quite useful on MSDOS.
    // To do: use the user input buffer as sliding window.
    Window : TPAByte;

    // Actual size of Window: 2 * WSize, except when the user input buffer
    // is directly used as sliding window.
    CurrentWindowSize : integer;

    // Link to older string with same hash index. to limit the size of this
    // array to 64K, this link is maintained only for the last 32K strings.
    // An index in this array is thus a window index modulo 32K.
    Previous : TPAWord;

    Head : TPAWord;  // heads of the hash chains or nil

    InsertHash : cardinal;  // hash index of string to be inserted
    HashSize   : cardinal;  // number of elements in hash table
    HashBits   : cardinal;  // log2(HashSize)
    HashMask   : cardinal;  // HashSize - 1

    // Number of bits by which InsertHash must be shifted at each input step.
    // It must be such that after MIN_MATCH steps, the oldest byte no longer
    // takes part in the hash key, that is:
    // HashShift * MIN_MATCH >= HashBits
    HashShift : cardinal;

    // Window position at the beginning of the current output block. Gets
    // negative when the window is moved backwards.
    BlockStart : integer;

    MatchLength    : cardinal;  // length of best match
    PreviousMatch  : cardinal;  // previous match
    MatchAvailable : boolean;   // set if previous match exists
    StringStart    : cardinal;  // start of string to insert
    MatchStart     : cardinal;  // start of matching string
    Lookahead      : cardinal;  // number of valid bytes ahead in window

    // Length of the best match at previous step. Matches not greater than this
    // are discarded. This is used in the lazy match evaluation.
    PreviousLength : cardinal;

    LiteralTree   : TLiteralTree;   // literal and length tree
    DistanceTree  : TDistanceTree;  // distance tree
    BitLengthTree : THuffmanTree;   // Huffman tree for bit lengths

    LiteralDescriptor   : TTreeDescriptor;  // Descriptor for literal tree
    DistanceDescriptor  : TTreeDescriptor;  // Descriptor for distance tree
    BitLengthDescriptor : TTreeDescriptor;  // Descriptor for bit length tree

    BitLengthCounts : array [0..MAX_BITS] of word;  // number of codes at each bit length for an optimal tree

    Heap        : array [0..2 * L_CODES] of integer;  // heap used to build the Huffman trees
    HeapLength  : integer;  // number of elements in the heap
    HeapMaximum : integer;  // element of largest frequency
    // The sons of Heap[N] are Heap[2 * N] and Heap[2 * N + 1]. Heap[0] is not used.
    // The same heap array is used to build all trees.

    Depth : array [0..2 * L_CODES] of byte;  // depth of each subtree used as tie breaker for trees of equal frequency

    LiteralBuffer : TPAByte;  // buffer for literals or lengths

    // Size of match buffer for literals/lengths. There are 4 reasons for limiting LiteralBufferSize to 64K:
    //  - frequencies can be kept in 16 bit counters
    //  - If compression is not successful for the first block, all input
    //    data is still in the window so we can still emit a stored block even
    //    when input comes from standard input. This can also be done for
    //    all blocks if LiteralBufferSize is not greater than 32K.
    //  - if compression is not successful for a file smaller than 64K, we can
    //    even emit a stored file instead of a stored block (saving 5 bytes).
    //    This is applicable only for zip (not gzip or zlib).
    //  - creating new Huffman trees less frequently may not provide fast
    //    adaptation to changes in the input data statistics. (Take for
    //    example a binary file with poorly compressible code followed by
    //    a highly compressible string table.) Smaller buffer sizes give
    //    fast adaptation but have of course the overhead of transmitting
    //    trees more frequently.
    //  - I can't count above 4
    LiteralBufferSize : cardinal;

    LastLiteral : cardinal;  // running index in LiteralBuffer

    // Buffer for distances. To simplify the code, DistanceBuffer and LiteralBuffer have
    // the same number of elements. To use different lengths, an extra flag array would be necessary.
    DistanceBuffer : TPAWord;

    OptimalLength    : integer;   // bit length of current block with optimal trees
    StaticLength     : integer;   // bit length of current block with static trees
    CompressedLength : integer;   // total bit length of compressed file
    Matches          : cardinal;  // number of string matches in current block
    LastEOBLength    : integer;   // bit length of EOB code for last block
    BitsBuffer       : word;      // Output buffer. Bits are inserted starting at the bottom (least significant bits).
    ValidBits        : integer;   // Number of valid bits in BitsBuffer. All Bits above the last valid bit are always zero.
  end;

//----------------- Huffmann trees -------------------------------------------------------------------------------------

const
  DIST_CODE_LEN = 512; // see definition of array dist_code below

  // The static literal tree. Since the bit lengths are imposed, there is no need for the L_CODES Extra codes used
  // during heap construction. However the codes 286 and 287 are needed to build a canonical tree (see TreeInit below).
  StaticLiteralTree : array [0..L_CODES + 1] of TTreeEntry =
  (
    (fc: (Frequency:  12); dl: (Len: 8)), (fc: (Frequency: 140); dl: (Len: 8)), (fc: (Frequency:  76); dl: (Len: 8)),
    (fc: (Frequency: 204); dl: (Len: 8)), (fc: (Frequency:  44); dl: (Len: 8)), (fc: (Frequency: 172); dl: (Len: 8)),
    (fc: (Frequency: 108); dl: (Len: 8)), (fc: (Frequency: 236); dl: (Len: 8)), (fc: (Frequency:  28); dl: (Len: 8)),
    (fc: (Frequency: 156); dl: (Len: 8)), (fc: (Frequency:  92); dl: (Len: 8)), (fc: (Frequency: 220); dl: (Len: 8)),
    (fc: (Frequency:  60); dl: (Len: 8)), (fc: (Frequency: 188); dl: (Len: 8)), (fc: (Frequency: 124); dl: (Len: 8)),
    (fc: (Frequency: 252); dl: (Len: 8)), (fc: (Frequency:   2); dl: (Len: 8)), (fc: (Frequency: 130); dl: (Len: 8)),
    (fc: (Frequency:  66); dl: (Len: 8)), (fc: (Frequency: 194); dl: (Len: 8)), (fc: (Frequency:  34); dl: (Len: 8)),
    (fc: (Frequency: 162); dl: (Len: 8)), (fc: (Frequency:  98); dl: (Len: 8)), (fc: (Frequency: 226); dl: (Len: 8)),
    (fc: (Frequency:  18); dl: (Len: 8)), (fc: (Frequency: 146); dl: (Len: 8)), (fc: (Frequency:  82); dl: (Len: 8)),
    (fc: (Frequency: 210); dl: (Len: 8)), (fc: (Frequency:  50); dl: (Len: 8)), (fc: (Frequency: 178); dl: (Len: 8)),
    (fc: (Frequency: 114); dl: (Len: 8)), (fc: (Frequency: 242); dl: (Len: 8)), (fc: (Frequency:  10); dl: (Len: 8)),
    (fc: (Frequency: 138); dl: (Len: 8)), (fc: (Frequency:  74); dl: (Len: 8)), (fc: (Frequency: 202); dl: (Len: 8)),
    (fc: (Frequency:  42); dl: (Len: 8)), (fc: (Frequency: 170); dl: (Len: 8)), (fc: (Frequency: 106); dl: (Len: 8)),
    (fc: (Frequency: 234); dl: (Len: 8)), (fc: (Frequency:  26); dl: (Len: 8)), (fc: (Frequency: 154); dl: (Len: 8)),
    (fc: (Frequency:  90); dl: (Len: 8)), (fc: (Frequency: 218); dl: (Len: 8)), (fc: (Frequency:  58); dl: (Len: 8)),
    (fc: (Frequency: 186); dl: (Len: 8)), (fc: (Frequency: 122); dl: (Len: 8)), (fc: (Frequency: 250); dl: (Len: 8)),
    (fc: (Frequency:   6); dl: (Len: 8)), (fc: (Frequency: 134); dl: (Len: 8)), (fc: (Frequency:  70); dl: (Len: 8)),
    (fc: (Frequency: 198); dl: (Len: 8)), (fc: (Frequency:  38); dl: (Len: 8)), (fc: (Frequency: 166); dl: (Len: 8)),
    (fc: (Frequency: 102); dl: (Len: 8)), (fc: (Frequency: 230); dl: (Len: 8)), (fc: (Frequency:  22); dl: (Len: 8)),
    (fc: (Frequency: 150); dl: (Len: 8)), (fc: (Frequency:  86); dl: (Len: 8)), (fc: (Frequency: 214); dl: (Len: 8)),
    (fc: (Frequency:  54); dl: (Len: 8)), (fc: (Frequency: 182); dl: (Len: 8)), (fc: (Frequency: 118); dl: (Len: 8)),
    (fc: (Frequency: 246); dl: (Len: 8)), (fc: (Frequency:  14); dl: (Len: 8)), (fc: (Frequency: 142); dl: (Len: 8)),
    (fc: (Frequency:  78); dl: (Len: 8)), (fc: (Frequency: 206); dl: (Len: 8)), (fc: (Frequency:  46); dl: (Len: 8)),
    (fc: (Frequency: 174); dl: (Len: 8)), (fc: (Frequency: 110); dl: (Len: 8)), (fc: (Frequency: 238); dl: (Len: 8)),
    (fc: (Frequency:  30); dl: (Len: 8)), (fc: (Frequency: 158); dl: (Len: 8)), (fc: (Frequency:  94); dl: (Len: 8)),
    (fc: (Frequency: 222); dl: (Len: 8)), (fc: (Frequency:  62); dl: (Len: 8)), (fc: (Frequency: 190); dl: (Len: 8)),
    (fc: (Frequency: 126); dl: (Len: 8)), (fc: (Frequency: 254); dl: (Len: 8)), (fc: (Frequency:   1); dl: (Len: 8)),
    (fc: (Frequency: 129); dl: (Len: 8)), (fc: (Frequency:  65); dl: (Len: 8)), (fc: (Frequency: 193); dl: (Len: 8)),
    (fc: (Frequency:  33); dl: (Len: 8)), (fc: (Frequency: 161); dl: (Len: 8)), (fc: (Frequency:  97); dl: (Len: 8)),
    (fc: (Frequency: 225); dl: (Len: 8)), (fc: (Frequency:  17); dl: (Len: 8)), (fc: (Frequency: 145); dl: (Len: 8)),
    (fc: (Frequency:  81); dl: (Len: 8)), (fc: (Frequency: 209); dl: (Len: 8)), (fc: (Frequency:  49); dl: (Len: 8)),
    (fc: (Frequency: 177); dl: (Len: 8)), (fc: (Frequency: 113); dl: (Len: 8)), (fc: (Frequency: 241); dl: (Len: 8)),
    (fc: (Frequency:   9); dl: (Len: 8)), (fc: (Frequency: 137); dl: (Len: 8)), (fc: (Frequency:  73); dl: (Len: 8)),
    (fc: (Frequency: 201); dl: (Len: 8)), (fc: (Frequency:  41); dl: (Len: 8)), (fc: (Frequency: 169); dl: (Len: 8)),
    (fc: (Frequency: 105); dl: (Len: 8)), (fc: (Frequency: 233); dl: (Len: 8)), (fc: (Frequency:  25); dl: (Len: 8)),
    (fc: (Frequency: 153); dl: (Len: 8)), (fc: (Frequency:  89); dl: (Len: 8)), (fc: (Frequency: 217); dl: (Len: 8)),
    (fc: (Frequency:  57); dl: (Len: 8)), (fc: (Frequency: 185); dl: (Len: 8)), (fc: (Frequency: 121); dl: (Len: 8)),
    (fc: (Frequency: 249); dl: (Len: 8)), (fc: (Frequency:   5); dl: (Len: 8)), (fc: (Frequency: 133); dl: (Len: 8)),
    (fc: (Frequency:  69); dl: (Len: 8)), (fc: (Frequency: 197); dl: (Len: 8)), (fc: (Frequency:  37); dl: (Len: 8)),
    (fc: (Frequency: 165); dl: (Len: 8)), (fc: (Frequency: 101); dl: (Len: 8)), (fc: (Frequency: 229); dl: (Len: 8)),
    (fc: (Frequency:  21); dl: (Len: 8)), (fc: (Frequency: 149); dl: (Len: 8)), (fc: (Frequency:  85); dl: (Len: 8)),
    (fc: (Frequency: 213); dl: (Len: 8)), (fc: (Frequency:  53); dl: (Len: 8)), (fc: (Frequency: 181); dl: (Len: 8)),
    (fc: (Frequency: 117); dl: (Len: 8)), (fc: (Frequency: 245); dl: (Len: 8)), (fc: (Frequency:  13); dl: (Len: 8)),
    (fc: (Frequency: 141); dl: (Len: 8)), (fc: (Frequency:  77); dl: (Len: 8)), (fc: (Frequency: 205); dl: (Len: 8)),
    (fc: (Frequency:  45); dl: (Len: 8)), (fc: (Frequency: 173); dl: (Len: 8)), (fc: (Frequency: 109); dl: (Len: 8)),
    (fc: (Frequency: 237); dl: (Len: 8)), (fc: (Frequency:  29); dl: (Len: 8)), (fc: (Frequency: 157); dl: (Len: 8)),
    (fc: (Frequency:  93); dl: (Len: 8)), (fc: (Frequency: 221); dl: (Len: 8)), (fc: (Frequency:  61); dl: (Len: 8)),
    (fc: (Frequency: 189); dl: (Len: 8)), (fc: (Frequency: 125); dl: (Len: 8)), (fc: (Frequency: 253); dl: (Len: 8)),
    (fc: (Frequency:  19); dl: (Len: 9)), (fc: (Frequency: 275); dl: (Len: 9)), (fc: (Frequency: 147); dl: (Len: 9)),
    (fc: (Frequency: 403); dl: (Len: 9)), (fc: (Frequency:  83); dl: (Len: 9)), (fc: (Frequency: 339); dl: (Len: 9)),
    (fc: (Frequency: 211); dl: (Len: 9)), (fc: (Frequency: 467); dl: (Len: 9)), (fc: (Frequency:  51); dl: (Len: 9)),
    (fc: (Frequency: 307); dl: (Len: 9)), (fc: (Frequency: 179); dl: (Len: 9)), (fc: (Frequency: 435); dl: (Len: 9)),
    (fc: (Frequency: 115); dl: (Len: 9)), (fc: (Frequency: 371); dl: (Len: 9)), (fc: (Frequency: 243); dl: (Len: 9)),
    (fc: (Frequency: 499); dl: (Len: 9)), (fc: (Frequency:  11); dl: (Len: 9)), (fc: (Frequency: 267); dl: (Len: 9)),
    (fc: (Frequency: 139); dl: (Len: 9)), (fc: (Frequency: 395); dl: (Len: 9)), (fc: (Frequency:  75); dl: (Len: 9)),
    (fc: (Frequency: 331); dl: (Len: 9)), (fc: (Frequency: 203); dl: (Len: 9)), (fc: (Frequency: 459); dl: (Len: 9)),
    (fc: (Frequency:  43); dl: (Len: 9)), (fc: (Frequency: 299); dl: (Len: 9)), (fc: (Frequency: 171); dl: (Len: 9)),
    (fc: (Frequency: 427); dl: (Len: 9)), (fc: (Frequency: 107); dl: (Len: 9)), (fc: (Frequency: 363); dl: (Len: 9)),
    (fc: (Frequency: 235); dl: (Len: 9)), (fc: (Frequency: 491); dl: (Len: 9)), (fc: (Frequency:  27); dl: (Len: 9)),
    (fc: (Frequency: 283); dl: (Len: 9)), (fc: (Frequency: 155); dl: (Len: 9)), (fc: (Frequency: 411); dl: (Len: 9)),
    (fc: (Frequency:  91); dl: (Len: 9)), (fc: (Frequency: 347); dl: (Len: 9)), (fc: (Frequency: 219); dl: (Len: 9)),
    (fc: (Frequency: 475); dl: (Len: 9)), (fc: (Frequency:  59); dl: (Len: 9)), (fc: (Frequency: 315); dl: (Len: 9)),
    (fc: (Frequency: 187); dl: (Len: 9)), (fc: (Frequency: 443); dl: (Len: 9)), (fc: (Frequency: 123); dl: (Len: 9)),
    (fc: (Frequency: 379); dl: (Len: 9)), (fc: (Frequency: 251); dl: (Len: 9)), (fc: (Frequency: 507); dl: (Len: 9)),
    (fc: (Frequency:   7); dl: (Len: 9)), (fc: (Frequency: 263); dl: (Len: 9)), (fc: (Frequency: 135); dl: (Len: 9)),
    (fc: (Frequency: 391); dl: (Len: 9)), (fc: (Frequency:  71); dl: (Len: 9)), (fc: (Frequency: 327); dl: (Len: 9)),
    (fc: (Frequency: 199); dl: (Len: 9)), (fc: (Frequency: 455); dl: (Len: 9)), (fc: (Frequency:  39); dl: (Len: 9)),
    (fc: (Frequency: 295); dl: (Len: 9)), (fc: (Frequency: 167); dl: (Len: 9)), (fc: (Frequency: 423); dl: (Len: 9)),
    (fc: (Frequency: 103); dl: (Len: 9)), (fc: (Frequency: 359); dl: (Len: 9)), (fc: (Frequency: 231); dl: (Len: 9)),
    (fc: (Frequency: 487); dl: (Len: 9)), (fc: (Frequency:  23); dl: (Len: 9)), (fc: (Frequency: 279); dl: (Len: 9)),
    (fc: (Frequency: 151); dl: (Len: 9)), (fc: (Frequency: 407); dl: (Len: 9)), (fc: (Frequency:  87); dl: (Len: 9)),
    (fc: (Frequency: 343); dl: (Len: 9)), (fc: (Frequency: 215); dl: (Len: 9)), (fc: (Frequency: 471); dl: (Len: 9)),
    (fc: (Frequency:  55); dl: (Len: 9)), (fc: (Frequency: 311); dl: (Len: 9)), (fc: (Frequency: 183); dl: (Len: 9)),
    (fc: (Frequency: 439); dl: (Len: 9)), (fc: (Frequency: 119); dl: (Len: 9)), (fc: (Frequency: 375); dl: (Len: 9)),
    (fc: (Frequency: 247); dl: (Len: 9)), (fc: (Frequency: 503); dl: (Len: 9)), (fc: (Frequency:  15); dl: (Len: 9)),
    (fc: (Frequency: 271); dl: (Len: 9)), (fc: (Frequency: 143); dl: (Len: 9)), (fc: (Frequency: 399); dl: (Len: 9)),
    (fc: (Frequency:  79); dl: (Len: 9)), (fc: (Frequency: 335); dl: (Len: 9)), (fc: (Frequency: 207); dl: (Len: 9)),
    (fc: (Frequency: 463); dl: (Len: 9)), (fc: (Frequency:  47); dl: (Len: 9)), (fc: (Frequency: 303); dl: (Len: 9)),
    (fc: (Frequency: 175); dl: (Len: 9)), (fc: (Frequency: 431); dl: (Len: 9)), (fc: (Frequency: 111); dl: (Len: 9)),
    (fc: (Frequency: 367); dl: (Len: 9)), (fc: (Frequency: 239); dl: (Len: 9)), (fc: (Frequency: 495); dl: (Len: 9)),
    (fc: (Frequency:  31); dl: (Len: 9)), (fc: (Frequency: 287); dl: (Len: 9)), (fc: (Frequency: 159); dl: (Len: 9)),
    (fc: (Frequency: 415); dl: (Len: 9)), (fc: (Frequency:  95); dl: (Len: 9)), (fc: (Frequency: 351); dl: (Len: 9)),
    (fc: (Frequency: 223); dl: (Len: 9)), (fc: (Frequency: 479); dl: (Len: 9)), (fc: (Frequency:  63); dl: (Len: 9)),
    (fc: (Frequency: 319); dl: (Len: 9)), (fc: (Frequency: 191); dl: (Len: 9)), (fc: (Frequency: 447); dl: (Len: 9)),
    (fc: (Frequency: 127); dl: (Len: 9)), (fc: (Frequency: 383); dl: (Len: 9)), (fc: (Frequency: 255); dl: (Len: 9)),
    (fc: (Frequency: 511); dl: (Len: 9)), (fc: (Frequency:   0); dl: (Len: 7)), (fc: (Frequency:  64); dl: (Len: 7)),
    (fc: (Frequency:  32); dl: (Len: 7)), (fc: (Frequency:  96); dl: (Len: 7)), (fc: (Frequency:  16); dl: (Len: 7)),
    (fc: (Frequency:  80); dl: (Len: 7)), (fc: (Frequency:  48); dl: (Len: 7)), (fc: (Frequency: 112); dl: (Len: 7)),
    (fc: (Frequency:   8); dl: (Len: 7)), (fc: (Frequency:  72); dl: (Len: 7)), (fc: (Frequency:  40); dl: (Len: 7)),
    (fc: (Frequency: 104); dl: (Len: 7)), (fc: (Frequency:  24); dl: (Len: 7)), (fc: (Frequency:  88); dl: (Len: 7)),
    (fc: (Frequency:  56); dl: (Len: 7)), (fc: (Frequency: 120); dl: (Len: 7)), (fc: (Frequency:   4); dl: (Len: 7)),
    (fc: (Frequency:  68); dl: (Len: 7)), (fc: (Frequency:  36); dl: (Len: 7)), (fc: (Frequency: 100); dl: (Len: 7)),
    (fc: (Frequency:  20); dl: (Len: 7)), (fc: (Frequency:  84); dl: (Len: 7)), (fc: (Frequency:  52); dl: (Len: 7)),
    (fc: (Frequency: 116); dl: (Len: 7)), (fc: (Frequency:   3); dl: (Len: 8)), (fc: (Frequency: 131); dl: (Len: 8)),
    (fc: (Frequency:  67); dl: (Len: 8)), (fc: (Frequency: 195); dl: (Len: 8)), (fc: (Frequency:  35); dl: (Len: 8)),
    (fc: (Frequency: 163); dl: (Len: 8)), (fc: (Frequency:  99); dl: (Len: 8)), (fc: (Frequency: 227); dl: (Len: 8))
  );

  // The static distance tree. (Actually a trivial tree since all lens use 5 Bits.)
  StaticDescriptorTree : array [0..D_CODES - 1] of TTreeEntry =
  (
    (fc: (Frequency:  0); dl: (Len: 5)), (fc: (Frequency: 16); dl: (Len: 5)), (fc: (Frequency:  8); dl: (Len: 5)),
    (fc: (Frequency: 24); dl: (Len: 5)), (fc: (Frequency:  4); dl: (Len: 5)), (fc: (Frequency: 20); dl: (Len: 5)),
    (fc: (Frequency: 12); dl: (Len: 5)), (fc: (Frequency: 28); dl: (Len: 5)), (fc: (Frequency:  2); dl: (Len: 5)),
    (fc: (Frequency: 18); dl: (Len: 5)), (fc: (Frequency: 10); dl: (Len: 5)), (fc: (Frequency: 26); dl: (Len: 5)),
    (fc: (Frequency:  6); dl: (Len: 5)), (fc: (Frequency: 22); dl: (Len: 5)), (fc: (Frequency: 14); dl: (Len: 5)),
    (fc: (Frequency: 30); dl: (Len: 5)), (fc: (Frequency:  1); dl: (Len: 5)), (fc: (Frequency: 17); dl: (Len: 5)),
    (fc: (Frequency:  9); dl: (Len: 5)), (fc: (Frequency: 25); dl: (Len: 5)), (fc: (Frequency:  5); dl: (Len: 5)),
    (fc: (Frequency: 21); dl: (Len: 5)), (fc: (Frequency: 13); dl: (Len: 5)), (fc: (Frequency: 29); dl: (Len: 5)),
    (fc: (Frequency:  3); dl: (Len: 5)), (fc: (Frequency: 19); dl: (Len: 5)), (fc: (Frequency: 11); dl: (Len: 5)),
    (fc: (Frequency: 27); dl: (Len: 5)), (fc: (Frequency:  7); dl: (Len: 5)), (fc: (Frequency: 23); dl: (Len: 5))
  );

  // Distance codes. The first 256 values correspond to the distances 3 .. 258, the last 256 values correspond to the
  // top 8 Bits of the 15 bit distances.
  DistanceCode : array [0..DIST_CODE_LEN - 1] of byte =
  (
     0,  1,  2,  3,  4,  4,  5,  5,  6,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  8,
     8,  8,  8,  8,  9,  9,  9,  9,  9,  9,  9,  9, 10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,  0,  0, 16, 17,
    18, 18, 19, 19, 20, 20, 20, 20, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22,
    23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29
  );

  // length code for each normalized match length (0 = MIN_MATCH)
  LengthCode : array [0..MAX_MATCH - MIN_MATCH] of byte =
  (
     0,  1,  2,  3,  4,  5,  6,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 12, 12,
    13, 13, 13, 13, 14, 14, 14, 14, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16,
    17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19,
    19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28
  );

  // first normalized length for each code (0 = MIN_MATCH)
  BaseLength : array [0..LENGTH_CODES - 1] of integer =
  (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 14, 16, 20, 24, 28, 32, 40, 48, 56,
    64, 80, 96, 112, 128, 160, 192, 224, 0
  );

  // first normalized distance for each code (0 = distance of 1)
  BaseDistance : array [0..D_CODES - 1] of integer =
  (
       0,     1,     2,     3,     4,     6,     8,    12,    16,    24,
      32,    48,    64,    96,   128,   192,   256,   384,   512,   768,
    1024,  1536,  2048,  3072,  4096,  6144,  8192, 12288, 16384, 24576
  );

  MIN_LOOKAHEAD = (MAX_MATCH + MIN_MATCH + 1);
  MAX_BL_BITS   = 7;    // bit length codes must not exceed MAX_BL_BITS bits
  END_BLOCK     = 256;  // end of block literal code
  REP_3_6       = 16;   // repeat previous bit length 3-6 times (2 Bits of repeat count)
  REPZ_3_10     = 17;   // repeat a zero length 3-10 times  (3 Bits of repeat count)
  REPZ_11_138   = 18;   // repeat a zero length 11-138 times  (7 Bits of repeat count)

  // extra bits for each length code
  ExtraLengthBits : array [0..LENGTH_CODES - 1] of integer =
  (
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0
  );

  // extra bits for each distance code
  ExtraDistanceBits : array [0..D_CODES-1] of integer =
  (
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10 ,10, 11, 11, 12, 12, 13, 13
  );

  // extra bits for each bit length code
  ExtraBitLengthBits : array [0..BL_CODES - 1] of integer =
  (
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 7
  );

  // The lengths of the bit length codes are sent in order of decreasing probability,
  // to avoid transmitting the lengths for unused bit length codes.
  BitLengthOrder : array [0..BL_CODES - 1] of byte =
  (
    16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
  );

  // Number of bits used within BitsBuffer. (BitsBuffer might be implemented on more than 16 bits on some systems.)
  BufferSize = 16;

  StaticLiteralDescriptor : TStaticTreeDescriptor =
  (
    StaticTree : @StaticLiteralTree;  // pointer to array of TTreeEntry
    ExtraBits  : @ExtraLengthBits;    // pointer to array of integer
    ExtraBase  : LITERALS + 1;
    Elements   : L_CODES;
    MaxLength  : MAX_BITS
  );

  StaticDistanceDescriptor : TStaticTreeDescriptor =
  (
    StaticTree : @StaticDescriptorTree;
    ExtraBits  : @ExtraDistanceBits;
    ExtraBase  : 0;
    Elements   : D_CODES;
    MaxLength  : MAX_BITS
  );

  StaticBitLengthDescriptor : TStaticTreeDescriptor =
  (
    StaticTree : nil;
    ExtraBits  : @ExtraBitLengthBits;
    ExtraBase  : 0;
    Elements   : BL_CODES;
    MaxLength  : MAX_BL_BITS
  );

var
  // build fixed tables only once -> keep them here
  FixedBuild : boolean = False;

//----------------------------------------------------------------------------------------------------------------------

type splitInt64 = record loCard, hiCard : cardinal; end;

function Min(dw1, dw2: int64) : int64;
begin
  if dw1 < dw2 then
    result := dw1
  else
    result := dw2;
end;

function SetFilePointer2(file_: THandle; pos: int64; flag: dword; outPos: TPInt64 = nil) : boolean;
begin
  SetLastError(0);
  splitInt64(pos).loCard := SetFilePointer(file_, splitInt64(pos).loCard, @splitInt64(pos).hiCard, flag);
  result := (splitInt64(pos).loCard <> dword(-1)) or (GetLastError = 0);
  if result and (outPos <> nil) then
    outPos^ := pos;
end;

function MapViewOfFile2(map: THandle; access: dword; pos: int64; size: dword) : pointer;
begin
  result := MapViewOfFile(map, access, splitInt64(pos).hiCard, splitInt64(pos).loCard, size);
end;

function CompressEx(srcFile, dstFile, srcMap, dstMap: THandle; outOffset: integer;
                    src, dst: pointer; srcLen, dstLen: int64; crc: TPCardinal = nil) : int64;
// win9x doesn't support big file maps, so there we might be forced to
// allocate the temporare buffers "srcBuf" and "dstBuf"
var srcBuf, dstBuf : pointer;

  procedure CheckSrcMap(var z: TZState);
  var c1 : dword;
  begin
    if (srcFile <> 0) and ((src = nil) or (z.AvailableInput = 0)) then begin
      if (srcMap <> 0) and (src <> nil) then begin
        UnmapViewOfFile(src);
        src := nil;
      end;
      z.AvailableInput := Min(CMapSize, srcLen - z.TotalInput);
      if z.AvailableInput <> 0 then begin
        if srcMap = 0 then begin
          if srcBuf = nil then
            srcBuf := VirtualAlloc(nil, CMapSize, MEM_COMMIT, PAGE_READWRITE);
          if SetFilePointer2(srcFile, z.TotalInput, FILE_BEGIN) and
             ReadFile(srcFile, srcBuf^, z.AvailableInput, c1, nil) and (c1 = z.AvailableInput) then
            src := srcBuf
          else
            src := nil;
        end else
          src := MapViewOfFile2(srcMap, FILE_MAP_READ, z.TotalInput, z.AvailableInput);
        z.NextInput := src;
        if crc <> nil then
          crc^ := updateCrc32(crc^, src^, z.AvailableInput);
      end;
    end;
  end;

  procedure FlushDstFile(var z: TZState);
  var c1, c2 : dword;
  begin
    if (dstFile <> 0) and (dstMap = 0) and (z.TotalOutput > 0) then begin
      c1 := z.TotalOutput mod CMapSize;
      if c1 = 0 then
        c1 := CMapSize;
      if (not WriteFile(dstFile, dstBuf^, c1, c2, nil)) or (c1 <> c2) then
        raise MadException.Create('Stream write error.');
    end;
  end;

  procedure CheckDstMap(var z: TZState);
  begin
    if (dstFile <> 0) and ((dst = nil) or (z.AvailableOutput = 0)) then begin
      FlushDstFile(z);
      if (dstMap <> 0) and (dst <> nil) then begin
        UnmapViewOfFile(dst);
        dst := nil;
      end;
      z.AvailableOutput := Min(CMapSize, dstLen - z.TotalOutput);
      if z.AvailableOutput <> 0 then begin
        if dstMap = 0 then begin
          if dstBuf = nil then
            dstBuf := VirtualAlloc(nil, CMapSize, MEM_COMMIT, PAGE_READWRITE);
          dst := dstBuf;
        end else
          dst := MapViewOfFile2(dstMap, FILE_MAP_ALL_ACCESS, z.TotalOutput, z.AvailableOutput);
        z.NextOutput := dst;
        if z.TotalOutput = 0 then begin
          z.TotalOutput := outOffset;
          inc(NativeUInt(z.NextOutput), outOffset);
          dec(z.AvailableOutput, outOffset);
        end;
      end;
    end;
  end;

  function LongestMatch(var S: TDeflateState; CurrentMatch: cardinal) : cardinal;
  // Sets MatchStart to the longest match starting at the given string and returns its length. Matches shorter or equal to
  // PreviousLength are discarded, in which case the result is equal to PreviousLength and MatchStart is garbage.
  // CurrentMatch is the head of the hash chain for the current string (StringStart) and its distance is <= MaxDistance,
  // and PreviousLength >= 1.
  // The match length will not be greater than S.Lookahead.
  const CGoodLen  =  4;
        CNiceLen  = 16;
        CMaxChain =  8;
  var ChainLength : cardinal;  // max hash chain length
      Scan        : PByte;     // current string
      Match       : PByte;     // matched string
      Len         : cardinal;  // length of current match
      BestLen     : cardinal;  // best match length so far
      NiceMatch   : cardinal;
      Limit       : cardinal;
      Previous    : TPAWord;
      WMask       : cardinal;
      StrEnd      : PByte;
      ScanEnd1    : byte;
      ScanEnd     : byte;
      MaxDistance : cardinal;
  begin
    ChainLength := CMaxChain;
    Scan := @S.Window[S.StringStart];
    BestLen := S.PreviousLength;
    NiceMatch := CNiceLen;
    MaxDistance := S.WindowSize - MIN_LOOKAHEAD;

    // In order to simplify the code, match distances are limited to MaxDistance instead of WSize.
    if S.StringStart > MaxDistance then
      Limit := S.StringStart - MaxDistance
    else
      Limit := 0;

    // Stop when CurrentMatch becomes <= Limit. To simplify the Code we prevent matches with the string of window index 0.
    Previous := S.Previous;
    WMask := S.WindowMask;

    StrEnd := @S.Window[S.StringStart + MAX_MATCH];
    {$ifopt R+} {$R-} {$define RangeCheck} {$endif}
    ScanEnd1 := TPAByte(Scan)[BestLen - 1];
    ScanEnd := TPAByte(Scan)[BestLen];
    {$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}

    // The code is optimized for HashBits >= 8 and MAX_MATCH - 2 multiple of 16.
    // It is easy to get rid of this optimization if necessary.
    // Do not waste too much time if we already have a good Match.
    if S.PreviousLength >= CGoodLen then
      ChainLength := ChainLength shr 2;

    // Do not look for matches beyond the end of the input. This is necessary to make Deflate deterministic.
    if NiceMatch > S.Lookahead then
      NiceMatch := S.Lookahead;

    repeat
      Match := @S.Window[CurrentMatch];

      // Skip to next match if the match length cannot increase or if the match length is less than 2.
      {$ifopt R+} {$R-} {$define RangeCheck} {$endif}
      if (TPAByte(Match)[BestLen] = ScanEnd) and (TPAByte(Match)[BestLen - 1] = ScanEnd1) and (Match^ = Scan^) then
      {$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
      begin
        Inc(Match);
        if Match^ = TPAByte(Scan)[1] then
        begin
          // The Check at BestLen - 1 can be removed because it will be made again later (this heuristic is not always a win).
          // It is not necessary to compare Scan[2] and Match[2] since they are always equal when the other bytes match,
          // given that the hash keys are equal and that HashBits >= 8.
          Inc(Scan, 2);
          Inc(Match);

          // We check for insufficient lookahead only every 8th comparison, the 256th check will be made at StringStart + 258.
          repeat
            Inc(Scan); Inc(Match); if (Scan^ <> Match^) then break;
            Inc(Scan); Inc(Match); if (Scan^ <> Match^) then break;
            Inc(Scan); Inc(Match); if (Scan^ <> Match^) then break;
            Inc(Scan); Inc(Match); if (Scan^ <> Match^) then break;
            Inc(Scan); Inc(Match); if (Scan^ <> Match^) then break;
            Inc(Scan); Inc(Match); if (Scan^ <> Match^) then break;
            Inc(Scan); Inc(Match); if (Scan^ <> Match^) then break;
            Inc(Scan); Inc(Match); if (Scan^ <> Match^) then break;
          until NativeUInt(Scan) >= NativeUInt(StrEnd);

          Len := MAX_MATCH - integer(NativeUInt(StrEnd) - NativeUInt(Scan));
          Scan := StrEnd;
          Dec(Scan, MAX_MATCH);

          if Len > BestLen then
          begin
            S.MatchStart := CurrentMatch;
            BestLen := Len;
            if Len >= NiceMatch then break;
            {$ifopt R+} {$R-} {$define RangeCheck} {$endif}
            ScanEnd1 := TPAByte(Scan)[BestLen - 1];
            ScanEnd := TPAByte(Scan)[BestLen];
            {$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
          end;
        end;
      end;
      CurrentMatch := Previous[CurrentMatch and WMask];
      Dec(ChainLength);
    until (CurrentMatch <= Limit) or (ChainLength = 0);

    if BestLen <= S.Lookahead then
      result := BestLen
    else
      result := S.Lookahead;
  end;

  procedure FillWindow(var S: TDeflateState);
  // Fills the window when the lookahead becomes insufficient, updates StringStart and Lookahead.
  // Lookahead must be less than MIN_LOOKAHEAD.
  // StringStart will be <= CurrentWindowSize - MIN_LOOKAHEAD on exit.
  // On exit at least one byte has been read, or AvailableInput = 0. Reads are performed for at least two bytes (required
  // for the zip translate_eol option -> not supported here).

    function ReadBuffer(ZState: PZState; Buffer: PByte; Size: cardinal) : cardinal;
    // Reads a new buffer from the current input stream, updates the Adler32 and total number of bytes read.  All Deflate
    // input goes through this function so some applications may wish to modify it to avoid allocating a large
    // ZState.NextInput buffer and copying from it (see also FlushPending).
    var Len : cardinal;
    begin
      Len := ZState.AvailableInput;

      if Len > Size then
        Len := Size;
      if Len > 0 then begin
        Dec(ZState.AvailableInput, Len);

        Move(ZState.NextInput^, Buffer^, Len);
        Inc(ZState.NextInput, Len);
        Inc(ZState.TotalInput, Len);
      end;
      result := Len;

      if (result < Size) and (ZState^.AvailableInput = 0) then begin
        CheckSrcMap(ZState^);
        if ZState^.AvailableInput <> 0 then
          result := result + ReadBuffer(ZState, pointer(NativeUInt(Buffer) + result), size - result);
      end;
    end;

  var N, M  : cardinal;
      P     : TPWord;
      More  : cardinal;  // amount of free space at the end of the window
      WSize : cardinal;
  begin
    WSize := S.WindowSize;
    repeat
      More := S.CurrentWindowSize - integer(S.Lookahead) - integer(S.StringStart);
      if (More = 0) and (S.StringStart = 0) and (S.Lookahead = 0) then
        More := WSize
      else
        if More = cardinal(-1) then
        begin
          // Very unlikely, but sometimes possible if StringStart = 0 and Lookahead = 1 (input done one byte at time)
          Dec(More);
          // If the Window is almost full and there is insufficient lookahead,
          // move the upper half to the lower one to make room in the upper half.
        end
        else
          if S.StringStart >= WSize + (WSize - MIN_LOOKAHEAD) then
          begin
            Move(S.Window[WSize], S.Window^, WSize);
            Dec(S.MatchStart, WSize);
            Dec(S.StringStart, WSize);
            // we now have StringStart >= MaxDistance
            Dec(S.BlockStart, integer(WSize));

            // Slide the hash table (could be avoided with 32 bit values at the expense of memory usage). We slide even when
            // Level = 0 to keep the hash table consistent if we switch back to Level > 0 later. (Using Level 0 permanently
            // is not an optimal usage of zlib, so we don't care about this pathological case.)
            N := S.HashSize;
            P := @S.Head[N];
            repeat
              Dec(P);
              M := P^;
              if M >= WSize then
                P^ := M - WSize
              else
                P^ := 0;
              Dec(N);
            until N = 0;

            N := WSize;
            P := @S.Previous[N];
            repeat
              Dec(P);
              M := P^;
              if M >= WSize then
                P^ := M - WSize
              else
                P^ := 0;
              // if N is not on any hash chain Previous[N] is garbage but its value will never be used
              Dec(N);
            until N = 0;

            Inc(More, WSize);
          end;

      if S.ZState.TotalInput = srcLen then
        exit;

      // If there was no sliding:
      //    StringStart <= WSize + MaxDistance - 1 and Lookahead <= MIN_LOOKAHEAD - 1 and
      //    More = CurrentWindowSize - Lookahead - StringStart
      // => More >= CurrentWindowSize - (MIN_LOOKAHEAD - 1 + WSize + MaxDistance - 1)
      // => More >= CurrentWindowSize - 2 * WSize + 2
      // In the BIG_MEM or MMAP case (not yet supported),
      //    CurrentWindowSize = input_size + MIN_LOOKAHEAD  and
      //    StringStart + S.Lookahead <= input_size => More >= MIN_LOOKAHEAD.
      // Otherwise, CurrentWindowSize = 2 * WSize so More >= 2.
      // If there was sliding More >= WSize. So in all cases More >= 2.

      N := ReadBuffer(S.ZState, @S.Window[S.StringStart + S.Lookahead], More);
      Inc(S.Lookahead, N);

      // Initialize the hash Value now that we have some input:
      if S.Lookahead >= MIN_MATCH then
      begin
        S.InsertHash := S.Window[S.StringStart];
        S.InsertHash := ((S.InsertHash shl S.HashShift) xor S.Window[S.StringStart + 1]) and S.HashMask;
      end;
      // If the whole input has less than MIN_MATCH bytes, InsertHash is garbage,
      // but this is not important since only literal bytes will be emitted.
    until (S.Lookahead >= MIN_LOOKAHEAD) or (S.ZState.TotalInput = srcLen);
  end;

  procedure InitializeBlock(var S: TDeflateState);
  var N: integer;
  begin
    // initialize the trees
    for N := 0 to L_CODES  - 1 do S.LiteralTree  [N].fc.Frequency := 0;
    for N := 0 to D_CODES  - 1 do S.DistanceTree [N].fc.Frequency := 0;
    for N := 0 to BL_CODES - 1 do S.BitLengthTree[N].fc.Frequency := 0;

    S.LiteralTree[END_BLOCK].fc.Frequency := 1;
    S.StaticLength := 0;
    S.OptimalLength := 0;
    S.Matches := 0;
    S.LastLiteral := 0;
  end;

  procedure FlushBlockOnly(var S: TDeflateState; EOF: boolean);
  // Flushs the current block with given end-of-file flag.
  // StringStart must be set to the end of the current match.

    procedure FlushPending(var ZState: TZState);
    // Flushs as much pending output as possible. All Deflate output goes through this function so some applications may
    // wish to modify it to avoid allocating a large ZState.NextOutput buffer and copying into it
    // (see also ReadBuffer).
    var Len : cardinal;
        S   : PDeflateState;
    begin
      S := PDeflateState(ZState.State);
      Len := S.Pending;

      if Len > ZState.AvailableOutput then
        Len := ZState.AvailableOutput;
      if Len > 0 then begin
        Move(S.PendingOutput^, ZState.NextOutput^, Len);
        Inc(ZState.NextOutput, Len);
        Inc(S.PendingOutput, Len);
        Inc(ZState.TotalOutput, Len);
        Dec(ZState.AvailableOutput, Len);
        Dec(S.Pending, Len);
        if S.Pending = 0 then
          S.PendingOutput := PByte(S.PendingBuffer);
      end;

      if (S.Pending > 0) and (ZState.AvailableOutput = 0) then begin
        CheckDstMap(ZState);
        if ZState.AvailableOutput > 0 then
          FlushPending(ZState);
      end;
    end;

    function TreeFlushBlock(var S: TDeflateState; Buffer: PByte; StoredLength: integer; EOF: boolean) : integer;
    // Determines the best encoding for the current block: dynamic trees, static trees or store, and outputs the encoded
    // block. Buffer contains the input block (or nil if too old), StoredLength the length of this block and EOF if this
    // is the last block.
    // Returns the total compressed length so far.

      procedure BuildTree(var S: TDeflateState; var Descriptor: TTreeDescriptor);
      // Constructs a Huffman tree and assigns the code bit strings and lengths.
      // Updates the total bit length for the current block. The field Frequency must be set for all tree elements on entry.
      //
      // result: the fields Len and Code are set to the optimal bit length and corresponding Code. The length OptimalLength
      // is updated; StaticLength is also updated if STree is not nil. The field MaxCode is set.

        procedure GenerateCodes(Tree: PTree; MaxCode: integer; const BitLengthCounts: array of word);
        // Generates the codes for a given tree and bit counts (which need not be optimal).
        // The array BitLengthCounts contains the bit length statistics for the given tree and the field Len is set for all
        // Tree elements. MaxCode is the largest code with non zero frequency and BitLengthCounts are the number of codes at
        // each bit length.
        // On exit the field code is set for all tree elements of non zero code length.

          function BitReverse(Code: word; Len: integer) : word;
          // Reverses the first Len bits of Code, using straightforward code (a faster
          // imMethod would use a table)
          begin
            result := 0;
            repeat
              result := result or (Code and 1);
              Code := Code shr 1;
              result := result shl 1;
              Dec(Len);
            until Len <= 0;
            result := result shr 1;
          end;

        var NextCode : array [0..MAX_BITS] of word;  // next code value for each bit length
            Code     : word;     // running code value
            Bits     : integer;  // bit Index
            N        : integer;  // code Index
            Len      : integer;
        begin
          Code := 0;

          // The distribution counts are first used to generate the code values without bit reversal.
          for Bits := 1 to MAX_BITS do
          begin
            Code := (Code + BitLengthCounts[Bits - 1]) shl 1;
            NextCode[Bits] := Code;
          end;

          // Check that the bit counts in BitLengthCounts are consistent. The last code must be all ones.
          for N := 0 to MaxCode do
          begin
            Len := Tree[N].dl.Len;
            if Len = 0 then
              Continue;
            Tree[N].fc.Code := BitReverse(NextCode[Len], Len);
            Inc(NextCode[Len]);
          end;
        end;

        procedure RestoreHeap(var S: TDeflateState; const Tree: TTree; K: integer);
        // Restores the heap property by moving down tree starting at node K,
        // exchanging a Node with the smallest of its two sons if necessary, stopping
        // when the heap property is re-established (each father smaller than its two sons).
        var V, J : integer;
        begin
          V := S.Heap[K];
          J := K shl 1;  // left son of K
          while J <= S.HeapLength do
          begin
            // set J to the smallest of the two sons:
            if (J < S.HeapLength) and
               ( (Tree[S.Heap[J + 1]].fc.Frequency < Tree[S.Heap[J]].fc.Frequency) or
                 ((Tree[S.Heap[J + 1]].fc.Frequency = Tree[S.Heap[J]].fc.Frequency) and (S.Depth[S.Heap[J + 1]] <= S.Depth[S.Heap[J]])) ) then
              Inc(J);

            // exit if V is smaller than both sons
            if ( (Tree[V].fc.Frequency < Tree[S.Heap[J]].fc.Frequency) or
                 ((Tree[V].fc.Frequency = Tree[S.Heap[J]].fc.Frequency) and (S.Depth[V] <= S.Depth[S.Heap[J]])) ) then
              break;

            // exchange V with the smallest son
            S.Heap[K] := S.Heap[J];
            K := J;

            // and xontinue down the tree, setting J to the left son of K
            J := J shl 1;
          end;
          S.Heap[K] := V;
        end;

        procedure GenerateBitLengths(var S: TDeflateState; var Descriptor: TTreeDescriptor);
        // Computes the optimal bit lengths for a tree and update the total bit length for the current block.
        // The fields Frequency and dad are set, Heap[HeapMaximum] and above are the tree nodes sorted by increasing frequency.
        //
        // result: The field Len is set to the optimal bit length, the array BitLengthCounts contains the frequencies for each
        // bit length. The length OptimalLength is updated. StaticLength is also updated if STree is not nil.
        var Tree      : PTree;
            MaxCode   : integer;
            STree     : PTree;
            Extra     : TPAInteger;
            Base      : integer;
            MaxLength : integer;
            H         : integer;    // heap Index
            N, M      : integer;    // iterate over the tree elements
            Bits      : word;       // bit length
            ExtraBits : integer;
            F         : word;       // frequency
            Overflow  : integer;    // number of elements with bit length too large

        begin
          Tree := Descriptor.DynamicTree;
          MaxCode := Descriptor.MaxCode;
          STree := Descriptor.StaticDescriptor.StaticTree;
          Extra := Descriptor.StaticDescriptor.ExtraBits;
          Base := Descriptor.StaticDescriptor.ExtraBase;
          MaxLength := Descriptor.StaticDescriptor.MaxLength;
          Overflow := 0;

          FillChar(S.BitLengthCounts, SizeOf(S.BitLengthCounts), 0);

          // in a first pass, compute the optimal bit lengths (which may overflow in the case of the bit length tree)
          Tree[S.Heap[S.HeapMaximum]].dl.Len := 0; // root of the heap

          for H := S.HeapMaximum + 1 to HEAP_SIZE - 1 do
          begin
            N := S.Heap[H];
            Bits := Tree[Tree[N].dl.Dad].dl.Len + 1;
            if Bits > MaxLength then
            begin
              Bits := MaxLength;
              Inc(Overflow);
            end;
            Tree[N].dl.Len := Bits;

            // overwrite Tree[N].dl.Dad which is no longer needed
            if N > MaxCode then
              Continue; // not a leaf node

            Inc(S.BitLengthCounts[Bits]);
            ExtraBits := 0;
            if N >= Base then
              ExtraBits := Extra[N - Base];
            F := Tree[N].fc.Frequency;
            Inc(S.OptimalLength, integer(F) * (Bits + ExtraBits));
            if Assigned(STree) then
              Inc(S.StaticLength, integer(F) * (STree[N].dl.Len + ExtraBits));
          end;
          // This happens for example on obj2 and pic of the Calgary corpus
          if Overflow = 0 then
            exit;

          // find the first bit length which could increase
          repeat
            Bits := MaxLength - 1;
            while S.BitLengthCounts[Bits] = 0 do
              Dec(Bits);
            // move one leaf down the tree
            Dec(S.BitLengthCounts[Bits]);
            // move one overflow item as its brother
            Inc(S.BitLengthCounts[Bits + 1], 2);
            // The brother of the overflow item also moves one step up,
            // but this does not affect BitLengthCounts[MaxLength]
            Dec(S.BitLengthCounts[MaxLength]);
            Dec(Overflow, 2);
          until Overflow <= 0;

          // Now recompute all bit lengths, scanning in increasing frequency.
          // H is still equal to HEAP_SIZE. (It is simpler to reconstruct all
          // lengths instead of fixing only the wrong ones. This idea is taken
          // from 'ar' written by Haruhiko Okumura.)
          H := HEAP_SIZE;
          for Bits := MaxLength downto 1 do
          begin
            N := S.BitLengthCounts[Bits];
            while (N <> 0) do
            begin
              Dec(H);
              M := S.Heap[H];
              if M > MaxCode then Continue;
              if Tree[M].dl.Len <> Bits then
              begin
                Inc(S.OptimalLength, (Bits - Tree[M].dl.Len) * Tree[M].fc.Frequency);
                Tree[M].dl.Len := word(Bits);
              end;
              Dec(N);
            end;
          end;
        end;

      var Tree     : PTree;
          STree    : PTree;
          Elements : integer;
          N, M     : integer;  // iterate over heap elements
          MaxCode  : integer;  // largest code with non zero frequency
          Node     : integer;  // new node being created
      begin
        Tree := Descriptor.DynamicTree;
        STree := Descriptor.StaticDescriptor.StaticTree;
        Elements := Descriptor.StaticDescriptor.Elements;
        MaxCode := -1;

        // Construct the initial Heap, with least frequent element in Heap[SMALLEST].
        // The sons of Heap[N] are Heap[2 * N] and Heap[2 * N + 1]. Heap[0] is not used.
        S.HeapLength := 0;
        S.HeapMaximum := HEAP_SIZE;

        for N := 0 to Elements - 1 do
          if Tree[N].fc.Frequency = 0 then
            Tree[N].dl.Len := 0
          else
          begin
            MaxCode := N;
            Inc(S.HeapLength);
            S.Heap[S.HeapLength] := N;
            S.Depth[N] := 0;
          end;

        // The pkzip format requires that at least one distance code exists and that at least one bit
        // should be sent even if there is only one possible code. So to avoid special checks later on we force at least
        // two codes of non zero frequency.
        while S.HeapLength < 2 do
        begin
          Inc(S.HeapLength);
          if MaxCode < 2 then
          begin
            Inc(MaxCode);
            S.Heap[S.HeapLength] := MaxCode;
            Node := MaxCode;
          end
          else
          begin
            S.Heap[S.HeapLength] := 0;
            Node := 0;
          end;
          Tree[Node].fc.Frequency := 1;
          S.Depth[Node] := 0;
          Dec(S.OptimalLength);
          if STree <> nil then
            Dec(S.StaticLength, STree[Node].dl.Len);
          // Node is 0 or 1 so it does not have extra bits
        end;
        Descriptor.MaxCode := MaxCode;

        // The elements Heap[HeapLength / 2 + 1 .. HeapLength] are leaves of the Tree,
        // establish sub-heaps of increasing lengths.
        for N := S.HeapLength div 2 downto 1 do
          RestoreHeap(S, Tree^, N);

        // construct the Huffman tree by repeatedly combining the least two frequent nodes
        Node := Elements; // next internal node of the tree
        repeat
          N := S.Heap[1];
          S.Heap[1] := S.Heap[S.HeapLength];
          Dec(S.HeapLength);
          RestoreHeap(S, Tree^, 1);

          // M := node of next least frequency
          M := S.Heap[1];
          Dec(S.HeapMaximum);
          // keep the nodes sorted by frequency
          S.Heap[S.HeapMaximum] := N;
          Dec(S.HeapMaximum);
          S.Heap[S.HeapMaximum] := M;

          // create a new node father of N and M
          Tree[Node].fc.Frequency := Tree[N].fc.Frequency + Tree[M].fc.Frequency;
          // maximum
          if S.Depth[N] >= S.Depth[M] then
            S.Depth[Node] := byte (S.Depth[N] + 1)
          else
            S.Depth[Node] := byte (S.Depth[M] + 1);

          Tree[M].dl.Dad := word(Node);
          Tree[N].dl.Dad := word(Node);
          // and insert the new node in the heap
          S.Heap[1] := Node;
          Inc(Node);
          RestoreHeap(S, Tree^, 1);
        until S.HeapLength < 2;

        Dec(S.HeapMaximum);
        S.Heap[S.HeapMaximum] := S.Heap[1];

        // At this point the fields Frequency and dad are set. We can now generate the bit lengths.
        GenerateBitLengths(S, Descriptor);

        // The field Len is now set, we can generate the bit codes
        GenerateCodes(Tree, MaxCode, S.BitLengthCounts);
      end;

      procedure BitsWindup(var S: TDeflateState);
      // flushs the bit buffer and aligns the output on a byte boundary
      begin
        if S.ValidBits > 8 then
        begin
          S.PendingBuffer[S.Pending] := byte(S.BitsBuffer and $FF);
          Inc(S.Pending);
          S.PendingBuffer[S.Pending] := byte(word(S.BitsBuffer) shr 8);;
          Inc(S.Pending);
        end
        else
          if S.ValidBits > 0 then
          begin
            S.PendingBuffer[S.Pending] := byte(S.BitsBuffer);
            Inc(S.Pending);
          end;

        S.BitsBuffer := 0;
        S.ValidBits := 0;
      end;

      procedure SendBits(var S: TDeflateState; Value: word; Length: integer);
      // Value contains what is to be sent
      // Length is the number of bits to send
      var c1 : dword;
      begin
        // If there's not enough room in BitsBuffer use (valid) bits from BitsBuffer and
        // (16 - ValidBits) bits from Value, leaving (width - (16 - ValidBits)) unused bits in Value.
        {$ifopt Q+} {$Q-} {$define OverflowCheck} {$endif}
        {$ifopt R+} {$R-} {$define RangeCheck} {$endif}
        if S.ValidBits > integer(BufferSize) - Length then
        begin
          c1 := Value shl S.ValidBits;
          S.BitsBuffer := S.BitsBuffer or c1;
          S.PendingBuffer[S.Pending] := S.BitsBuffer and $FF;
          Inc(S.Pending);
          S.PendingBuffer[S.Pending] := S.BitsBuffer shr 8;
          Inc(S.Pending);

          S.BitsBuffer := Value shr (BufferSize - S.ValidBits);
          Inc(S.ValidBits, Length - BufferSize);
        end
        else
        begin
          S.BitsBuffer := S.BitsBuffer or (Value shl S.ValidBits);
          Inc(S.ValidBits, Length);
        end;
        {$ifdef OverflowCheck} {$Q+} {$undef OverflowCheck} {$endif}
        {$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
      end;

      procedure SendAllTrees(var S: TDeflateState; lcodes, dcodes, blcodes: integer);
      // Sends the header for a block using dynamic Huffman trees: the counts, the
      // lengths of the bit length codes, the literal tree and the distance tree.
      // lcodes must be >= 257, dcodes >= 1 and blcodes >= 4

        procedure SendTree(var S: TDeflateState; const Tree: array of TTreeEntry; MaxCode: integer);
        // Sends the given tree in compressed form using the codes in BitLengthTree.
        // MaxCode is the tree's largest code of non zero frequency.
        var N           : integer;  // iterates over all tree elements
            PreviousLen : integer;  // last emitted length
            CurrentLen  : integer;  // length of current code
            NextLen     : integer;  // length of next code
            Count       : integer;  // repeat count of the current code
            MaxCount    : integer;  // max repeat count
            MinCount    : integer;  // min repeat count
        begin
          PreviousLen := -1;
          NextLen := Tree[0].dl.Len;
          Count := 0;
          MaxCount := 7;
          MinCount := 4;

          // guard is already set
          if NextLen = 0 then
          begin
            MaxCount := 138;
            MinCount := 3;
          end;

          for N := 0 to MaxCode do
          begin
            CurrentLen := NextLen;
            NextLen := Tree[N + 1].dl.Len;
            Inc(Count);
            if (Count < MaxCount) and (CurrentLen = NextLen) then
              Continue
            else
              if Count < MinCount then
              begin
                repeat
                  SendBits(S, S.BitLengthTree[CurrentLen].fc.Code, S.BitLengthTree[CurrentLen].dl.Len);
                  Dec(Count);
                until Count = 0;
              end
              else
                if CurrentLen <> 0 then
                begin
                  if CurrentLen <> PreviousLen then
                  begin
                    SendBits(S, S.BitLengthTree[CurrentLen].fc.Code, S.BitLengthTree[CurrentLen].dl.Len);
                    Dec(Count);
                  end;
                  SendBits(S, S.BitLengthTree[REP_3_6].fc.Code, S.BitLengthTree[REP_3_6].dl.Len);
                  SendBits(S, Count - 3, 2);
                end
                else
                  if Count <= 10 then
                  begin
                    SendBits(S, S.BitLengthTree[REPZ_3_10].fc.Code, S.BitLengthTree[REPZ_3_10].dl.Len);
                    SendBits(S, Count - 3, 3);
                  end
                  else
                  begin
                    SendBits(S, S.BitLengthTree[REPZ_11_138].fc.Code, S.BitLengthTree[REPZ_11_138].dl.Len);
                    SendBits(S, Count - 11, 7);
                  end;
            Count := 0;
            PreviousLen := CurrentLen;
            if NextLen = 0 then
            begin
              MaxCount := 138;
              MinCount := 3;
            end
            else
              if CurrentLen = NextLen then
              begin
                MaxCount := 6;
                MinCount := 3;
              end
              else
              begin
                MaxCount := 7;
                MinCount := 4;
              end;
          end;
        end;

      var Rank: integer;
      begin
        SendBits(S, lcodes - 257, 5); // not +255 as stated in appnote.txt
        SendBits(S, dcodes - 1,   5);
        SendBits(S, blcodes - 4,  4); // not -3 as stated in appnote.txt

        for Rank := 0 to blcodes - 1 do
          SendBits(S, S.BitLengthTree[BitLengthOrder[Rank]].dl.Len, 3);
        SendTree(S, S.LiteralTree,  lcodes - 1);
        SendTree(S, S.DistanceTree, dcodes - 1);
      end;

      function BuildBitLengthTree(var S: TDeflateState) : integer;
      // Constructs the Huffman tree for the bit lengths and returns the Index in BitLengthOrder
      // of the last bit length code to send.

        procedure ScanTree(var S: TDeflateState; var Tree: array of TTreeEntry; MaxCode: integer);
        // Scans a given tree to determine the frequencies of the codes in the bit length tree.
        // MaxCode is the tree's largest code of non zero frequency.
        var N           : integer;  // iterates over all tree elements
            PreviousLen : integer;  // last emitted length
            CurrentLen  : integer;  // Length of current code
            NextLen     : integer;  // length of next code
            Count       : integer;  // repeat count of the current xode
            MaxCount    : integer;  // max repeat count
            MinCount    : integer;  // min repeat count

        begin
          PreviousLen := -1;
          NextLen := Tree[0].dl.Len;
          Count := 0;
          MaxCount := 7;
          MinCount := 4;

          if NextLen = 0 then
          begin
            MaxCount := 138;
            MinCount := 3;
          end;
          Tree[MaxCode + 1].dl.Len := word($FFFF); // guard

          for N := 0 to MaxCode do
          begin
            CurrentLen := NextLen;
            NextLen := Tree[N + 1].dl.Len;
            Inc(Count);
            if (Count < MaxCount) and (CurrentLen = NextLen) then
              Continue
            else
              if Count < MinCount then
                Inc(S.BitLengthTree[CurrentLen].fc.Frequency, Count)
              else
                if CurrentLen <> 0 then
                begin
                  if (CurrentLen <> PreviousLen) then Inc(S.BitLengthTree[CurrentLen].fc.Frequency);
                  Inc(S.BitLengthTree[REP_3_6].fc.Frequency);
                end
                else
                  if Count <= 10 then
                    Inc(S.BitLengthTree[REPZ_3_10].fc.Frequency)
                  else
                    Inc(S.BitLengthTree[REPZ_11_138].fc.Frequency);
            Count := 0;
            PreviousLen := CurrentLen;
            if NextLen = 0 then
            begin
              MaxCount := 138;
              MinCount := 3;
            end
            else
              if CurrentLen = NextLen then
              begin
                MaxCount := 6;
                MinCount := 3;
              end
              else
              begin
                MaxCount := 7;
                MinCount := 4;
              end;
          end;
        end;

      begin
        // determine the bit length frequencies for literal and distance trees
        ScanTree(S, S.LiteralTree, S.LiteralDescriptor.MaxCode);
        ScanTree(S, S.DistanceTree, S.DistanceDescriptor.MaxCode);

        // build the bit length tree
        BuildTree(S, S.BitLengthDescriptor);
        // OptimalLength now includes the length of the tree representations, except
        // the lengths of the bit lengths codes and the 5 + 5 + 4 (= 14) bits for the counts.

        // Determine the number of bit length codes to send. The pkzip format requires that at least 4 bit length codes
        // be sent. (appnote.txt says 3 but the actual value used is 4.)
        for result := BL_CODES - 1 downto 3 do
          if S.BitLengthTree[BitLengthOrder[result]].dl.Len <> 0 then
            break;

        // update OptimalLength to include the bit length tree and counts
        Inc(S.OptimalLength, 3 * (result + 1) + 14);
      end;

      procedure TreeStroredBlock(var S: TDeflateState; Buffer: PByte; StoredLength: integer; EOF: boolean);
      // sends a stored block
      // Buffer contains the input data, Len the buffer length and EOF is True if this is the last block for a file.

        procedure CopyBlock(var S: TDeflateState; Buffer: PByte; Len: cardinal; Header: boolean);
        // copies a stored block, storing first the length and its one's complement if requested
        // Buffer contains the input data, Len the buffer length and Header is True if the block Header must be written too.
        begin
          BitsWindup(S);        // align on byte boundary
          S.LastEOBLength := 8; // enough lookahead for Inflate

          if Header then
          begin
            S.PendingBuffer[S.Pending] := byte(word(Len) and $FF);
            Inc(S.Pending);
            S.PendingBuffer[S.Pending] := byte(word(Len) shr 8);
            Inc(S.Pending);
            S.PendingBuffer[S.Pending] := byte(word(not Len) and $FF);
            Inc(S.Pending);
            S.PendingBuffer[S.Pending] := byte(word(not Len) shr 8);
            Inc(S.Pending);
          end;

          while Len > 0 do
          begin
            Dec(Len);
            S.PendingBuffer[S.Pending] := Buffer^;
            Inc(Buffer);
            Inc(S.Pending);
          end;
        end;

      begin
        SendBits(S, (STORED_BLOCK shl 1) + Ord(EOF), 3);  // send block type
        S.CompressedLength := (S.CompressedLength + 10) and integer(not 7);
        Inc(S.CompressedLength, (StoredLength + 4) shl 3);

        // copy with header
        CopyBlock(S, Buffer, cardinal(StoredLength), True);
      end;

      procedure CompressBlock(var S: TDeflateState; const LiteralTree, DistanceTree: array of TTreeEntry);
      // sends the block data compressed using the given Huffman trees
      var Distance : cardinal;  // distance of matched string
          lc       : integer;   // match length or unmatched char (if Distance = 0)
          I        : cardinal;
          Code     : cardinal;  // the code to send
          Extra    : integer;   // number of extra bits to send
      begin
        I := 0;
        if S.LastLiteral <> 0 then
        repeat
          Distance := S.DistanceBuffer[I];
          lc := S.LiteralBuffer[I];
          Inc(I);
          if Distance = 0 then
          begin
            // send a literal byte
            SendBits(S, LiteralTree[lc].fc.Code, LiteralTree[lc].dl.Len);
          end
          else
          begin
            // Here, lc is the match length - MIN_MATCH
            Code := LengthCode[lc];
            // send the length code 
            SendBits(S, LiteralTree[Code + LITERALS + 1].fc.Code, LiteralTree[Code + LITERALS + 1].dl.Len);
            Extra := ExtraLengthBits[Code];
            if Extra <> 0 then
            begin
              Dec(lc, BaseLength[Code]);
              // send the extra length bits
              SendBits(S, lc, Extra);
            end;
            Dec(Distance); // Distance is now the match distance - 1
            if Distance < 256 then
              Code := DistanceCode[Distance]
            else
              Code := DistanceCode[256 + (Distance shr 7)];

            // send the distance code
            SendBits(S, DistanceTree[Code].fc.Code, DistanceTree[Code].dl.Len);
            Extra := ExtraDistanceBits[Code];
            if Extra <> 0 then
            begin
              Dec(Distance, BaseDistance[Code]);
              SendBits(S, Distance, Extra);   // send the extra distance bits
            end;
          end; // literal or match pair?

          // Check that the overlay between PendingBuffer and DistanceBuffer + LiteralBuffer is ok
        until I >= S.LastLiteral;

        SendBits(S, LiteralTree[END_BLOCK].fc.Code, LiteralTree[END_BLOCK].dl.Len);
        S.LastEOBLength := LiteralTree[END_BLOCK].dl.Len;
      end;

    var OptimalByteLength : integer;
        StaticByteLength  : integer;  // OptimalLength and StaticLength in bytes
        MacBLIndex        : integer;  // index of last bit length code of non zero frequency
    begin
      // construct the literal and distance trees
      // After this, OptimalLength and StaticLength are the total bit lengths of
      // the compressed block data, excluding the tree representations.
      BuildTree(S, S.LiteralDescriptor);
      BuildTree(S, S.DistanceDescriptor);

      // Build the bit length tree for the above two trees and get the index
      // in BitLengthOrder of the last bit length code to send.
      MacBLIndex := BuildBitLengthTree(S);

      // determine the best encoding, compute first the block length in bytes
      OptimalByteLength := (S.OptimalLength + 10) shr 3;
      StaticByteLength := (S.StaticLength + 10) shr 3;
      if StaticByteLength <= OptimalByteLength then
        OptimalByteLength := StaticByteLength;

      // if Iompression failed and this is the first and last block,
      // and if the .zip file can be seeked (to rewrite the local header),
      // the whole file is transformed into a stored file.
      // (4 are the two words for the lengths)
      if (StoredLength + 4 <= OptimalByteLength) and Assigned(Buffer) then
      begin
        // The test Buffer <> nil is only necessary if LiteralBufferSize > WSize.
        // Otherwise we can't have processed more than WSize input bytes since
        // the last block dlush, because compression would have been successful.
        // if LiteralBufferSize <= WSize, it is never too late to transform a block into a stored block.
        TreeStroredBlock(S, Buffer, StoredLength, EOF);
      end
      else
        if StaticByteLength = OptimalByteLength then
        begin
          // force static trees
          SendBits(S, (STATIC_TREES shl 1) + Ord(EOF), 3);
          CompressBlock(S, StaticLiteralTree, StaticDescriptorTree);
          Inc(S.CompressedLength, 3 + S.StaticLength);
        end
        else
        begin
          SendBits(S, (DYN_TREES shl 1) + Ord(EOF), 3);
          SendAllTrees(S, S.LiteralDescriptor.MaxCode + 1, S.DistanceDescriptor.MaxCode + 1, MacBLIndex + 1);
          CompressBlock(S, S.LiteralTree, S.DistanceTree);
          Inc(S.CompressedLength, 3 + S.OptimalLength);
        end;
      InitializeBlock(S);

      if EOF then
      begin
        BitsWindup(S);
        // align on byte boundary
        Inc(S.CompressedLength, 7);
      end;

      result := S.CompressedLength shr 3;
    end;

  begin
    if S.BlockStart >= 0 then
      TreeFlushBlock(S, @S.Window[cardinal(S.BlockStart)], integer(S.StringStart) - S.BlockStart, EOF)
    else
      TreeFlushBlock(S, nil, integer(S.StringStart) - S.BlockStart, EOF);

    S.BlockStart := S.StringStart;
    FlushPending(S.ZState^);
  end;

  function TreeTally(var S: TDeflateState; Distance: cardinal; lc: cardinal) : boolean;
  // Saves the match info and tallies the frequency counts. Returns True if the current block must be flushed.
  // Distance is the distance of the matched string and lc either match length minus MIN_MATCH or the unmatch character
  // (if Distance = 0).
  var Code : word;
  begin
    S.DistanceBuffer[S.LastLiteral] := word(Distance);
    S.LiteralBuffer[S.LastLiteral] := byte(lc);
    Inc(S.LastLiteral);
    if Distance = 0 then
    begin
      // lc is the unmatched char
      Inc(S.LiteralTree[lc].fc.Frequency);
    end
    else
    begin
      Inc(S.Matches);
      // here, lc is the match length - MIN_MATCH
      Dec(Distance);
      if Distance < 256 then
        Code := DistanceCode[Distance]
      else
        Code := DistanceCode[256 + (Distance shr 7)];
      Inc(S.LiteralTree[LengthCode[lc] + LITERALS + 1].fc.Frequency);
      Inc(S.DistanceTree[Code].fc.Frequency);
    end;

    result := (S.LastLiteral = S.LiteralBufferSize - 1);
    // We avoid equality with LiteralBufferSize because stored blocks are restricted to 64K - 1 bytes.
  end;

  procedure InsertString(var S: TDeflateState; Str: cardinal; var MatchHead: cardinal);
  // Inserts Str into the dictionary and sets MatchHead to the previous head of the hash chain (the most recent string
  // with same hash key). All calls to to InsertString are made with consecutive input characters and the first MIN_MATCH
  // bytes of Str are valid (except for the last MIN_MATCH - 1 bytes of the input file).
  // Returns the previous length of the hash chain.
  begin
    S.InsertHash := ((S.InsertHash shl S.HashShift) xor (S.Window[(Str) + (MIN_MATCH - 1)])) and S.HashMask;

    MatchHead := S.Head[S.InsertHash];
    S.Previous[(Str) and S.WindowMask] := MatchHead;
    S.Head[S.InsertHash] := word(Str);
  end;

const CMaxInsertLen = 5;
var z          : TZState;
    s          : PDeflateState;
    Overlay    : TPAWord;
    // We overlay PendingBuffer and DistanceBuffer + LiteralBuffer. This works since the average
    // output size for (length, distance) codes is <= 24 Bits.
    HashHead   : cardinal;  // head of the hash chain
    BlockFlush : boolean;   // set if current block must be flushed
begin
  result := 0;
  srcBuf := nil;
  dstBuf := nil;
  ZeroMemory(@z, sizeOf(z));
  z.NextInput       := src;
  z.AvailableInput  := srcLen;
  z.NextOutput      := dst;
  z.AvailableOutput := dstLen;
  z.TotalOutput     := 0;
  z.TotalInput      := 0;
  if crc <> nil then
    crc^ := $ffffffff;
  CheckSrcMap(z);
  CheckDstMap(z);
  GetMem(s, SizeOf(TDeflateState));
  ZeroMemory(s, SizeOf(TDeflateState));
  try
    z.State  := pointer(s);
    s.ZState := @z;
    s.WindowSize := 1 shl CWindowBits;
    s.WindowMask := s.WindowSize - 1;
    s.HashBits   := CMemLevel + 7;
    s.HashSize   := 1 shl s.HashBits;
    s.HashMask   := s.HashSize - 1;
    s.HashShift  := (s.HashBits + MIN_MATCH - 1) div MIN_MATCH;
    GetMem(s.Window,   s.WindowSize * 2 * SizeOf(byte));
    GetMem(s.Previous, s.WindowSize *     SizeOf(word));
    GetMem(s.Head,     s.HashSize   *     SizeOf(word));
    s.LiteralBufferSize := 1 shl (CMemLevel + 6); // 16K elements by default
    Overlay := VirtualAlloc(nil, s.LiteralBufferSize * (SizeOf(word) + 2), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    s.PendingBuffer := TPAByte(Overlay);
    s.PendingBufferSize := s.LiteralBufferSize * (SizeOf(word) + 2);
    s.DistanceBuffer := @Overlay[s.LiteralBufferSize div SizeOf(word)];
    s.LiteralBuffer := @s.PendingBuffer[(1 + SizeOf(word)) * s.LiteralBufferSize];
    s.Pending := 0;
    s.PendingOutput := PByte(s.PendingBuffer);
    s.CompressedLength := 0;
    s.LiteralDescriptor.DynamicTree := @s.LiteralTree;
    s.LiteralDescriptor.StaticDescriptor := @StaticLiteralDescriptor;
    s.DistanceDescriptor.DynamicTree := @s.DistanceTree;
    s.DistanceDescriptor.StaticDescriptor := @StaticDistanceDescriptor;
    s.BitLengthDescriptor.DynamicTree := @s.BitLengthTree;
    s.BitLengthDescriptor.StaticDescriptor := @StaticBitLengthDescriptor;
    s.BitsBuffer := 0;
    s.ValidBits := 0;
    s.LastEOBLength := 8; // enough Lookahead for Inflate
    InitializeBlock(s^);
    s.CurrentWindowSize := 2 * s.WindowSize;
    s.Head[s.HashSize - 1] := 0;
    ZeroMemory(s.Head, (s.HashSize - 1) * SizeOf(s.Head[0]));
    s.StringStart := 0;
    s.BlockStart := 0;
    s.Lookahead := 0;
    s.PreviousLength := MIN_MATCH - 1;
    s.MatchLength := MIN_MATCH - 1;
    s.MatchAvailable := false;
    s.InsertHash := 0;
    HashHead := 0;
    while true do begin
      // Make sure that we always have enough lookahead, except at the end of the input file. We need MAX_MATCH bytes
      // for the next match plus MIN_MATCH bytes to insert the string following the next match.
      if s.Lookahead < MIN_LOOKAHEAD then begin
        FillWindow(s^);
        // flush the current block
        if s.Lookahead = 0 then begin
          FlushBlockOnly(S^, true);
          if z.AvailableOutput <> 0 then
            result := z.TotalOutput - dword(outOffset);
          break;
        end;
      end;

      // Insert the string Window[StringStart .. StringStart + 2] in the
      // dictionary and set HashHead to the head of the hash chain.
      if s.Lookahead >= MIN_MATCH then
        InsertString(s^, s.StringStart, HashHead);

      // Find the longest match, discarding those <= PreviousLength.
      // At this point we have always MatchLength < MIN_MATCH.
      if (HashHead <> 0) and (s.StringStart - HashHead <= (s.WindowSize - MIN_LOOKAHEAD)) then
        s.MatchLength := LongestMatch(s^, HashHead);
      if s.MatchLength >= MIN_MATCH then begin
        BlockFlush := TreeTally(s^, s.StringStart - s.MatchStart, s.MatchLength - MIN_MATCH);
        Dec(s.Lookahead, s.MatchLength);

        // Insert new strings in the hash table only if the match length
        // is not too large. This saves time but degrades compression.
        if (s.MatchLength <= CMaxInsertLen) and (s.Lookahead >= MIN_MATCH) then begin
          // string at StringStart already in hash table
          Dec(s.MatchLength);
          repeat
            Inc(s.StringStart);
            InsertString(s^, s.StringStart, HashHead);
            // StringStart never exceeds WSize - MAX_MATCH, so there are always MIN_MATCH bytes ahead.
            Dec(s.MatchLength);
          until s.MatchLength = 0;
          Inc(s.StringStart);
        end else begin
          Inc(s.StringStart, s.MatchLength);
          s.MatchLength := 0;
          s.InsertHash := s.Window[s.StringStart];
          s.InsertHash := ((s.InsertHash shl s.HashShift) xor s.Window[s.StringStart + 1]) and s.HashMask;

          // if Lookahead < MIN_MATCH, InsertHash is garbage, but it does not
          // matter since it will be recomputed at next Deflate call.
        end;
      end else begin
        // no match, output a literal byte
        BlockFlush := TreeTally(s^, 0, s.Window[s.StringStart]);
        Dec(s.Lookahead);
        Inc(s.StringStart);
      end;
      if BlockFlush then
        FlushBlockOnly(s^, False);
    end;
    FlushDstFile(z);
  except result := 0 end;
  if (srcMap <> 0) and (src <> nil) then
    UnmapViewOfFile(src);
  if (dstMap <> 0) and (dst <> nil) then
    UnmapViewOfFile(dst);
  VirtualFree(s.PendingBuffer, 0, MEM_RELEASE);
  FreeMem(s.Head);
  FreeMem(s.Previous);
  FreeMem(s.Window);
  FreeMem(s);
  if srcBuf <> nil then
    VirtualFree(srcBuf, 0, MEM_RELEASE);
  if dstBuf <> nil then
    VirtualFree(dstBuf, 0, MEM_RELEASE);
  if crc <> nil then
    crc^ := not crc^;
end;

const
  // number of hufts used by fixed tables
  FIXEDH = 544;

var
  FixedTablesMemory  : array [0..FIXEDH - 1] of TInflateHuft;
  FixedLiteralBits   : cardinal;
  FixedDistanceBits  : cardinal;
  FixedLiteralTable  : PInflateHuft;
  FixedDistanceTable : PInflateHuft;

//----------------------------------------------------------------------------------------------------------------------

function UncompressEx(srcFile, dstFile, srcMap, dstMap: THandle; inOffset: integer;
                      src, dst: pointer; srcLen, dstLen: int64; crc: TPCardinal = nil) : int64;
var srcBuf, dstBuf : pointer;

  procedure CheckSrcMap(var z: TZState);
  var c1 : dword;
  begin
    if (srcFile <> 0) and ((src = nil) or (z.AvailableInput = 0)) then begin
      if (srcMap <> 0) and (src <> nil) then begin
        UnmapViewOfFile(src);
        src := nil;
      end;
      z.AvailableInput := Min(CMapSize, srcLen - z.TotalInput);
      if z.AvailableInput <> 0 then begin
        if srcMap = 0 then begin
          if srcBuf = nil then
            srcBuf := VirtualAlloc(nil, CMapSize, MEM_COMMIT, PAGE_READWRITE);
          if SetFilePointer2(srcFile, z.TotalInput, FILE_BEGIN) and ReadFile(srcFile, srcBuf^, z.AvailableInput, c1, nil) and (c1 = z.AvailableInput) then
            src := srcBuf
          else
            src := nil;
        end else
          src := MapViewOfFile2(srcMap, FILE_MAP_READ, z.TotalInput, z.AvailableInput);
        z.NextInput := src;
        if z.TotalInput = 0 then begin
          z.TotalInput := inOffset;
          inc(NativeUInt(z.NextInput), inOffset);
          dec(z.AvailableInput, inOffset);
        end;
      end;
    end;
  end;

  procedure FlushDstFile(var z: TZState);
  var c1, c2 : dword;
  begin
    if z.TotalOutput > 0 then begin
      c1 := z.TotalOutput mod CMapSize;
      if c1 = 0 then
        c1 := CMapSize;
      if (dst <> nil) and (crc <> nil) then
        crc^ := updateCrc32(crc^, dst^, c1);
      if (dstFile <> 0) and (dstMap = 0) and ((not WriteFile(dstFile, dstBuf^, c1, c2, nil)) or (c1 <> c2)) then
        raise MadException.Create('Stream write error.');
    end;
  end;

  procedure CheckDstMap(var z: TZState);
  begin
    if (dstFile <> 0) and ((dst = nil) or (z.AvailableOutput = 0)) then begin
      FlushDstFile(z);
      if (dstMap <> 0) and (dst <> nil) then begin
        UnmapViewOfFile(dst);
        dst := nil;
      end;
      z.AvailableOutput := Min(CMapSize, dstLen - z.TotalOutput);
      if z.AvailableOutput <> 0 then begin
        if dstMap = 0 then begin
          if dstBuf = nil then
            dstBuf := VirtualAlloc(nil, CMapSize, MEM_COMMIT, PAGE_READWRITE);
          dst := dstBuf;
        end else
          dst := MapViewOfFile2(dstMap, FILE_MAP_ALL_ACCESS, z.TotalOutput, z.AvailableOutput);
        z.NextOutput := dst;
      end;
    end;
  end;

  const
    // Maximum Size of dynamic tree. The maximum found in an integer but non-exhaustive search was 1004 huft structures
    // (850 for length/literals and 154 for distances, the latter actually the result of an exhaustive search).
    // The actual maximum is not known, but the value below is more than safe.
    MANY = 1440;

    // Tables for deflate from PKZIP'S appnote.txt
    // copy lengths for literal codes 257..285 (actually lengths - 2; also see note #13 above about 258)
    CopyLengths : array [0..30] of cardinal =
    (
      3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35,
      43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0
    );

    INVALID_CODE = 112;
    // extra bits for literal codes 257..285
    CopyLiteralExtra : array [0..30] of cardinal =
    (
      0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
      3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, INVALID_CODE, INVALID_CODE
    );

    // copy offsets for distance codes 0..29
    CopyOffsets : array [0..29] of cardinal =
    (
      1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385,
      513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
    );

    // extra bits for distance codes
    CopyExtra : array [0..29] of cardinal =
    (
      0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7,
      7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13
    );

    // Huffman code decoding is performed using a multi-Level table lookup.
    // Fastest way to decode is to simply build a lookup table whose
    // size is determined by the longest code. However, the time it takes
    // to build this table can also be a factor if the data being decoded
    // is not very integer. The most common codes are necessarily the
    // shortest codes so those codes dominate the decoding time and hence
    // the speed. The idea is you can have a shorter table that decodes the
    // shorter, More probable codes, and then point to subsidiary tables for
    // the longer codes. The time it costs to decode the longer codes is
    // then traded against the time it takes to make longer tables.
    //
    // This results of this trade are in the variables LiteralTreeBits and DistanceTreeBits
    // below. LiteralTreeBits is the number of bits the first level table for literal/
    // length codes can decode in one step, and DistanceTreeBits is the same thing for
    // the distance codes. Subsequent tables are also less than or equal to those sizes.
    // These values may be adjusted either when all of the
    // codes are shorter than that, in which case the longest code length in
    // bits is used, or when the shortest code is *longer* than the requested
    // table size, in which case the length of the shortest code in bits is used.
    //
    // There are two different values for the two tables, since they code a
    // different number of possibilities each. The literal/length table
    // codes 286 possible values, or in a flat code, a little over eight
    // bits. The distance table codes 30 possible values, or a little less
    // than five bits, flat. The optimum values for speed end up being
    // about one bit more than those, so LiteralTreeBits is 8 + 1 and DistanceTreeBits is 5 + 1.
    // The optimum values may differ though from machine to machine, and possibly even between compilers.

  const
    // maximum bit length of any code,
    // If BMAX needs to be larger than 16, then H and X[] should be cardinal.
    BMAX = 15;

  function InflateBlocks(var S: TInflateBlocksState; var Z: TZState; R: integer) : integer;

    const
      InflateMask : array [0..16] of cardinal =
      (
        $0000, $0001, $0003, $0007, $000F, $001F, $003F, $007F, $00FF,
        $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF, $FFFF
      );

    function InflateFlush(var S: TInflateBlocksState; var Z: TZState; R: integer) : integer;
    // copies as much as possible from the sliding window to the output area
    var N : cardinal;
        P : PByte;
        Q : PByte;

      procedure Store;
      begin
        if N > Z.AvailableOutput then begin

          if Z.AvailableOutput > 0 then begin
            if R = Z_BUF_ERROR then
              R := Z_OK;

            // copy as far as end of Window
            Move(Q^, P^, Z.AvailableOutput);
            Inc(P, Z.AvailableOutput);
            Inc(Q, Z.AvailableOutput);

            // update counters
            Inc(Z.TotalOutput, Z.AvailableOutput);
            Dec(N, Z.AvailableOutput);
            Z.AvailableOutput := 0;
          end;

          CheckDstMap(Z);
          P := Z.NextOutput;
          if N > Z.AvailableOutput then
            N := Z.AvailableOutput;
        end;

        if (N <> 0) and (R = Z_BUF_ERROR) then
          R := Z_OK;

        // update counters
        Dec(Z.AvailableOutput, N);
        Inc(Z.TotalOutput, N);

        // copy as far as end of Window
        Move(Q^, P^, N);
        Inc(P, N);
        Inc(Q, N);
      end;

    begin
      // local copies of source and destination pointers
      P := Z.NextOutput;
      Q := S.Read;

      // compute number of bytes to copy as far as end of window
      if NativeUInt(Q) <= NativeUInt(S.Write) then
        N := NativeUInt(S.Write) - NativeUInt(Q)
      else
        N := NativeUInt(S.zend ) - NativeUInt(Q);
      Store;

      // see if more to copy at beginning of window
      if Q = S.zend then begin
        // wrap pointers
        Q := S.Window;
        if S.write = S.zend then
          S.write := S.Window;

        // compute bytes to copy
        N := NativeUInt(S.write) - NativeUInt(Q);
        Store;
      end;

      // update pointers
      Z.NextOutput := P;
      S.Read := Q;

      result := R;
    end;

    //----------------------------------------------------------------------------------------------------------------------

    function InflateFast(LiteralBits, DistanceBits: cardinal; TL, TD: PInflateHuft; var S: TInflateBlocksState; var Z: TZState) : integer;
    // Called with number of bytes left to write in window at least 258 (the maximum string length) and number of input
    // bytes available at least ten. The ten bytes are six bytes for the longest length/distance pair plus four bytes for
    // overloading the bit buffer.
    var Temp  : PInflateHuft;
        Extra : cardinal;   // extra bits or operation
        B     : cardinal;
        K     : cardinal;   // bits in bit buffer
        P     : PByte;      // input data pointer
        N     : cardinal;   // bytes available there
        Q     : PByte;      // output window write pointer
        M     : cardinal;   // bytes to end of window or read pointer
        ml    : cardinal;   // mask for literal/length tree
        md    : cardinal;   // mask for distance tree
        C     : cardinal;   // bytes to copy
        D     : cardinal;   // distance back to copy from
        R     : PByte;      // copy source pointer

      procedure UpdatePointers;
      begin
        C := Z.AvailableInput - N;
        if (K shr 3) < C then
          C := K shr 3;
        Inc(N, C);
        Dec(P, C);
        Dec(K, C shl 3);
        S.bitb := B;
        S.bitk := K;
        Z.AvailableInput := N;
        Inc(Z.TotalInput, NativeUInt(P) - NativeUInt(Z.NextInput));
        Z.NextInput := P;
        S.write := Q;
      end;

    begin
      // load input, output, bit values
      P := Z.NextInput;
      N := Z.AvailableInput;
      B := S.bitb;
      K := S.bitk;
      Q := S.write;
      if NativeUInt(Q) < NativeUInt(S.Read) then
        M := NativeUInt(S.read) - NativeUInt(Q) - 1
      else
        M := NativeUInt(S.zend) - NativeUInt(Q);

      // initialize masks
      ml := InflateMask[LiteralBits];
      md := InflateMask[DistanceBits];

      // do until not enough input or output space for fast loop,
      // assume called with (M >= 258) and (N >= 10)
      repeat
        // get literal/length Code
        while K < 20 do begin
          Dec(N);
          B := B or (cardinal(P^) shl K);
          Inc(P);
          Inc(K, 8);
        end;

        Temp := @PHuftField(TL)[B and ml];

        Extra := Temp.exop;
        if Extra = 0 then begin
          B := B shr Temp.Bits;
          Dec(K, Temp.Bits);
          Q^ := Temp.Base;
          Inc(Q);
          Dec(M);
          Continue;
        end;

        repeat
          B := B shr Temp.Bits;
          Dec(K, Temp.Bits);

          if (Extra and 16) <> 0 then begin
            // get extra bits for length
            Extra := Extra and 15;
            C := Temp.Base + (B and InflateMask[Extra]);
            B := B shr Extra;
            Dec(K, Extra);
            // decode distance base of block to copy
            while K < 15 do begin
              Dec(N);
              B := B or (cardinal(P^) shl K);
              Inc(P);
              Inc(K, 8);
            end;

            Temp := @PHuftField(TD)[B and md];
            Extra := Temp.exop;
            repeat
              B := B shr Temp.Bits;
              Dec(K, Temp.Bits);

              if (Extra and 16) <> 0 then begin
                // get extra bits to add to distance base
                Extra := Extra and 15;
                while K < Extra do begin
                  Dec(N);
                  B := B or (cardinal(P^) shl K);
                  Inc(P);
                  Inc(K, 8);
                end;

                D := Temp.Base + (B and InflateMask[Extra]);
                B := B shr Extra;
                Dec(K, Extra);

                // do the copy
                Dec(M, C);
                // offset before Dest
                if (NativeUInt(Q) - NativeUInt(S.Window)) >= D then begin
                  //  just copy
                  R := Q;
                  Dec(R, D);
                  Q^ := R^;  Inc(Q); Inc(R); Dec(C); // minimum count is three,
                  Q^ := R^;  Inc(Q); Inc(R); Dec(C); // so unroll loop a little
                end else begin
                  // offset after destination,
                  // bytes from offset to end
                  Extra := D - (NativeUInt(Q) - NativeUInt(S.Window));
                  R := S.zend;
                  // pointer to offset
                  Dec(R, Extra);
                  if C > Extra then begin
                    // copy to end of window
                    Dec(C, Extra);
                    repeat
                      Q^ := R^;
                      Inc(Q);
                      Inc(R);
                      Dec(Extra);
                    until Extra = 0;
                    // copy rest from start of window
                    R := S.Window;
                  end;
                end;

                // copy all or what's left
                repeat
                  Q^ := R^;
                  Inc(Q);
                  Inc(R);
                  Dec(C);
                until C = 0;
                break;
              end else
                if (Extra and 64) = 0 then begin
                  Inc(Temp, Temp.Base + (B and InflateMask[Extra]));
                  Extra := Temp.exop;
                end else begin
                  UpdatePointers;
                  result := Z_DATA_ERROR;
                  exit;
                end;
            until False;
            break;
          end;

          if (Extra and 64) = 0 then begin
            Inc(Temp, Temp.Base + (B and InflateMask[Extra]));
            Extra := Temp.exop;
            if Extra = 0 then begin
              B := B shr Temp.Bits;
              Dec(K, Temp.Bits);

              Q^ := Temp.Base;
              Inc(Q);
              Dec(M);
              break;
            end;
          end else begin
            UpdatePointers;
            if (Extra and 32) <> 0 then
              result := Z_STREAM_END
            else
              result := Z_DATA_ERROR;
            exit;
          end;
        until False;
      until (M < 258) or (N < 10);

      // not enough input or output -> restore pointers and return
      UpdatePointers;
      result := Z_OK;
    end;

    //----------------------------------------------------------------------------------------------------------------------

    function InflateCodesNew(LiteralBits: cardinal; DistanceBits: cardinal; TL, TD: PInflateHuft; var Z: TZState) : PInflateCodesState;
    begin
      GetMem(result, SizeOf(TInflateCodesState));
      result.Mode := icmStart;
      result.LiteralTreeBits := LiteralBits;
      result.DistanceTreeBits := DistanceBits;
      result.LiteralTree := TL;
      result.DistanceTree := TD;
    end;

    //----------------------------------------------------------------------------------------------------------------------

    function InflateCodes(var S: TInflateBlocksState; var Z: TZState; R: integer) : integer;
    var J     : cardinal;      // temporary storage
        Temp  : PInflateHuft;
        Extra : cardinal;      // extra bits or operation
        B     : cardinal;
        K     : cardinal;      // bits in bit buffer
        P     : PByte;         // input data pointer
        N     : cardinal;      // bytes available there
        Q     : PByte;         // output window write pointer
        M     : cardinal;      // bytes to end of window or read pointer
        F     : PByte;         // pointer to copy strings from
        C     : PInflateCodesState;

      function UpdatePointers(flush: boolean = true) : integer;
      begin
        S.bitb := B;
        S.bitk := K;
        Z.AvailableInput := N;
        Inc(Z.TotalInput, NativeUInt(P) - NativeUInt(Z.NextInput));
        Z.NextInput := P;
        S.write := Q;
        if flush then
          result := InflateFlush(S, Z, R)
        else
          result := 0;
      end;

    begin
      C := S.sub.decode.codes;  // codes state

      // copy input/output information to locals
      P := Z.NextInput;
      N := Z.AvailableInput;
      B := S.bitb;
      K := S.bitk;
      Q := S.write;
      if NativeUInt(Q) < NativeUInt(S.read) then
        M := NativeUInt(S.read) - NativeUInt(Q) - 1
      else
        M := NativeUInt(S.zend) - NativeUInt(Q);

      // process input and output based on current state
      while True do begin
        case C.Mode of
          icmStart:
            begin
              if (M >= 258) and (N >= 10) then begin
                UpdatePointers(false);

                R := InflateFast(C.LiteralTreeBits, C.DistanceTreeBits, C.LiteralTree, C.DistanceTree, S, Z);
                P := Z.NextInput;
                N := Z.AvailableInput;
                B := S.bitb;
                K := S.bitk;
                Q := S.write;
                if NativeUInt(Q) < NativeUInt(S.read) then
                  M := NativeUInt(S.read) - NativeUInt(Q) - 1
                else
                  M := NativeUInt(S.zend) - NativeUInt(Q);

                if R <> Z_OK then begin
                  if R = Z_STREAM_END then
                    C.mode := icmWash
                  else
                    C.mode := icmBadCode;
                  Continue;
                end;
              end;
              C.sub.Code.need := C.LiteralTreeBits;
              C.sub.Code.Tree := C.LiteralTree;
              C.mode := icmLen;
            end;
          icmLen: // I: get length/literal/eob next
            begin
              J := C.sub.Code.need;
              while K < J do begin
                if N = 0 then begin
                  result := UpdatePointers;
                  exit;
                end;
                R := Z_OK;
                Dec(N);
                B := B or (cardinal(P^) shl K);
                Inc(P);
                Inc(K, 8);
              end;
              Temp := C.sub.Code.Tree;
              Inc(Temp, B and InflateMask[J]);
              B := B shr Temp.Bits;
              Dec(K, Temp.Bits);

              Extra := Temp.exop;
              // literal
              if Extra = 0 then begin
                C.sub.lit := Temp.Base;
                C.mode := icmLit;
                Continue;
              end;
              // length
              if (Extra and 16) <> 0 then begin
                C.sub.copy.get := Extra and 15;
                C.Len := Temp.Base;
                C.mode := icmLenNext;
                Continue;
              end;
              // next table
              if (Extra and 64) = 0 then begin
                C.sub.Code.need := Extra;
                C.sub.Code.Tree := @PHuftField(Temp)[Temp.Base];
                Continue;
              end;
              // end of block
              if (Extra and 32) <> 0 then begin
                C.mode := icmWash;
                Continue;
              end;
              // invalid code
              C.mode := icmBadCode;
              R := Z_DATA_ERROR;
              result := UpdatePointers;
              exit;
            end;
          icmLenNext: // I: getting length extra (have base)
            begin
              J := C.sub.copy.get;
              while K < J do begin
                if N = 0 then begin
                  result := UpdatePointers;
                  exit;
                end;
                R := Z_OK;
                Dec(N);
                B := B or (cardinal(P^) shl K);
                Inc(P);
                Inc(K, 8);
              end;
              Inc(C.Len, B and InflateMask[J]);
              B := B shr J;
              Dec(K, J);

              C.sub.Code.need := C.DistanceTreeBits;
              C.sub.Code.Tree := C.DistanceTree;
              C.mode := icmDistance;
            end;
          icmDistance: // I: get distance next
            begin
              J := C.sub.Code.need;
              while K < J do begin
                if N = 0 then begin
                  result := UpdatePointers;
                  exit;
                end;
                R := Z_OK;
                Dec(N);
                B := B or (cardinal(P^) shl K);
                Inc(P);
                Inc(K, 8);
              end;
              Temp := @PHuftField(C.sub.Code.Tree)[B and InflateMask[J]];
              B := B shr Temp.Bits;
              Dec(K, Temp.Bits);

              Extra := Temp.exop;
              // distance
              if (Extra and 16) <> 0 then begin
                C.sub.copy.get := Extra and 15;
                C.sub.copy.Distance := Temp.Base;
                C.mode := icmDistExt;
                Continue;
              end;
              // next table
              if (Extra and 64) = 0 then begin
                C.sub.Code.need := Extra;
                C.sub.Code.Tree := @PHuftField(Temp)[Temp.Base];
                Continue;
              end;
              // invalid code
              C.mode := icmBadCode;
              R := Z_DATA_ERROR;
              result := UpdatePointers;
              exit;
            end;
          icmDistExt: // I: getting distance extra
            begin
              J := C.sub.copy.get;
              while K < J do begin
                if N = 0 then begin
                  result := UpdatePointers;
                  exit;
                end;
                R := Z_OK;
                Dec(N);
                B := B or (cardinal(P^) shl K);
                Inc(P);
                Inc(K, 8);
              end;
              Inc(C.sub.copy.Distance, B and InflateMask[J]);
              B := B shr J;
              Dec(K, J);
              C.mode := icmCopy;
            end;
          icmCopy: // O: copying bytes in window, waiting for space
            begin
              F := Q;
              Dec(F, C.sub.copy.Distance);
              if (NativeUInt(Q) - NativeUInt(S.Window)) < C.sub.copy.Distance then begin
                F := S.zend;
                Dec(F, C.sub.copy.Distance - (NativeUInt(Q) - NativeUInt(S.Window)));
              end;

              while C.Len <> 0 do begin
                if M = 0 then begin
                  if (Q = S.zend) and (S.read <> S.Window) then begin
                    Q := S.Window;
                    if NativeUInt(Q) < NativeUInt(S.read) then
                      M := NativeUInt(S.read) - NativeUInt(Q) - 1
                    else
                      M := NativeUInt(S.zend) - NativeUInt(Q);
                  end;

                  if M = 0 then begin
                    S.write := Q;
                    R := InflateFlush(S, Z, R);
                    Q := S.write;
                    if NativeUInt(Q) < NativeUInt(S.read) then
                      M := NativeUint(S.read) - NativeUInt(Q) - 1
                    else
                      M := NativeUInt(S.zend) - NativeUInt(Q);

                    if (Q = S.zend) and (S.read <> S.Window) then begin
                      Q := S.Window;
                      if NativeUInt(Q) < NativeUInt(S.read) then
                        M := NativeUInt(S.read) - NativeUInt(Q) - 1
                      else
                        M := NativeUInt(S.zend) - NativeUInt(Q);
                    end;

                    if M = 0 then begin
                      result := UpdatePointers;
                      exit;
                    end;
                  end;
                end;
                R := Z_OK;

                Q^ := F^;
                Inc(Q);
                Inc(F);
                Dec(M);

                if (F = S.zend) then F := S.Window;
                Dec(C.Len);
              end;
              C.mode := icmStart;
            end;
          icmLit: // O: got literal, waiting for output space
            begin
              if M = 0 then begin
                if (Q = S.zend) and (S.read <> S.Window) then begin
                  Q := S.Window;
                  if NativeUInt(Q) < NativeUInt(S.read) then
                    M := NativeUInt(S.read) - NativeUInt(Q) - 1
                  else
                    M := NativeUInt(S.zend) - NativeUInt(Q);
                end;

                if M = 0 then begin
                  S.write := Q;
                  R := InflateFlush(S, Z, R);
                  Q := S.write;
                  if NativeUInt(Q) < NativeUInt(S.read) then
                    M := NativeUInt(S.read) - NativeUInt(Q) - 1
                  else
                    M := NativeUInt(S.zend) - NativeUInt(Q);

                  if (Q = S.zend) and (S.read <> S.Window) then begin
                    Q := S.Window;
                    if NativeUInt(Q) < NativeUInt(S.read) then
                      M := NativeUInt(S.read) - NativeUInt(Q) - 1
                    else
                      M := NativeUInt(S.zend) - NativeUInt(Q);
                  end;

                  if M = 0 then begin
                    result := UpdatePointers;
                    exit;
                  end;
                end;
              end;
              R := Z_OK;
              Q^ := C.sub.lit;
              Inc(Q);
              Dec(M);
              C.mode := icmStart;
            end;
          icmWash: // O: got eob, possibly More output
            begin
              // return unused byte, if any
              if K > 7 then begin
                Dec(K, 8);
                Inc(N);
                Dec(P);
                // can always return one
              end;
              S.write := Q;
              R := InflateFlush(S, Z, R);
              Q := S.write;
              if NativeUInt(Q) < NativeUInt(S.read) then
                M := NativeUInt(S.read) - NativeUInt(Q) - 1
              else
                M := NativeUInt(S.zend) - NativeUInt(Q);

              if S.read <> S.write then begin
                result := UpdatePointers;
                exit;
              end;
              C.mode := icmZEnd;
            end;
          icmZEnd:
            begin
              R := Z_STREAM_END;
              result := UpdatePointers;
              exit;
            end;
          icmBadCode: // X: got error
            begin
              R := Z_DATA_ERROR;
              result := UpdatePointers;
              exit;
            end;
        else
          begin
            R := Z_STREAM_ERROR;
            result := UpdatePointers;
            exit;
          end;
        end;
      end;
  
      result := Z_STREAM_ERROR;
    end;

    function BuildHuffmanTables(const B: array of cardinal; N, S: cardinal; const D, Extra: array of cardinal;
                                Temp: PPInflateHuft; var M: cardinal; var HP: array of TInflateHuft; var HN: cardinal;
                                var V: array of cardinal) : integer;
    // Given a list of code lengths and a maximum table size, make a set of tables to decode that set of codes. Returns Z_OK
    // on success, Z_BUF_ERROR if the given code set is incomplete (the tables are still built in this case), Z_DATA_ERROR
    // if the input is invalid (an over-subscribed set of lengths), or Z_MEM_ERROR if not enough memory.
    //
    // Input pareters:
    // B contains the code lenths in bits (all assumed <= BMAX)
    // N is the number of codes (<= NMAX)
    // S is the number of simple valued codes (0..S - 1)
    // D contains a list of base values for non-simple codes
    // Extra carries a list of extra bits for non-simple codes
    //
    // Output parameters:
    // Temp points to the starting table
    // M receives the maxium lookup bits (actual space for trees)
    // HP receives the Huffman tables
    // while HN decribes how many of HP is actually used
    // finally V is a working area which receives values in order of bit length
    var A    : cardinal;                             // counter for codes of length K
        C    : array [0..BMAX] of cardinal;          // bit length count table
        F    : cardinal;                             // I repeats in table every F entries
        G    : integer;                              // maximum code Length
        H    : integer;                              // table Level
        I    : cardinal;                             // counter, current code
        J    : cardinal;                             // counter
        K    : integer;                              // number of bits in current code
        L    : integer;			                           // bits per table (returned in M)
        Mask : cardinal;                             // (1 shl W) - 1, to avoid cc - O bug on HP
        P    : TPCardinal;                           // pointer into C[], B[], or V[]
        Q    : PInflateHuft;                         // points to current table
        R    : TInflateHuft;                         // table entry for structure assignment
        U    : array [0..BMAX - 1] of PInflateHuft;  // table stack
        W    : integer;                              // bits before this table = (L * H)
        X    : array [0..BMAX] of cardinal;          // bit offsets, then code stack
        XP   : TPCardinal;                           // pointer into X
        Y    : integer;                              // number of dummy codes added
        Z    : cardinal;                             // number of entries in current table
    Begin
      // generate counts for each bit length
      FillChar(C, SizeOf(C), 0);

      // assume all entries <= BMAX
      for I := 0 to N - 1 do
        Inc(C[B[I]]);

      // nil input -> all zero length codes
      if C[0] = N then
      Begin
        Temp^ := nil;
        M := 0 ;
        result := Z_OK;
        exit;
      end ;

      // find minimum and maximum length, bound [M] by those
      L := M;
      for J := 1 to BMAX do
        if C[J] <> 0 then break;
      // minimum code Length
      K := J ;
      if cardinal(L) < J then
        L := J;
      for I := BMAX downto 1 do
        if C[I] <> 0 then
          break;
      // maximum code length
      G := I ;
      if cardinal(L) > I then
        L := I;
      M := L;

      // adjust last length count to fill out codes if needed
      Y := 1 shl J;
      while J < I do
      begin
        Dec(Y, C[J]);
        if Y < 0 then
        begin
          // bad input: more codes than bits
          result := Z_DATA_ERROR;
          exit;
        end ;
        Inc(J);
        Y := Y shl 1;
      end;
      Dec (Y, C[I]);
      if Y < 0 then
      begin
        // bad input: more codes than bits
        result := Z_DATA_ERROR;
        exit;
      end;
      Inc(C[I], Y);

      // generate starting offsets into the value table for each length
      X[1] := 0;
      J := 0;

      P := @C[1];
      XP := @X[2];
      // note that I = G from above
      Dec(I);
      while (I > 0) do
      begin
        Inc(J, P^);
        XP^ := J;
        Inc(P);
        Inc(XP);
        Dec(I);
      end;

      // make a table of values in order of bit lengths
      for I := 0 to N - 1 do
      begin
        J := B[I];
        if J <> 0 then
        begin
          V[X[J]] := I;
          Inc(X[J]);
        end;
      end;
      // set N to Length of V
      N := X[G];

      // generate the Huffman codes and for each make the table entries
      I := 0;
      // first Huffman code is zero
      X[0] := 0;
      // grab values in bit order
      P := @V;
      // no tables yet -> Level - 1
      H := -1;
      // bits decoded = (L * H)
      W := -L;

      U[0] := nil;
      Q := nil;
      Z := 0;        

      // go through the bit lengths (K already is bits in shortest code) 
      while K <= G Do
      begin
        A := C[K];
        while A <> 0 Do
        begin
          Dec(A);
          // here I is the Huffman code of length K bits for value P^ 
          // make tables up to required level
          while K > W + L do
          begin
            Inc(H);
            // add bits already decoded, previous table always L Bits
            Inc(W, L);
            // compute minimum size table less than or equal to L bits
            Z := G - W;
            if Z > cardinal(L) then
              Z := L;

            // try a K - W bit table
            J := K - W;
            F := 1 shl J;
            // too few codes for K - W bit table
            if F > A + 1 then
            begin
              // deduct codes from patterns left
              Dec(F,A + 1);
              XP := @C[K];
              if J < Z then
              begin
                Inc(J);
                while J < Z do
                begin
                  // try smaller tables up to Z bits
                  F := F shl 1;
                  Inc(XP);
                  // enough codes to use up J Bits
                  if F <= XP^ then
                    break;
                  // else deduct codes from patterns
                  Dec(F, XP^);
                  Inc(J);
                end;
              end;
            end;

            // table entries for J-bit table
            Z := 1 shl J;
            // allocate new table (note: doesn't matter for fixed)
            if HN + Z > MANY then
            begin
              result := Z_MEM_ERROR;
              exit;
            end;

            Q := @HP[HN];
            U[H] := Q;
            Inc(HN, Z);

            // connect to last table, if there is one
            if H <> 0 then
            begin
              // save pattern for backing up
              X[H] := I;
              // bits to dump before this table
              R.Bits := L;
              // bits in this table
              R.exop := J;
              J := I shr (W - L);
              R.Base := (NativeUInt(Q) - NativeUInt(U[H - 1]) ) div SizeOf(Q^) - J;
              // connect to last table
              PHuftField(U[H - 1])[J] := R;
            end
            else
              // first table is returned result
              Temp^ := Q;
          end;

          // set up table entry in R
          R.Bits := byte(K - W);

          // out of values -> invalid code
          if NativeUInt(P) >= NativeUInt(@V[N]) then
            R.exop := 128 + 64
          else
            if P^ < S then
            begin
              // 256 is end-of-block code
              if P^ < 256 then
                R.exop := 0
              else
                R.exop := 32 + 64;
              // simple code is just the value
              R.Base := P^;
              Inc(P);
            end
            else
            begin
              // non-simple -> look up in lists
              R.exop := byte(Extra[P^ - S] + 16 + 64);
              R.Base := D[P^ - S];
              Inc (P);
            end;

          // fill xode-like entries with R
          F := 1 shl (K - W);
          J := I shr W;
          while J < Z do
          begin
            PHuftField(Q)[J] := R;
            Inc(J, F);
          end;

          // backwards increment the K-bit code I 
          J := 1 shl (K - 1) ;
          while (I and J) <> 0 do
          begin
            I := I xor J;         
            J := J shr 1
          end;
          I := I xor J;

          // backup over finished tables
          // needed on HP, cc -O bug
          Mask := (1 shl W) - 1;
          while (I and Mask) <> X[H] do
          begin
            // don't need to update Q
            Dec(H);
            Dec(W, L);
            Mask := (1 shl W) - 1;
          end;
        end;
        Inc(K);
      end;

      // Return Z_BUF_ERROR if we were given an incomplete table 
      if (Y <> 0) and (G <> 1) then
        result := Z_BUF_ERROR
      else
        result := Z_OK;
    end;

    //----------------------------------------------------------------------------------------------------------------------

    function InflateTreesBits(var C: array of cardinal; var BB: cardinal; var TB: PInflateHuft;
                              var HP: array of TInflateHuft; var Z: TZState) : integer;
    // C holds 19 code lengths
    // BB - bits tree desired/actual depth
    // TB - bits tree result
    // HP - space for trees
    // Z - for messages
    var R  : integer;
        HN : cardinal;     // hufts used in space
        V  : TPACardinal;  // work area for BuildHuffmanTables

    begin
      HN := 0;
      GetMem(V, 19 * SizeOf(cardinal));
      try
        R := BuildHuffmanTables(C, 19, 19, CopyLengths, CopyLiteralExtra, @TB, BB, HP, HN, V^);
        if (R = Z_BUF_ERROR) or (BB = 0) then
          R := Z_DATA_ERROR;
        result := R;
      finally
        FreeMem(V);
      end;
    end;

    //----------------------------------------------------------------------------------------------------------------------

    function InflateTreesDynamic(NL: cardinal; ND: cardinal; var C: array of cardinal; var LiteralBits: cardinal;
                                 var DistanceBits: cardinal; var TL: PInflateHuft; var TD: PInflateHuft; var HP: array of TInflateHuft;
                                 var Z: TZState) : integer;
    // NL - number of literal/length codes
    // ND - number of distance codes
    // C - code lengths
    // LiteralBits - literal desired/actual bit depth
    // DistanceBits - distance desired/actual bit depth
    // TL - literal/length tree result
    // TD - distance tree result
    // HP - space for trees
    // Z - for messages
    var R  : integer;
        HN : cardinal;      // hufts used in space
        V  : TPACardinal;   // work area for BuildHuffmanTables
    begin
      HN := 0;
      // allocate work area
      GetMem(V, 288 * SizeOf(cardinal));
      try
        result := Z_OK;

        // build literal/length tree
        R := BuildHuffmanTables(C, NL, 257, CopyLengths, CopyLiteralExtra, @TL, LiteralBits, HP, HN, V^);
        if (R <> Z_OK) or (LiteralBits = 0) then
        begin
          FreeMem(V);
          result := R;
          exit;
        end;

        // build distance tree
        R := BuildHuffmanTables(TPACardinal(@C[NL])^, ND, 0, CopyOffsets, CopyExtra, @TD, DistanceBits, HP, HN, V^);
        if (R <> Z_OK) or ((DistanceBits = 0) and (NL > 257)) then
        begin
          if R = Z_BUF_ERROR then
            R := Z_DATA_ERROR
          else
            if R <> Z_MEM_ERROR then
              R := Z_DATA_ERROR;
          FreeMem(V);
          result := R;
        end;
      finally
        FreeMem(V);
      end;
    end;

    //----------------------------------------------------------------------------------------------------------------------

    function InflateTreesFixed(var LiteralBits: cardinal; var DistanceBits: cardinal; var TL, TD: PInflateHuft; var Z: TZState) : integer;
    type PFixedTable = ^TFixedTable;
         TFixedTable = array [0..287] of cardinal;
    var K : integer;      // temporary variable
        C : PFixedTable;  // length list for BuildHuffmanTables
        V : TPACardinal;  // work area for BuildHuffmanTables
        F : cardinal;     // number of hufts used in FixedTablesMemory
    begin
      // build fixed tables if not already (multiple overlapped executions ok)
      if not FixedBuild then begin
        F := 0;
        C := nil;
        V := nil;

        try
          GetMem(C, 288 * SizeOf(cardinal));
          GetMem(V, 288 * SizeOf(cardinal));
          // literal table
          for K :=   0 to 143 do C[K] := 8;
          for K := 144 to 255 do C[K] := 9;
          for K := 256 to 279 do C[K] := 7;
          for K := 280 to 287 do C[K] := 8;
          FixedLiteralBits := 9;
          BuildHuffmanTables(C^, 288, 257, CopyLengths, CopyLiteralExtra, @FixedLiteralTable, FixedLiteralBits, FixedTablesMemory, F, V^);

          // distance table
          for K := 0 to 29 do
            C[K] := 5;
          FixedDistanceBits := 5;
          BuildHuffmanTables(C^, 30, 0, CopyOffsets, CopyExtra, @FixedDistanceTable, FixedDistanceBits, FixedTablesMemory, F, V^);

          FixedBuild := True;
        finally
          if Assigned(V) then
            FreeMem(V);
          if Assigned(C) then
            FreeMem(C);
        end;
      end;
      LiteralBits := FixedLiteralBits;
      DistanceBits := FixedDistanceBits;
      TL := FixedLiteralTable;
      TD := FixedDistanceTable;
      result := Z_OK;
    end;

    //----------------------------------------------------------------------------------------------------------------------

    // tables for Deflate from PKZIP'S appnote.txt.
    const
      // order of the bit length code lengths
      BitOrder : array [0..18] of word =
      (
        16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
      );

  // R contains the initial return code
  var Temp         : cardinal;
      B            : cardinal;    // bit buffer
      K            : cardinal;    // bits in bit buffer
      P            : PByte;       // input data pointer
      N            : cardinal;    // bytes available there
      Q            : PByte;       // output Window write pointer
      M            : cardinal;    // bytes to end of window or read pointer
      // fixed code blocks
      LiteralBits  : cardinal;
      DistanceBits : cardinal;
      TL, TD       : PInflateHuft;
      H            : PInflateHuft;
      I, J, C      : cardinal;
      CodeState    : PInflateCodesState;

    function UpdatePointers(flush: boolean = true) : integer;
    begin
      S.bitb := B;
      S.bitk := K;
      Z.AvailableInput := N;
      Inc(Z.TotalInput, NativeUInt(P) - NativeUInt(Z.NextInput));
      Z.NextInput := P;
      S.write := Q;
      if flush then
        result := InflateFlush(S, Z, R)
      else
        result := 0;
    end;

  begin
    // copy input/output information to locals
    P := Z.NextInput;
    N := Z.AvailableInput;
    B := S.bitb;
    K := S.bitk;
    Q := S.write;
    if NativeUInt(Q) < NativeUInt(S.read) then
      M := NativeUInt(S.read) - NativeUInt(Q) - 1
    else
      M := NativeUInt(S.zend) - NativeUInt(Q);

    // decompress an inflated block
    // process input based on current state
    while True do begin
      case S.mode of
        ibmZType:
          begin
            while K < 3 do begin
              if N = 0 then begin
                result := UpdatePointers;
                exit;
              end;
              R := Z_OK;
              Dec(N);
              B := B or (cardinal(P^) shl K);
              Inc(P);
              Inc(K, 8);
            end;

            Temp := B and 7;
            S.last := boolean(Temp and 1);
            case Temp shr 1 of
              0: // stored
                begin
                  B := B shr 3;
                  Dec(K, 3);
                  // go to byte boundary
                  Temp := K and 7;
                  B := B shr Temp;
                  Dec(K, Temp);
                  // get length of stored block
                  S.mode := ibmLens;
                end;
              1: // fixed
                begin
                  InflateTreesFixed(LiteralBits, DistanceBits, TL, TD, Z);
                  S.sub.decode.codes := InflateCodesNew(LiteralBits, DistanceBits, TL, TD, Z);
                  if S.sub.decode.codes = nil then begin
                    R := Z_MEM_ERROR;
                    result := UpdatePointers;
                    exit;
                  end;
                  B := B shr 3;
                  Dec(K, 3);
                  S.mode := ibmCodes;
                end;
              2: // dynamic
                begin
                  B := B shr 3;
                  Dec(K, 3);
                  S.mode := ibmTable;
                end;
              3: // illegal
                begin
                  B := B shr 3;
                  Dec(K, 3);
                  S.mode := ibmBlockBad;
                  R := Z_DATA_ERROR;
                  result := UpdatePointers;
                  exit;
                end;
            end;
          end;
        ibmLens:
          begin
            while K < 32 do begin
              if N = 0 then begin
                result := UpdatePointers;
                exit;
              end;
              R := Z_OK;
              Dec(N);
              B := B or (cardinal(P^) shl K);
              Inc(P);
              Inc(K, 8);
            end;

            if (((not B) shr 16) and $FFFF) <> (B and $FFFF) then begin
              S.mode := ibmBlockBad;
              R := Z_DATA_ERROR;
              result := UpdatePointers;
              exit;
            end;
            S.sub.left := B and $FFFF;
            K := 0;
            B := 0;
            if S.sub.left <> 0 then
              S.mode := ibmStored
            else
              if S.last then
                S.mode := ibmDry
              else
                S.mode := ibmZType;
          end;
        ibmStored:
          begin
            if N = 0 then begin
              result := UpdatePointers;
              exit;
            end;

            if M = 0 then begin
              if (Q = S.zend) and (S.read <> S.Window) then begin
                Q := S.Window;
                if NativeUInt(Q) < NativeUInt(S.read) then
                  M := NativeUInt(S.read) - NativeUInt(Q) - 1
                else
                  M := NativeUInt(S.zend) - NativeUInt(Q);
              end;

              if M = 0 then begin
                S.write := Q;
                R := InflateFlush(S, Z, R);
                Q := S.write;
                if NativeUInt(Q) < NativeUInt(S.read) then
                  M := NativeUInt(S.read) - NativeUInt(Q) - 1
                else
                  M := NativeUInt(S.zend) - NativeUInt(Q);
                if (Q = S.zend) and (S.read <> S.Window) then begin
                  Q := S.Window;
                  if NativeUInt(Q) < NativeUInt(S.read) then
                    M := NativeUInt(S.read) - NativeUInt(Q) - 1
                  else
                    M := NativeUInt(S.zend) - NativeUInt(Q);
                end;

                if M = 0 then begin
                  result := UpdatePointers;
                  exit;
                end;
              end;
            end;
            R := Z_OK;

            Temp := S.sub.left;
            if Temp > N then
              Temp := N;
            if Temp > M then
              Temp := M;
            Move(P^, Q^, Temp);
            Inc(P, Temp);
            Dec(N, Temp);
            Inc(Q, Temp);
            Dec(M, Temp);
            Dec(S.sub.left, Temp);
            if S.sub.left = 0 then
              if S.last then
                S.mode := ibmDry
              else
                S.mode := ibmZType;
          end;
        ibmTable:
          begin
            while K < 14 do begin
              if N = 0 then begin
                result := UpdatePointers;
                exit;
              end;
              R := Z_OK;
              Dec(N);
              B := B or (cardinal(P^) shl K);
              Inc(P);
              Inc(K, 8);
            end;

            Temp := B and $3FFF;
            S.sub.trees.table := Temp;
            if ((Temp and $1F) > 29) or (((Temp shr 5) and $1F) > 29) then begin
              S.mode := ibmBlockBad;
              R := Z_DATA_ERROR;
              result := UpdatePointers;
              exit;
            end;
            Temp := 258 + (Temp and $1F) + ((Temp shr 5) and $1F);
            try
              GetMem(S.sub.trees.blens, Temp * SizeOf(cardinal));
            except
              R := Z_MEM_ERROR;
              UpdatePointers;
              raise;
            end;
            B := B shr 14;
            Dec(K, 14);

            S.sub.trees.Index := 0;
            S.mode := ibmBitTree;
          end;
        ibmBitTree:
          begin
            while (S.sub.trees.Index < 4 + (S.sub.trees.table shr 10)) do begin
              while K < 3 do begin
                if N = 0 then begin
                  result := UpdatePointers;
                  exit;
                end;
                R := Z_OK;
                Dec(N);
                B := B or (cardinal(P^) shl K);
                Inc(P);
                Inc(K, 8);
              end;

              S.sub.trees.blens[BitOrder[S.sub.trees.Index]] := B and 7;
              Inc(S.sub.trees.Index);
              B := B shr 3;
              Dec(K, 3);
            end;

            while S.sub.trees.Index < 19 do begin
              S.sub.trees.blens[BitOrder[S.sub.trees.Index]] := 0;
              Inc(S.sub.trees.Index);
            end;
            S.sub.trees.BB := 7;
            Temp := InflateTreesBits(S.sub.trees.blens^, S.sub.trees.BB, S.sub.trees.TB, S.hufts^, Z);
            if Temp <> Z_OK then begin
              FreeMem(S.sub.trees.blens);
              R := Temp;
              if R = Z_DATA_ERROR then S.mode := ibmBlockBad;
              result := UpdatePointers;
              exit;
            end;
            S.sub.trees.Index := 0;
            S.mode := ibmDistTree;
          end;
        ibmDistTree:
          begin
            while True do begin
              Temp := S.sub.trees.table;
              if not (S.sub.trees.Index < 258 + (Temp and $1F) + ((Temp shr 5) and $1F)) then
                break;
              Temp := S.sub.trees.BB;
              while K < Temp do begin
                if N = 0 then begin
                  result := UpdatePointers;
                  exit;
                end;
                R := Z_OK;
                Dec(N);
                B := B or (cardinal(P^) shl K);
                Inc(P);
                Inc(K, 8);
              end;

              H := S.sub.trees.TB;
              Inc(H, B and InflateMask[Temp]);
              Temp := H^.Bits;
              C := H^.Base;

              if C < 16 then begin
                B := B shr Temp;
                Dec(K, Temp);
                S.sub.trees.blens^[S.sub.trees.Index] := C;
                Inc(S.sub.trees.Index);
              end else begin
                // C = 16..18
                if C = 18 then begin
                  I := 7;
                  J := 11;
                end else begin
                  I := C - 14;
                  J := 3;
                end;

                while K < Temp + I do begin
                  if N = 0 then begin
                    result := UpdatePointers;
                    exit;
                  end;
                  R := Z_OK;
                  Dec(N);
                  B := B or (cardinal(P^) shl K);
                  Inc(P);
                  Inc(K, 8);
                end;

                B := B shr Temp;
                Dec(K, Temp);

                Inc(J, B and InflateMask[I]);
                B := B shr I;
                Dec(K, I);

                I := S.sub.trees.Index;
                Temp := S.sub.trees.table;
                if (I + J > 258 + (Temp and $1F) + ((Temp shr 5) and $1F)) or ((C = 16) and (I < 1)) then begin
                  FreeMem(S.sub.trees.blens);
                  S.mode := ibmBlockBad;
                  R := Z_DATA_ERROR;
                  result := UpdatePointers;
                  exit;
                end;

                if C = 16 then
                  C := S.sub.trees.blens[I - 1]
                else
                  C := 0;
                repeat
                  S.sub.trees.blens[I] := C;
                  Inc(I);
                  Dec(J);
                until J = 0;
                S.sub.trees.Index := I;
              end;
            end; // while

            S.sub.trees.TB := nil;
            begin
              LiteralBits := 9;
              DistanceBits := 6;
              Temp := S.sub.trees.table;
              Temp := InflateTreesDynamic(257 + (Temp and $1F), 1 + ((Temp shr 5) and $1F),
                                          S.sub.trees.blens^, LiteralBits, DistanceBits, TL, TD, S.hufts^, Z);
              FreeMem(S.sub.trees.blens);
              if Temp <> Z_OK then begin
                if integer(Temp) = Z_DATA_ERROR then S.mode := ibmBlockBad;
                R := Temp;
                result := UpdatePointers;
                exit;
              end;
              CodeState := InflateCodesNew(LiteralBits, DistanceBits, TL, TD, Z);
              if CodeState = nil then begin
                R := Z_MEM_ERROR;
                result := UpdatePointers;
                exit;
              end;
              S.sub.decode.codes := CodeState;
            end;
            S.mode := ibmCodes;
          end;
        ibmCodes:
          begin
            UpdatePointers(false);
            R := InflateCodes(S, Z, R);

            if R <> Z_STREAM_END then begin
              result := InflateFlush(S, Z, R);
              exit;
            end;
            R := Z_OK;
            Freemem(S.sub.decode.codes);
            // load local pointers
            P := Z.NextInput;
            N := Z.AvailableInput;
            B := S.bitb;
            K := S.bitk;
            Q := S.write;
            if NativeUInt(Q) < NativeUInt(S.read) then
              M := NativeUInt(S.read) - NativeUInt(Q) - 1
            else
              M := NativeUInt(S.zend) - NativeUInt(Q);
            if not S.last then begin
              S.mode := ibmZType;
              Continue;
            end;
            S.mode := ibmDry;
          end;
        ibmDry:
          begin
            S.write := Q;
            R := InflateFlush(S, Z, R);
            Q := S.write;

            if S.read <> S.write then begin
              result := UpdatePointers;
              exit;
            end;
            S.mode := ibmBlockDone;
          end;
        ibmBlockDone:
          begin
            R := Z_STREAM_END;
            result := UpdatePointers;
            exit;
          end;
        ibmBlockBad:
          begin
            R := Z_DATA_ERROR;
            result := UpdatePointers;
            exit;
          end;
        else
          begin
            R := Z_STREAM_ERROR;
            result := UpdatePointers;
            exit;
          end;
      end;  // case S.mode of
    end;
  end;

  // Notes beyond the 1.93a appnote.txt:
  // 1. Distance pointers never point before the beginning of the output stream.
  // 2. Distance pointers can point back across blocks, up to 32k away.
  // 3. There is an implied maximum of 7 Bits for the bit Length table and 15 Bits for the actual data.
  // 4. if only one Code exists, then it is encoded using one bit. (zero would be more efficient, but perhaps a little
  //    confusing.) If two codes exist, they are coded using one bit each (0 and 1).
  // 5. There is no way of sending zero distance codes -> a dummy must be sent if there are none. (History: a pre 2.0
  //    Version of PKZIP would store blocks with no distance codes, but this was discovered to be
  //    too harsh a criterion.) Valid only for 1.93a. 2.04c does allow zero distance codes, which is sent as one Code of
  //    zero Bits in length.
  // 6. There are up to 286 literal/Length codes. Code 256 represents the end-of-block. Note however that the static
  //    length Tree defines 288 codes just to fill out the Huffman codes. Codes 286 and 287 cannot be used though, since
  //    there is no length base or extra bits defined for them. Similarily, there are up to 30 distance codes. However,
  //    static trees defines 32 codes (all 5 Bits) to fill out the Huffman codes, but the last two had better not show up
  //    in the data.
  // 7. Unzip can check dynamic Huffman blocks for complete code sets. The exception is that a single code would not be
  //    complete (see #4).
  // 8. The five Bits following the block type is really the number of literal codes sent minus 257.
  // 9. Length codes 8, 16, 16 are interpreted as 13 Length codes of 8 bits (1 + 6 + 6). Therefore, to output three times
  //    the length, you output three codes (1 + 1 + 1), whereas to output four times the same length,
  //    you only need two codes (1+3).  Hmm.
  // 10. In the tree reconstruction algorithm, Code = Code + Increment only if BitLength(I) is not zero (pretty obvious).
  // 11. Correction: 4 Bits: # of Bit Length codes - 4 (4 - 19)
  // 12. Note: length code 284 can represent 227 - 258, but length code 285 really is 258. The last length deserves its
  //     own, short code since it gets used a lot in very redundant files. The length 258 is special since 258 - 3 (the
  //     min match length) is 255.
  // 13. The literal/length and distance code bit lengths are read as a single stream of lengths.  It is possible (and
  //     advantageous) for a repeat code (16, 17, or 18) to go across the boundary between the two sets of lengths.
  //----------------------------------------------------------------------------------------------------------------------

  procedure InflateBlockReset(var S: TInflateBlocksState; var Z: TZState);
  begin
    if (S.mode = ibmBitTree) or (S.mode = ibmDistTree) then
      FreeMem(S.sub.trees.blens);
    if S.mode = ibmCodes then
      FreeMem(S.sub.decode.codes);

    S.mode := ibmZType;
    S.bitk := 0;
    S.bitb := 0;

    S.write := S.Window;
    S.read := S.Window;
  end;

  function InflateBlocksNew(var Z: TZState; W: cardinal) : PInflateBlocksState;
  // W is the window size
  var S : PInflateBlocksState;
  begin
    GetMem(S, SizeOf(TInflateBlocksState));
    if S = nil then
      result := S
    else
      try
        GetMem(S.hufts, SizeOf(TInflateHuft) * MANY);

        GetMem(S.Window, W);
        S.zend := S.Window;
        Inc(S.zend, W);
        S.mode := ibmZType;
        InflateBlockReset(S^, Z);
        result := S;
      except
        if Assigned(S.Window) then
          FreeMem(S.Window);
        if Assigned(S.hufts) then
          FreeMem(S.hufts);
        FreeMem(S);
        raise;
      end;
  end;

var z  : TZState;
    c1 : dword;
begin
  result := 0;
  srcBuf := nil;
  dstBuf := nil;
  ZeroMemory(@z, sizeOf(z));
  z.NextInput       := src;
  z.AvailableInput  := srcLen;
  z.NextOutput      := dst;
  z.AvailableOutput := dstLen;
  z.TotalOutput     := 0;
  z.TotalInput      := 0;
  if crc <> nil then
    crc^ := $ffffffff;
  CheckSrcMap(z);
  CheckDstMap(z);
  try
    Z.State := InflateBlocksNew(z, 1 shl CWindowBits);
    InflateBlockReset(Z.State^, z);
    repeat
      c1 := InflateBlocks(Z.State^, z, Z_BUF_ERROR);
      CheckSrcMap(z);
    until (c1 <> Z_OK) or (z.TotalInput = srcLen);
    if ((c1 = Z_STREAM_END) or (c1 = Z_OK)) and
       (z.TotalInput = srcLen) and (z.TotalOutput = dstLen) then
      result := z.TotalOutput;
    InflateBlockReset(z.State^, z);
  except result := 0 end;
  FlushDstFile(z);
  if (crc <> nil) and (dst <> nil) then
    crc^ := not crc^;
  if (srcMap <> 0) and (src <> nil) then
    UnmapViewOfFile(src);
  if (dstMap <> 0) and (dst <> nil) then
    UnmapViewOfFile(dst);
  FreeMem(z.State.Window);
  FreeMem(z.State.hufts);
  FreeMem(z.State);
  if srcBuf <> nil then
    VirtualFree(srcBuf, 0, MEM_RELEASE);
  if dstBuf <> nil then
    VirtualFree(dstBuf, 0, MEM_RELEASE);
end;

function Compress(src, dst: pointer; srcLen, dstLen: integer) : integer; overload;
begin
  result := CompressEx(0, 0, 0, 0, 0, src, dst, srcLen, dstLen);
end;

function Uncompress(src, dst: pointer; srcLen, dstLen: integer) : integer; overload;
begin
  result := UncompressEx(0, 0, 0, 0, 0, src, dst, srcLen, dstLen);
end;

function Compress(const data: AnsiString; failIfGrow: boolean = false) : AnsiString; overload;
var i1 : integer;
begin
  SetLength(result, 12 + length(data) * 11 div 10 + 12);
  TPAInt64   (result)^[0] := length(data);
  TPACardinal(result)^[2] := not updateCrc32($FFFFFFFF, PAnsiChar(data)^, length(data));
  i1 := Compress(PAnsiChar(data), PAnsiChar(result) + 12, length(data), length(result) - 12);
  if (i1 > 0) and ( (12 + i1 < length(data)) or (not failIfGrow) ) then
    SetLength(result, 12 + i1)
  else
    result := '';
end;

function Uncompress(const data: AnsiString) : AnsiString; overload;
begin
  if Length(data) > 12 then begin
    SetLength(result, TPInt64(data)^);
    SetLength(result, Uncompress(PAnsiChar(data) + 12, PAnsiChar(result), length(data) - 12, length(result)));
    if (result <> '') and ((not updateCrc32($FFFFFFFF, PAnsiChar(result)^, length(result))) <> TPACardinal(data)^[2]) then
      result := '';
  end else
    result := '';
end;

function Compress(const srcFile, dstFile: UnicodeString; failIfGrow: boolean = false) : boolean; overload;
var sf, df : THandle;
    sm, dm : THandle;
    sl, dl : int64;
    err    : dword;
    crc    : dword;
    c1     : dword;
begin
  result := false;
  err    := 0;
  try
    if GetVersion and $80000000 = 0 then
      sf := CreateFileW(PWideChar(srcFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0)
    else
      sf := CreateFileA(PAnsiChar(AnsiString(srcFile)), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if sf <> INVALID_HANDLE_VALUE then begin
      if GetVersion and $80000000 = 0 then
        df := CreateFileW(PWideChar(dstFile), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0)
      else
        df := CreateFileA(PAnsiChar(AnsiString(dstFile)), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
      if df <> INVALID_HANDLE_VALUE then begin
        splitInt64(sl).loCard := GetFileSize(sf, @splitInt64(sl).hiCard);
        dl := 12 + sl * 11 div 10 + 12;
        if (sl <= CMapSize) or (GetVersion and $80000000 = 0) then begin
          sm := CreateFileMappingA(sf, nil, PAGE_READONLY, 0, 0, nil);
          if sm <> 0 then begin
            dm := CreateFileMappingA(df, nil, PAGE_READWRITE, splitInt64(dl).hiCard, splitInt64(dl).loCard, nil);
            if dm <> 0 then begin
              dl := CompressEx(sf, df, sm, dm, 12, nil, nil, sl, dl, @crc);
              CloseHandle(dm);
            end else begin
              dl := 0;
              err := GetLastError;
            end;
            CloseHandle(sm);
          end else begin
            dl := 0;
            err := GetLastError;
          end;
        end else
          dl := CompressEx(sf, df, 0, 0, 12, nil, nil, sl, dl, @crc);
        if (dl > 0) and ((dl + 12 < sl) or (not failIfGrow)) then begin
          SetFilePointer2(df, 0, FILE_BEGIN);
          result := WriteFile(df, sl,  8, c1, nil) and (c1 = 8) and
                    WriteFile(df, crc, 4, c1, nil) and (c1 = 4);
          if not result then
            err := GetLastError;
        end else
          err := GetLastError;
        if result then begin
          inc(dl, 12);
          SetFilePointer2(df, dl, FILE_BEGIN);
          SetEndOfFile(df);
        end;
        CloseHandle(df);
        if not result then
          if GetVersion and $80000000 = 0 then
            DeleteFileW(PWideChar(dstFile))
          else
            DeleteFileA(PAnsiChar(AnsiString(dstFile)));
      end else
        err := GetLastError;
      CloseHandle(sf);
    end else
      err := GetLastError;
  except
    if GetVersion and $80000000 = 0 then begin
      SetFileAttributesW(PWideChar(dstFile), 0);
      DeleteFileW(PWideChar(dstFile));
    end else begin
      SetFileAttributesA(PAnsiChar(AnsiString(dstFile)), 0);
      DeleteFileA(PAnsiChar(AnsiString(dstFile)));
    end;
    err := ERROR_ACCESS_DENIED;
  end;
  if not result then
    SetLastError(err);
end;

function Uncompress(const srcFile, dstFile: UnicodeString; lastWriteTime: int64 = 0; attr: dword = 0) : boolean; overload;
var sf, df     : THandle;
    sm, dm     : THandle;
    sl, dl     : int64;
    err        : dword;
    crc1, crc2 : dword;
    c1         : dword;
begin
  result := false;
  err    := 0;
  try
    if GetVersion and $80000000 = 0 then
      sf := CreateFileW(PWideChar(srcFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0)
    else
      sf := CreateFileA(PAnsiChar(AnsiString(srcFile)), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
    if sf <> INVALID_HANDLE_VALUE then begin
      if GetVersion and $80000000 = 0 then
        df := CreateFileW(PWideChar(dstFile), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, attr or FILE_FLAG_SEQUENTIAL_SCAN, 0)
      else
        df := CreateFileA(PAnsiChar(AnsiString(dstFile)), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, attr or FILE_FLAG_SEQUENTIAL_SCAN, 0);
      if df <> INVALID_HANDLE_VALUE then begin
        if ReadFile(sf, dl,   8, c1, nil) and (c1 = 8) and
           ReadFile(sf, crc1, 4, c1, nil) and (c1 = 4) then begin
          splitInt64(sl).loCard := GetFileSize(sf, @splitInt64(sl).hiCard);
          if (dl <= CMapSize) or (GetVersion and $80000000 = 0) then begin
            sm := CreateFileMappingA(sf, nil, PAGE_READONLY, 0, 0, nil);
            if sm <> 0 then begin
              dm := CreateFileMappingA(df, nil, PAGE_READWRITE, splitInt64(dl).hiCard, splitInt64(dl).loCard, nil);
              if dm <> 0 then begin
                dl := UncompressEx(sf, df, sm, dm, 12, nil, nil, sl, dl, @crc2);
                CloseHandle(dm);
              end else begin
                dl := 0;
                err := GetLastError;
              end;
              CloseHandle(sm);
            end else begin
              dl := 0;
              err := GetLastError;
            end;
          end else
            dl := UncompressEx(sf, df, 0, 0, 12, nil, nil, sl, dl, @crc2);
          result := (dl > 0) and (crc1 = crc2);
        end else
          err := GetLastError;
        if result then begin
          SetFilePointer2(df, dl, FILE_BEGIN);
          SetEndOfFile(df);
        end;
        if result and (lastWriteTime <> 0) then
          SetFileTime(df, nil, nil, @lastWriteTime);
        CloseHandle(df);
        if result then begin
          if (attr <> 0) and (GetVersion and $80000000 = 0) then
            if GetVersion and $80000000 = 0 then
              SetFileAttributesW(PWideChar(dstFile), attr)
            else
              SetFileAttributesA(PAnsiChar(AnsiString(dstFile)), attr);
        end else
          if GetVersion and $80000000 = 0 then
            DeleteFileW(PWideChar(dstFile))
          else
            DeleteFileA(PAnsiChar(AnsiString(dstFile)));
      end else
        err := GetLastError;
      CloseHandle(sf);
    end else
      err := GetLastError;
  except
    if GetVersion and $80000000 = 0 then begin
      SetFileAttributesW(PWideChar(dstFile), 0);
      DeleteFileW(PWideChar(dstFile));
    end else begin
      SetFileAttributesA(PAnsiChar(AnsiString(dstFile)), 0);
      DeleteFileA(PAnsiChar(AnsiString(dstFile)));
    end;
    err := ERROR_ACCESS_DENIED;
  end;
  if not result then
    SetLastError(err);
end;

function IsCompressedFileEqual(const uncomprFile, comprFile: UnicodeString) : boolean;
var size1, size2 : int64;
    crc1, crc2   : dword;
begin
  result := GetCompressedFileInfo  (comprFile,   size1, crc1) and
            GetUncompressedFileInfo(uncomprFile, size2, crc2) and
            (size1 = size2) and (crc1 = crc2);
end;

function GetCompressedFileInfo(const comprFile: UnicodeString; var size: int64; var crc32: dword) : boolean;
var file_ : THandle;
    c1    : dword;
begin
  result := false;
  crc32  := 0;
  if GetVersion and $80000000 = 0 then
    file_ := CreateFileW(PWideChar(comprFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0)
  else
    file_ := CreateFileA(PAnsiChar(AnsiString(comprFile)), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if file_ <> INVALID_HANDLE_VALUE then begin
    result := ReadFile(file_, size,  8, c1, nil) and (c1 = 8) and
              ReadFile(file_, crc32, 4, c1, nil) and (c1 = 4);
    CloseHandle(file_);
  end;
end;

function GetUncompressedFileInfo(const uncomprFile: UnicodeString; var size: int64; var crc32: dword) : boolean;
var file_, map : THandle;
    buf        : pointer;
    i64        : int64;
    crc        : dword;
begin
  result := false;
  if GetVersion and $80000000 = 0 then
    file_ := CreateFileW(PWideChar(uncomprFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0)
  else
    file_ := CreateFileA(PAnsiChar(AnsiString(uncomprFile)), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if file_ <> INVALID_HANDLE_VALUE then begin
    splitInt64(size).loCard := GetFileSize(file_, @splitInt64(size).hiCard);
    map := CreateFileMappingA(file_, nil, PAGE_READONLY, 0, 0, nil);
    if map <> 0 then begin
      i64 := size;
      crc := $ffffffff;
      while i64 > CMapSize do begin
        buf := MapViewOfFile2(map, FILE_MAP_READ, size - i64, CMapSize);
        if buf <> nil then begin
          crc := updateCrc32(crc, buf^, CMapSize);
          UnmapViewOfFile(buf);
          i64 := i64 - CMapSize;
        end else
          break;
      end;
      if i64 <= CMapSize then begin
        buf := MapViewOfFile2(map, FILE_MAP_READ, size - i64, i64);
        if buf <> nil then begin
          crc := updateCrc32(crc, buf^, i64);
          UnmapViewOfFile(buf);
          result := true;
          crc32 := not crc;
        end;
      end;
      CloseHandle(map);
    end;
    CloseHandle(file_);
  end;
end;

const crc32Tab : array [0..255] of cardinal =
                 ($00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
                  $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
                  $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
                  $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
                  $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
                  $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
                  $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
                  $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
                  $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
                  $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
                  $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
                  $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
                  $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
                  $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
                  $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
                  $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
                  $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
                  $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
                  $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
                  $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
                  $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
                  $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
                  $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
                  $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
                  $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
                  $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
                  $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
                  $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
                  $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
                  $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
                  $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
                  $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d);
function UpdateCrc32(crc32: cardinal; const inBuf; inLen: integer) : cardinal;
var i1 : integer;
    ab : array [0..maxInt - 1] of byte absolute inBuf;
begin
  result := crc32;
  for i1 := 0 to inLen - 1 do
    result := crc32Tab[byte(result xor ab[i1])] xor (result shr 8);
end;

function Zip(const zip: UnicodeString; const files: array of UnicodeString; const zipAs: TDAUnicodeString) : boolean;
type
  TFileInfo = packed record
    neededVersion : word;            // $14
    flags         : word;            // 0
    zipMethod     : word;            // 8 (deflate)
    lastModTime   : word;            // dos format
    lastModDate   : word;            // dos format
    crc32         : dword;
    zipSize       : dword;
    fullSize      : dword;
    nameLen       : word;            // length(name)
    extraLen      : word;            // 0
  end;
  TLocalFileHeader = packed record
    signature     : dword;           // $04034b50
    fileInfo      : TFileInfo;
  end;
  TFileHeader = packed record
    signature     : dword;           // $02014b50
    madeBy        : word;            // $14
    fileInfo      : TFileInfo;
    commentLen    : word;            // 0
    firstDiskNo   : word;            // 0
    intFileAttr   : word;            // 0 = binary; 1 = text
    extFileAttr   : dword;           // dos file attributes
    localHeadOff  : dword;           // @TLocalFileHeader
  end;
  TLastHeader = packed record
    signature     : dword;           // $06054b50
    thisDisk      : word;            // 0
    headerDisk    : word;            // 0
    thisFiles     : word;            // 1
    totalFiles    : word;            // 1
    headerSize    : dword;           // sizeOf(TFileHeaders + names)
    headerOffset  : dword;           // @TFileHeader
    commentLen    : word;            // 0
  end;
var i1, i2, i3 : integer;
    dstFh      : THandle;
    srcFh      : THandle;
    ft         : TFileTime;
    c1         : dword;
    lfhr       : TLocalFileHeader;
    srcBuf     : pointer;
    dstBuf     : pointer;
    size       : dword;
    us1        : UnicodeString;
    zipRec     : array of record
                   name : AnsiString;
                   fhr  : TFileHeader;
                 end;
    lhr        : TLastHeader;
    i64        : int64;
begin
  if GetVersion and $80000000 = 0 then
    dstFh := CreateFileW(PWideChar(zip), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0)
  else
    dstFh := CreateFileA(PAnsiChar(AnsiString(zip)), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
  result := dstFh <> INVALID_HANDLE_VALUE;
  if result then begin
    SetLength(zipRec, Length(files));
    i2 := 0;
    for i1 := 0 to high(files) do
      with zipRec[i2] do begin
        if i1 >= length(zipAs) then begin
          us1 := files[i1];
          for i3 := Length(us1) downto 1 do
            if us1[i3] = '\' then begin
              Delete(us1, 1, i3);
              break;
            end;
        end else
          us1 := zipAs[i1];
        name := EncodeUtf8(us1);
        if GetVersion and $80000000 = 0 then
          srcFh := CreateFileW(PWideChar(files[i1]), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0)
        else
          srcFh := CreateFileA(PAnsiChar(AnsiString(files[i1])), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
        if srcFh <> INVALID_HANDLE_VALUE then begin
          size := GetFileSize(srcFh, nil);
          srcBuf := pointer(LocalAlloc(LPTR, size));
          if srcBuf <> nil then begin
            dstBuf := pointer(LocalAlloc(LPTR, size * 11 div 10 + 12));
            if dstBuf <> nil then begin
              if ReadFile(srcFh, srcBuf^, size, c1, nil) and (c1 = size) then begin
                with lfhr, fileInfo do begin
                  signature     := $04034b50;
                  neededVersion := $14;
                  flags         := $0800;  // UTF-8 encoded file names
                  zipMethod     := 8;
                  crc32         := not UpdateCrc32(dword(-1), srcBuf^, size);
                  zipSize       := Compress(srcBuf, dstBuf, size, size * 11 div 10 + 12);
                  fullSize      := size;
                  nameLen       := length(name);
                  extraLen      := 0;
                  GetFileTime(srcFh, nil, nil, @ft);
                  FileTimeToLocalFileTime(ft, ft);
                  FileTimeToDosDateTime(ft, lastModDate, lastModTime);
                end;
                with fhr do begin
                  signature     := $02014b50;
                  madeBy        := $14;
                  fileInfo      := lfhr.fileInfo;
                  commentLen    := 0;
                  firstDiskNo   := 0;
                  intFileAttr   := 0;
                  if GetVersion and $80000000 = 0 then
                    extFileAttr   := GetFileAttributesW(PWideChar(files[i1]))
                  else
                    extFileAttr   := GetFileAttributesA(PAnsiChar(AnsiString(files[i1])));
                  i64 := 0;
                  SetFilePointer2(dstFh, 0, FILE_CURRENT, @i64);
                  localHeadOff  := i64;
                end;
                result := WriteFile(dstFh, lfhr,             sizeOf(lfhr),          c1, nil) and (c1 = sizeOf(lfhr)         ) and
                          WriteFile(dstFh, PAnsiChar(name)^, length(name),          c1, nil) and (c1 = dword(length(name))  ) and
                          WriteFile(dstFh, dstBuf^,          lfhr.fileInfo.zipSize, c1, nil) and (c1 = lfhr.fileInfo.zipSize);
                inc(i2);
              end;
              LocalFree(NativeUInt(dstBuf));
            end;
            LocalFree(NativeUInt(srcBuf));
          end;
          CloseHandle(srcFh);
        end;
        if not result then
          break;
      end;
    result := result and (i2 > 0);
    if result then begin
      with lhr do begin
        signature     := $06054b50;
        thisDisk      := 0;
        headerDisk    := 0;
        thisFiles     := i2;
        totalFiles    := i2;
        headerSize    := 0;
        SetFilePointer2(dstFh, 0, FILE_CURRENT, @i64);
        headerOffset  := i64;
        commentLen    := 0;
      end;
      for i1 := 0 to i2 - 1 do
        with zipRec[i1] do begin
          inc(lhr.headerSize, sizeOf(TFileHeader) + length(name));
          if not ( WriteFile(dstFh, fhr,              sizeOf(fhr),  c1, nil) and (c1 = sizeOf(fhr)         ) and
                   WriteFile(dstFh, PAnsiChar(name)^, length(name), c1, nil) and (c1 = dword(length(name)) )     ) then begin
            result := false;
            break;
          end;
        end;
      result := result and WriteFile(dstFh, lhr, sizeOf(lhr), c1, nil) and (c1 = sizeOf(lhr));
    end;
    CloseHandle(dstFh);
    if not result then
      if GetVersion and $80000000 = 0 then
        DeleteFileW(PWideChar(zip))
      else
        DeleteFileA(PAnsiChar(AnsiString(zip)));
  end;
end;

end.

