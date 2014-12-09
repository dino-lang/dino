# !/bin/sh
# Usage: <script> d_file arguments
aout=`basename $1 .d`
f=$aout.hs
cp $1 $f && ghc -rtsopts -O2 $f -o $aout || (rm -f $f $aout && exit 1)
shift 1
./$aout $* && rm -rf $f $aout && exit 0
rm -f $f $aout
exit 1
