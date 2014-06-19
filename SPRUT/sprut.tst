#!/bin/sh

#
# Script to test of internal representation description translator `sprut'.
# The script tests only semantically incorrect descriptions (B-tests).
#
# Copyright (C) 1997-2007 Vladimir Makarov.
# 
# Written by Vladimir Makarov <vmakarov@users.sourceforge.net>
# 
# This file is part of the tool SPRUT.
# 
# This is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# This software is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GNU CC; see the file COPYING.  If not, write to the Free
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
#
# Usage: sprut.tst [start_test_number]
#
# The script outputs self-explanatory messages and returns zero exit code if 
# it is all ok.
#

SPRUT="./sprut -v"
script_file=$0
# Do not change `test_file', `test_file_1' values
test_prefix=_test
test_prefix_1=_test_1
test_file=$test_prefix.sprut
test_file_1=$test_prefix_1.sprut
temp_file=_temp.out
temp2_file=_temp2.out
result=ok

if test x$1 = x
then
    start_test_number=1
else
    if expr $1 + 0 >/dev/null
    then
        start_test_number=`expr $1 + 0`
    else
        echo $script_file:invalid argument $1 2>&1
        exit 1
    fi
fi

# Test 1.
if test $result = ok -a $start_test_number -le 1; then
        cat >$test_file <<'TEST1'
abracadabra
TEST1

        echo test 1:
        echo '      ' $SPRUT $test_file -galimatya "2>$temp_file"
        if $SPRUT $test_file -galimatya 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT1' && cmp $temp_file $temp2_file; then
sprut: unknown flag `-galimatya'
OUTPUT1
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 2.
if test $result = ok -a $start_test_number -le 2; then
        cat >$test_file <<'TEST2'
abracadabra
TEST2

        echo test 2:
        echo '      ' $SPRUT $test_file $test_file "2>$temp_file"
        if $SPRUT $test_file $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT2' && cmp $temp_file $temp2_file; then
sprut: one specification file must be on command line
OUTPUT2
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 3.
if test $result = ok -a $start_test_number -le 3; then

        echo test 3:
        echo '      ' $SPRUT abracadabra.c "2>$temp_file"
        if $SPRUT abracadabra.c 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT3' && cmp $temp_file $temp2_file; then
sprut: specification file must have suffix `.sprut'
OUTPUT3
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 4.
if test $result = ok -a $start_test_number -le 4; then

        echo test 4:
        echo '      ' $SPRUT abracadabra.sprut "2>$temp_file"
        if $SPRUT abracadabra.sprut 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT4' && cmp $temp_file $temp2_file; then
fatal error -- `abracadabra.sprut': No such file or directory
OUTPUT4
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 5.
if test $result = ok -a $start_test_number -le 5; then
        cat >$test_file <<'TEST5'
%skeleton
%%
TEST5

        echo test 5:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT5' && cmp $temp_file $temp2_file; then
_test.sprut:1:1: syntax error in declaration
OUTPUT5
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 6.
if test $result = ok -a $start_test_number -le 6; then
        cat >$test_file <<'TEST6'
%type
%skeleton
%%
TEST6

        echo test 6:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT6' && cmp $temp_file $temp2_file; then
_test.sprut:2:1: syntax error in predefined type declaration
OUTPUT6
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 7.
if test $result = ok -a $start_test_number -le 7; then
        cat >$test_file <<'TEST7'
%double
%skeleton
%%
TEST7

        echo test 7:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT7' && cmp $temp_file $temp2_file; then
_test.sprut:2:1: syntax error in double node type declaration
OUTPUT7
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 8.
if test $result = ok -a $start_test_number -le 8; then
        cat >$test_file <<'TEST8'
%%
%skeleton
TEST8

        echo test 8:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT8' && cmp $temp_file $temp2_file; then
_test.sprut:2:1: syntax error in node type definition
OUTPUT8
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 9.
if test $result = ok -a $start_test_number -le 9; then
        cat >$test_file <<'TEST9'
%%
a :: %root
%skeleton a
b : %root
TEST9

        echo test 9:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT9' && cmp $temp_file $temp2_file; then
_test.sprut:4:1: syntax error in field definition
OUTPUT9
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 10.
if test $result = ok -a $start_test_number -le 10; then
        cat >$test_file <<'TEST10'
%%
a :: %root
%class
f : %double %root
TEST10

        echo test 10:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT10' && cmp $temp_file $temp2_file; then
_test.sprut:4:1: class field `f' is double
OUTPUT10
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 11.
if test $result = ok -a $start_test_number -le 11; then
        cat >$test_file <<'TEST11'
%extend
_test_1
%%
a :: %root
%class
f : %root
TEST11
        cat >$test_file_1 <<'TEST11'
%extend
_test
%%
b :: %root
%class
f1 : %root
TEST11

        echo test 11:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=two_files_fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT11' && cmp $temp_file $temp2_file; then
In file processed from _test.sprut:2:8:
_test_1.sprut:2:1: fatal error -- cycle on inclusion of file `_test.sprut'
OUTPUT11
                        result=ok
                else
                        result=two_files_fail
                fi
        fi
fi

# Test 12.
if test $result = ok -a $start_test_number -le 12; then
        cat >$test_file <<'TEST12'
%extend
__test__
%%
a :: %root
%class
f : %root
TEST12

        echo test 12:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT12' && cmp $temp_file $temp2_file; then
_test.sprut:2:1: fatal error -- `__test__.sprut': No such file or directory
OUTPUT12
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 13.
if test $result = ok -a $start_test_number -le 13; then
        cat >$test_file <<'TEST13'
%local {
''
'
'\9
'\0000
"aa
}
%%
a :: %root
%class
f : %root
TEST13

        echo test 13:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT13' && cmp $temp_file $temp2_file; then
_test.sprut:2:2: invalid character constant
_test.sprut:3:2: invalid character constant
_test.sprut:4:4: invalid character constant
_test.sprut:5:6: invalid character constant
_test.sprut:6:4: string end is absent
OUTPUT13
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 14.
if test $result = ok -a $start_test_number -le 14; then
        cat >$test_file <<'TEST14'
%%
a :: %root
%class
f : %root [/* aaaa
TEST14

        echo test 14:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT14' && cmp $temp_file $temp2_file; then
_test.sprut:4:11: constraint end is absent
OUTPUT14
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 15.
if test $result = ok -a $start_test_number -le 15; then
        cat >$test_file <<'TEST15'
%local {}
%%
a :: %root
%class
{ /* aaaaaaaaaa  */
f : %root
TEST15

        echo test 15:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT15' && cmp $temp_file $temp2_file; then
_test.sprut:5:1: C code insertion end is absent
OUTPUT15
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 16.
if test $result = ok -a $start_test_number -le 16; then
        cat >$test_file <<'TEST16'
%skelton
%%
a :: %root
%class
f : %root
TEST16

        echo test 16:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT16' && cmp $temp_file $temp2_file; then
_test.sprut:1:1: unknown keyword `%skelton'
OUTPUT16
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 17.
if test $result = ok -a $start_test_number -le 17; then
        cat >$test_file <<'TEST17'
%double a
%
%%
a :: %root
%class
f : %root
TEST17

        echo test 17:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT17' && cmp $temp_file $temp2_file; then
_test.sprut:2:1: invalid input character '%'
OUTPUT17
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 18.
if test $result = ok -a $start_test_number -le 18; then
        cat >$test_file <<'TEST18'
%double a
-
%%
a :: %root
%class
f : %root
TEST18

        echo test 18:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT18' && cmp $temp_file $temp2_file; then
_test.sprut:2:1: invalid input character '-'
OUTPUT18
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 19.
if test $result = ok -a $start_test_number -le 19; then
        cat >$test_file <<'TEST19'
%extend _test_1
%type a
%%
b :: %root
%class
f1 : %root
TEST19

        cat >$test_file_1 <<'TEST19'
%%
a :: %root
%class
f2 : %root
TEST19

        echo test 19:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=two_files_fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT19' && cmp $temp_file $temp2_file; then
_test.sprut:2:7: identifier `a' is declared early as node type
_test_1.sprut:2:1: here declaration of the node type
OUTPUT19
                        result=ok
                else
                        result=two_files_fail
                fi
        fi
fi

# Test 20.
if test $result = ok -a $start_test_number -le 20; then
        cat >$test_file <<'TEST20'
%type a
%%
a :: %root
%class
f : %root
TEST20

        echo test 20:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT20' && cmp $temp_file $temp2_file; then
_test.sprut:3:1: identifier `a' is declared early as predefined type
_test.sprut:1:7: here declaration of predefined type
OUTPUT20
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 21.
if test $result = ok -a $start_test_number -le 21; then
        cat >$test_file <<'TEST21'
%%
a :: %root
%class
f1 : %root
  ;
a :: %root
%class
f2 : %root
TEST21

        echo test 21:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT21' && cmp $temp_file $temp2_file; then
_test.sprut:6:1: `a' is declared early with basic super types
_test.sprut:2:1: here declaration with basic super types
OUTPUT21
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 22.
if test $result = ok -a $start_test_number -le 22; then
        cat >$test_file <<'TEST22'
%%
a :: %root
%class
f1 : %root
  ;
%abstract
a
%class
f2 : %root
TEST22

        echo test 22:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT22' && cmp $temp_file $temp2_file; then
_test.sprut:7:1: `%abstract' can't be without basic super types
OUTPUT22
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 23.
if test $result = ok -a $start_test_number -le 23; then
        cat >$test_file <<'TEST23'
%%
a
%class
f1 : %root
  ;
a
%class
f2 : %root
TEST23

        echo test 23:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT23' && cmp $temp_file $temp2_file; then
_test.sprut:2:1: basic super types aren't defined anywhere for node type `a'
OUTPUT23
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 24.
if test $result = ok -a $start_test_number -le 24; then
        cat >$test_file <<'TEST24'
%%
a :: b
%class
f : %root
TEST24

        echo test 24:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT24' && cmp $temp_file $temp2_file; then
_test.sprut:2:6: undefined identifier `b' is used as super type
OUTPUT24
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 25.
if test $result = ok -a $start_test_number -le 25; then
        cat >$test_file <<'TEST25'
%type b
%%
a :: b
%class
f : %root
TEST25

        echo test 25:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT25' && cmp $temp_file $temp2_file; then
_test.sprut:3:6: predefined type `b' is used as super type
_test.sprut:1:7: here declaration of predefined type
OUTPUT25
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 26.
if test $result = ok -a $start_test_number -le 26; then
        cat >$test_file <<'TEST26'
%%
a :: b
%class
f1 : %root
  ;
a
%class
f2 : %root
   ;
b :: a
TEST26

        echo test 26:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT26' && cmp $temp_file $temp2_file; then
_test.sprut:2:1: cycle in super type `a' of node type `b'
_test.sprut:10:1: cycle in super type `b' of node type `a'
OUTPUT26
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 27.
if test $result = ok -a $start_test_number -le 27; then
        cat >$test_file <<'TEST27'
%%
a :: %root
%class
f1 : b
  ;
a
%class
f2 : %root
   ;
TEST27

        echo test 27:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT27' && cmp $temp_file $temp2_file; then
_test.sprut:4:6: undefined identifier `b' is used as field type
OUTPUT27
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 28.
if test $result = ok -a $start_test_number -le 28; then
        cat >$test_file <<'TEST28'
%%
a :: %root
%class
f1 : %root
  ;
a
%class
f1 : %root
f1 : %root
   ;
b :: a
TEST28

        echo test 28:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT28' && cmp $temp_file $temp2_file; then
_test.sprut:8:1: field `f1' is declared early in node type `a'
_test.sprut:4:1: here first declaration of the field
_test.sprut:9:1: field `f1' is declared early in node type `a'
_test.sprut:4:1: here first declaration of the field
OUTPUT28
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 29.
if test $result = ok -a $start_test_number -le 29; then
        cat >$test_file <<'TEST29'
%%
a :: %root
%class
f1 : %root
  ;
a
%class
f2 : %root
   ;
b :: a
%class
f1 : %root
  ;
b
%class
f2 : %root
TEST29

        echo test 29:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT29' && cmp $temp_file $temp2_file; then
_test.sprut:12:1: field `f1' is declared early in super type `a'
_test.sprut:4:1: here first declaration of the field
_test.sprut:16:1: field `f2' is declared early in super type `a'
_test.sprut:8:1: here first declaration of the field
OUTPUT29
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 30.
if test $result = ok -a $start_test_number -le 30; then
        cat >$test_file <<'TEST30'
%%
a :: %root
%class
f1 : %root
  ;
b :: %root
%class
f1 : b
  ;
b
%class
f2 : %root
  ;
c :: %root
%class
f1 : b
TEST30

        echo test 30:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT30' && cmp $temp_file $temp2_file; then
_test.sprut:8:1: field `f1' in node type `a' has another type
_test.sprut:4:1: here first declaration of the field
_test.sprut:16:1: field `f1' in node type `a' has another type
_test.sprut:4:1: here first declaration of the field
OUTPUT30
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 31.
if test $result = ok -a $start_test_number -le 31; then
        cat >$test_file <<'TEST31'
%%
a :: %root
%other
f1 : %double %root
  ;
b :: %root
%other
f1 : %root
  ;
b
%class
f2 : %root
  ;
c :: %root
%other
f1 : %root
TEST31

        echo test 31:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT31' && cmp $temp_file $temp2_file; then
_test.sprut:8:1: field `f1' in node type `a' has another double attribute
_test.sprut:4:1: here first declaration of the field
_test.sprut:16:1: field `f1' in node type `a' has another double attribute
_test.sprut:4:1: here first declaration of the field
OUTPUT31
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 32.
if test $result = ok -a $start_test_number -le 32; then
        cat >$test_file <<'TEST32'
%%
a :: %root
%skeleton
f1 : %root
  ;
b :: %root
%class
f1 : %root
  ;
b
%class
f2 : %root
  ;
c :: %root
%class
f1 : %root
TEST32

        echo test 32:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT32' && cmp $temp_file $temp2_file; then
_test.sprut:8:1: field `f1' in node type `a' is in another (non-class) part
_test.sprut:4:1: here first declaration of the field
_test.sprut:16:1: field `f1' in node type `a' is in another (non-class) part
_test.sprut:4:1: here first declaration of the field
OUTPUT32
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 33.
if test $result = ok -a $start_test_number -le 33; then
        cat >$test_file <<'TEST33'
%extend
_test_1
%extend
_test_1
%%
a :: %root
%class
f : %root
TEST33
        cat >$test_file_1 <<'TEST33'
%%
b :: %root
%class
f1 : %root
TEST33

        echo test 33:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT33' && cmp $temp_file $temp2_file; then
_test.sprut:4:1: warning -- repeated extension of description `_test_1' is ignored
OUTPUT33
                        result=ok
                else
                        result=two_files_fail
                fi
        else
                result=two_files_fail
        fi
fi

# Test 34.
if test $result = ok -a $start_test_number -le 34; then
        cat >$test_file <<'TEST34'
%double
a
%double
a
%double
a
%%
a :: %root
%class
f : %root
TEST34

        echo test 34:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT34' && cmp $temp_file $temp2_file; then
_test.sprut:4:1: warning -- repeated declaration `a' as double node type
_test.sprut:2:1: here the first declaration of double node type
_test.sprut:6:1: warning -- repeated declaration `a' as double node type
_test.sprut:2:1: here the first declaration of double node type
OUTPUT34
                        result=ok
                else
                        result=fail
                fi
        else
                result=fail
        fi
fi

# Test 35.
if test $result = ok -a $start_test_number -le 35; then
        cat >$test_file <<'TEST35'
%type
a
%type
a
%type
a
%%
b :: %root
%class
f : %root
TEST35

        echo test 35:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT35' && cmp $temp_file $temp2_file; then
_test.sprut:4:1: warning -- repeated declaration of predefined type `a'
_test.sprut:2:1: here first declaration of predefined type
_test.sprut:6:1: warning -- repeated declaration of predefined type `a'
_test.sprut:2:1: here first declaration of predefined type
OUTPUT35
                        result=ok
                else
                        result=fail
                fi
        else
                result=fail
        fi
fi

# Test 36.
if test $result = ok -a $start_test_number -le 36; then
        cat >$test_file <<'TEST36'
%%
a :: %root
%class
f : %root
  ;
b :: %root
%class
f : %root
  ;
c :: %root
%class
f : %root
TEST36

        echo test 36:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT36' && cmp $temp_file $temp2_file; then
_test.sprut:8:1: warning -- there is also field `f' in node type `a'
_test.sprut:4:1: here previous declaration of the field
_test.sprut:12:1: warning -- there is also field `f' in node type `b'
_test.sprut:8:1: here previous declaration of the field
OUTPUT36
                        result=ok
                else
                        result=fail
                fi
        else
                result=fail
        fi
fi

# Test 37.
if test $result = ok -a $start_test_number -le 37; then
        cat >$test_file <<'TEST37'
%%
a :: %root
%class
fa : %root
  ;
b :: a
%class
fb : %root
  ;
c :: a
%class
fc : %root
  ;
d :: a
%class
fd : %root
;
d :: , c
;
TEST37

        echo test 37:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                result=fail
        else
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT37' && cmp $temp_file $temp2_file; then
_test.sprut:18:8: node type `d' has intersected super types `c' and `a'
OUTPUT37
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 38.
if test $result = ok -a $start_test_number -le 38; then
        cat >$test_file <<'TEST38'
%%
a :: %root
%class
fa : %root
  ;
b :: a
%class
fb : %root
  ;
b :: , a
  ;
TEST38

        echo test 38:
        echo '      ' $SPRUT $test_file "2>$temp_file"
        if $SPRUT $test_file 2>$temp_file; then
                echo '      ' cmp $temp_file $temp2_file
                if cat >$temp2_file <<'OUTPUT38' && cmp $temp_file $temp2_file; then
_test.sprut:10:8: repeated occurrence `a' as super type of `b' is ignored
OUTPUT38
                        result=ok
                else
                        result=fail
                fi
        else
                result=fail
        fi
fi


# Final message

rm -f $test_prefix.[ch] $test_prefix_1.[ch]

if test $result = ok; then
        echo $script_file: it is all ok
        rm -f $test_file $test_file_1 $temp_file $temp2_file a.out
        exit 0
elif test $result = fail; then
        echo '***' $script_file:test is failed see files $test_file, $temp_file, $temp2_file 
        exit 1
else
        echo '***' $script_file:test is failed
        echo '   '  see files $test_file, $test_file_1, $temp_file, $temp2_file 
        exit 1
fi
