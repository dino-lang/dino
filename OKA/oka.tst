#!/bin/sh
#
# Script to testing Oka.
#
# Copyright (C) 1997-2007 Vladimir Makarov.
# 
# Written by Vladimir Makarov <vmakarov@users.sourceforge.net>
# 
# This file is part of the tool OKA.
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
#
#
# Usage: oka.tst
#
# The script requires GPERF.
#

OKA=./oka
SED=sed
script_file=$0
test_oka=_temp.oka
test_cfile=_temp.c
test_hfile=_temp.h
stderr=_stderr.out
ftemp=_temp

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

result=ok

if uname | fgrep CYGWIN; then
   CMP="eval sh -c 'tr -d \\\\r <\$0 >__tmp && mv __tmp \$0 && tr -d \\\\r <\$1 >__tmp && mv __tmp \$1 && cmp \$0 \$1'"
   SCMP="eval sh -c 'tr -d \\\\r <\$0 >__tmp && mv __tmp \$0 && cmp \$0 \$1'"
else
   CMP=cmp
   SCMP=cmp
fi

# Test 1.
if test $result = ok -a $start_test_number -le 1; then
	echo test 1
        cat >$test_oka <<'TEST1'
%automaton integer1 integer2
%unit <integer1> ib0
%unit <integer2> ib1

%instruction LDL

%%

LDL: (ib0 | ib1)
;
TEST1
        echo '      ' $OKA $test_oka "2>$stderr"
        if $OKA $test_oka 2>$stderr; then
           result=fail
        else
                echo '      ' $CMP $stderr $ftemp
                if cat >$ftemp <<'OUTPUT1' && $CMP $stderr $ftemp; then
_temp.oka:2:18: Units `ib0' and `ib1' should be in the same automaton
OUTPUT1
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 2.
if test $result = ok -a $start_test_number -le 2; then
	echo test 2
        cat >$test_oka <<'TEST2'
%automaton integer float

%unit <integer> ib0 ib1 load_store only_load multiplier multiplier_write_back

%unit <float> float_divider divider_write_back

%instruction LDL LDQ LDQ_U LDL_L LDQ_L LDS LDT
             STL STQ STQ_U
             STL_C STQ_C
             LDAH LDA ADDL ADDQ S4ADDL S4ADDQ S8ADDL S8ADDQ S4SUBL S4SUBQ
             S8SUBL S8SUBQ SUBL SUBQ CMPEQ CMPLT CMPLE CMPULT CMPULE CMPBGE AND
             BIC BIS ORNOT XOR EQV CMOVEQ CMOVNE CMOVLBS CMOVLT CMOVGE
             CMOVLBC CMOVLE CMOVGT SLL SRA SRL EXTBL EXTWL EXTLL EXTQL
             EXTWH EXTLH EXTQH INSBL INSWL INSLL INSQL INSWH INSLH INSQH
             MSKBL MSKWL MSKLL MSKQL MSKWH MSKLH MSKQH ZAP ZAPNOT
             STS STT
             MB TRAPB RPCC CALL_PAL JMP JSR RET JSR_COROUTINE BR BSR
             BLBC BLBS BEQ BNE BLT BLE BGT BGE
             FBEQ FBNE FBLT FBLE FBGT FBGE
             UMULH MULQ
             MULL
             ADDS ADDT SUBS SUBT MULS MULT CMPTEQ CMPTLT CMPTLE
             CPYS CPYSN CPYSE CVTLQ CVTQL CVTQS CVTQT CVTTQ CVTTS
             FCMOVEQ FCMOVNE FCMOVLE FCMOVLT FCMOVGE FCMOVGT
             DIVS
             DIVT
             RESERVED

%%


LDL, LDQ, LDQ_U, LDL_L, LDQ_L, LDS, LDT: (ib0 | ib1) + load_store
;

STL, STQ, STQ_U: ib1 + load_store
;

STL_C, STQ_C: (ib0 | ib1) + load_store  %nothing  only_load
;

LDAH, LDA, ADDL, ADDQ, S4ADDL, S4ADDQ, S8ADDL, S8ADDQ, S4SUBL, S4SUBQ, S8SUBL,
  S8SUBQ, SUBL, SUBQ, CMPEQ, CMPLT, CMPLE, CMPULT, CMPULE, CMPBGE, AND,
  BIC, BIS, ORNOT, XOR, EQV, CMOVEQ, CMOVNE, CMOVLBS, CMOVLT, CMOVGE,
  CMOVLBC, CMOVLE, CMOVGT, SLL, SRA, SRL, EXTBL, EXTWL, EXTLL, EXTQL,
  EXTWH, EXTLH, EXTQH, INSBL, INSWL, INSLL, INSQL, INSWH, INSLH, INSQH,
  MSKBL, MSKWL, MSKLL, MSKQL, MSKWH, MSKLH, MSKQH, ZAP, ZAPNOT:
             ib0 + multiplier_write_back
;

STS, STT: ib0 + load_store
;

MB, TRAPB, RPCC, CALL_PAL, JMP, JSR, RET, JSR_COROUTINE, BR, BSR, BLBC, BLBS,
   BEQ, BNE, BLT, BLE, BGT, BGE: ib1
;

FBEQ, FBNE, FBLT, FBLE, FBGT, FBGE: ib0
;

UMULH, MULQ: ib0 + multiplier*18  multiplier_write_back + multiplier*3
;

MULL:        ib0 + multiplier*16  multiplier_write_back + multiplier*3
;

ADDS, ADDT, SUBS, SUBT, MULS, MULT, CMPTEQ, CMPTLT, CMPTLE,
   CPYS, CPYSN, CPYSE, CVTLQ, CVTQL, CVTQS, CVTQT, CVTTQ, CVTTS,
   FCMOVEQ, FCMOVNE, FCMOVLE, FCMOVLT, FCMOVGE, FCMOVGT:
             ib1 + divider_write_back
;

DIVS: ib1 + float_divider*24  (float_divider + divider_write_back)*2
      float_divider*2
;

DIVT: ib1 + float_divider*53  (float_divider + divider_write_back)*2
      float_divider*4
;

RESERVED: ib1
;
TEST2
        echo '      ' $OKA $test_oka "2>&1 |" $SED "/transformation/d |" $SED "/minimization/d |" $SED "/generation/d >$stderr"
        if $OKA $test_oka 2>&1|$SED '/transformation/d'|$SED '/minimization/d'|$SED '/generation/d' >$stderr; then
                echo '      ' $CMP $stderr $ftemp
                if cat >$ftemp <<'OUTPUT2' && $CMP $stderr $ftemp; then

Automaton `integer'
      1176 NDFA states,            3544 NDFA arcs
      1176 DFA states,             3632 DFA arcs
       146 minimal DFA states,      611 minimal DFA arcs
       129 all instructions           9 instruction equivalence classes

Automaton `float'
       167 NDFA states,             420 NDFA arcs
       167 DFA states,              420 DFA arcs
       167 minimal DFA states,      420 minimal DFA arcs
       129 all instructions           5 instruction equivalence classes

  1659 all allocated states,       4062 all allocated arcs
  1777 all allocated alternative states
  1068 all comb vector elements,   2149 all transition table elements
    21 locked states number

OUTPUT2
                        result=ok
                else
                        result=fail
                fi
        else
           result=fail
        fi
fi

# Test 3.
if test $result = ok -a $start_test_number -le 3; then
	echo test 3
        cat >$test_oka <<'TEST3'
%automaton integer float

%unit <integer> e0 e1 load_store_1 load_store_2 store_reservation
                multiplier_write_back multiplier 

%unit <float> fa fm float_divider divider_write_back

%instruction LDL LDQ LDQ_U LDS LDT STL STQ STQ_U STS STT
             LDL_L LDQ_L MB WMB STL_C STQ_C FETCH
             RS RC HW_MFPR HW_MTPR BLBC BLBS BEQ BNE BLT BLE BGT BGE
             FBEQ FBNE FBLT FBLE FBGT FBGE
             JMP JSR RET JSR_COROUTINE BSR BR HW_REI CALLPAL
             LDAH LDA ADDL ADDLV ADDQ ADDQV S4ADDL S4ADDQ S8ADDL S8ADDQ
             S4SUBL S4SUBQ S8SUBL S8SUBQ SUBL SUBLV SUBQ SUBQV
             AND BIC BIS ORNOT XOR EQV
             SLL SRA SRL EXTBL EXTWL EXTLL EXTQL
             EXTWH EXTLH EXTQH INSBL INSWL INSLL INSQL INSWH INSLH INSQH
             MSKBL MSKWL MSKLL MSKQL MSKWH MSKLH MSKQH ZAP ZAPNOT
             CMOVEQ CMOVNE CMOVLBS CMOVLT CMOVGE CMOVLBC CMOVLE CMOVGT
             CMPEQ CMPLT CMPLE CMPULT CMPULE CMPBGE
             MULL MULLV MULL1 MULLV1 MULL2 MULLV2
             MULQ MULQV MULQ1 MULQV1 MULQ2 MULQV2 UMULH UMULH1 UMULH2
             ADDS ADDT SUBS SUBT CPYSN CPYSE CVTLQ CVTQL CVTTQ
             FCMOVEQ FCMOVNE FCMOVLE FCMOVLT FCMOVGE FCMOVGT
             DIVS DIVT MULS MULT CPYS RPCC TRAPB UNOP

%%

/* Class LD:
   o An instruction of class LD can not be simulteniously issued with
     an instruction of class ST;
   o An instruction of class LD can not be issued in the second cycle
     after an instruction of class ST is issued. */
LDL, LDQ, LDQ_U, LDS, LDT:
       (e0 + multiplier_write_back | e1) + (load_store_1 | load_store_2)
       + store_reservation
;

/* Class ST:
   o An instruction of class LD can not be simulteniously issued with
     an instruction of class ST;
   o An instruction of class LD can not be issued in the second cycle
     after an instruction of class ST is issued. */
STL, STQ, STQ_U, STS, STT:
       e0 + multiplier_write_back + load_store_1 + load_store_2  %nothing
       store_reservation
;

/* Class MBX */
LDL_L, LDQ_L, MB, WMB/*???*/, STL_C, STQ_C, FETCH/*???*/:
       e0 + multiplier_write_back
;

/* Class RX */
RS/*???*/, RC/*???*/: e0 + multiplier_write_back
;

/* Class MXPR */
HW_MFPR, HW_MTPR/*???*/: %nothing /*???*/
;

/* Class IBR */
BLBC, BLBS, BEQ, BNE, BLT, BLE, BGT, BGE: e1
;

/* Class FBR */
FBEQ, FBNE, FBLT, FBLE, FBGT, FBGE: fa
;

/* Class JSR */
JMP, JSR, RET, JSR_COROUTINE, BSR, BR, HW_REI/*???*/, CALLPAL: e1
;

/* Class IADD */
LDAH, LDA, ADDL, ADDLV/*???*/, ADDQ, ADDQV, S4ADDL, S4ADDQ, S8ADDL, S8ADDQ,
  S4SUBL, S4SUBQ, S8SUBL, S8SUBQ, SUBL, SUBLV/*???*/, SUBQ, SUBQV/*???*/:
             e0 + multiplier_write_back
;

/* Class ILOG */
AND, BIC, BIS, ORNOT, XOR, EQV :  (e0 + multiplier_write_back | e1)
;

/* Class SHIFT */
SLL, SRA, SRL, EXTBL, EXTWL, EXTLL, EXTQL,
  EXTWH, EXTLH, EXTQH, INSBL, INSWL, INSLL, INSQL, INSWH, INSLH, INSQH,
  MSKBL, MSKWL, MSKLL, MSKQL, MSKWH, MSKLH, MSKQH, ZAP, ZAPNOT:
             e0 + multiplier_write_back
;

/* Class CMOV */
CMOVEQ, CMOVNE, CMOVLBS, CMOVLT, CMOVGE, CMOVLBC, CMOVLE, CMOVGT:
             (e0 + multiplier_write_back | e1)
;

/* Class ICMP */
CMPEQ, CMPLT, CMPLE, CMPULT, CMPULE, CMPBGE: (e0 + multiplier_write_back | e1)
;

/* Class IMULL:
   o Thirty-two-bit multiplies have an 8-cycle latency, and the
     multiplier can start a second multiply after 4 cycles, provided
     that the second multiply has no data dependency on the first;
   o No instruction can be issued to pipe e0 exactly two cycles before
     an integer multiplication complete.  */
MULL, MULLV/*???*/:  e0 + multiplier_write_back + multiplier*4  %nothing*2
                 multiplier_write_back
;

/* Class IMULL with 1 cycle delay */
MULL1, MULLV1/*???*/:  e0 + multiplier_write_back  %nothing + multiplier*4
                       %nothing*2 multiplier_write_back
;

/* Class IMULL with 2 cycles delay */
MULL2, MULLV2/*???*/:  e0 + multiplier_write_back  %nothing*2 + multiplier*4
                       %nothing*2 multiplier_write_back
;

/* Class IMULQ:
   o Sixty-for-bit signed multiplies have an 12-cycle latency, and the
     multiplier can start a second multiply after 8 cycles, provided
     that the second multiply has no data dependency on the first;
   o No instruction can be issued to pipe e0 exactly two cycles before
     an integer multiplication complete. */
MULQ, MULQV/*???*/: e0 + multiplier_write_back + multiplier*8  %nothing*2
                    multiplier_write_back
;

/* Class IMULQ with 1 cycle delay */
MULQ1, MULQV1/*???*/: e0 + multiplier_write_back  %nothing + multiplier*8
                      %nothing*2  multiplier_write_back
;

/* Class IMULQ with 2 cycles delay */
MULQ2, MULQV2/*???*/: e0 + multiplier_write_back  %nothing*2 + multiplier*8
                      %nothing*2  multiplier_write_back
;

/* Class IMULH
   o Sixty-for-bit unsigend multiplies have an 14-cycle latency, and
     the multiplier can start a second multiply after 8 cycles, provided
     that the second multiply has no data dependency on the first;
   o No instruction can be issued to pipe e0 exactly two cycles before
     an integer multiplication complete. */
UMULH: e0 + multiplier_write_back + multiplier*8  %nothing*4
       multiplier_write_back
;

/* Class IMULH with 1 cycle delay */
UMULH1: e0 + multiplier_write_back  %nothing + multiplier*8  %nothing*4
       multiplier_write_back
;

/* Class IMULH with 2 cycles delay */
UMULH2: e0 + multiplier_write_back  %nothing*2 + multiplier*8  %nothing*4
       multiplier_write_back
;

/* Class FADD */
ADDS, ADDT, SUBS, SUBT, CPYSN, CPYSE, CVTLQ, CVTQL, CVTTQ,
   FCMOVEQ, FCMOVNE, FCMOVLE, FCMOVLT, FCMOVGE, FCMOVGT:
             fa + divider_write_back
;

/* Class FDIV:
   o 2.4 bits per cycle average rate.  The next floating divide can be
     issued in the same cycle the result of the previous divide's result
     is avialable.
   o Instruction issue to teh add pipeline continues whaile a divide
     is in progress until the result is ready.  At that point the issue
     stage in the instruction umit stalls one cycle to allow the
     quotient to be sent the round adder and then be written into the
     register file. */
DIVS: fa + float_divider*18/*???*/ + divider_write_back 
;

/* Class FDIV:
   o 2.4 bits per cycle average rate.  The next floating divide can be
     issued in the same cycle the result of the previous divide's result
     is avialable.
   o Instruction issue to teh add pipeline continues whaile a divide
     is in progress until the result is ready.  At that point the issue
     stage in the instruction umit stalls one cycle to allow the
     quotient to be sent the round adder and then be written into the
     register file. */
DIVT: fa + float_divider*30/*???*/ + divider_write_back 
;

/* Class FMUL */
MULS, MULT: fm
;

/* Class FCPYS */
CPYS: (fa + divider_write_back | fm)
;

/* Class MISC */
RPCC, TRAPB: e0 + multiplier_write_back
;

/* Class UNOP */
UNOP: %nothing
;
TEST3
        echo '      ' $OKA $test_oka "2>&1 |" $SED "/transformation/d |" $SED "/minimization/d |" $SED "/generation/d >$stderr"
        if $OKA $test_oka 2>&1|$SED '/transformation/d'|$SED '/minimization/d'|$SED '/generation/d' >$stderr; then
                echo '      ' $CMP $stderr $ftemp
                if cat >$ftemp <<'OUTPUT3' && $CMP $stderr $ftemp; then

Automaton `integer'
      3048 NDFA states,           12732 NDFA arcs
      2651 DFA states,            11527 DFA arcs
      1619 minimal DFA states,     8720 minimal DFA arcs
       146 all instructions          16 instruction equivalence classes

Automaton `float'
       180 NDFA states,             720 NDFA arcs
       209 DFA states,              867 DFA arcs
       149 minimal DFA states,      687 minimal DFA arcs
       146 all instructions           8 instruction equivalence classes

  4520 all allocated states,      15219 all allocated arcs
  3640 all allocated alternative states
 11706 all comb vector elements,  27096 all transition table elements
     0 locked states number

OUTPUT3
                        result=ok
                else
                        result=fail
                fi
        else
           result=fail
        fi
fi

# Test 4.
if test $result = ok -a $start_test_number -le 4; then
	echo test 4
        cat >$test_oka <<'TEST4'
%automaton integer1
%unit <integer1> ib0 ib1
%exclusion ib0 : ib0

%instruction LDL

%%

LDL: (ib0 | ib1)
;
TEST4
        echo '      ' $OKA $test_oka "2>$stderr"
        if $OKA $test_oka 2>$stderr; then
           result=fail
        else
                echo '      ' $CMP $stderr $ftemp
                if cat >$ftemp <<'OUTPUT4' && $CMP $stderr $ftemp; then
_temp.oka:3:12: unit `ib0' excludes itself
_temp.oka:3:18: unit `ib0' excludes itself
OUTPUT4
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 5.
if test $result = ok -a $start_test_number -le 5; then
	echo test 5
        cat >$test_oka <<'TEST5'
%automaton integer1 integer2
%unit <integer1> ib0
%unit <integer2> ib1
%exclusion ib0 : ib1

%instruction LDL

%%

LDL: (ib0 | ib1)
;
TEST5
        echo '      ' $OKA $test_oka "2>$stderr"
        if $OKA $test_oka 2>$stderr; then
           result=fail
        else
                echo '      ' $CMP $stderr $ftemp
                if cat >$ftemp <<'OUTPUT5' && $CMP $stderr $ftemp; then
_temp.oka:4:12: units `ib1' and `ib0' in exclusion set belong to different automata
_temp.oka:4:18: units `ib0' and `ib1' in exclusion set belong to different automata
OUTPUT5
                        result=ok
                else
                        result=fail
                fi
        fi
fi

# Test 6.
if test $result = ok -a $start_test_number -le 6; then
	echo test 6
        cat >$test_oka <<'TEST6'
%automaton integer1
%unit <integer1> ib0
%unit <integer1> ib1
%exclusion ib0 : ib1

%instruction LDL

%%

LDL: (ib0 | ib1)
;
TEST6
        echo '      ' $OKA $test_oka "2>&1 |" $SED "/transformation/d |" $SED "/minimization/d |" $SED "/generation/d >$stderr"
        if $OKA $test_oka 2>&1|$SED '/transformation/d'|$SED '/minimization/d'|$SED '/generation/d' >$stderr; then
                echo '      ' $CMP $stderr $ftemp
                if cat >$ftemp <<'OUTPUT6' && $CMP $stderr $ftemp; then

Automaton `integer1'
         3 NDFA states,               5 NDFA arcs
         2 DFA states,                3 DFA arcs
         2 minimal DFA states,        3 minimal DFA arcs
         2 all instructions           2 instruction equivalence classes

     4 all allocated states,          6 all allocated arcs
     4 all allocated alternative states
     3 all comb vector elements,      4 all transition table elements
     1 locked states number

OUTPUT6
                        result=ok
                else
                        result=fail
                fi
        else
           result=fail
        fi
fi

# Test 7.
if test $result = ok -a $start_test_number -le 7; then
	echo test 7
        cat >$test_oka <<'TEST7'
%unit ib0 ib1
%exclusion ib0 : ib1

%instruction LDL

%%

LDL: (ib0 | ib1)
;
TEST7
        echo '      ' $OKA $test_oka "2>&1 |" $SED "/transformation/d |" $SED "/minimization/d |" $SED "/generation/d >$stderr"
        if $OKA $test_oka 2>&1|$SED '/transformation/d'|$SED '/minimization/d'|$SED '/generation/d' >$stderr; then
                echo '      ' $CMP $stderr $ftemp
                if cat >$ftemp <<'OUTPUT7' && $CMP $stderr $ftemp; then

Automaton #0
         3 NDFA states,               5 NDFA arcs
         2 DFA states,                3 DFA arcs
         2 minimal DFA states,        3 minimal DFA arcs
         2 all instructions           2 instruction equivalence classes

     4 all allocated states,          6 all allocated arcs
     4 all allocated alternative states
     3 all comb vector elements,      4 all transition table elements
     1 locked states number

OUTPUT7
                        result=ok
                else
                        result=fail
                fi
        else
           result=fail
        fi
fi

# Final message

if test $result = ok; then
        echo $script_file: it is all ok
        rm -f $test_cfile $test_hfile $test_oka $stderr $ftemp
        exit 0
else
        echo '***' $script_file: test is failed see files $test_oka $stderr $ftemp
        exit 1
fi
