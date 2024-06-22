{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}


module Qafny.Syntax.Parser(scanAndParse) where
import qualified Qafny.Syntax.Lexer as L
import           Qafny.Syntax.ParserUtils
import           Qafny.Syntax.AST
import           Control.Monad
import           Data.Sum
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t9 t10 t11 t13 t14 t19 t25 t26 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49
	= HappyTerminal (L.SToken)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 (Toplevel')
	| HappyAbsSyn7 ([ Conds ])
	| HappyAbsSyn8 (Conds)
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 (QTy)
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 (Stmt')
	| HappyAbsSyn16 (Exp')
	| HappyAbsSyn17 (GuardExp)
	| HappyAbsSyn18 (Partition)
	| HappyAbsSyn19 t19
	| HappyAbsSyn21 ((SpecExp', AmpExp', PhaseExp))
	| HappyAbsSyn22 (AmpExp')
	| HappyAbsSyn23 (PhaseExp)
	| HappyAbsSyn24 (PhaseBinder)
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn30 (Op2)
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44
	| HappyAbsSyn45 t45
	| HappyAbsSyn46 t46
	| HappyAbsSyn47 t47
	| HappyAbsSyn48 t48
	| HappyAbsSyn49 t49

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,758) ([0,0,0,12,0,0,0,0,0,0,24576,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,256,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,216,1,0,0,0,0,0,0,0,0,128,0,0,0,32768,135,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,480,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,62480,65,53280,0,0,0,0,32776,4000,2,1665,0,0,0,0,0,0,0,16,0,0,0,512,59424,131,41024,1,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,8,0,0,0,4096,0,0,0,4,0,0,0,0,432,2,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,49152,26624,0,0,0,0,0,0,32,0,2,0,0,0,0,32768,0,0,0,0,0,0,0,768,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,4104,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,1,0,0,0,16384,1024,4221,2048,52,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,128,1536,4,8,0,0,0,0,0,0,8192,0,0,0,0,8208,32768,257,512,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,0,0,0,4,0,0,0,0,4097,16884,8192,208,0,0,0,0,0,0,8448,2048,0,0,0,64,32004,16,13320,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,128,64008,32,26640,0,0,0,0,0,0,0,2176,0,0,0,0,0,0,2048,0,0,0,0,0,0,1536,49152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,512,0,0,32832,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,128,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,256,2,0,0,0,0,0,0,0,0,0,0,0,2,0,16384,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,32768,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,1026,0,0,0,32768,0,0,4096,32,0,0,0,0,0,0,256,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,8,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15360,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,256,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10288,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,16,0,0,0,16,8001,4,3330,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,33,0,0,0,32768,0,0,0,0,0,0,2048,41088,527,33024,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,8192,33280,2110,1024,26,0,0,0,0,0,0,64,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,32768,2048,8442,4096,104,0,0,0,0,0,0,0,0,0,0,0,32,16002,8,6660,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,64008,32,26640,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,4,0,0,0,0,0,0,512,0,0,0,0,0,0,96,8200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,960,0,0,0,0,0,0,0,0,0,0,512,0,0,0,512,59424,131,41024,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,4097,16884,8192,208,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,4,0,0,0,0,0,0,8192,0,0,0,0,0,0,6,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32776,4000,2,1665,0,0,0,0,0,0,0,16,0,0,0,0,0,3072,0,0,0,0,0,0,0,0,0,0,0,0,32768,2048,8442,4096,104,0,0,0,0,0,0,128,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,16384,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,64,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,1024,4221,2048,52,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,64,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,48,4096,0,0,0,0,4097,16884,8192,208,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,8194,33768,16384,416,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,59424,131,41024,1,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,32768,0,0,0,0,4097,16884,8192,208,0,0,0,2048,41088,527,33024,6,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,8,0,0,0,0,0,512,0,0,0,0,128,64008,32,26640,0,0,0,0,480,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,512,0,0,0,16384,1024,4221,2048,52,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,4,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,2048,41088,527,33024,6,3,0,0,0,0,0,4096,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,512,0,0,0,0,0,0,0,32,0,0,0,0,0,0,24,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,16384,1024,4221,2048,52,24,0,0,512,59424,131,41024,49153,0,0,0,0,0,0,0,256,0,0,0,0,32768,2048,0,0,0,0,0,0,0,0,0,0,0,0,32,16002,8,6660,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,8194,33768,16384,416,0,0,0,0,0,0,512,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,4,0,0,0,32776,4000,2,1665,0,0,0,0,0,0,32768,0,0,0,0,0,0,4096,0,0,0,0,0,16,8001,4,3330,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,4096,16640,1055,512,13,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,32776,4000,2,1665,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,8,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_runParser","AST","toplevels","toplevel","conds","cond","bindings","binding","ty","qty","block","stmts","stmt","splitAt","guardExpr","partition","range","spec","qspec","aspec","pspec","pbinder","expr","qops","logicOrExp","logicAndExp","cmpExpr","cmp","modArithExpr","modarith","multArithExpr","multarith","arithExpr","arith","atomic","basis__expr__","many__cond__","many__stmt__","many__toplevel__","manyComma__binding__","manyComma__range__","opt__block__","opt__splitAt__","tuple__expr__","tuple__qspec__","manyComma__expr__","manyComma__qspec__","digits","dafny","\"method\"","\"ensures\"","\"requires\"","\"separates\"","\"invariant\"","\"with\"","\"at\"","\"split\"","\"for\"","\"returns\"","\"not\"","\"nat\"","\"int\"","\"in\"","\"bool\"","\"seq\"","\"nor\"","\"had\"","\"H\"","\"QFT\"","\"RQFT\"","\"repr\"","\"meas\"","\"en\"","\"qreg\"","\"en01\"","\"var\"","\"if\"","\"\955\"","\"\931\"","\"\8855\"","\"\969\"","\"\937\"","\"\8712\"","\"\8614\"","\"assert\"","\"||\"","\"&&\"","'+'","'-'","'*'","'\\%'","'|'","'('","')'","'<'","'>'","\"\10217\"","'['","']'","'{'","'}'","id","'_'","','","':'","'.'","';'","\"==\"","\"=>\"","\">=\"","\"<=\"","\":=\"","\"*=\"","\"..\"","'~'","\"sqrt\"","\"sin\"","\"cos\"","'^'","'/'","%eof"]
        bit_start = st Prelude.* 123
        bit_end = (st Prelude.+ 1) Prelude.* 123
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..122]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (51) = happyShift action_5
action_0 (52) = happyShift action_6
action_0 (4) = happyGoto action_7
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (41) = happyGoto action_4
action_0 _ = happyReduce_99

action_1 (51) = happyShift action_5
action_1 (52) = happyShift action_6
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (41) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (51) = happyShift action_5
action_3 (52) = happyShift action_6
action_3 (6) = happyGoto action_3
action_3 (41) = happyGoto action_9
action_3 _ = happyReduce_99

action_4 _ = happyReduce_2

action_5 _ = happyReduce_3

action_6 (104) = happyShift action_8
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (123) = happyAccept
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (95) = happyShift action_10
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_100

action_10 (104) = happyShift action_14
action_10 (9) = happyGoto action_11
action_10 (10) = happyGoto action_12
action_10 (42) = happyGoto action_13
action_10 _ = happyReduce_101

action_11 (96) = happyShift action_17
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (106) = happyShift action_16
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_11

action_14 (107) = happyShift action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (63) = happyShift action_28
action_15 (64) = happyShift action_29
action_15 (66) = happyShift action_30
action_15 (67) = happyShift action_31
action_15 (76) = happyShift action_32
action_15 (11) = happyGoto action_27
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (104) = happyShift action_14
action_16 (10) = happyGoto action_12
action_16 (42) = happyGoto action_26
action_16 _ = happyReduce_101

action_17 (53) = happyShift action_21
action_17 (54) = happyShift action_22
action_17 (55) = happyShift action_23
action_17 (56) = happyShift action_24
action_17 (61) = happyShift action_25
action_17 (7) = happyGoto action_18
action_17 (8) = happyGoto action_19
action_17 (39) = happyGoto action_20
action_17 _ = happyReduce_95

action_18 (102) = happyShift action_68
action_18 (13) = happyGoto action_66
action_18 (44) = happyGoto action_67
action_18 _ = happyReduce_105

action_19 (53) = happyShift action_21
action_19 (54) = happyShift action_22
action_19 (55) = happyShift action_23
action_19 (56) = happyShift action_24
action_19 (8) = happyGoto action_19
action_19 (39) = happyGoto action_65
action_19 _ = happyReduce_95

action_20 _ = happyReduce_6

action_21 (50) = happyShift action_46
action_21 (62) = happyShift action_47
action_21 (68) = happyShift action_48
action_21 (70) = happyShift action_49
action_21 (71) = happyShift action_50
action_21 (72) = happyShift action_51
action_21 (73) = happyShift action_52
action_21 (74) = happyShift action_53
action_21 (80) = happyShift action_54
action_21 (95) = happyShift action_55
action_21 (102) = happyShift action_56
action_21 (104) = happyShift action_57
action_21 (105) = happyShift action_58
action_21 (20) = happyGoto action_36
action_21 (25) = happyGoto action_64
action_21 (26) = happyGoto action_38
action_21 (27) = happyGoto action_39
action_21 (28) = happyGoto action_40
action_21 (29) = happyGoto action_41
action_21 (31) = happyGoto action_42
action_21 (33) = happyGoto action_43
action_21 (35) = happyGoto action_44
action_21 (37) = happyGoto action_45
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (50) = happyShift action_46
action_22 (62) = happyShift action_47
action_22 (68) = happyShift action_48
action_22 (70) = happyShift action_49
action_22 (71) = happyShift action_50
action_22 (72) = happyShift action_51
action_22 (73) = happyShift action_52
action_22 (74) = happyShift action_53
action_22 (80) = happyShift action_54
action_22 (95) = happyShift action_55
action_22 (102) = happyShift action_56
action_22 (104) = happyShift action_57
action_22 (105) = happyShift action_58
action_22 (20) = happyGoto action_36
action_22 (25) = happyGoto action_63
action_22 (26) = happyGoto action_38
action_22 (27) = happyGoto action_39
action_22 (28) = happyGoto action_40
action_22 (29) = happyGoto action_41
action_22 (31) = happyGoto action_42
action_22 (33) = happyGoto action_43
action_22 (35) = happyGoto action_44
action_22 (37) = happyGoto action_45
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (104) = happyShift action_62
action_23 (18) = happyGoto action_59
action_23 (19) = happyGoto action_60
action_23 (43) = happyGoto action_61
action_23 _ = happyReduce_103

action_24 (50) = happyShift action_46
action_24 (62) = happyShift action_47
action_24 (68) = happyShift action_48
action_24 (70) = happyShift action_49
action_24 (71) = happyShift action_50
action_24 (72) = happyShift action_51
action_24 (73) = happyShift action_52
action_24 (74) = happyShift action_53
action_24 (80) = happyShift action_54
action_24 (95) = happyShift action_55
action_24 (102) = happyShift action_56
action_24 (104) = happyShift action_57
action_24 (105) = happyShift action_58
action_24 (20) = happyGoto action_36
action_24 (25) = happyGoto action_37
action_24 (26) = happyGoto action_38
action_24 (27) = happyGoto action_39
action_24 (28) = happyGoto action_40
action_24 (29) = happyGoto action_41
action_24 (31) = happyGoto action_42
action_24 (33) = happyGoto action_43
action_24 (35) = happyGoto action_44
action_24 (37) = happyGoto action_45
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (95) = happyShift action_35
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_102

action_27 _ = happyReduce_12

action_28 _ = happyReduce_13

action_29 _ = happyReduce_14

action_30 _ = happyReduce_15

action_31 (97) = happyShift action_34
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (100) = happyShift action_33
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (50) = happyShift action_109
action_33 (104) = happyShift action_110
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (63) = happyShift action_28
action_34 (64) = happyShift action_29
action_34 (66) = happyShift action_30
action_34 (67) = happyShift action_31
action_34 (76) = happyShift action_32
action_34 (11) = happyGoto action_108
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (104) = happyShift action_14
action_35 (9) = happyGoto action_107
action_35 (10) = happyGoto action_12
action_35 (42) = happyGoto action_13
action_35 _ = happyReduce_101

action_36 _ = happyReduce_56

action_37 _ = happyReduce_9

action_38 _ = happyReduce_58

action_39 _ = happyReduce_66

action_40 (88) = happyShift action_106
action_40 _ = happyReduce_71

action_41 (89) = happyShift action_105
action_41 _ = happyReduce_73

action_42 (97) = happyShift action_100
action_42 (98) = happyShift action_101
action_42 (110) = happyShift action_102
action_42 (112) = happyShift action_103
action_42 (113) = happyShift action_104
action_42 (30) = happyGoto action_99
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (93) = happyShift action_97
action_43 (121) = happyShift action_98
action_43 (32) = happyGoto action_96
action_43 _ = happyReduce_81

action_44 (92) = happyShift action_95
action_44 (34) = happyGoto action_94
action_44 _ = happyReduce_85

action_45 (90) = happyShift action_92
action_45 (91) = happyShift action_93
action_45 (92) = happyReduce_88
action_45 (93) = happyReduce_88
action_45 (97) = happyReduce_88
action_45 (98) = happyReduce_88
action_45 (110) = happyReduce_88
action_45 (112) = happyReduce_88
action_45 (113) = happyReduce_88
action_45 (121) = happyReduce_88
action_45 (36) = happyGoto action_91
action_45 _ = happyReduce_54

action_46 _ = happyReduce_91

action_47 (50) = happyShift action_46
action_47 (95) = happyShift action_55
action_47 (104) = happyShift action_90
action_47 (37) = happyGoto action_89
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (95) = happyShift action_88
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_67

action_50 _ = happyReduce_68

action_51 _ = happyReduce_69

action_52 (95) = happyShift action_87
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (104) = happyShift action_86
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (95) = happyShift action_85
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (50) = happyShift action_46
action_55 (62) = happyShift action_47
action_55 (68) = happyShift action_48
action_55 (70) = happyShift action_49
action_55 (71) = happyShift action_50
action_55 (72) = happyShift action_51
action_55 (73) = happyShift action_52
action_55 (74) = happyShift action_53
action_55 (80) = happyShift action_54
action_55 (95) = happyShift action_55
action_55 (102) = happyShift action_56
action_55 (104) = happyShift action_57
action_55 (105) = happyShift action_58
action_55 (20) = happyGoto action_36
action_55 (25) = happyGoto action_84
action_55 (26) = happyGoto action_38
action_55 (27) = happyGoto action_39
action_55 (28) = happyGoto action_40
action_55 (29) = happyGoto action_41
action_55 (31) = happyGoto action_42
action_55 (33) = happyGoto action_43
action_55 (35) = happyGoto action_44
action_55 (37) = happyGoto action_45
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (104) = happyShift action_62
action_56 (18) = happyGoto action_83
action_56 (19) = happyGoto action_60
action_56 (43) = happyGoto action_61
action_56 _ = happyReduce_103

action_57 (95) = happyShift action_82
action_57 (46) = happyGoto action_81
action_57 _ = happyReduce_92

action_58 _ = happyReduce_55

action_59 _ = happyReduce_10

action_60 (106) = happyShift action_80
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_36

action_62 (100) = happyShift action_79
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_7

action_64 _ = happyReduce_8

action_65 _ = happyReduce_96

action_66 _ = happyReduce_106

action_67 _ = happyReduce_5

action_68 (51) = happyShift action_73
action_68 (60) = happyShift action_74
action_68 (78) = happyShift action_75
action_68 (79) = happyShift action_76
action_68 (87) = happyShift action_77
action_68 (104) = happyShift action_78
action_68 (115) = happyReduce_103
action_68 (14) = happyGoto action_69
action_68 (15) = happyGoto action_70
action_68 (18) = happyGoto action_71
action_68 (19) = happyGoto action_60
action_68 (40) = happyGoto action_72
action_68 (43) = happyGoto action_61
action_68 _ = happyReduce_97

action_69 (103) = happyShift action_144
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (51) = happyShift action_73
action_70 (60) = happyShift action_74
action_70 (78) = happyShift action_75
action_70 (79) = happyShift action_76
action_70 (87) = happyShift action_77
action_70 (104) = happyShift action_78
action_70 (115) = happyReduce_103
action_70 (15) = happyGoto action_70
action_70 (18) = happyGoto action_71
action_70 (19) = happyGoto action_60
action_70 (40) = happyGoto action_143
action_70 (43) = happyGoto action_61
action_70 _ = happyReduce_97

action_71 (115) = happyShift action_142
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_24

action_73 _ = happyReduce_25

action_74 (104) = happyShift action_141
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (104) = happyShift action_14
action_75 (10) = happyGoto action_140
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (95) = happyShift action_139
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (50) = happyShift action_46
action_77 (62) = happyShift action_47
action_77 (68) = happyShift action_48
action_77 (70) = happyShift action_49
action_77 (71) = happyShift action_50
action_77 (72) = happyShift action_51
action_77 (73) = happyShift action_52
action_77 (74) = happyShift action_53
action_77 (80) = happyShift action_54
action_77 (95) = happyShift action_55
action_77 (102) = happyShift action_56
action_77 (104) = happyShift action_57
action_77 (105) = happyShift action_58
action_77 (20) = happyGoto action_36
action_77 (25) = happyGoto action_138
action_77 (26) = happyGoto action_38
action_77 (27) = happyGoto action_39
action_77 (28) = happyGoto action_40
action_77 (29) = happyGoto action_41
action_77 (31) = happyGoto action_42
action_77 (33) = happyGoto action_43
action_77 (35) = happyGoto action_44
action_77 (37) = happyGoto action_45
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (95) = happyShift action_82
action_78 (100) = happyShift action_79
action_78 (114) = happyShift action_137
action_78 (46) = happyGoto action_136
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (50) = happyShift action_46
action_79 (62) = happyShift action_47
action_79 (68) = happyShift action_48
action_79 (70) = happyShift action_49
action_79 (71) = happyShift action_50
action_79 (72) = happyShift action_51
action_79 (73) = happyShift action_52
action_79 (74) = happyShift action_53
action_79 (80) = happyShift action_54
action_79 (95) = happyShift action_55
action_79 (102) = happyShift action_56
action_79 (104) = happyShift action_57
action_79 (105) = happyShift action_58
action_79 (20) = happyGoto action_36
action_79 (25) = happyGoto action_135
action_79 (26) = happyGoto action_38
action_79 (27) = happyGoto action_39
action_79 (28) = happyGoto action_40
action_79 (29) = happyGoto action_41
action_79 (31) = happyGoto action_42
action_79 (33) = happyGoto action_43
action_79 (35) = happyGoto action_44
action_79 (37) = happyGoto action_45
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (104) = happyShift action_62
action_80 (19) = happyGoto action_60
action_80 (43) = happyGoto action_134
action_80 _ = happyReduce_103

action_81 _ = happyReduce_64

action_82 (50) = happyShift action_46
action_82 (62) = happyShift action_47
action_82 (68) = happyShift action_48
action_82 (70) = happyShift action_49
action_82 (71) = happyShift action_50
action_82 (72) = happyShift action_51
action_82 (73) = happyShift action_52
action_82 (74) = happyShift action_53
action_82 (80) = happyShift action_54
action_82 (95) = happyShift action_55
action_82 (102) = happyShift action_56
action_82 (104) = happyShift action_57
action_82 (105) = happyShift action_58
action_82 (20) = happyGoto action_36
action_82 (25) = happyGoto action_132
action_82 (26) = happyGoto action_38
action_82 (27) = happyGoto action_39
action_82 (28) = happyGoto action_40
action_82 (29) = happyGoto action_41
action_82 (31) = happyGoto action_42
action_82 (33) = happyGoto action_43
action_82 (35) = happyGoto action_44
action_82 (37) = happyGoto action_45
action_82 (48) = happyGoto action_133
action_82 _ = happyReduce_111

action_83 (103) = happyShift action_130
action_83 (107) = happyShift action_131
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (96) = happyShift action_129
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (83) = happyShift action_125
action_85 (84) = happyShift action_126
action_85 (104) = happyShift action_127
action_85 (105) = happyShift action_128
action_85 (24) = happyGoto action_124
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_59

action_87 (104) = happyShift action_62
action_87 (19) = happyGoto action_123
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (50) = happyShift action_46
action_88 (95) = happyShift action_55
action_88 (104) = happyShift action_90
action_88 (37) = happyGoto action_122
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_60

action_90 _ = happyReduce_92

action_91 (50) = happyShift action_46
action_91 (95) = happyShift action_55
action_91 (104) = happyShift action_90
action_91 (35) = happyGoto action_121
action_91 (37) = happyGoto action_116
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_89

action_93 _ = happyReduce_90

action_94 (50) = happyShift action_46
action_94 (95) = happyShift action_55
action_94 (104) = happyShift action_90
action_94 (33) = happyGoto action_120
action_94 (35) = happyGoto action_44
action_94 (37) = happyGoto action_116
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_86

action_96 (50) = happyShift action_46
action_96 (95) = happyShift action_55
action_96 (104) = happyShift action_90
action_96 (31) = happyGoto action_119
action_96 (33) = happyGoto action_43
action_96 (35) = happyGoto action_44
action_96 (37) = happyGoto action_116
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_82

action_98 _ = happyReduce_83

action_99 (50) = happyShift action_46
action_99 (95) = happyShift action_55
action_99 (104) = happyShift action_90
action_99 (31) = happyGoto action_118
action_99 (33) = happyGoto action_43
action_99 (35) = happyGoto action_44
action_99 (37) = happyGoto action_116
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_76

action_101 _ = happyReduce_75

action_102 _ = happyReduce_79

action_103 _ = happyReduce_77

action_104 _ = happyReduce_78

action_105 (50) = happyShift action_46
action_105 (95) = happyShift action_55
action_105 (104) = happyShift action_90
action_105 (28) = happyGoto action_117
action_105 (29) = happyGoto action_41
action_105 (31) = happyGoto action_42
action_105 (33) = happyGoto action_43
action_105 (35) = happyGoto action_44
action_105 (37) = happyGoto action_116
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (50) = happyShift action_46
action_106 (95) = happyShift action_55
action_106 (104) = happyShift action_90
action_106 (27) = happyGoto action_115
action_106 (28) = happyGoto action_40
action_106 (29) = happyGoto action_41
action_106 (31) = happyGoto action_42
action_106 (33) = happyGoto action_43
action_106 (35) = happyGoto action_44
action_106 (37) = happyGoto action_116
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (96) = happyShift action_114
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (98) = happyShift action_113
action_108 _ = happyFail (happyExpListPerState 108)

action_109 (101) = happyShift action_112
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (101) = happyShift action_111
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_18

action_112 _ = happyReduce_17

action_113 _ = happyReduce_16

action_114 (53) = happyShift action_21
action_114 (54) = happyShift action_22
action_114 (55) = happyShift action_23
action_114 (56) = happyShift action_24
action_114 (7) = happyGoto action_168
action_114 (8) = happyGoto action_19
action_114 (39) = happyGoto action_20
action_114 _ = happyReduce_95

action_115 _ = happyReduce_70

action_116 (90) = happyShift action_92
action_116 (91) = happyShift action_93
action_116 (36) = happyGoto action_91
action_116 _ = happyReduce_88

action_117 _ = happyReduce_72

action_118 _ = happyReduce_74

action_119 _ = happyReduce_80

action_120 _ = happyReduce_84

action_121 _ = happyReduce_87

action_122 (106) = happyShift action_167
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (96) = happyShift action_166
action_123 _ = happyFail (happyExpListPerState 123)

action_124 (117) = happyShift action_165
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (95) = happyShift action_164
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (104) = happyShift action_163
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (111) = happyShift action_162
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_51

action_129 _ = happyReduce_93

action_130 _ = happyReduce_57

action_131 (68) = happyShift action_158
action_131 (69) = happyShift action_159
action_131 (75) = happyShift action_160
action_131 (77) = happyShift action_161
action_131 (12) = happyGoto action_157
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (106) = happyShift action_156
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (96) = happyShift action_155
action_133 _ = happyFail (happyExpListPerState 133)

action_134 _ = happyReduce_104

action_135 (116) = happyShift action_154
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (109) = happyShift action_153
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (50) = happyShift action_46
action_137 (62) = happyShift action_47
action_137 (68) = happyShift action_48
action_137 (70) = happyShift action_49
action_137 (71) = happyShift action_50
action_137 (72) = happyShift action_51
action_137 (73) = happyShift action_52
action_137 (74) = happyShift action_53
action_137 (80) = happyShift action_54
action_137 (95) = happyShift action_55
action_137 (102) = happyShift action_56
action_137 (104) = happyShift action_57
action_137 (105) = happyShift action_58
action_137 (20) = happyGoto action_36
action_137 (25) = happyGoto action_152
action_137 (26) = happyGoto action_38
action_137 (27) = happyGoto action_39
action_137 (28) = happyGoto action_40
action_137 (29) = happyGoto action_41
action_137 (31) = happyGoto action_42
action_137 (33) = happyGoto action_43
action_137 (35) = happyGoto action_44
action_137 (37) = happyGoto action_45
action_137 _ = happyFail (happyExpListPerState 137)

action_138 (109) = happyShift action_151
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (104) = happyShift action_62
action_139 (17) = happyGoto action_149
action_139 (18) = happyGoto action_150
action_139 (19) = happyGoto action_60
action_139 (43) = happyGoto action_61
action_139 _ = happyReduce_103

action_140 (109) = happyShift action_147
action_140 (114) = happyShift action_148
action_140 _ = happyFail (happyExpListPerState 140)

action_141 (65) = happyShift action_146
action_141 _ = happyFail (happyExpListPerState 141)

action_142 (50) = happyShift action_46
action_142 (62) = happyShift action_47
action_142 (68) = happyShift action_48
action_142 (70) = happyShift action_49
action_142 (71) = happyShift action_50
action_142 (72) = happyShift action_51
action_142 (73) = happyShift action_52
action_142 (74) = happyShift action_53
action_142 (80) = happyShift action_54
action_142 (95) = happyShift action_55
action_142 (102) = happyShift action_56
action_142 (104) = happyShift action_57
action_142 (105) = happyShift action_58
action_142 (20) = happyGoto action_36
action_142 (25) = happyGoto action_145
action_142 (26) = happyGoto action_38
action_142 (27) = happyGoto action_39
action_142 (28) = happyGoto action_40
action_142 (29) = happyGoto action_41
action_142 (31) = happyGoto action_42
action_142 (33) = happyGoto action_43
action_142 (35) = happyGoto action_44
action_142 (37) = happyGoto action_45
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_98

action_144 _ = happyReduce_23

action_145 (109) = happyShift action_185
action_145 _ = happyFail (happyExpListPerState 145)

action_146 (100) = happyShift action_184
action_146 _ = happyFail (happyExpListPerState 146)

action_147 _ = happyReduce_27

action_148 (50) = happyShift action_46
action_148 (62) = happyShift action_47
action_148 (68) = happyShift action_48
action_148 (70) = happyShift action_49
action_148 (71) = happyShift action_50
action_148 (72) = happyShift action_51
action_148 (73) = happyShift action_52
action_148 (74) = happyShift action_53
action_148 (80) = happyShift action_54
action_148 (95) = happyShift action_55
action_148 (102) = happyShift action_56
action_148 (104) = happyShift action_57
action_148 (105) = happyShift action_58
action_148 (20) = happyGoto action_36
action_148 (25) = happyGoto action_183
action_148 (26) = happyGoto action_38
action_148 (27) = happyGoto action_39
action_148 (28) = happyGoto action_40
action_148 (29) = happyGoto action_41
action_148 (31) = happyGoto action_42
action_148 (33) = happyGoto action_43
action_148 (35) = happyGoto action_44
action_148 (37) = happyGoto action_45
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (96) = happyShift action_182
action_149 _ = happyFail (happyExpListPerState 149)

action_150 (59) = happyShift action_181
action_150 (16) = happyGoto action_179
action_150 (45) = happyGoto action_180
action_150 _ = happyReduce_107

action_151 _ = happyReduce_26

action_152 (109) = happyShift action_178
action_152 _ = happyFail (happyExpListPerState 152)

action_153 _ = happyReduce_33

action_154 (50) = happyShift action_46
action_154 (62) = happyShift action_47
action_154 (68) = happyShift action_48
action_154 (70) = happyShift action_49
action_154 (71) = happyShift action_50
action_154 (72) = happyShift action_51
action_154 (73) = happyShift action_52
action_154 (74) = happyShift action_53
action_154 (80) = happyShift action_54
action_154 (95) = happyShift action_55
action_154 (102) = happyShift action_56
action_154 (104) = happyShift action_57
action_154 (105) = happyShift action_58
action_154 (20) = happyGoto action_36
action_154 (25) = happyGoto action_177
action_154 (26) = happyGoto action_38
action_154 (27) = happyGoto action_39
action_154 (28) = happyGoto action_40
action_154 (29) = happyGoto action_41
action_154 (31) = happyGoto action_42
action_154 (33) = happyGoto action_43
action_154 (35) = happyGoto action_44
action_154 (37) = happyGoto action_45
action_154 _ = happyFail (happyExpListPerState 154)

action_155 _ = happyReduce_109

action_156 (50) = happyShift action_46
action_156 (62) = happyShift action_47
action_156 (68) = happyShift action_48
action_156 (70) = happyShift action_49
action_156 (71) = happyShift action_50
action_156 (72) = happyShift action_51
action_156 (73) = happyShift action_52
action_156 (74) = happyShift action_53
action_156 (80) = happyShift action_54
action_156 (95) = happyShift action_55
action_156 (102) = happyShift action_56
action_156 (104) = happyShift action_57
action_156 (105) = happyShift action_58
action_156 (20) = happyGoto action_36
action_156 (25) = happyGoto action_132
action_156 (26) = happyGoto action_38
action_156 (27) = happyGoto action_39
action_156 (28) = happyGoto action_40
action_156 (29) = happyGoto action_41
action_156 (31) = happyGoto action_42
action_156 (33) = happyGoto action_43
action_156 (35) = happyGoto action_44
action_156 (37) = happyGoto action_45
action_156 (48) = happyGoto action_176
action_156 _ = happyReduce_111

action_157 (86) = happyShift action_175
action_157 _ = happyFail (happyExpListPerState 157)

action_158 _ = happyReduce_19

action_159 _ = happyReduce_20

action_160 _ = happyReduce_21

action_161 _ = happyReduce_22

action_162 (50) = happyShift action_46
action_162 (62) = happyShift action_47
action_162 (68) = happyShift action_48
action_162 (70) = happyShift action_49
action_162 (71) = happyShift action_50
action_162 (72) = happyShift action_51
action_162 (73) = happyShift action_52
action_162 (74) = happyShift action_53
action_162 (80) = happyShift action_54
action_162 (95) = happyShift action_55
action_162 (102) = happyShift action_56
action_162 (104) = happyShift action_57
action_162 (105) = happyShift action_58
action_162 (20) = happyGoto action_36
action_162 (25) = happyGoto action_174
action_162 (26) = happyGoto action_38
action_162 (27) = happyGoto action_39
action_162 (28) = happyGoto action_40
action_162 (29) = happyGoto action_41
action_162 (31) = happyGoto action_42
action_162 (33) = happyGoto action_43
action_162 (35) = happyGoto action_44
action_162 (37) = happyGoto action_45
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (85) = happyShift action_173
action_163 _ = happyFail (happyExpListPerState 163)

action_164 (104) = happyShift action_172
action_164 _ = happyFail (happyExpListPerState 164)

action_165 (104) = happyShift action_171
action_165 _ = happyFail (happyExpListPerState 165)

action_166 _ = happyReduce_65

action_167 (50) = happyShift action_170
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (102) = happyShift action_68
action_168 (13) = happyGoto action_66
action_168 (44) = happyGoto action_169
action_168 _ = happyReduce_105

action_169 _ = happyReduce_4

action_170 (96) = happyShift action_201
action_170 _ = happyFail (happyExpListPerState 170)

action_171 (111) = happyShift action_200
action_171 _ = happyFail (happyExpListPerState 171)

action_172 (106) = happyShift action_199
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (100) = happyShift action_198
action_173 _ = happyFail (happyExpListPerState 173)

action_174 (96) = happyShift action_197
action_174 _ = happyFail (happyExpListPerState 174)

action_175 (81) = happyShift action_193
action_175 (82) = happyShift action_194
action_175 (95) = happyShift action_195
action_175 (105) = happyShift action_196
action_175 (21) = happyGoto action_191
action_175 (47) = happyGoto action_192
action_175 _ = happyFail (happyExpListPerState 175)

action_176 _ = happyReduce_112

action_177 (101) = happyShift action_190
action_177 _ = happyFail (happyExpListPerState 177)

action_178 _ = happyReduce_29

action_179 _ = happyReduce_108

action_180 _ = happyReduce_35

action_181 (58) = happyShift action_189
action_181 _ = happyFail (happyExpListPerState 181)

action_182 (53) = happyShift action_21
action_182 (54) = happyShift action_22
action_182 (55) = happyShift action_23
action_182 (56) = happyShift action_24
action_182 (8) = happyGoto action_188
action_182 _ = happyFail (happyExpListPerState 182)

action_183 (109) = happyShift action_187
action_183 _ = happyFail (happyExpListPerState 183)

action_184 (50) = happyShift action_46
action_184 (62) = happyShift action_47
action_184 (68) = happyShift action_48
action_184 (70) = happyShift action_49
action_184 (71) = happyShift action_50
action_184 (72) = happyShift action_51
action_184 (73) = happyShift action_52
action_184 (74) = happyShift action_53
action_184 (80) = happyShift action_54
action_184 (95) = happyShift action_55
action_184 (102) = happyShift action_56
action_184 (104) = happyShift action_57
action_184 (105) = happyShift action_58
action_184 (20) = happyGoto action_36
action_184 (25) = happyGoto action_186
action_184 (26) = happyGoto action_38
action_184 (27) = happyGoto action_39
action_184 (28) = happyGoto action_40
action_184 (29) = happyGoto action_41
action_184 (31) = happyGoto action_42
action_184 (33) = happyGoto action_43
action_184 (35) = happyGoto action_44
action_184 (37) = happyGoto action_45
action_184 _ = happyFail (happyExpListPerState 184)

action_185 _ = happyReduce_30

action_186 (116) = happyShift action_215
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_28

action_188 (102) = happyShift action_68
action_188 (13) = happyGoto action_214
action_188 _ = happyFail (happyExpListPerState 188)

action_189 (50) = happyShift action_46
action_189 (62) = happyShift action_47
action_189 (68) = happyShift action_48
action_189 (70) = happyShift action_49
action_189 (71) = happyShift action_50
action_189 (72) = happyShift action_51
action_189 (73) = happyShift action_52
action_189 (74) = happyShift action_53
action_189 (80) = happyShift action_54
action_189 (95) = happyShift action_55
action_189 (102) = happyShift action_56
action_189 (104) = happyShift action_57
action_189 (105) = happyShift action_58
action_189 (20) = happyGoto action_36
action_189 (25) = happyGoto action_213
action_189 (26) = happyGoto action_38
action_189 (27) = happyGoto action_39
action_189 (28) = happyGoto action_40
action_189 (29) = happyGoto action_41
action_189 (31) = happyGoto action_42
action_189 (33) = happyGoto action_43
action_189 (35) = happyGoto action_44
action_189 (37) = happyGoto action_45
action_189 _ = happyFail (happyExpListPerState 189)

action_190 _ = happyReduce_37

action_191 (103) = happyShift action_212
action_191 _ = happyFail (happyExpListPerState 191)

action_192 (103) = happyShift action_211
action_192 _ = happyFail (happyExpListPerState 192)

action_193 (104) = happyShift action_210
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (104) = happyShift action_209
action_194 _ = happyFail (happyExpListPerState 194)

action_195 (81) = happyShift action_193
action_195 (82) = happyShift action_194
action_195 (105) = happyShift action_196
action_195 (21) = happyGoto action_207
action_195 (49) = happyGoto action_208
action_195 _ = happyReduce_113

action_196 _ = happyReduce_43

action_197 _ = happyReduce_62

action_198 (50) = happyShift action_46
action_198 (62) = happyShift action_47
action_198 (68) = happyShift action_48
action_198 (70) = happyShift action_49
action_198 (71) = happyShift action_50
action_198 (72) = happyShift action_51
action_198 (73) = happyShift action_52
action_198 (74) = happyShift action_53
action_198 (80) = happyShift action_54
action_198 (95) = happyShift action_55
action_198 (102) = happyShift action_56
action_198 (104) = happyShift action_57
action_198 (105) = happyShift action_58
action_198 (20) = happyGoto action_36
action_198 (25) = happyGoto action_206
action_198 (26) = happyGoto action_38
action_198 (27) = happyGoto action_39
action_198 (28) = happyGoto action_40
action_198 (29) = happyGoto action_41
action_198 (31) = happyGoto action_42
action_198 (33) = happyGoto action_43
action_198 (35) = happyGoto action_44
action_198 (37) = happyGoto action_45
action_198 _ = happyFail (happyExpListPerState 198)

action_199 (104) = happyShift action_205
action_199 _ = happyFail (happyExpListPerState 199)

action_200 (83) = happyShift action_203
action_200 (84) = happyShift action_204
action_200 (23) = happyGoto action_202
action_200 _ = happyReduce_48

action_201 _ = happyReduce_61

action_202 (50) = happyShift action_46
action_202 (62) = happyShift action_47
action_202 (68) = happyShift action_48
action_202 (70) = happyShift action_49
action_202 (71) = happyShift action_50
action_202 (72) = happyShift action_51
action_202 (73) = happyShift action_52
action_202 (74) = happyShift action_53
action_202 (80) = happyShift action_54
action_202 (95) = happyShift action_55
action_202 (102) = happyShift action_56
action_202 (104) = happyShift action_57
action_202 (105) = happyShift action_58
action_202 (20) = happyGoto action_36
action_202 (25) = happyGoto action_225
action_202 (26) = happyGoto action_38
action_202 (27) = happyGoto action_39
action_202 (28) = happyGoto action_40
action_202 (29) = happyGoto action_41
action_202 (31) = happyGoto action_42
action_202 (33) = happyGoto action_43
action_202 (35) = happyGoto action_44
action_202 (37) = happyGoto action_45
action_202 _ = happyFail (happyExpListPerState 202)

action_203 (95) = happyShift action_224
action_203 _ = happyFail (happyExpListPerState 203)

action_204 (104) = happyShift action_223
action_204 _ = happyFail (happyExpListPerState 204)

action_205 (96) = happyShift action_222
action_205 _ = happyFail (happyExpListPerState 205)

action_206 (116) = happyShift action_221
action_206 _ = happyFail (happyExpListPerState 206)

action_207 (106) = happyShift action_220
action_207 _ = happyFail (happyExpListPerState 207)

action_208 (96) = happyShift action_219
action_208 _ = happyFail (happyExpListPerState 208)

action_209 (108) = happyShift action_218
action_209 _ = happyFail (happyExpListPerState 209)

action_210 (85) = happyShift action_217
action_210 _ = happyFail (happyExpListPerState 210)

action_211 _ = happyReduce_38

action_212 _ = happyReduce_39

action_213 _ = happyReduce_34

action_214 _ = happyReduce_31

action_215 (50) = happyShift action_46
action_215 (62) = happyShift action_47
action_215 (68) = happyShift action_48
action_215 (70) = happyShift action_49
action_215 (71) = happyShift action_50
action_215 (72) = happyShift action_51
action_215 (73) = happyShift action_52
action_215 (74) = happyShift action_53
action_215 (80) = happyShift action_54
action_215 (95) = happyShift action_55
action_215 (102) = happyShift action_56
action_215 (104) = happyShift action_57
action_215 (105) = happyShift action_58
action_215 (20) = happyGoto action_36
action_215 (25) = happyGoto action_216
action_215 (26) = happyGoto action_38
action_215 (27) = happyGoto action_39
action_215 (28) = happyGoto action_40
action_215 (29) = happyGoto action_41
action_215 (31) = happyGoto action_42
action_215 (33) = happyGoto action_43
action_215 (35) = happyGoto action_44
action_215 (37) = happyGoto action_45
action_215 _ = happyFail (happyExpListPerState 215)

action_216 (101) = happyShift action_233
action_216 _ = happyFail (happyExpListPerState 216)

action_217 (100) = happyShift action_232
action_217 _ = happyFail (happyExpListPerState 217)

action_218 (95) = happyShift action_82
action_218 (46) = happyGoto action_231
action_218 _ = happyFail (happyExpListPerState 218)

action_219 _ = happyReduce_110

action_220 (81) = happyShift action_193
action_220 (82) = happyShift action_194
action_220 (105) = happyShift action_196
action_220 (21) = happyGoto action_207
action_220 (49) = happyGoto action_230
action_220 _ = happyReduce_113

action_221 (50) = happyShift action_46
action_221 (62) = happyShift action_47
action_221 (68) = happyShift action_48
action_221 (70) = happyShift action_49
action_221 (71) = happyShift action_50
action_221 (72) = happyShift action_51
action_221 (73) = happyShift action_52
action_221 (74) = happyShift action_53
action_221 (80) = happyShift action_54
action_221 (95) = happyShift action_55
action_221 (102) = happyShift action_56
action_221 (104) = happyShift action_57
action_221 (105) = happyShift action_58
action_221 (20) = happyGoto action_36
action_221 (25) = happyGoto action_229
action_221 (26) = happyGoto action_38
action_221 (27) = happyGoto action_39
action_221 (28) = happyGoto action_40
action_221 (29) = happyGoto action_41
action_221 (31) = happyGoto action_42
action_221 (33) = happyGoto action_43
action_221 (35) = happyGoto action_44
action_221 (37) = happyGoto action_45
action_221 _ = happyFail (happyExpListPerState 221)

action_222 _ = happyReduce_52

action_223 (85) = happyShift action_228
action_223 _ = happyFail (happyExpListPerState 223)

action_224 (50) = happyShift action_46
action_224 (62) = happyShift action_47
action_224 (68) = happyShift action_48
action_224 (70) = happyShift action_49
action_224 (71) = happyShift action_50
action_224 (72) = happyShift action_51
action_224 (73) = happyShift action_52
action_224 (74) = happyShift action_53
action_224 (80) = happyShift action_54
action_224 (95) = happyShift action_55
action_224 (102) = happyShift action_56
action_224 (104) = happyShift action_57
action_224 (105) = happyShift action_58
action_224 (20) = happyGoto action_36
action_224 (25) = happyGoto action_227
action_224 (26) = happyGoto action_38
action_224 (27) = happyGoto action_39
action_224 (28) = happyGoto action_40
action_224 (29) = happyGoto action_41
action_224 (31) = happyGoto action_42
action_224 (33) = happyGoto action_43
action_224 (35) = happyGoto action_44
action_224 (37) = happyGoto action_45
action_224 _ = happyFail (happyExpListPerState 224)

action_225 (96) = happyShift action_226
action_225 _ = happyFail (happyExpListPerState 225)

action_226 _ = happyReduce_63

action_227 (106) = happyShift action_238
action_227 _ = happyFail (happyExpListPerState 227)

action_228 (100) = happyShift action_237
action_228 _ = happyFail (happyExpListPerState 228)

action_229 (101) = happyShift action_236
action_229 _ = happyFail (happyExpListPerState 229)

action_230 _ = happyReduce_114

action_231 _ = happyReduce_40

action_232 (50) = happyShift action_46
action_232 (62) = happyShift action_47
action_232 (68) = happyShift action_48
action_232 (70) = happyShift action_49
action_232 (71) = happyShift action_50
action_232 (72) = happyShift action_51
action_232 (73) = happyShift action_52
action_232 (74) = happyShift action_53
action_232 (80) = happyShift action_54
action_232 (95) = happyShift action_55
action_232 (102) = happyShift action_56
action_232 (104) = happyShift action_57
action_232 (105) = happyShift action_58
action_232 (20) = happyGoto action_36
action_232 (25) = happyGoto action_235
action_232 (26) = happyGoto action_38
action_232 (27) = happyGoto action_39
action_232 (28) = happyGoto action_40
action_232 (29) = happyGoto action_41
action_232 (31) = happyGoto action_42
action_232 (33) = happyGoto action_43
action_232 (35) = happyGoto action_44
action_232 (37) = happyGoto action_45
action_232 _ = happyFail (happyExpListPerState 232)

action_233 (57) = happyShift action_234
action_233 _ = happyFail (happyExpListPerState 233)

action_234 (104) = happyShift action_62
action_234 (17) = happyGoto action_243
action_234 (18) = happyGoto action_150
action_234 (19) = happyGoto action_60
action_234 (43) = happyGoto action_61
action_234 _ = happyReduce_103

action_235 (116) = happyShift action_242
action_235 _ = happyFail (happyExpListPerState 235)

action_236 (108) = happyShift action_241
action_236 _ = happyFail (happyExpListPerState 236)

action_237 (50) = happyShift action_46
action_237 (62) = happyShift action_47
action_237 (68) = happyShift action_48
action_237 (70) = happyShift action_49
action_237 (71) = happyShift action_50
action_237 (72) = happyShift action_51
action_237 (73) = happyShift action_52
action_237 (74) = happyShift action_53
action_237 (80) = happyShift action_54
action_237 (95) = happyShift action_55
action_237 (102) = happyShift action_56
action_237 (104) = happyShift action_57
action_237 (105) = happyShift action_58
action_237 (20) = happyGoto action_36
action_237 (25) = happyGoto action_240
action_237 (26) = happyGoto action_38
action_237 (27) = happyGoto action_39
action_237 (28) = happyGoto action_40
action_237 (29) = happyGoto action_41
action_237 (31) = happyGoto action_42
action_237 (33) = happyGoto action_43
action_237 (35) = happyGoto action_44
action_237 (37) = happyGoto action_45
action_237 _ = happyFail (happyExpListPerState 237)

action_238 (50) = happyShift action_46
action_238 (62) = happyShift action_47
action_238 (68) = happyShift action_48
action_238 (70) = happyShift action_49
action_238 (71) = happyShift action_50
action_238 (72) = happyShift action_51
action_238 (73) = happyShift action_52
action_238 (74) = happyShift action_53
action_238 (80) = happyShift action_54
action_238 (95) = happyShift action_55
action_238 (102) = happyShift action_56
action_238 (104) = happyShift action_57
action_238 (105) = happyShift action_58
action_238 (20) = happyGoto action_36
action_238 (25) = happyGoto action_239
action_238 (26) = happyGoto action_38
action_238 (27) = happyGoto action_39
action_238 (28) = happyGoto action_40
action_238 (29) = happyGoto action_41
action_238 (31) = happyGoto action_42
action_238 (33) = happyGoto action_43
action_238 (35) = happyGoto action_44
action_238 (37) = happyGoto action_45
action_238 _ = happyFail (happyExpListPerState 238)

action_239 (96) = happyShift action_248
action_239 _ = happyFail (happyExpListPerState 239)

action_240 (116) = happyShift action_247
action_240 _ = happyFail (happyExpListPerState 240)

action_241 (95) = happyShift action_246
action_241 _ = happyFail (happyExpListPerState 241)

action_242 (50) = happyShift action_46
action_242 (62) = happyShift action_47
action_242 (68) = happyShift action_48
action_242 (70) = happyShift action_49
action_242 (71) = happyShift action_50
action_242 (72) = happyShift action_51
action_242 (73) = happyShift action_52
action_242 (74) = happyShift action_53
action_242 (80) = happyShift action_54
action_242 (95) = happyShift action_55
action_242 (102) = happyShift action_56
action_242 (104) = happyShift action_57
action_242 (105) = happyShift action_58
action_242 (20) = happyGoto action_36
action_242 (25) = happyGoto action_245
action_242 (26) = happyGoto action_38
action_242 (27) = happyGoto action_39
action_242 (28) = happyGoto action_40
action_242 (29) = happyGoto action_41
action_242 (31) = happyGoto action_42
action_242 (33) = happyGoto action_43
action_242 (35) = happyGoto action_44
action_242 (37) = happyGoto action_45
action_242 _ = happyFail (happyExpListPerState 242)

action_243 (53) = happyShift action_21
action_243 (54) = happyShift action_22
action_243 (55) = happyShift action_23
action_243 (56) = happyShift action_24
action_243 (7) = happyGoto action_244
action_243 (8) = happyGoto action_19
action_243 (39) = happyGoto action_20
action_243 _ = happyReduce_95

action_244 (102) = happyShift action_68
action_244 (13) = happyGoto action_253
action_244 _ = happyFail (happyExpListPerState 244)

action_245 (101) = happyShift action_252
action_245 _ = happyFail (happyExpListPerState 245)

action_246 (104) = happyShift action_251
action_246 _ = happyFail (happyExpListPerState 246)

action_247 (50) = happyShift action_46
action_247 (62) = happyShift action_47
action_247 (68) = happyShift action_48
action_247 (70) = happyShift action_49
action_247 (71) = happyShift action_50
action_247 (72) = happyShift action_51
action_247 (73) = happyShift action_52
action_247 (74) = happyShift action_53
action_247 (80) = happyShift action_54
action_247 (95) = happyShift action_55
action_247 (102) = happyShift action_56
action_247 (104) = happyShift action_57
action_247 (105) = happyShift action_58
action_247 (20) = happyGoto action_36
action_247 (25) = happyGoto action_250
action_247 (26) = happyGoto action_38
action_247 (27) = happyGoto action_39
action_247 (28) = happyGoto action_40
action_247 (29) = happyGoto action_41
action_247 (31) = happyGoto action_42
action_247 (33) = happyGoto action_43
action_247 (35) = happyGoto action_44
action_247 (37) = happyGoto action_45
action_247 _ = happyFail (happyExpListPerState 247)

action_248 (108) = happyShift action_249
action_248 _ = happyFail (happyExpListPerState 248)

action_249 _ = happyReduce_49

action_250 (101) = happyShift action_256
action_250 _ = happyFail (happyExpListPerState 250)

action_251 (106) = happyShift action_255
action_251 _ = happyFail (happyExpListPerState 251)

action_252 (108) = happyShift action_254
action_252 _ = happyFail (happyExpListPerState 252)

action_253 _ = happyReduce_32

action_254 (50) = happyShift action_46
action_254 (62) = happyShift action_47
action_254 (68) = happyShift action_48
action_254 (70) = happyShift action_49
action_254 (71) = happyShift action_50
action_254 (72) = happyShift action_51
action_254 (73) = happyShift action_52
action_254 (74) = happyShift action_53
action_254 (80) = happyShift action_54
action_254 (95) = happyShift action_55
action_254 (102) = happyShift action_56
action_254 (104) = happyShift action_57
action_254 (105) = happyShift action_58
action_254 (119) = happyShift action_261
action_254 (120) = happyShift action_262
action_254 (20) = happyGoto action_36
action_254 (22) = happyGoto action_259
action_254 (25) = happyGoto action_260
action_254 (26) = happyGoto action_38
action_254 (27) = happyGoto action_39
action_254 (28) = happyGoto action_40
action_254 (29) = happyGoto action_41
action_254 (31) = happyGoto action_42
action_254 (33) = happyGoto action_43
action_254 (35) = happyGoto action_44
action_254 (37) = happyGoto action_45
action_254 _ = happyReduce_44

action_255 (104) = happyShift action_258
action_255 _ = happyFail (happyExpListPerState 255)

action_256 (108) = happyShift action_257
action_256 _ = happyFail (happyExpListPerState 256)

action_257 (95) = happyShift action_268
action_257 _ = happyFail (happyExpListPerState 257)

action_258 (96) = happyShift action_267
action_258 _ = happyFail (happyExpListPerState 258)

action_259 (83) = happyShift action_203
action_259 (84) = happyShift action_204
action_259 (23) = happyGoto action_266
action_259 _ = happyReduce_48

action_260 (122) = happyShift action_265
action_260 _ = happyFail (happyExpListPerState 260)

action_261 (95) = happyShift action_264
action_261 _ = happyFail (happyExpListPerState 261)

action_262 (95) = happyShift action_263
action_262 _ = happyFail (happyExpListPerState 262)

action_263 (50) = happyShift action_46
action_263 (62) = happyShift action_47
action_263 (68) = happyShift action_48
action_263 (70) = happyShift action_49
action_263 (71) = happyShift action_50
action_263 (72) = happyShift action_51
action_263 (73) = happyShift action_52
action_263 (74) = happyShift action_53
action_263 (80) = happyShift action_54
action_263 (95) = happyShift action_55
action_263 (102) = happyShift action_56
action_263 (104) = happyShift action_57
action_263 (105) = happyShift action_58
action_263 (119) = happyShift action_261
action_263 (120) = happyShift action_262
action_263 (20) = happyGoto action_36
action_263 (22) = happyGoto action_275
action_263 (25) = happyGoto action_260
action_263 (26) = happyGoto action_38
action_263 (27) = happyGoto action_39
action_263 (28) = happyGoto action_40
action_263 (29) = happyGoto action_41
action_263 (31) = happyGoto action_42
action_263 (33) = happyGoto action_43
action_263 (35) = happyGoto action_44
action_263 (37) = happyGoto action_45
action_263 _ = happyReduce_44

action_264 (50) = happyShift action_46
action_264 (62) = happyShift action_47
action_264 (68) = happyShift action_48
action_264 (70) = happyShift action_49
action_264 (71) = happyShift action_50
action_264 (72) = happyShift action_51
action_264 (73) = happyShift action_52
action_264 (74) = happyShift action_53
action_264 (80) = happyShift action_54
action_264 (95) = happyShift action_55
action_264 (102) = happyShift action_56
action_264 (104) = happyShift action_57
action_264 (105) = happyShift action_58
action_264 (119) = happyShift action_261
action_264 (120) = happyShift action_262
action_264 (20) = happyGoto action_36
action_264 (22) = happyGoto action_274
action_264 (25) = happyGoto action_260
action_264 (26) = happyGoto action_38
action_264 (27) = happyGoto action_39
action_264 (28) = happyGoto action_40
action_264 (29) = happyGoto action_41
action_264 (31) = happyGoto action_42
action_264 (33) = happyGoto action_43
action_264 (35) = happyGoto action_44
action_264 (37) = happyGoto action_45
action_264 _ = happyReduce_44

action_265 (118) = happyShift action_273
action_265 _ = happyFail (happyExpListPerState 265)

action_266 (82) = happyShift action_271
action_266 (94) = happyShift action_272
action_266 (38) = happyGoto action_270
action_266 _ = happyFail (happyExpListPerState 266)

action_267 _ = happyReduce_53

action_268 (50) = happyShift action_46
action_268 (62) = happyShift action_47
action_268 (68) = happyShift action_48
action_268 (70) = happyShift action_49
action_268 (71) = happyShift action_50
action_268 (72) = happyShift action_51
action_268 (73) = happyShift action_52
action_268 (74) = happyShift action_53
action_268 (80) = happyShift action_54
action_268 (95) = happyShift action_55
action_268 (102) = happyShift action_56
action_268 (104) = happyShift action_57
action_268 (105) = happyShift action_58
action_268 (20) = happyGoto action_36
action_268 (25) = happyGoto action_269
action_268 (26) = happyGoto action_38
action_268 (27) = happyGoto action_39
action_268 (28) = happyGoto action_40
action_268 (29) = happyGoto action_41
action_268 (31) = happyGoto action_42
action_268 (33) = happyGoto action_43
action_268 (35) = happyGoto action_44
action_268 (37) = happyGoto action_45
action_268 _ = happyFail (happyExpListPerState 268)

action_269 (106) = happyShift action_281
action_269 _ = happyFail (happyExpListPerState 269)

action_270 _ = happyReduce_41

action_271 (104) = happyShift action_280
action_271 _ = happyFail (happyExpListPerState 271)

action_272 (50) = happyShift action_46
action_272 (62) = happyShift action_47
action_272 (68) = happyShift action_48
action_272 (70) = happyShift action_49
action_272 (71) = happyShift action_50
action_272 (72) = happyShift action_51
action_272 (73) = happyShift action_52
action_272 (74) = happyShift action_53
action_272 (80) = happyShift action_54
action_272 (95) = happyShift action_55
action_272 (102) = happyShift action_56
action_272 (104) = happyShift action_57
action_272 (105) = happyShift action_58
action_272 (20) = happyGoto action_36
action_272 (25) = happyGoto action_132
action_272 (26) = happyGoto action_38
action_272 (27) = happyGoto action_39
action_272 (28) = happyGoto action_40
action_272 (29) = happyGoto action_41
action_272 (31) = happyGoto action_42
action_272 (33) = happyGoto action_43
action_272 (35) = happyGoto action_44
action_272 (37) = happyGoto action_45
action_272 (48) = happyGoto action_279
action_272 _ = happyReduce_111

action_273 (95) = happyShift action_278
action_273 _ = happyFail (happyExpListPerState 273)

action_274 (96) = happyShift action_277
action_274 _ = happyFail (happyExpListPerState 274)

action_275 (96) = happyShift action_276
action_275 _ = happyFail (happyExpListPerState 275)

action_276 (108) = happyShift action_287
action_276 _ = happyFail (happyExpListPerState 276)

action_277 (108) = happyShift action_286
action_277 _ = happyFail (happyExpListPerState 277)

action_278 (50) = happyShift action_46
action_278 (62) = happyShift action_47
action_278 (68) = happyShift action_48
action_278 (70) = happyShift action_49
action_278 (71) = happyShift action_50
action_278 (72) = happyShift action_51
action_278 (73) = happyShift action_52
action_278 (74) = happyShift action_53
action_278 (80) = happyShift action_54
action_278 (95) = happyShift action_55
action_278 (102) = happyShift action_56
action_278 (104) = happyShift action_57
action_278 (105) = happyShift action_58
action_278 (20) = happyGoto action_36
action_278 (25) = happyGoto action_285
action_278 (26) = happyGoto action_38
action_278 (27) = happyGoto action_39
action_278 (28) = happyGoto action_40
action_278 (29) = happyGoto action_41
action_278 (31) = happyGoto action_42
action_278 (33) = happyGoto action_43
action_278 (35) = happyGoto action_44
action_278 (37) = happyGoto action_45
action_278 _ = happyFail (happyExpListPerState 278)

action_279 (99) = happyShift action_284
action_279 _ = happyFail (happyExpListPerState 279)

action_280 (85) = happyShift action_283
action_280 _ = happyFail (happyExpListPerState 280)

action_281 (50) = happyShift action_46
action_281 (62) = happyShift action_47
action_281 (68) = happyShift action_48
action_281 (70) = happyShift action_49
action_281 (71) = happyShift action_50
action_281 (72) = happyShift action_51
action_281 (73) = happyShift action_52
action_281 (74) = happyShift action_53
action_281 (80) = happyShift action_54
action_281 (95) = happyShift action_55
action_281 (102) = happyShift action_56
action_281 (104) = happyShift action_57
action_281 (105) = happyShift action_58
action_281 (20) = happyGoto action_36
action_281 (25) = happyGoto action_282
action_281 (26) = happyGoto action_38
action_281 (27) = happyGoto action_39
action_281 (28) = happyGoto action_40
action_281 (29) = happyGoto action_41
action_281 (31) = happyGoto action_42
action_281 (33) = happyGoto action_43
action_281 (35) = happyGoto action_44
action_281 (37) = happyGoto action_45
action_281 _ = happyFail (happyExpListPerState 281)

action_282 (96) = happyShift action_290
action_282 _ = happyFail (happyExpListPerState 282)

action_283 (100) = happyShift action_289
action_283 _ = happyFail (happyExpListPerState 283)

action_284 _ = happyReduce_94

action_285 (96) = happyShift action_288
action_285 _ = happyFail (happyExpListPerState 285)

action_286 _ = happyReduce_46

action_287 _ = happyReduce_47

action_288 (108) = happyShift action_293
action_288 _ = happyFail (happyExpListPerState 288)

action_289 (50) = happyShift action_46
action_289 (62) = happyShift action_47
action_289 (68) = happyShift action_48
action_289 (70) = happyShift action_49
action_289 (71) = happyShift action_50
action_289 (72) = happyShift action_51
action_289 (73) = happyShift action_52
action_289 (74) = happyShift action_53
action_289 (80) = happyShift action_54
action_289 (95) = happyShift action_55
action_289 (102) = happyShift action_56
action_289 (104) = happyShift action_57
action_289 (105) = happyShift action_58
action_289 (20) = happyGoto action_36
action_289 (25) = happyGoto action_292
action_289 (26) = happyGoto action_38
action_289 (27) = happyGoto action_39
action_289 (28) = happyGoto action_40
action_289 (29) = happyGoto action_41
action_289 (31) = happyGoto action_42
action_289 (33) = happyGoto action_43
action_289 (35) = happyGoto action_44
action_289 (37) = happyGoto action_45
action_289 _ = happyFail (happyExpListPerState 289)

action_290 (108) = happyShift action_291
action_290 _ = happyFail (happyExpListPerState 290)

action_291 _ = happyReduce_50

action_292 (116) = happyShift action_294
action_292 _ = happyFail (happyExpListPerState 292)

action_293 _ = happyReduce_45

action_294 (50) = happyShift action_46
action_294 (62) = happyShift action_47
action_294 (68) = happyShift action_48
action_294 (70) = happyShift action_49
action_294 (71) = happyShift action_50
action_294 (72) = happyShift action_51
action_294 (73) = happyShift action_52
action_294 (74) = happyShift action_53
action_294 (80) = happyShift action_54
action_294 (95) = happyShift action_55
action_294 (102) = happyShift action_56
action_294 (104) = happyShift action_57
action_294 (105) = happyShift action_58
action_294 (20) = happyGoto action_36
action_294 (25) = happyGoto action_295
action_294 (26) = happyGoto action_38
action_294 (27) = happyGoto action_39
action_294 (28) = happyGoto action_40
action_294 (29) = happyGoto action_41
action_294 (31) = happyGoto action_42
action_294 (33) = happyGoto action_43
action_294 (35) = happyGoto action_44
action_294 (37) = happyGoto action_45
action_294 _ = happyFail (happyExpListPerState 294)

action_295 (101) = happyShift action_296
action_295 _ = happyFail (happyExpListPerState 295)

action_296 (108) = happyShift action_297
action_296 _ = happyFail (happyExpListPerState 296)

action_297 (95) = happyShift action_82
action_297 (46) = happyGoto action_298
action_297 _ = happyFail (happyExpListPerState 297)

action_298 _ = happyReduce_42

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (( _, L.TDafny happy_var_1  )))
	 =  HappyAbsSyn6
		 (inj (QDafny happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyMonadReduce 11 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn44  happy_var_11) `HappyStk`
	(HappyAbsSyn7  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_2     ))) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (((  ((\(rs, es) -> inj (QMethod happy_var_2 happy_var_4 happy_var_8 rs es happy_var_11)) `fmap` (requireEnsures happy_var_10))))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_5 = happyMonadReduce 7 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn44  happy_var_7) `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_2     ))) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (((  ((\(rs, es) -> inj (QMethod happy_var_2 happy_var_4 [] rs es happy_var_7)) `fmap` (requireEnsures happy_var_6))))
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Requires happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Ensures happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Invariants happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  8 happyReduction_10
happyReduction_10 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Separates happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (( _, L.TId happy_var_1     )))
	 =  HappyAbsSyn10
		 (Binding happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn11
		 (TNat
	)

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn11
		 (TInt
	)

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn11
		 (TBool
	)

happyReduce_16 = happyReduce 4 11 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TSeq happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 11 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyTerminal (( _, L.TLitInt happy_var_3 ))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TQReg (ANat happy_var_3)
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 11 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_3     ))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (TQReg (AVar happy_var_3)
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn12
		 (TNor
	)

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn12
		 (THad
	)

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn12
		 (TEN
	)

happyReduce_22 = happySpecReduce_1  12 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn12
		 (TEN01
	)

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Block happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  15 happyReduction_25
happyReduction_25 (HappyTerminal (( _, L.TDafny happy_var_1  )))
	 =  HappyAbsSyn15
		 (SDafny happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  15 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (SAssert happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (SVar happy_var_2 Nothing
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 5 15 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SVar happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 15 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_1     ))) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (happy_var_1 ::=: happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 15 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (happy_var_1 :*=: happy_var_3
	) `HappyStk` happyRest

happyReduce_31 = happyMonadReduce 6 15 happyReduction_31
happyReduction_31 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( do sep <- separatesOnly happy_var_5; return $ SIf happy_var_3 sep happy_var_6))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_32 = happyMonadReduce 12 15 happyReduction_32
happyReduction_32 ((HappyAbsSyn13  happy_var_12) `HappyStk`
	(HappyAbsSyn7  happy_var_11) `HappyStk`
	(HappyAbsSyn17  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_2     ))) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( do (invs, sep) <- invariantSeperates happy_var_11; return $ SFor happy_var_2 happy_var_5 happy_var_7 happy_var_10 invs sep happy_var_12))
	) (\r -> happyReturn (HappyAbsSyn15 r))

happyReduce_33 = happySpecReduce_3  15 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn46  happy_var_2)
	(HappyTerminal (( _, L.TId happy_var_1     )))
	 =  HappyAbsSyn15
		 (SCall happy_var_1 happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 (HappyAbsSyn25  happy_var_3)
	_
	_
	 =  HappyAbsSyn16
		 (happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  17 happyReduction_35
happyReduction_35 (HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (GEPartition happy_var_1 happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  18 happyReduction_36
happyReduction_36 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn18
		 (Partition $ happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happyReduce 6 19 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_1     ))) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Range happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 7 20 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn47  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (ESpec happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 7 20 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (ESpec happy_var_2 happy_var_4 [happy_var_6]
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 4 21 happyReduction_40
happyReduction_40 ((HappyAbsSyn46  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_2     ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (SESpecNor happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 12 21 happyReduction_41
happyReduction_41 ((HappyAbsSyn38  happy_var_12) `HappyStk`
	(HappyAbsSyn23  happy_var_11) `HappyStk`
	(HappyAbsSyn22  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_2     ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((SESpecEN happy_var_2 (Intv happy_var_5 happy_var_7) happy_var_12, happy_var_10, happy_var_11)
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 21 21 happyReduction_42
happyReduction_42 ((HappyAbsSyn46  happy_var_21) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_18) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_16) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_13     ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_11) `HappyStk`
	(HappyAbsSyn22  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_2     ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((SESpecEN01 happy_var_2 (Intv happy_var_5 happy_var_7) happy_var_13 (Intv happy_var_16 happy_var_18) happy_var_21, happy_var_10, happy_var_11)
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn21
		 ((SEWildcard, PhaseZ)
	)

happyReduce_44 = happySpecReduce_0  22 happyReduction_44
happyReduction_44  =  HappyAbsSyn22
		 (AExp (ENum 1)
	)

happyReduce_45 = happyReduce 7 22 happyReduction_45
happyReduction_45 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (ADivSq happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_46 = happyReduce 5 22 happyReduction_46
happyReduction_46 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (ASin happy_var_3
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 5 22 happyReduction_47
happyReduction_47 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (ASin happy_var_3
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_0  23 happyReduction_48
happyReduction_48  =  HappyAbsSyn23
		 (PhaseZ
	)

happyReduce_49 = happyReduce 7 23 happyReduction_49
happyReduction_49 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (PhaseOmega happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 15 23 happyReduction_50
happyReduction_50 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_13) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_2     ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (PhaseSumOmega (Range happy_var_2 happy_var_5 happy_var_7) happy_var_11 happy_var_13
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_1  24 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn24
		 (PhaseWildCard
	)

happyReduce_52 = happyReduce 6 24 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_5     ))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_3     ))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (PhaseOmega happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 14 24 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_13     ))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_11     ))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_2     ))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (PhaseSumOmega (Range happy_var_2 happy_var_5 happy_var_7) happy_var_11 happy_var_13
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_1  25 happyReduction_54
happyReduction_54 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  25 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn25
		 (EWildcard
	)

happyReduce_56 = happySpecReduce_1  25 happyReduction_56
happyReduction_56 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  25 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (EPartition happy_var_2
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  25 happyReduction_58
happyReduction_58 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  25 happyReduction_59
happyReduction_59 (HappyTerminal (( _, L.TId happy_var_2     )))
	_
	 =  HappyAbsSyn25
		 (EMea happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  25 happyReduction_60
happyReduction_60 (HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (EOp1 ONot happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 6 25 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyTerminal (( _, L.TLitInt happy_var_5 ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn37  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (EOp2 ONor happy_var_3 (ENum happy_var_5)
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 6 25 happyReduction_62
happyReduction_62 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_3     ))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ELambda PhaseWildCard happy_var_3 Nothing happy_var_5
	) `HappyStk` happyRest

happyReduce_63 = happyReduce 9 25 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_8) `HappyStk`
	(HappyAbsSyn23  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (( _, L.TId happy_var_5     ))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ELambda happy_var_3 happy_var_5 (Just happy_var_7) happy_var_8
	) `HappyStk` happyRest

happyReduce_64 = happySpecReduce_2  25 happyReduction_64
happyReduction_64 (HappyAbsSyn46  happy_var_2)
	(HappyTerminal (( _, L.TId happy_var_1     )))
	 =  HappyAbsSyn25
		 (EApp happy_var_1 happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happyReduce 4 25 happyReduction_65
happyReduction_65 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (ERepr happy_var_3
	) `HappyStk` happyRest

happyReduce_66 = happySpecReduce_1  25 happyReduction_66
happyReduction_66 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  26 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn26
		 (EHad
	)

happyReduce_68 = happySpecReduce_1  26 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn26
		 (EQFT
	)

happyReduce_69 = happySpecReduce_1  26 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn26
		 (ERQFT
	)

happyReduce_70 = happySpecReduce_3  27 happyReduction_70
happyReduction_70 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (EOp2 OOr happy_var_1 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  27 happyReduction_71
happyReduction_71 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  28 happyReduction_72
happyReduction_72 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (EOp2 OAnd happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  28 happyReduction_73
happyReduction_73 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  29 happyReduction_74
happyReduction_74 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (EOp2 happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  30 happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn30
		 (OGt
	)

happyReduce_76 = happySpecReduce_1  30 happyReduction_76
happyReduction_76 _
	 =  HappyAbsSyn30
		 (OLt
	)

happyReduce_77 = happySpecReduce_1  30 happyReduction_77
happyReduction_77 _
	 =  HappyAbsSyn30
		 (OGe
	)

happyReduce_78 = happySpecReduce_1  30 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn30
		 (OLe
	)

happyReduce_79 = happySpecReduce_1  30 happyReduction_79
happyReduction_79 _
	 =  HappyAbsSyn30
		 (OEq
	)

happyReduce_80 = happySpecReduce_3  31 happyReduction_80
happyReduction_80 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (EOp2 happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  31 happyReduction_81
happyReduction_81 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  32 happyReduction_82
happyReduction_82 _
	 =  HappyAbsSyn30
		 (OMod
	)

happyReduce_83 = happySpecReduce_1  32 happyReduction_83
happyReduction_83 _
	 =  HappyAbsSyn30
		 (OExp
	)

happyReduce_84 = happySpecReduce_3  33 happyReduction_84
happyReduction_84 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (EOp2 happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_84 _ _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  33 happyReduction_85
happyReduction_85 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  34 happyReduction_86
happyReduction_86 _
	 =  HappyAbsSyn30
		 (OMod
	)

happyReduce_87 = happySpecReduce_3  35 happyReduction_87
happyReduction_87 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn16
		 (EOp2 happy_var_2 happy_var_1 happy_var_3
	)
happyReduction_87 _ _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  35 happyReduction_88
happyReduction_88 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  36 happyReduction_89
happyReduction_89 _
	 =  HappyAbsSyn30
		 (OAdd
	)

happyReduce_90 = happySpecReduce_1  36 happyReduction_90
happyReduction_90 _
	 =  HappyAbsSyn30
		 (OSub
	)

happyReduce_91 = happySpecReduce_1  37 happyReduction_91
happyReduction_91 (HappyTerminal (( _, L.TLitInt happy_var_1 )))
	 =  HappyAbsSyn37
		 (ENum happy_var_1
	)
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_1  37 happyReduction_92
happyReduction_92 (HappyTerminal (( _, L.TId happy_var_1     )))
	 =  HappyAbsSyn37
		 (EVar happy_var_1
	)
happyReduction_92 _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  37 happyReduction_93
happyReduction_93 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (happy_var_2
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  38 happyReduction_94
happyReduction_94 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn38
		 (happy_var_2
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_0  39 happyReduction_95
happyReduction_95  =  HappyAbsSyn39
		 ([]
	)

happyReduce_96 = happySpecReduce_2  39 happyReduction_96
happyReduction_96 (HappyAbsSyn39  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn39
		 (happy_var_1 : happy_var_2
	)
happyReduction_96 _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_0  40 happyReduction_97
happyReduction_97  =  HappyAbsSyn40
		 ([]
	)

happyReduce_98 = happySpecReduce_2  40 happyReduction_98
happyReduction_98 (HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1 : happy_var_2
	)
happyReduction_98 _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_0  41 happyReduction_99
happyReduction_99  =  HappyAbsSyn41
		 ([]
	)

happyReduce_100 = happySpecReduce_2  41 happyReduction_100
happyReduction_100 (HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1 : happy_var_2
	)
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_0  42 happyReduction_101
happyReduction_101  =  HappyAbsSyn42
		 ([]
	)

happyReduce_102 = happySpecReduce_3  42 happyReduction_102
happyReduction_102 (HappyAbsSyn42  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1 : happy_var_3
	)
happyReduction_102 _ _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_0  43 happyReduction_103
happyReduction_103  =  HappyAbsSyn43
		 ([]
	)

happyReduce_104 = happySpecReduce_3  43 happyReduction_104
happyReduction_104 (HappyAbsSyn43  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn43
		 (happy_var_1 : happy_var_3
	)
happyReduction_104 _ _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_0  44 happyReduction_105
happyReduction_105  =  HappyAbsSyn44
		 (Nothing
	)

happyReduce_106 = happySpecReduce_1  44 happyReduction_106
happyReduction_106 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn44
		 (Just happy_var_1
	)
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_0  45 happyReduction_107
happyReduction_107  =  HappyAbsSyn45
		 (Nothing
	)

happyReduce_108 = happySpecReduce_1  45 happyReduction_108
happyReduction_108 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn45
		 (Just happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_3  46 happyReduction_109
happyReduction_109 _
	(HappyAbsSyn48  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (happy_var_2
	)
happyReduction_109 _ _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_3  47 happyReduction_110
happyReduction_110 _
	(HappyAbsSyn49  happy_var_2)
	_
	 =  HappyAbsSyn47
		 (happy_var_2
	)
happyReduction_110 _ _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_0  48 happyReduction_111
happyReduction_111  =  HappyAbsSyn48
		 ([]
	)

happyReduce_112 = happySpecReduce_3  48 happyReduction_112
happyReduction_112 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn48
		 (happy_var_1 : happy_var_3
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_0  49 happyReduction_113
happyReduction_113  =  HappyAbsSyn49
		 ([]
	)

happyReduce_114 = happySpecReduce_3  49 happyReduction_114
happyReduction_114 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1 : happy_var_3
	)
happyReduction_114 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 123 123 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	( _, L.TLitInt happy_dollar_dollar ) -> cont 50;
	( _, L.TDafny happy_dollar_dollar  ) -> cont 51;
	( _, L.TMethod    ) -> cont 52;
	( _, L.TEnsures   ) -> cont 53;
	( _, L.TRequires  ) -> cont 54;
	( _, L.TSeparates ) -> cont 55;
	( _, L.TInv       ) -> cont 56;
	( _, L.TWith      ) -> cont 57;
	( _, L.TAt      ) -> cont 58;
	( _, L.TSplit      ) -> cont 59;
	( _, L.TFor       ) -> cont 60;
	( _, L.TReturns   ) -> cont 61;
	( _, L.TNot       ) -> cont 62;
	( _, L.TNat       ) -> cont 63;
	( _, L.TInt       ) -> cont 64;
	( _, L.TIn        ) -> cont 65;
	( _, L.TBool      ) -> cont 66;
	( _, L.TSeq       ) -> cont 67;
	( _, L.TNor       ) -> cont 68;
	( _, L.THad       ) -> cont 69;
	( _, L.THApp      ) -> cont 70;
	( _, L.TQFT       ) -> cont 71;
	( _, L.TRQFT      ) -> cont 72;
	( _, L.TRepr      ) -> cont 73;
	( _, L.TMea       ) -> cont 74;
	( _, L.TEN        ) -> cont 75;
	( _, L.TQReg      ) -> cont 76;
	( _, L.TEN01      ) -> cont 77;
	( _, L.TVar       ) -> cont 78;
	( _, L.TIf        ) -> cont 79;
	( _, L.TCl        ) -> cont 80;
	( _, L.TUnicodeSum    ) -> cont 81;
	( _, L.TUnicodeTensor ) -> cont 82;
	( _, L.TUnicodeOmega ) -> cont 83;
	( _, L.TUnicodeSumOmega ) -> cont 84;
	( _, L.TUnicodeIn     ) -> cont 85;
	( _, L.TUnicodeMap    ) -> cont 86;
	( _, L.TAssert    ) -> cont 87;
	( _, L.TOr        ) -> cont 88;
	( _, L.TAnd       ) -> cont 89;
	( _, L.TAdd       ) -> cont 90;
	( _, L.TSub       ) -> cont 91;
	( _, L.TMul       ) -> cont 92;
	( _, L.TMod       ) -> cont 93;
	( _, L.TBar       ) -> cont 94;
	( _, L.TLPar      ) -> cont 95;
	( _, L.TRPar      ) -> cont 96;
	( _, L.TLAng      ) -> cont 97;
	( _, L.TRAng      ) -> cont 98;
	( _, L.TRAngA     ) -> cont 99;
	( _, L.TLBracket  ) -> cont 100;
	( _, L.TRBracket  ) -> cont 101;
	( _, L.TLBrace    ) -> cont 102;
	( _, L.TRBrace    ) -> cont 103;
	( _, L.TId happy_dollar_dollar     ) -> cont 104;
	( _, L.TWildcard  ) -> cont 105;
	( _, L.TComma     ) -> cont 106;
	( _, L.TColon     ) -> cont 107;
	( _, L.TDot       ) -> cont 108;
	( _, L.TSemi      ) -> cont 109;
	( _, L.TEq        ) -> cont 110;
	( _, L.TArrow     ) -> cont 111;
	( _, L.TGe        ) -> cont 112;
	( _, L.TLe        ) -> cont 113;
	( _, L.TAssign    ) -> cont 114;
	( _, L.TApply     ) -> cont 115;
	( _, L.TDots      ) -> cont 116;
	( _, L.TTilde     ) -> cont 117;
	( _, L.TSqrt      ) -> cont 118;
	( _, L.TSin     ) -> cont 119;
	( _, L.TCos      ) -> cont 120;
	( _, L.TExp      ) -> cont 121;
	( _, L.TDiv      ) -> cont 122;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 123 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Parser a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(L.SToken)], [Prelude.String]) -> Parser a
happyError' = (\(tokens, _) -> parseError tokens)
runParser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


scanAndParse :: String -> Parser AST
scanAndParse = runParser <=< L.runScanner
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
