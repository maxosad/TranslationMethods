{-# OPTIONS_GHC -w #-}
module Parser where
import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,126) ([0,256,0,0,2,0,2048,0,0,0,0,256,0,0,4,0,256,0,0,0,0,0,32797,0,16,0,0,1,0,59392,1024,0,464,8,0,4096,0,0,48,2048,0,0,16,0,0,1,0,0,768,0,1536,6,0,3084,30720,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,464,8,0,0,0,0,0,0,24576,0,0,192,0,32768,1,0,768,0,0,6,60,1008,0,0,0,0,0,0,16,0,0,0,0,0,0,32768,0,0,30,0,16384,0,0,128,0,0,0,48,0,24576,0,0,192,0,32768,1,0,768,0,0,6,0,0,0,0,0,0,0,0,0,0,960,0,32768,7,0,3840,0,0,30,0,15360,0,0,120,0,61440,0,0,0,0,0,8192,0,2048,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Root","Block","Text","Str","Arithm","Unary","LogicUnary","Cmp","Logic","While","If","'+'","'-'","'*'","'/'","'{'","'}'","'('","')'","';'","'='","'int'","'main'","'return'","'while'","'if'","'else'","'True'","'False'","'>'","'<'","'<='","'>='","'=='","'!='","cnst","var","%eof"]
        bit_start = st * 41
        bit_end = (st + 1) * 41
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..40]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (25) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (25) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (26) = happyShift action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (41) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (21) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (22) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (19) = happyShift action_8
action_6 (5) = happyGoto action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_1

action_8 (25) = happyShift action_13
action_8 (27) = happyShift action_14
action_8 (28) = happyShift action_15
action_8 (29) = happyShift action_16
action_8 (40) = happyShift action_17
action_8 (6) = happyGoto action_9
action_8 (7) = happyGoto action_10
action_8 (13) = happyGoto action_11
action_8 (14) = happyGoto action_12
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (20) = happyShift action_29
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (23) = happyShift action_28
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (25) = happyShift action_13
action_11 (27) = happyShift action_14
action_11 (28) = happyShift action_15
action_11 (29) = happyShift action_16
action_11 (40) = happyShift action_17
action_11 (6) = happyGoto action_27
action_11 (7) = happyGoto action_10
action_11 (13) = happyGoto action_11
action_11 (14) = happyGoto action_12
action_11 _ = happyReduce_8

action_12 (25) = happyShift action_13
action_12 (27) = happyShift action_14
action_12 (28) = happyShift action_15
action_12 (29) = happyShift action_16
action_12 (40) = happyShift action_17
action_12 (6) = happyGoto action_26
action_12 (7) = happyGoto action_10
action_12 (13) = happyGoto action_11
action_12 (14) = happyGoto action_12
action_12 _ = happyReduce_6

action_13 (40) = happyShift action_25
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (39) = happyShift action_23
action_14 (40) = happyShift action_24
action_14 (8) = happyGoto action_21
action_14 (9) = happyGoto action_22
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (21) = happyShift action_20
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (21) = happyShift action_19
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (24) = happyShift action_18
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (39) = happyShift action_23
action_18 (40) = happyShift action_24
action_18 (8) = happyGoto action_43
action_18 (9) = happyGoto action_22
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (31) = happyShift action_40
action_19 (32) = happyShift action_41
action_19 (39) = happyShift action_23
action_19 (40) = happyShift action_24
action_19 (8) = happyGoto action_36
action_19 (9) = happyGoto action_22
action_19 (10) = happyGoto action_37
action_19 (11) = happyGoto action_38
action_19 (12) = happyGoto action_42
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (31) = happyShift action_40
action_20 (32) = happyShift action_41
action_20 (39) = happyShift action_23
action_20 (40) = happyShift action_24
action_20 (8) = happyGoto action_36
action_20 (9) = happyGoto action_22
action_20 (10) = happyGoto action_37
action_20 (11) = happyGoto action_38
action_20 (12) = happyGoto action_39
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (15) = happyShift action_32
action_21 (16) = happyShift action_33
action_21 (17) = happyShift action_34
action_21 (18) = happyShift action_35
action_21 _ = happyReduce_12

action_22 _ = happyReduce_17

action_23 _ = happyReduce_18

action_24 _ = happyReduce_19

action_25 (24) = happyShift action_31
action_25 _ = happyReduce_10

action_26 _ = happyReduce_5

action_27 _ = happyReduce_7

action_28 (25) = happyShift action_13
action_28 (27) = happyShift action_14
action_28 (28) = happyShift action_15
action_28 (29) = happyShift action_16
action_28 (40) = happyShift action_17
action_28 (6) = happyGoto action_30
action_28 (7) = happyGoto action_10
action_28 (13) = happyGoto action_11
action_28 (14) = happyGoto action_12
action_28 _ = happyReduce_3

action_29 _ = happyReduce_2

action_30 _ = happyReduce_4

action_31 (39) = happyShift action_23
action_31 (40) = happyShift action_24
action_31 (8) = happyGoto action_56
action_31 (9) = happyGoto action_22
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (39) = happyShift action_23
action_32 (40) = happyShift action_24
action_32 (8) = happyGoto action_55
action_32 (9) = happyGoto action_22
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (39) = happyShift action_23
action_33 (40) = happyShift action_24
action_33 (8) = happyGoto action_54
action_33 (9) = happyGoto action_22
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (39) = happyShift action_23
action_34 (40) = happyShift action_24
action_34 (8) = happyGoto action_53
action_34 (9) = happyGoto action_22
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (39) = happyShift action_23
action_35 (40) = happyShift action_24
action_35 (8) = happyGoto action_52
action_35 (9) = happyGoto action_22
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (15) = happyShift action_32
action_36 (16) = happyShift action_33
action_36 (17) = happyShift action_34
action_36 (18) = happyShift action_35
action_36 (33) = happyShift action_46
action_36 (34) = happyShift action_47
action_36 (35) = happyShift action_48
action_36 (36) = happyShift action_49
action_36 (37) = happyShift action_50
action_36 (38) = happyShift action_51
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_29

action_38 _ = happyReduce_28

action_39 (22) = happyShift action_45
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_20

action_41 _ = happyReduce_21

action_42 (22) = happyShift action_44
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (15) = happyShift action_32
action_43 (16) = happyShift action_33
action_43 (17) = happyShift action_34
action_43 (18) = happyShift action_35
action_43 _ = happyReduce_11

action_44 (19) = happyShift action_8
action_44 (5) = happyGoto action_64
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (19) = happyShift action_8
action_45 (5) = happyGoto action_63
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (39) = happyShift action_23
action_46 (40) = happyShift action_24
action_46 (8) = happyGoto action_62
action_46 (9) = happyGoto action_22
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (39) = happyShift action_23
action_47 (40) = happyShift action_24
action_47 (8) = happyGoto action_61
action_47 (9) = happyGoto action_22
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (39) = happyShift action_23
action_48 (40) = happyShift action_24
action_48 (8) = happyGoto action_60
action_48 (9) = happyGoto action_22
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (39) = happyShift action_23
action_49 (40) = happyShift action_24
action_49 (8) = happyGoto action_59
action_49 (9) = happyGoto action_22
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (39) = happyShift action_23
action_50 (40) = happyShift action_24
action_50 (8) = happyGoto action_58
action_50 (9) = happyGoto action_22
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (39) = happyShift action_23
action_51 (40) = happyShift action_24
action_51 (8) = happyGoto action_57
action_51 (9) = happyGoto action_22
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (15) = happyShift action_32
action_52 (16) = happyShift action_33
action_52 (17) = happyShift action_34
action_52 (18) = happyShift action_35
action_52 _ = happyReduce_16

action_53 (15) = happyShift action_32
action_53 (16) = happyShift action_33
action_53 (17) = happyShift action_34
action_53 (18) = happyShift action_35
action_53 _ = happyReduce_15

action_54 (15) = happyShift action_32
action_54 (16) = happyShift action_33
action_54 (17) = happyShift action_34
action_54 (18) = happyShift action_35
action_54 _ = happyReduce_14

action_55 (15) = happyShift action_32
action_55 (16) = happyShift action_33
action_55 (17) = happyShift action_34
action_55 (18) = happyShift action_35
action_55 _ = happyReduce_13

action_56 (15) = happyShift action_32
action_56 (16) = happyShift action_33
action_56 (17) = happyShift action_34
action_56 (18) = happyShift action_35
action_56 _ = happyReduce_9

action_57 (15) = happyShift action_32
action_57 (16) = happyShift action_33
action_57 (17) = happyShift action_34
action_57 (18) = happyShift action_35
action_57 _ = happyReduce_27

action_58 (15) = happyShift action_32
action_58 (16) = happyShift action_33
action_58 (17) = happyShift action_34
action_58 (18) = happyShift action_35
action_58 _ = happyReduce_26

action_59 (15) = happyShift action_32
action_59 (16) = happyShift action_33
action_59 (17) = happyShift action_34
action_59 (18) = happyShift action_35
action_59 _ = happyReduce_24

action_60 (15) = happyShift action_32
action_60 (16) = happyShift action_33
action_60 (17) = happyShift action_34
action_60 (18) = happyShift action_35
action_60 _ = happyReduce_25

action_61 (15) = happyShift action_32
action_61 (16) = happyShift action_33
action_61 (17) = happyShift action_34
action_61 (18) = happyShift action_35
action_61 _ = happyReduce_22

action_62 (15) = happyShift action_32
action_62 (16) = happyShift action_33
action_62 (17) = happyShift action_34
action_62 (18) = happyShift action_35
action_62 _ = happyReduce_23

action_63 _ = happyReduce_30

action_64 (30) = happyShift action_65
action_64 _ = happyReduce_31

action_65 (19) = happyShift action_8
action_65 (5) = happyGoto action_66
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_32

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (RootBlock happy_var_5
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Blck happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Line happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Lines happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn6
		 (LinesIf happy_var_1 happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn6
		 (LineIf happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn6
		 (LinesWhile happy_var_1 happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn6
		 (LineWhile happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 7 happyReduction_9
happyReduction_9 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (Assign happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_2  7 happyReduction_10
happyReduction_10 (HappyTerminal (TVar happy_var_2))
	_
	 =  HappyAbsSyn7
		 (Declar happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_3)
	_
	(HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn7
		 (Set happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  7 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TReturn happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 :+: happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 :-: happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 :*: happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 :/: happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  8 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyTerminal (TConst happy_var_1))
	 =  HappyAbsSyn9
		 (Const happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 (HappyTerminal (TVar happy_var_1))
	 =  HappyAbsSyn9
		 (Var   happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  10 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn10
		 (BoolValue True
	)

happyReduce_21 = happySpecReduce_1  10 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn10
		 (BoolValue False
	)

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :<: happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :>: happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  11 happyReduction_24
happyReduction_24 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :>=: happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  11 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :<=: happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  11 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :==: happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  11 happyReduction_27
happyReduction_27 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 :!=: happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happyReduce 5 13 happyReduction_30
happyReduction_30 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (WhileExpr happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 5 14 happyReduction_31
happyReduction_31 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (IfExpr happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 7 14 happyReduction_32
happyReduction_32 ((HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (IfElExpr happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TPlus -> cont 15;
	TMinus -> cont 16;
	TMul -> cont 17;
	TDiv -> cont 18;
	TOF -> cont 19;
	TCF -> cont 20;
	TOP -> cont 21;
	TCP -> cont 22;
	TEOL -> cont 23;
	TEq -> cont 24;
	TInt -> cont 25;
	TMain -> cont 26;
	TReturn -> cont 27;
	TWhile -> cont 28;
	TIF -> cont 29;
	TELSE -> cont 30;
	TTrue -> cont 31;
	TFalse -> cont 32;
	TG -> cont 33;
	TL -> cont 34;
	TLE -> cont 35;
	TGE -> cont 36;
	TEQ -> cont 37;
	TNEQ -> cont 38;
	TConst happy_dollar_dollar -> cont 39;
	TVar happy_dollar_dollar -> cont 40;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 41 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Text = Line Str 
		  | Lines Str Text
          | LineIf IfExpr 
          | LinesIf IfExpr Text
		  | LineWhile WhileExpr 
          | LinesWhile WhileExpr Text		  
          deriving (Eq)


data Block = Blck Text deriving (Eq)
 
data ArithmExpr = ArithmExpr :+: ArithmExpr 
                | ArithmExpr :-: ArithmExpr  
                | ArithmExpr :*: ArithmExpr 
                | ArithmExpr :/: ArithmExpr 
                | Const Int
                | Var String				
				deriving (Eq)
				
data Str = Assign String ArithmExpr 
         | Declar String 
         | Set String ArithmExpr
         | TReturn ArithmExpr
		 deriving (Eq)

data Root = RootBlock Block	
  deriving (Eq)

data LogicExpr = BoolValue Bool 
               | ArithmExpr :<: ArithmExpr
               | ArithmExpr :>: ArithmExpr
               | ArithmExpr :>=: ArithmExpr
               | ArithmExpr :<=: ArithmExpr
               | ArithmExpr :==: ArithmExpr
               | ArithmExpr :!=: ArithmExpr
               deriving (Eq)

data IfExpr = IfExpr LogicExpr Block
            | IfElExpr LogicExpr Block Block
           	deriving (Eq)

data WhileExpr = WhileExpr LogicExpr Block
  deriving (Eq)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "D:/GitHub/haskell-platform/build/ghc-bindist/local/lib/include/ghcversion.h" #-}















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "F:/Users/randy/AppData/Local/Temp/ghc1900_0/ghc_2.h" #-}


























































































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates\\\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates\\\\GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 75 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 84 "templates\\\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates\\\\GenericTemplate.hs" #-}

{-# LINE 147 "templates\\\\GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = case happyDrop (k - ((1) :: Int)) sts of
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





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates\\\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates\\\\GenericTemplate.hs" #-}
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
