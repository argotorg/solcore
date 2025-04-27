{-# OPTIONS_GHC -w #-}
module Solcore.Frontend.Parser.SolcoreParser where

import Data.List.NonEmpty (NonEmpty, cons, singleton)

import Solcore.Frontend.Lexer.SolcoreLexer hiding (lexer)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree
import Solcore.Primitives.Primitives hiding (pairTy)
import Language.Yul
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (CompUnit)
	| HappyAbsSyn5 ([Import])
	| HappyAbsSyn6 (Import)
	| HappyAbsSyn7 ([TopDecl])
	| HappyAbsSyn8 (TopDecl)
	| HappyAbsSyn9 (Pragma)
	| HappyAbsSyn10 (PragmaStatus)
	| HappyAbsSyn11 (NonEmpty Name)
	| HappyAbsSyn12 (Contract)
	| HappyAbsSyn13 ([ContractDecl])
	| HappyAbsSyn14 (ContractDecl)
	| HappyAbsSyn15 (TySym)
	| HappyAbsSyn16 (Field)
	| HappyAbsSyn17 (DataTy)
	| HappyAbsSyn18 ([Constr])
	| HappyAbsSyn20 (Constr)
	| HappyAbsSyn21 (Class)
	| HappyAbsSyn22 ([Signature])
	| HappyAbsSyn23 ([Ty])
	| HappyAbsSyn25 ([Pred])
	| HappyAbsSyn26 (Pred)
	| HappyAbsSyn27 ([Signature ])
	| HappyAbsSyn28 (Signature)
	| HappyAbsSyn29 (([Ty], [Pred]))
	| HappyAbsSyn31 ([Param])
	| HappyAbsSyn32 (Param)
	| HappyAbsSyn33 (Instance)
	| HappyAbsSyn37 ([FunDef])
	| HappyAbsSyn39 (FunDef)
	| HappyAbsSyn40 (Maybe Ty)
	| HappyAbsSyn41 (Constructor)
	| HappyAbsSyn42 ([Stmt])
	| HappyAbsSyn44 (Stmt)
	| HappyAbsSyn45 ([Exp])
	| HappyAbsSyn46 (Maybe Exp)
	| HappyAbsSyn47 (Exp)
	| HappyAbsSyn51 ([([Pat], [Stmt])])
	| HappyAbsSyn52 (([Pat], [Stmt]))
	| HappyAbsSyn53 ([Pat])
	| HappyAbsSyn54 (Pat)
	| HappyAbsSyn57 (Literal)
	| HappyAbsSyn58 (Ty)
	| HappyAbsSyn60 (([Ty], Ty))
	| HappyAbsSyn62 (Name)
	| HappyAbsSyn63 (YulBlock)
	| HappyAbsSyn65 ([YulStmt])
	| HappyAbsSyn66 (YulStmt)
	| HappyAbsSyn69 (YulCases)
	| HappyAbsSyn70 ((YLiteral, YulBlock))
	| HappyAbsSyn71 (Maybe YulBlock)
	| HappyAbsSyn74 (Maybe YulExp)
	| HappyAbsSyn76 ([Name])
	| HappyAbsSyn77 (YulExp)
	| HappyAbsSyn78 ([YulExp])
	| HappyAbsSyn80 (YLiteral)
	| HappyAbsSyn81 (())

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,647) ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,32776,544,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,132,8712,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,24,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,57344,0,0,0,0,0,32768,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,8192,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,64,0,0,0,0,0,16,0,0,0,0,0,0,1024,2,0,0,0,0,0,0,16384,0,64,0,0,0,0,0,2048,0,1024,0,0,0,0,0,2,0,0,0,0,0,0,16384,0,0,16,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1248,25856,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,28672,32770,50,4,0,0,0,0,32768,1,1280,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,14,1024,128,0,0,0,0,49152,1,128,16,0,0,0,0,0,0,0,2,0,0,0,0,1792,0,16386,0,0,0,0,0,4096,0,2048,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,32768,0,0,0,0,0,64,0,4096,0,0,0,0,0,0,0,16,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,32,0,0,0,0,1024,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,2,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,2,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,1,0,0,0,0,0,0,64,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,16,0,0,0,0,0,0,0,4,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,1028,576,0,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,512,0,0,0,0,0,1,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,2,6144,2,0,0,0,0,0,0,16384,0,0,0,0,0,16,0,0,0,0,0,0,0,256,0,5,0,0,0,0,0,0,0,64,0,0,0,0,0,4,12288,0,0,0,0,0,0,0,0,0,0,0,0,0,1248,231,8192,0,0,0,0,0,768,0,8,0,0,0,0,0,0,0,0,0,0,0,0,28672,0,32,4,0,0,0,0,3584,0,32772,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,1024,0,0,0,0,0,256,0,5,0,0,0,0,0,32,32768,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,128,0,0,0,0,0,224,16384,2048,0,0,0,0,0,4,0,256,0,0,0,0,0,64,0,34,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,28,0,0,0,0,0,0,0,0,0,128,0,0,0,0,28672,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,512,64,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,64,0,0,0,0,0,256,0,4,0,0,0,0,0,0,0,0,0,0,0,0,14336,0,16,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,512,0,256,0,0,0,0,0,0,0,64,0,0,0,0,4096,0,0,4,0,0,0,0,0,0,4096,0,0,0,0,0,8192,0,4096,0,0,0,0,0,0,0,4096,0,0,0,0,0,257,144,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,32768,0,0,0,0,0,0,0,0,2,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,2,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1024,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,2048,0,0,2,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,32768,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,8,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,1024,0,0,0,0,0,2048,0,96,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,16,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,4096,0,0,0,0,32768,3,0,40,0,0,0,0,0,0,0,0,0,0,0,0,0,1,32768,0,0,0,0,0,0,16,0,0,0,0,0,0,56,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,16,32768,0,0,0,0,0,0,0,1024,0,0,0,0,0,896,0,0,0,0,0,0,0,33392,115,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,224,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,64,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,448,32768,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,2048,0,0,0,0,0,0,0,128,0,0,0,0,0,0,384,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,14,0,160,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,32,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,256,0,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,4,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,16,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,112,0,1280,0,0,0,0,0,14,0,160,0,0,0,0,49152,9,202,16,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,448,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,1792,0,20480,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","CompilationUnit","ImportList","Import","TopDeclList","TopDecl","Pragma","Status","NameList","Contract","DeclList","Decl","TypeSynonym","FieldDef","DataDef","DataCons","Constrs","Constr","ClassDef","ClassBody","OptParam","VarCommaList","ConstraintList","Constraint","Signatures","Signature","SigPrefix","Context","ParamList","Param","InstDef","OptTypeParam","TypeCommaList","Tyvars","Functions","InstBody","Function","OptRetTy","Constructor","Body","StmtList","Stmt","MatchArgList","InitOpt","Expr","TupleArgs","FunArgs","ExprCommaList","Equations","Equation","PatCommaList","Pattern","PatternList","PatList","Literal","Type","TupleTy","LamType","Var","Name","AsmBlock","YulBlock","YulStmts","YulStmt","YulFor","YulSwitch","YulCases","YulCase","YulDefault","YulIf","YulVarDecl","YulOptAss","YulAssignment","IdentifierList","YulExp","YulFunArgs","YulExpCommaList","YulLiteral","OptSemi","identifier","number","stringlit","'contract'","'import'","'let'","'='","'.'","'forall'","'class'","'instance'","'if'","'for'","'switch'","'case'","'default'","'leave'","'continue'","'break'","'assembly'","'data'","'match'","'function'","'constructor'","'return'","'lam'","'type'","'no-patterson-condition'","'no-coverage-condition'","'no-bounded-variable-condition'","'pragma'","';'","':='","':'","','","'->'","'_'","'=>'","'('","')'","'{'","'}'","'|'","%eof"]
        bit_start = st Prelude.* 125
        bit_end = (st Prelude.+ 1) Prelude.* 125
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..124]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (85) = happyShift action_16
action_2 (86) = happyShift action_17
action_2 (90) = happyShift action_18
action_2 (102) = happyShift action_19
action_2 (108) = happyShift action_20
action_2 (112) = happyShift action_21
action_2 (125) = happyReduce_6
action_2 (6) = happyGoto action_4
action_2 (7) = happyGoto action_5
action_2 (8) = happyGoto action_6
action_2 (9) = happyGoto action_7
action_2 (12) = happyGoto action_8
action_2 (15) = happyGoto action_9
action_2 (17) = happyGoto action_10
action_2 (21) = happyGoto action_11
action_2 (28) = happyGoto action_12
action_2 (29) = happyGoto action_13
action_2 (33) = happyGoto action_14
action_2 (39) = happyGoto action_15
action_2 _ = happyReduce_49

action_3 (125) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_2

action_5 _ = happyReduce_1

action_6 (85) = happyShift action_16
action_6 (90) = happyShift action_18
action_6 (102) = happyShift action_19
action_6 (108) = happyShift action_20
action_6 (112) = happyShift action_21
action_6 (125) = happyReduce_6
action_6 (7) = happyGoto action_37
action_6 (8) = happyGoto action_6
action_6 (9) = happyGoto action_7
action_6 (12) = happyGoto action_8
action_6 (15) = happyGoto action_9
action_6 (17) = happyGoto action_10
action_6 (21) = happyGoto action_11
action_6 (28) = happyGoto action_12
action_6 (29) = happyGoto action_13
action_6 (33) = happyGoto action_14
action_6 (39) = happyGoto action_15
action_6 _ = happyReduce_49

action_7 _ = happyReduce_13

action_8 _ = happyReduce_7

action_9 _ = happyReduce_12

action_10 _ = happyReduce_11

action_11 _ = happyReduce_9

action_12 (122) = happyShift action_36
action_12 (42) = happyGoto action_35
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (91) = happyShift action_32
action_13 (92) = happyShift action_33
action_13 (104) = happyShift action_34
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_10

action_15 _ = happyReduce_8

action_16 (82) = happyShift action_26
action_16 (62) = happyGoto action_31
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (82) = happyShift action_26
action_17 (62) = happyGoto action_30
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (82) = happyShift action_26
action_18 (89) = happyReduce_64
action_18 (36) = happyGoto action_28
action_18 (62) = happyGoto action_29
action_18 _ = happyReduce_64

action_19 (82) = happyShift action_26
action_19 (62) = happyGoto action_27
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (82) = happyShift action_26
action_20 (62) = happyGoto action_25
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (109) = happyShift action_22
action_21 (110) = happyShift action_23
action_21 (111) = happyShift action_24
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (82) = happyShift action_26
action_22 (10) = happyGoto action_72
action_22 (11) = happyGoto action_69
action_22 (62) = happyGoto action_70
action_22 _ = happyReduce_18

action_23 (82) = happyShift action_26
action_23 (10) = happyGoto action_71
action_23 (11) = happyGoto action_69
action_23 (62) = happyGoto action_70
action_23 _ = happyReduce_18

action_24 (82) = happyShift action_26
action_24 (10) = happyGoto action_68
action_24 (11) = happyGoto action_69
action_24 (62) = happyGoto action_70
action_24 _ = happyReduce_18

action_25 (89) = happyShift action_61
action_25 (120) = happyShift action_62
action_25 (23) = happyGoto action_67
action_25 _ = happyReduce_39

action_26 _ = happyReduce_124

action_27 (89) = happyShift action_61
action_27 (120) = happyShift action_62
action_27 (23) = happyGoto action_66
action_27 _ = happyReduce_39

action_28 (89) = happyShift action_65
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (82) = happyShift action_26
action_29 (89) = happyShift action_61
action_29 (36) = happyGoto action_64
action_29 (62) = happyGoto action_29
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (89) = happyShift action_61
action_30 (113) = happyShift action_63
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (89) = happyShift action_61
action_31 (120) = happyShift action_62
action_31 (23) = happyGoto action_60
action_31 _ = happyReduce_39

action_32 (82) = happyShift action_26
action_32 (61) = happyGoto action_58
action_32 (62) = happyGoto action_59
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (82) = happyShift action_26
action_33 (120) = happyShift action_57
action_33 (58) = happyGoto action_53
action_33 (59) = happyGoto action_54
action_33 (60) = happyGoto action_55
action_33 (62) = happyGoto action_56
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (82) = happyShift action_26
action_34 (62) = happyGoto action_52
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_68

action_36 (82) = happyShift action_26
action_36 (83) = happyShift action_44
action_36 (84) = happyShift action_45
action_36 (87) = happyShift action_46
action_36 (101) = happyShift action_47
action_36 (103) = happyShift action_48
action_36 (106) = happyShift action_49
action_36 (107) = happyShift action_50
action_36 (120) = happyShift action_51
action_36 (43) = happyGoto action_38
action_36 (44) = happyGoto action_39
action_36 (47) = happyGoto action_40
action_36 (57) = happyGoto action_41
action_36 (62) = happyGoto action_42
action_36 (63) = happyGoto action_43
action_36 _ = happyReduce_74

action_37 _ = happyReduce_5

action_38 (123) = happyShift action_109
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (82) = happyShift action_26
action_39 (83) = happyShift action_44
action_39 (84) = happyShift action_45
action_39 (87) = happyShift action_46
action_39 (101) = happyShift action_47
action_39 (103) = happyShift action_48
action_39 (106) = happyShift action_49
action_39 (107) = happyShift action_50
action_39 (120) = happyShift action_51
action_39 (43) = happyGoto action_108
action_39 (44) = happyGoto action_39
action_39 (47) = happyGoto action_40
action_39 (57) = happyGoto action_41
action_39 (62) = happyGoto action_42
action_39 (63) = happyGoto action_43
action_39 _ = happyReduce_74

action_40 (88) = happyShift action_104
action_40 (89) = happyShift action_105
action_40 (113) = happyShift action_106
action_40 (115) = happyShift action_107
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_87

action_42 (88) = happyReduce_90
action_42 (89) = happyShift action_61
action_42 (113) = happyReduce_90
action_42 (115) = happyReduce_90
action_42 (116) = happyReduce_90
action_42 (120) = happyShift action_103
action_42 (121) = happyReduce_90
action_42 (122) = happyReduce_90
action_42 (49) = happyGoto action_102
action_42 _ = happyReduce_90

action_43 _ = happyReduce_81

action_44 _ = happyReduce_116

action_45 _ = happyReduce_117

action_46 (82) = happyShift action_26
action_46 (62) = happyGoto action_101
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (122) = happyShift action_100
action_47 (64) = happyGoto action_99
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (82) = happyShift action_26
action_48 (83) = happyShift action_44
action_48 (84) = happyShift action_45
action_48 (107) = happyShift action_50
action_48 (120) = happyShift action_51
action_48 (45) = happyGoto action_97
action_48 (47) = happyGoto action_98
action_48 (57) = happyGoto action_41
action_48 (62) = happyGoto action_42
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (82) = happyShift action_26
action_49 (83) = happyShift action_44
action_49 (84) = happyShift action_45
action_49 (107) = happyShift action_50
action_49 (120) = happyShift action_51
action_49 (47) = happyGoto action_96
action_49 (57) = happyGoto action_41
action_49 (62) = happyGoto action_42
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (120) = happyShift action_95
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (82) = happyShift action_26
action_51 (83) = happyShift action_44
action_51 (84) = happyShift action_45
action_51 (107) = happyShift action_50
action_51 (120) = happyShift action_51
action_51 (47) = happyGoto action_93
action_51 (48) = happyGoto action_94
action_51 (57) = happyGoto action_41
action_51 (62) = happyGoto action_42
action_51 _ = happyReduce_97

action_52 (89) = happyShift action_61
action_52 (120) = happyShift action_92
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (115) = happyShift action_91
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_120

action_55 _ = happyReduce_119

action_56 (88) = happyReduce_59
action_56 (89) = happyShift action_61
action_56 (113) = happyReduce_59
action_56 (115) = happyReduce_59
action_56 (116) = happyReduce_59
action_56 (120) = happyShift action_90
action_56 (121) = happyReduce_59
action_56 (122) = happyReduce_59
action_56 (34) = happyGoto action_89
action_56 _ = happyReduce_59

action_57 (82) = happyShift action_26
action_57 (120) = happyShift action_57
action_57 (35) = happyGoto action_87
action_57 (58) = happyGoto action_88
action_57 (59) = happyGoto action_54
action_57 (60) = happyGoto action_55
action_57 (62) = happyGoto action_56
action_57 _ = happyReduce_62

action_58 (115) = happyShift action_86
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (89) = happyShift action_61
action_59 _ = happyReduce_123

action_60 (122) = happyShift action_85
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (82) = happyShift action_84
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (82) = happyShift action_26
action_62 (24) = happyGoto action_82
action_62 (61) = happyGoto action_83
action_62 (62) = happyGoto action_59
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_4

action_64 _ = happyReduce_63

action_65 (120) = happyShift action_81
action_65 (30) = happyGoto action_80
action_65 _ = happyReduce_51

action_66 (88) = happyShift action_79
action_66 (18) = happyGoto action_78
action_66 _ = happyReduce_32

action_67 (88) = happyShift action_77
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (113) = happyShift action_76
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_17

action_70 (89) = happyShift action_61
action_70 (116) = happyShift action_75
action_70 _ = happyReduce_20

action_71 (113) = happyShift action_74
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (113) = happyShift action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_15

action_74 _ = happyReduce_14

action_75 (82) = happyShift action_26
action_75 (11) = happyGoto action_173
action_75 (62) = happyGoto action_70
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_16

action_77 (82) = happyShift action_26
action_77 (120) = happyShift action_57
action_77 (58) = happyGoto action_172
action_77 (59) = happyGoto action_54
action_77 (60) = happyGoto action_55
action_77 (62) = happyGoto action_56
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (113) = happyShift action_171
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (82) = happyShift action_26
action_79 (19) = happyGoto action_168
action_79 (20) = happyGoto action_169
action_79 (62) = happyGoto action_170
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_48

action_81 (82) = happyShift action_26
action_81 (120) = happyShift action_57
action_81 (25) = happyGoto action_165
action_81 (26) = happyGoto action_166
action_81 (58) = happyGoto action_167
action_81 (59) = happyGoto action_54
action_81 (60) = happyGoto action_55
action_81 (62) = happyGoto action_56
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (121) = happyShift action_164
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (116) = happyShift action_163
action_83 _ = happyReduce_41

action_84 _ = happyReduce_125

action_85 (82) = happyShift action_26
action_85 (90) = happyShift action_18
action_85 (102) = happyShift action_19
action_85 (105) = happyShift action_162
action_85 (123) = happyReduce_23
action_85 (13) = happyGoto action_154
action_85 (14) = happyGoto action_155
action_85 (16) = happyGoto action_156
action_85 (17) = happyGoto action_157
action_85 (28) = happyGoto action_12
action_85 (29) = happyGoto action_158
action_85 (39) = happyGoto action_159
action_85 (41) = happyGoto action_160
action_85 (62) = happyGoto action_161
action_85 _ = happyReduce_49

action_86 (82) = happyShift action_26
action_86 (62) = happyGoto action_153
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (121) = happyShift action_152
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (116) = happyShift action_151
action_88 _ = happyReduce_61

action_89 _ = happyReduce_118

action_90 (82) = happyShift action_26
action_90 (120) = happyShift action_57
action_90 (35) = happyGoto action_150
action_90 (58) = happyGoto action_88
action_90 (59) = happyGoto action_54
action_90 (60) = happyGoto action_55
action_90 (62) = happyGoto action_56
action_90 _ = happyReduce_62

action_91 (82) = happyShift action_26
action_91 (62) = happyGoto action_149
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (82) = happyShift action_26
action_92 (31) = happyGoto action_148
action_92 (32) = happyGoto action_143
action_92 (62) = happyGoto action_144
action_92 _ = happyReduce_54

action_93 (89) = happyShift action_105
action_93 (115) = happyShift action_107
action_93 (116) = happyShift action_146
action_93 (121) = happyShift action_147
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (121) = happyShift action_145
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (82) = happyShift action_26
action_95 (31) = happyGoto action_142
action_95 (32) = happyGoto action_143
action_95 (62) = happyGoto action_144
action_95 _ = happyReduce_54

action_96 (89) = happyShift action_105
action_96 (113) = happyShift action_141
action_96 (115) = happyShift action_107
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (122) = happyShift action_140
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (89) = happyShift action_105
action_98 (115) = happyShift action_107
action_98 (116) = happyShift action_139
action_98 _ = happyReduce_82

action_99 _ = happyReduce_126

action_100 (82) = happyShift action_26
action_100 (83) = happyShift action_130
action_100 (84) = happyShift action_131
action_100 (87) = happyShift action_132
action_100 (93) = happyShift action_133
action_100 (94) = happyShift action_134
action_100 (95) = happyShift action_135
action_100 (98) = happyShift action_136
action_100 (99) = happyShift action_137
action_100 (100) = happyShift action_138
action_100 (122) = happyShift action_100
action_100 (62) = happyGoto action_118
action_100 (64) = happyGoto action_119
action_100 (65) = happyGoto action_120
action_100 (66) = happyGoto action_121
action_100 (67) = happyGoto action_122
action_100 (68) = happyGoto action_123
action_100 (72) = happyGoto action_124
action_100 (73) = happyGoto action_125
action_100 (75) = happyGoto action_126
action_100 (76) = happyGoto action_127
action_100 (77) = happyGoto action_128
action_100 (80) = happyGoto action_129
action_100 _ = happyReduce_129

action_101 (88) = happyShift action_116
action_101 (89) = happyShift action_61
action_101 (115) = happyShift action_117
action_101 (46) = happyGoto action_115
action_101 _ = happyReduce_84

action_102 _ = happyReduce_86

action_103 (82) = happyShift action_26
action_103 (83) = happyShift action_44
action_103 (84) = happyShift action_45
action_103 (107) = happyShift action_50
action_103 (120) = happyShift action_51
action_103 (47) = happyGoto action_113
action_103 (50) = happyGoto action_114
action_103 (57) = happyGoto action_41
action_103 (62) = happyGoto action_42
action_103 _ = happyReduce_100

action_104 (82) = happyShift action_26
action_104 (83) = happyShift action_44
action_104 (84) = happyShift action_45
action_104 (107) = happyShift action_50
action_104 (120) = happyShift action_51
action_104 (47) = happyGoto action_112
action_104 (57) = happyGoto action_41
action_104 (62) = happyGoto action_42
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (82) = happyShift action_26
action_105 (62) = happyGoto action_111
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_78

action_107 (82) = happyShift action_26
action_107 (120) = happyShift action_57
action_107 (58) = happyGoto action_110
action_107 (59) = happyGoto action_54
action_107 (60) = happyGoto action_55
action_107 (62) = happyGoto action_56
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_73

action_109 _ = happyReduce_72

action_110 _ = happyReduce_93

action_111 (88) = happyReduce_91
action_111 (89) = happyShift action_61
action_111 (113) = happyReduce_91
action_111 (115) = happyReduce_91
action_111 (116) = happyReduce_91
action_111 (120) = happyShift action_103
action_111 (121) = happyReduce_91
action_111 (122) = happyReduce_91
action_111 (49) = happyGoto action_219
action_111 _ = happyReduce_91

action_112 (89) = happyShift action_105
action_112 (113) = happyShift action_218
action_112 (115) = happyShift action_107
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (89) = happyShift action_105
action_113 (115) = happyShift action_107
action_113 (116) = happyShift action_217
action_113 _ = happyReduce_99

action_114 (121) = happyShift action_216
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (113) = happyShift action_215
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (82) = happyShift action_26
action_116 (83) = happyShift action_44
action_116 (84) = happyShift action_45
action_116 (107) = happyShift action_50
action_116 (120) = happyShift action_51
action_116 (47) = happyGoto action_214
action_116 (57) = happyGoto action_41
action_116 (62) = happyGoto action_42
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (82) = happyShift action_26
action_117 (120) = happyShift action_57
action_117 (58) = happyGoto action_213
action_117 (59) = happyGoto action_54
action_117 (60) = happyGoto action_55
action_117 (62) = happyGoto action_56
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (89) = happyShift action_61
action_118 (114) = happyReduce_152
action_118 (116) = happyShift action_211
action_118 (120) = happyShift action_212
action_118 (78) = happyGoto action_210
action_118 _ = happyReduce_155

action_119 _ = happyReduce_131

action_120 (123) = happyShift action_209
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (113) = happyShift action_208
action_121 (81) = happyGoto action_207
action_121 _ = happyReduce_164

action_122 _ = happyReduce_136

action_123 _ = happyReduce_135

action_124 _ = happyReduce_134

action_125 _ = happyReduce_132

action_126 _ = happyReduce_130

action_127 (114) = happyShift action_206
action_127 _ = happyFail (happyExpListPerState 127)

action_128 _ = happyReduce_133

action_129 _ = happyReduce_154

action_130 _ = happyReduce_161

action_131 _ = happyReduce_162

action_132 (82) = happyShift action_26
action_132 (62) = happyGoto action_204
action_132 (76) = happyGoto action_205
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (82) = happyShift action_26
action_133 (83) = happyShift action_130
action_133 (84) = happyShift action_131
action_133 (62) = happyGoto action_200
action_133 (77) = happyGoto action_203
action_133 (80) = happyGoto action_129
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (122) = happyShift action_100
action_134 (64) = happyGoto action_202
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (82) = happyShift action_26
action_135 (83) = happyShift action_130
action_135 (84) = happyShift action_131
action_135 (62) = happyGoto action_200
action_135 (77) = happyGoto action_201
action_135 (80) = happyGoto action_129
action_135 _ = happyFail (happyExpListPerState 135)

action_136 _ = happyReduce_139

action_137 _ = happyReduce_137

action_138 _ = happyReduce_138

action_139 (82) = happyShift action_26
action_139 (83) = happyShift action_44
action_139 (84) = happyShift action_45
action_139 (107) = happyShift action_50
action_139 (120) = happyShift action_51
action_139 (45) = happyGoto action_199
action_139 (47) = happyGoto action_98
action_139 (57) = happyGoto action_41
action_139 (62) = happyGoto action_42
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (124) = happyShift action_198
action_140 (51) = happyGoto action_196
action_140 (52) = happyGoto action_197
action_140 _ = happyReduce_103

action_141 _ = happyReduce_79

action_142 (121) = happyShift action_195
action_142 _ = happyFail (happyExpListPerState 142)

action_143 (116) = happyShift action_194
action_143 _ = happyReduce_52

action_144 (89) = happyShift action_61
action_144 (115) = happyShift action_193
action_144 _ = happyReduce_56

action_145 _ = happyReduce_94

action_146 (82) = happyShift action_26
action_146 (83) = happyShift action_44
action_146 (84) = happyShift action_45
action_146 (107) = happyShift action_50
action_146 (120) = happyShift action_51
action_146 (47) = happyGoto action_191
action_146 (48) = happyGoto action_192
action_146 (57) = happyGoto action_41
action_146 (62) = happyGoto action_42
action_146 _ = happyReduce_97

action_147 _ = happyReduce_88

action_148 (121) = happyShift action_190
action_148 _ = happyFail (happyExpListPerState 148)

action_149 (89) = happyShift action_61
action_149 (120) = happyShift action_90
action_149 (122) = happyReduce_59
action_149 (34) = happyGoto action_189
action_149 _ = happyReduce_59

action_150 (121) = happyShift action_188
action_150 _ = happyFail (happyExpListPerState 150)

action_151 (82) = happyShift action_26
action_151 (120) = happyShift action_57
action_151 (35) = happyGoto action_187
action_151 (58) = happyGoto action_88
action_151 (59) = happyGoto action_54
action_151 (60) = happyGoto action_55
action_151 (62) = happyGoto action_56
action_151 _ = happyReduce_62

action_152 (117) = happyShift action_186
action_152 _ = happyReduce_121

action_153 (89) = happyShift action_61
action_153 (120) = happyShift action_62
action_153 (23) = happyGoto action_185
action_153 _ = happyReduce_39

action_154 (123) = happyShift action_184
action_154 _ = happyFail (happyExpListPerState 154)

action_155 (82) = happyShift action_26
action_155 (90) = happyShift action_18
action_155 (102) = happyShift action_19
action_155 (105) = happyShift action_162
action_155 (123) = happyReduce_23
action_155 (13) = happyGoto action_183
action_155 (14) = happyGoto action_155
action_155 (16) = happyGoto action_156
action_155 (17) = happyGoto action_157
action_155 (28) = happyGoto action_12
action_155 (29) = happyGoto action_158
action_155 (39) = happyGoto action_159
action_155 (41) = happyGoto action_160
action_155 (62) = happyGoto action_161
action_155 _ = happyReduce_49

action_156 _ = happyReduce_24

action_157 _ = happyReduce_25

action_158 (104) = happyShift action_34
action_158 _ = happyFail (happyExpListPerState 158)

action_159 _ = happyReduce_26

action_160 _ = happyReduce_27

action_161 (89) = happyShift action_61
action_161 (115) = happyShift action_182
action_161 _ = happyFail (happyExpListPerState 161)

action_162 (120) = happyShift action_181
action_162 _ = happyFail (happyExpListPerState 162)

action_163 (82) = happyShift action_26
action_163 (24) = happyGoto action_180
action_163 (61) = happyGoto action_83
action_163 (62) = happyGoto action_59
action_163 _ = happyFail (happyExpListPerState 163)

action_164 _ = happyReduce_38

action_165 (121) = happyShift action_179
action_165 _ = happyFail (happyExpListPerState 165)

action_166 (116) = happyShift action_178
action_166 _ = happyReduce_43

action_167 (115) = happyShift action_177
action_167 _ = happyFail (happyExpListPerState 167)

action_168 _ = happyReduce_31

action_169 (124) = happyShift action_176
action_169 _ = happyReduce_34

action_170 (89) = happyShift action_61
action_170 (113) = happyReduce_59
action_170 (120) = happyShift action_90
action_170 (124) = happyReduce_59
action_170 (34) = happyGoto action_175
action_170 _ = happyReduce_59

action_171 _ = happyReduce_30

action_172 (113) = happyShift action_174
action_172 _ = happyFail (happyExpListPerState 172)

action_173 _ = happyReduce_19

action_174 _ = happyReduce_28

action_175 _ = happyReduce_35

action_176 (82) = happyShift action_26
action_176 (19) = happyGoto action_258
action_176 (20) = happyGoto action_169
action_176 (62) = happyGoto action_170
action_176 _ = happyFail (happyExpListPerState 176)

action_177 (82) = happyShift action_26
action_177 (62) = happyGoto action_257
action_177 _ = happyFail (happyExpListPerState 177)

action_178 (82) = happyShift action_26
action_178 (120) = happyShift action_57
action_178 (25) = happyGoto action_256
action_178 (26) = happyGoto action_166
action_178 (58) = happyGoto action_167
action_178 (59) = happyGoto action_54
action_178 (60) = happyGoto action_55
action_178 (62) = happyGoto action_56
action_178 _ = happyFail (happyExpListPerState 178)

action_179 (119) = happyShift action_255
action_179 _ = happyFail (happyExpListPerState 179)

action_180 _ = happyReduce_40

action_181 (82) = happyShift action_26
action_181 (31) = happyGoto action_254
action_181 (32) = happyGoto action_143
action_181 (62) = happyGoto action_144
action_181 _ = happyReduce_54

action_182 (82) = happyShift action_26
action_182 (120) = happyShift action_57
action_182 (58) = happyGoto action_253
action_182 (59) = happyGoto action_54
action_182 (60) = happyGoto action_55
action_182 (62) = happyGoto action_56
action_182 _ = happyFail (happyExpListPerState 182)

action_183 _ = happyReduce_22

action_184 _ = happyReduce_21

action_185 (122) = happyShift action_252
action_185 (22) = happyGoto action_251
action_185 _ = happyFail (happyExpListPerState 185)

action_186 (82) = happyShift action_26
action_186 (120) = happyShift action_57
action_186 (58) = happyGoto action_250
action_186 (59) = happyGoto action_54
action_186 (60) = happyGoto action_55
action_186 (62) = happyGoto action_56
action_186 _ = happyFail (happyExpListPerState 186)

action_187 _ = happyReduce_60

action_188 _ = happyReduce_58

action_189 (122) = happyShift action_249
action_189 (38) = happyGoto action_248
action_189 _ = happyFail (happyExpListPerState 189)

action_190 (117) = happyShift action_244
action_190 (40) = happyGoto action_247
action_190 _ = happyReduce_70

action_191 (89) = happyShift action_105
action_191 (115) = happyShift action_107
action_191 (116) = happyShift action_146
action_191 _ = happyReduce_95

action_192 _ = happyReduce_96

action_193 (82) = happyShift action_26
action_193 (120) = happyShift action_57
action_193 (58) = happyGoto action_246
action_193 (59) = happyGoto action_54
action_193 (60) = happyGoto action_55
action_193 (62) = happyGoto action_56
action_193 _ = happyFail (happyExpListPerState 193)

action_194 (82) = happyShift action_26
action_194 (31) = happyGoto action_245
action_194 (32) = happyGoto action_143
action_194 (62) = happyGoto action_144
action_194 _ = happyReduce_54

action_195 (117) = happyShift action_244
action_195 (40) = happyGoto action_243
action_195 _ = happyReduce_70

action_196 (123) = happyShift action_242
action_196 _ = happyFail (happyExpListPerState 196)

action_197 (124) = happyShift action_198
action_197 (51) = happyGoto action_241
action_197 (52) = happyGoto action_197
action_197 _ = happyReduce_103

action_198 (82) = happyShift action_26
action_198 (83) = happyShift action_44
action_198 (84) = happyShift action_45
action_198 (118) = happyShift action_239
action_198 (120) = happyShift action_240
action_198 (53) = happyGoto action_234
action_198 (54) = happyGoto action_235
action_198 (55) = happyGoto action_236
action_198 (57) = happyGoto action_237
action_198 (62) = happyGoto action_238
action_198 _ = happyReduce_113

action_199 _ = happyReduce_83

action_200 (89) = happyShift action_61
action_200 (120) = happyShift action_212
action_200 (78) = happyGoto action_210
action_200 _ = happyReduce_155

action_201 (96) = happyShift action_233
action_201 (69) = happyGoto action_231
action_201 (70) = happyGoto action_232
action_201 _ = happyReduce_143

action_202 (82) = happyShift action_26
action_202 (83) = happyShift action_130
action_202 (84) = happyShift action_131
action_202 (62) = happyGoto action_200
action_202 (77) = happyGoto action_230
action_202 (80) = happyGoto action_129
action_202 _ = happyFail (happyExpListPerState 202)

action_203 (122) = happyShift action_100
action_203 (64) = happyGoto action_229
action_203 _ = happyFail (happyExpListPerState 203)

action_204 (89) = happyShift action_61
action_204 (116) = happyShift action_211
action_204 _ = happyReduce_152

action_205 (114) = happyShift action_228
action_205 (74) = happyGoto action_227
action_205 _ = happyReduce_150

action_206 (82) = happyShift action_26
action_206 (83) = happyShift action_130
action_206 (84) = happyShift action_131
action_206 (62) = happyGoto action_200
action_206 (77) = happyGoto action_226
action_206 (80) = happyGoto action_129
action_206 _ = happyFail (happyExpListPerState 206)

action_207 (82) = happyShift action_26
action_207 (83) = happyShift action_130
action_207 (84) = happyShift action_131
action_207 (87) = happyShift action_132
action_207 (93) = happyShift action_133
action_207 (94) = happyShift action_134
action_207 (95) = happyShift action_135
action_207 (98) = happyShift action_136
action_207 (99) = happyShift action_137
action_207 (100) = happyShift action_138
action_207 (122) = happyShift action_100
action_207 (62) = happyGoto action_118
action_207 (64) = happyGoto action_119
action_207 (65) = happyGoto action_225
action_207 (66) = happyGoto action_121
action_207 (67) = happyGoto action_122
action_207 (68) = happyGoto action_123
action_207 (72) = happyGoto action_124
action_207 (73) = happyGoto action_125
action_207 (75) = happyGoto action_126
action_207 (76) = happyGoto action_127
action_207 (77) = happyGoto action_128
action_207 (80) = happyGoto action_129
action_207 _ = happyReduce_129

action_208 _ = happyReduce_163

action_209 _ = happyReduce_127

action_210 _ = happyReduce_156

action_211 (82) = happyShift action_26
action_211 (62) = happyGoto action_204
action_211 (76) = happyGoto action_224
action_211 _ = happyFail (happyExpListPerState 211)

action_212 (82) = happyShift action_26
action_212 (83) = happyShift action_130
action_212 (84) = happyShift action_131
action_212 (62) = happyGoto action_200
action_212 (77) = happyGoto action_222
action_212 (79) = happyGoto action_223
action_212 (80) = happyGoto action_129
action_212 _ = happyReduce_159

action_213 (88) = happyShift action_116
action_213 (46) = happyGoto action_221
action_213 _ = happyReduce_84

action_214 (89) = happyShift action_105
action_214 (115) = happyShift action_107
action_214 _ = happyReduce_85

action_215 _ = happyReduce_77

action_216 _ = happyReduce_98

action_217 (82) = happyShift action_26
action_217 (83) = happyShift action_44
action_217 (84) = happyShift action_45
action_217 (107) = happyShift action_50
action_217 (120) = happyShift action_51
action_217 (47) = happyGoto action_113
action_217 (50) = happyGoto action_220
action_217 (57) = happyGoto action_41
action_217 (62) = happyGoto action_42
action_217 _ = happyReduce_100

action_218 _ = happyReduce_75

action_219 _ = happyReduce_89

action_220 _ = happyReduce_101

action_221 (113) = happyShift action_282
action_221 _ = happyFail (happyExpListPerState 221)

action_222 (116) = happyShift action_281
action_222 _ = happyReduce_158

action_223 (121) = happyShift action_280
action_223 _ = happyFail (happyExpListPerState 223)

action_224 _ = happyReduce_153

action_225 _ = happyReduce_128

action_226 _ = happyReduce_151

action_227 _ = happyReduce_148

action_228 (82) = happyShift action_26
action_228 (83) = happyShift action_130
action_228 (84) = happyShift action_131
action_228 (62) = happyGoto action_200
action_228 (77) = happyGoto action_279
action_228 (80) = happyGoto action_129
action_228 _ = happyFail (happyExpListPerState 228)

action_229 _ = happyReduce_147

action_230 (122) = happyShift action_100
action_230 (64) = happyGoto action_278
action_230 _ = happyFail (happyExpListPerState 230)

action_231 (97) = happyShift action_277
action_231 (71) = happyGoto action_276
action_231 _ = happyReduce_146

action_232 (96) = happyShift action_233
action_232 (69) = happyGoto action_275
action_232 (70) = happyGoto action_232
action_232 _ = happyReduce_143

action_233 (83) = happyShift action_130
action_233 (84) = happyShift action_131
action_233 (80) = happyGoto action_274
action_233 _ = happyFail (happyExpListPerState 233)

action_234 (119) = happyShift action_273
action_234 _ = happyFail (happyExpListPerState 234)

action_235 (116) = happyShift action_272
action_235 _ = happyReduce_105

action_236 _ = happyReduce_111

action_237 _ = happyReduce_109

action_238 (89) = happyShift action_61
action_238 (120) = happyShift action_271
action_238 (55) = happyGoto action_270
action_238 _ = happyReduce_113

action_239 _ = happyReduce_108

action_240 (82) = happyShift action_26
action_240 (83) = happyShift action_44
action_240 (84) = happyShift action_45
action_240 (118) = happyShift action_239
action_240 (120) = happyShift action_240
action_240 (54) = happyGoto action_268
action_240 (55) = happyGoto action_236
action_240 (56) = happyGoto action_269
action_240 (57) = happyGoto action_237
action_240 (62) = happyGoto action_238
action_240 _ = happyReduce_113

action_241 _ = happyReduce_102

action_242 _ = happyReduce_80

action_243 (122) = happyShift action_36
action_243 (42) = happyGoto action_267
action_243 _ = happyFail (happyExpListPerState 243)

action_244 (82) = happyShift action_26
action_244 (120) = happyShift action_57
action_244 (58) = happyGoto action_266
action_244 (59) = happyGoto action_54
action_244 (60) = happyGoto action_55
action_244 (62) = happyGoto action_56
action_244 _ = happyFail (happyExpListPerState 244)

action_245 _ = happyReduce_53

action_246 _ = happyReduce_55

action_247 _ = happyReduce_47

action_248 _ = happyReduce_57

action_249 (90) = happyShift action_18
action_249 (123) = happyReduce_66
action_249 (28) = happyGoto action_12
action_249 (29) = happyGoto action_158
action_249 (37) = happyGoto action_264
action_249 (39) = happyGoto action_265
action_249 _ = happyReduce_49

action_250 _ = happyReduce_122

action_251 _ = happyReduce_36

action_252 (90) = happyShift action_18
action_252 (123) = happyReduce_46
action_252 (27) = happyGoto action_262
action_252 (28) = happyGoto action_263
action_252 (29) = happyGoto action_158
action_252 _ = happyReduce_49

action_253 (88) = happyShift action_116
action_253 (46) = happyGoto action_261
action_253 _ = happyReduce_84

action_254 (121) = happyShift action_260
action_254 _ = happyFail (happyExpListPerState 254)

action_255 _ = happyReduce_50

action_256 _ = happyReduce_42

action_257 (89) = happyShift action_61
action_257 (116) = happyReduce_59
action_257 (120) = happyShift action_90
action_257 (121) = happyReduce_59
action_257 (34) = happyGoto action_259
action_257 _ = happyReduce_59

action_258 _ = happyReduce_33

action_259 _ = happyReduce_44

action_260 (122) = happyShift action_36
action_260 (42) = happyGoto action_298
action_260 _ = happyFail (happyExpListPerState 260)

action_261 (113) = happyShift action_297
action_261 _ = happyFail (happyExpListPerState 261)

action_262 (123) = happyShift action_296
action_262 _ = happyFail (happyExpListPerState 262)

action_263 (113) = happyShift action_295
action_263 _ = happyFail (happyExpListPerState 263)

action_264 (123) = happyShift action_294
action_264 _ = happyFail (happyExpListPerState 264)

action_265 (90) = happyShift action_18
action_265 (123) = happyReduce_66
action_265 (28) = happyGoto action_12
action_265 (29) = happyGoto action_158
action_265 (37) = happyGoto action_293
action_265 (39) = happyGoto action_265
action_265 _ = happyReduce_49

action_266 _ = happyReduce_69

action_267 _ = happyReduce_92

action_268 (116) = happyShift action_291
action_268 (121) = happyShift action_292
action_268 _ = happyFail (happyExpListPerState 268)

action_269 (121) = happyShift action_290
action_269 _ = happyFail (happyExpListPerState 269)

action_270 _ = happyReduce_107

action_271 (82) = happyShift action_26
action_271 (83) = happyShift action_44
action_271 (84) = happyShift action_45
action_271 (118) = happyShift action_239
action_271 (120) = happyShift action_240
action_271 (54) = happyGoto action_289
action_271 (55) = happyGoto action_236
action_271 (56) = happyGoto action_269
action_271 (57) = happyGoto action_237
action_271 (62) = happyGoto action_238
action_271 _ = happyReduce_113

action_272 (82) = happyShift action_26
action_272 (83) = happyShift action_44
action_272 (84) = happyShift action_45
action_272 (118) = happyShift action_239
action_272 (120) = happyShift action_240
action_272 (53) = happyGoto action_288
action_272 (54) = happyGoto action_235
action_272 (55) = happyGoto action_236
action_272 (57) = happyGoto action_237
action_272 (62) = happyGoto action_238
action_272 _ = happyReduce_113

action_273 (82) = happyShift action_26
action_273 (83) = happyShift action_44
action_273 (84) = happyShift action_45
action_273 (87) = happyShift action_46
action_273 (101) = happyShift action_47
action_273 (103) = happyShift action_48
action_273 (106) = happyShift action_49
action_273 (107) = happyShift action_50
action_273 (120) = happyShift action_51
action_273 (43) = happyGoto action_287
action_273 (44) = happyGoto action_39
action_273 (47) = happyGoto action_40
action_273 (57) = happyGoto action_41
action_273 (62) = happyGoto action_42
action_273 (63) = happyGoto action_43
action_273 _ = happyReduce_74

action_274 (122) = happyShift action_100
action_274 (64) = happyGoto action_286
action_274 _ = happyFail (happyExpListPerState 274)

action_275 _ = happyReduce_142

action_276 _ = happyReduce_141

action_277 (122) = happyShift action_100
action_277 (64) = happyGoto action_285
action_277 _ = happyFail (happyExpListPerState 277)

action_278 (122) = happyShift action_100
action_278 (64) = happyGoto action_284
action_278 _ = happyFail (happyExpListPerState 278)

action_279 _ = happyReduce_149

action_280 _ = happyReduce_157

action_281 (82) = happyShift action_26
action_281 (83) = happyShift action_130
action_281 (84) = happyShift action_131
action_281 (62) = happyGoto action_200
action_281 (77) = happyGoto action_222
action_281 (79) = happyGoto action_283
action_281 (80) = happyGoto action_129
action_281 _ = happyReduce_159

action_282 _ = happyReduce_76

action_283 _ = happyReduce_160

action_284 _ = happyReduce_140

action_285 _ = happyReduce_145

action_286 _ = happyReduce_144

action_287 _ = happyReduce_104

action_288 _ = happyReduce_106

action_289 (116) = happyShift action_291
action_289 (121) = happyReduce_114
action_289 _ = happyReduce_114

action_290 _ = happyReduce_112

action_291 (82) = happyShift action_26
action_291 (83) = happyShift action_44
action_291 (84) = happyShift action_45
action_291 (118) = happyShift action_239
action_291 (120) = happyShift action_240
action_291 (54) = happyGoto action_289
action_291 (55) = happyGoto action_236
action_291 (56) = happyGoto action_300
action_291 (57) = happyGoto action_237
action_291 (62) = happyGoto action_238
action_291 _ = happyReduce_113

action_292 _ = happyReduce_110

action_293 _ = happyReduce_65

action_294 _ = happyReduce_67

action_295 (90) = happyShift action_18
action_295 (123) = happyReduce_46
action_295 (27) = happyGoto action_299
action_295 (28) = happyGoto action_263
action_295 (29) = happyGoto action_158
action_295 _ = happyReduce_49

action_296 _ = happyReduce_37

action_297 _ = happyReduce_29

action_298 _ = happyReduce_71

action_299 _ = happyReduce_45

action_300 _ = happyReduce_115

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (CompUnit happy_var_1 happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn62  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Import happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 ([]
	)

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn8
		 (TContr happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn8
		 (TFunDef happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn8
		 (TClassDef happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn8
		 (TInstDef happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn8
		 (TDataDef happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn8
		 (TSym happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (TPragmaDecl happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 9 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Pragma NoCoverageCondition happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 9 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Pragma NoPattersonCondition happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 9 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Pragma NoBoundVariableCondition happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (DisableFor happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  10 happyReduction_18
happyReduction_18  =  HappyAbsSyn10
		 (DisableAll
	)

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn11
		 (cons happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn11
		 (singleton happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 6 12 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn62  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Contract happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_2  13 happyReduction_22
happyReduction_22 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  13 happyReduction_23
happyReduction_23  =  HappyAbsSyn13
		 ([]
	)

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 (CFieldDecl happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  14 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn14
		 (CDataDecl happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn14
		 (CFunDecl happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn14
		 (CConstrDecl happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happyReduce 6 15 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn58  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn62  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (TySym happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 5 16 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn46  happy_var_4) `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn62  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (Field happy_var_1 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 5 17 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	(HappyAbsSyn62  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (DataTy happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_2  18 happyReduction_31
happyReduction_31 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  18 happyReduction_32
happyReduction_32  =  HappyAbsSyn18
		 ([]
	)

happyReduce_33 = happySpecReduce_3  19 happyReduction_33
happyReduction_33 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_2  20 happyReduction_35
happyReduction_35 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn20
		 (Constr happy_var_1 happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 7 21 happyReduction_36
happyReduction_36 ((HappyAbsSyn22  happy_var_7) `HappyStk`
	(HappyAbsSyn23  happy_var_6) `HappyStk`
	(HappyAbsSyn62  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Class (snd happy_var_1) happy_var_5 happy_var_6 happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_3  22 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  23 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0  23 happyReduction_39
happyReduction_39  =  HappyAbsSyn23
		 ([]
	)

happyReduce_40 = happySpecReduce_3  24 happyReduction_40
happyReduction_40 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  24 happyReduction_41
happyReduction_41 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  25 happyReduction_42
happyReduction_42 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1 : happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn25
		 ([happy_var_1]
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 26 happyReduction_44
happyReduction_44 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	(HappyAbsSyn62  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn58  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (InCls happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_3  27 happyReduction_45
happyReduction_45 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 : happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  27 happyReduction_46
happyReduction_46  =  HappyAbsSyn27
		 ([]
	)

happyReduce_47 = happyReduce 7 28 happyReduction_47
happyReduction_47 ((HappyAbsSyn40  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn62  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 (Signature (fst happy_var_1) (snd happy_var_1) happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 4 29 happyReduction_48
happyReduction_48 ((HappyAbsSyn25  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_0  29 happyReduction_49
happyReduction_49  =  HappyAbsSyn29
		 (([], [])
	)

happyReduce_50 = happyReduce 4 30 happyReduction_50
happyReduction_50 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (happy_var_2
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_0  30 happyReduction_51
happyReduction_51  =  HappyAbsSyn25
		 ([]
	)

happyReduce_52 = happySpecReduce_1  31 happyReduction_52
happyReduction_52 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  31 happyReduction_53
happyReduction_53 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1 : happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  31 happyReduction_54
happyReduction_54  =  HappyAbsSyn31
		 ([]
	)

happyReduce_55 = happySpecReduce_3  32 happyReduction_55
happyReduction_55 (HappyAbsSyn58  happy_var_3)
	_
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn32
		 (Typed happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  32 happyReduction_56
happyReduction_56 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn32
		 (Untyped happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happyReduce 7 33 happyReduction_57
happyReduction_57 ((HappyAbsSyn37  happy_var_7) `HappyStk`
	(HappyAbsSyn23  happy_var_6) `HappyStk`
	(HappyAbsSyn62  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn58  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (Instance (snd happy_var_1) happy_var_5 happy_var_6 happy_var_3 happy_var_7
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_3  34 happyReduction_58
happyReduction_58 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_0  34 happyReduction_59
happyReduction_59  =  HappyAbsSyn23
		 ([]
	)

happyReduce_60 = happySpecReduce_3  35 happyReduction_60
happyReduction_60 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1 : happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  35 happyReduction_61
happyReduction_61 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn23
		 ([happy_var_1]
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_0  35 happyReduction_62
happyReduction_62  =  HappyAbsSyn23
		 ([]
	)

happyReduce_63 = happySpecReduce_2  36 happyReduction_63
happyReduction_63 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn23
		 ((TyCon happy_var_1 []) : happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_0  36 happyReduction_64
happyReduction_64  =  HappyAbsSyn23
		 ([]
	)

happyReduce_65 = happySpecReduce_2  37 happyReduction_65
happyReduction_65 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 : happy_var_2
	)
happyReduction_65 _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_0  37 happyReduction_66
happyReduction_66  =  HappyAbsSyn37
		 ([]
	)

happyReduce_67 = happySpecReduce_3  38 happyReduction_67
happyReduction_67 _
	(HappyAbsSyn37  happy_var_2)
	_
	 =  HappyAbsSyn37
		 (happy_var_2
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_2  39 happyReduction_68
happyReduction_68 (HappyAbsSyn42  happy_var_2)
	(HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn39
		 (FunDef happy_var_1 happy_var_2
	)
happyReduction_68 _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  40 happyReduction_69
happyReduction_69 (HappyAbsSyn58  happy_var_2)
	_
	 =  HappyAbsSyn40
		 (Just happy_var_2
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  40 happyReduction_70
happyReduction_70  =  HappyAbsSyn40
		 (Nothing
	)

happyReduce_71 = happyReduce 5 41 happyReduction_71
happyReduction_71 ((HappyAbsSyn42  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn41
		 (Constructor happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_72 = happySpecReduce_3  42 happyReduction_72
happyReduction_72 _
	(HappyAbsSyn42  happy_var_2)
	_
	 =  HappyAbsSyn42
		 (happy_var_2
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_2  43 happyReduction_73
happyReduction_73 (HappyAbsSyn42  happy_var_2)
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn42
		 (happy_var_1 : happy_var_2
	)
happyReduction_73 _ _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_0  43 happyReduction_74
happyReduction_74  =  HappyAbsSyn42
		 ([]
	)

happyReduce_75 = happyReduce 4 44 happyReduction_75
happyReduction_75 (_ `HappyStk`
	(HappyAbsSyn47  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn44
		 (Assign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_76 = happyReduce 6 44 happyReduction_76
happyReduction_76 (_ `HappyStk`
	(HappyAbsSyn46  happy_var_5) `HappyStk`
	(HappyAbsSyn58  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn62  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn44
		 (Let happy_var_2 (Just happy_var_4) happy_var_5
	) `HappyStk` happyRest

happyReduce_77 = happyReduce 4 44 happyReduction_77
happyReduction_77 (_ `HappyStk`
	(HappyAbsSyn46  happy_var_3) `HappyStk`
	(HappyAbsSyn62  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn44
		 (Let happy_var_2 Nothing happy_var_3
	) `HappyStk` happyRest

happyReduce_78 = happySpecReduce_2  44 happyReduction_78
happyReduction_78 _
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn44
		 (StmtExp happy_var_1
	)
happyReduction_78 _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  44 happyReduction_79
happyReduction_79 _
	(HappyAbsSyn47  happy_var_2)
	_
	 =  HappyAbsSyn44
		 (Return happy_var_2
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happyReduce 5 44 happyReduction_80
happyReduction_80 (_ `HappyStk`
	(HappyAbsSyn51  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn45  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn44
		 (Match happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_81 = happySpecReduce_1  44 happyReduction_81
happyReduction_81 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn44
		 (Asm happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  45 happyReduction_82
happyReduction_82 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn45
		 ([happy_var_1]
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_3  45 happyReduction_83
happyReduction_83 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1 : happy_var_3
	)
happyReduction_83 _ _ _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_0  46 happyReduction_84
happyReduction_84  =  HappyAbsSyn46
		 (Nothing
	)

happyReduce_85 = happySpecReduce_2  46 happyReduction_85
happyReduction_85 (HappyAbsSyn47  happy_var_2)
	_
	 =  HappyAbsSyn46
		 (Just happy_var_2
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_2  47 happyReduction_86
happyReduction_86 (HappyAbsSyn45  happy_var_2)
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn47
		 (ExpName Nothing happy_var_1 happy_var_2
	)
happyReduction_86 _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  47 happyReduction_87
happyReduction_87 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn47
		 (Lit happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_3  47 happyReduction_88
happyReduction_88 _
	(HappyAbsSyn47  happy_var_2)
	_
	 =  HappyAbsSyn47
		 (happy_var_2
	)
happyReduction_88 _ _ _  = notHappyAtAll 

happyReduce_89 = happyReduce 4 47 happyReduction_89
happyReduction_89 ((HappyAbsSyn45  happy_var_4) `HappyStk`
	(HappyAbsSyn62  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn47  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 (ExpName (Just happy_var_1) happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_90 = happySpecReduce_1  47 happyReduction_90
happyReduction_90 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn47
		 (ExpVar Nothing happy_var_1
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_3  47 happyReduction_91
happyReduction_91 (HappyAbsSyn62  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (ExpVar (Just happy_var_1) happy_var_3
	)
happyReduction_91 _ _ _  = notHappyAtAll 

happyReduce_92 = happyReduce 6 47 happyReduction_92
happyReduction_92 ((HappyAbsSyn42  happy_var_6) `HappyStk`
	(HappyAbsSyn40  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn47
		 (Lam happy_var_3 happy_var_6 happy_var_5
	) `HappyStk` happyRest

happyReduce_93 = happySpecReduce_3  47 happyReduction_93
happyReduction_93 (HappyAbsSyn58  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (TyExp happy_var_1 happy_var_3
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_3  47 happyReduction_94
happyReduction_94 _
	(HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn47
		 (tupleExp happy_var_2
	)
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_3  48 happyReduction_95
happyReduction_95 (HappyAbsSyn47  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn45
		 ([happy_var_1, happy_var_3]
	)
happyReduction_95 _ _ _  = notHappyAtAll 

happyReduce_96 = happySpecReduce_3  48 happyReduction_96
happyReduction_96 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1 : happy_var_3
	)
happyReduction_96 _ _ _  = notHappyAtAll 

happyReduce_97 = happySpecReduce_0  48 happyReduction_97
happyReduction_97  =  HappyAbsSyn45
		 ([]
	)

happyReduce_98 = happySpecReduce_3  49 happyReduction_98
happyReduction_98 _
	(HappyAbsSyn45  happy_var_2)
	_
	 =  HappyAbsSyn45
		 (happy_var_2
	)
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_1  50 happyReduction_99
happyReduction_99 (HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn45
		 ([happy_var_1]
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_0  50 happyReduction_100
happyReduction_100  =  HappyAbsSyn45
		 ([]
	)

happyReduce_101 = happySpecReduce_3  50 happyReduction_101
happyReduction_101 (HappyAbsSyn45  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1 : happy_var_3
	)
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_2  51 happyReduction_102
happyReduction_102 (HappyAbsSyn51  happy_var_2)
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn51
		 (happy_var_1 : happy_var_2
	)
happyReduction_102 _ _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_0  51 happyReduction_103
happyReduction_103  =  HappyAbsSyn51
		 ([]
	)

happyReduce_104 = happyReduce 4 52 happyReduction_104
happyReduction_104 ((HappyAbsSyn42  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn53  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn52
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_105 = happySpecReduce_1  53 happyReduction_105
happyReduction_105 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 ([happy_var_1]
	)
happyReduction_105 _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_3  53 happyReduction_106
happyReduction_106 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_1 : happy_var_3
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_2  54 happyReduction_107
happyReduction_107 (HappyAbsSyn53  happy_var_2)
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn54
		 (Pat happy_var_1 happy_var_2
	)
happyReduction_107 _ _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  54 happyReduction_108
happyReduction_108 _
	 =  HappyAbsSyn54
		 (PWildcard
	)

happyReduce_109 = happySpecReduce_1  54 happyReduction_109
happyReduction_109 (HappyAbsSyn57  happy_var_1)
	 =  HappyAbsSyn54
		 (PLit happy_var_1
	)
happyReduction_109 _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_3  54 happyReduction_110
happyReduction_110 _
	(HappyAbsSyn54  happy_var_2)
	_
	 =  HappyAbsSyn54
		 (happy_var_2
	)
happyReduction_110 _ _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_1  54 happyReduction_111
happyReduction_111 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn54
		 (Pat (Name "pair") happy_var_1
	)
happyReduction_111 _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  55 happyReduction_112
happyReduction_112 _
	(HappyAbsSyn53  happy_var_2)
	_
	 =  HappyAbsSyn53
		 (happy_var_2
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_0  55 happyReduction_113
happyReduction_113  =  HappyAbsSyn53
		 ([]
	)

happyReduce_114 = happySpecReduce_1  56 happyReduction_114
happyReduction_114 (HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 ([happy_var_1]
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_3  56 happyReduction_115
happyReduction_115 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 (happy_var_1 : happy_var_3
	)
happyReduction_115 _ _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_1  57 happyReduction_116
happyReduction_116 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn57
		 (IntLit $ toInteger happy_var_1
	)
happyReduction_116 _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_1  57 happyReduction_117
happyReduction_117 (HappyTerminal (Token _ (TString happy_var_1)))
	 =  HappyAbsSyn57
		 (StrLit happy_var_1
	)
happyReduction_117 _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_2  58 happyReduction_118
happyReduction_118 (HappyAbsSyn23  happy_var_2)
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn58
		 (TyCon happy_var_1 happy_var_2
	)
happyReduction_118 _ _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_1  58 happyReduction_119
happyReduction_119 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn58
		 (uncurry funtype happy_var_1
	)
happyReduction_119 _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  58 happyReduction_120
happyReduction_120 (HappyAbsSyn58  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_3  59 happyReduction_121
happyReduction_121 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn58
		 (mkTupleTy happy_var_2
	)
happyReduction_121 _ _ _  = notHappyAtAll 

happyReduce_122 = happyReduce 5 60 happyReduction_122
happyReduction_122 ((HappyAbsSyn58  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 ((happy_var_2, happy_var_5)
	) `HappyStk` happyRest

happyReduce_123 = happySpecReduce_1  61 happyReduction_123
happyReduction_123 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn58
		 (TyCon happy_var_1 []
	)
happyReduction_123 _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_1  62 happyReduction_124
happyReduction_124 (HappyTerminal (Token _ (TIdent happy_var_1)))
	 =  HappyAbsSyn62
		 (Name happy_var_1
	)
happyReduction_124 _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_3  62 happyReduction_125
happyReduction_125 (HappyTerminal (Token _ (TIdent happy_var_3)))
	_
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn62
		 (QualName happy_var_1 happy_var_3
	)
happyReduction_125 _ _ _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_2  63 happyReduction_126
happyReduction_126 (HappyAbsSyn63  happy_var_2)
	_
	 =  HappyAbsSyn63
		 (happy_var_2
	)
happyReduction_126 _ _  = notHappyAtAll 

happyReduce_127 = happySpecReduce_3  64 happyReduction_127
happyReduction_127 _
	(HappyAbsSyn65  happy_var_2)
	_
	 =  HappyAbsSyn63
		 (happy_var_2
	)
happyReduction_127 _ _ _  = notHappyAtAll 

happyReduce_128 = happySpecReduce_3  65 happyReduction_128
happyReduction_128 (HappyAbsSyn65  happy_var_3)
	_
	(HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn65
		 (happy_var_1 : happy_var_3
	)
happyReduction_128 _ _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_0  65 happyReduction_129
happyReduction_129  =  HappyAbsSyn65
		 ([]
	)

happyReduce_130 = happySpecReduce_1  66 happyReduction_130
happyReduction_130 (HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn66
		 (happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  66 happyReduction_131
happyReduction_131 (HappyAbsSyn63  happy_var_1)
	 =  HappyAbsSyn66
		 (YBlock happy_var_1
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happySpecReduce_1  66 happyReduction_132
happyReduction_132 (HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn66
		 (happy_var_1
	)
happyReduction_132 _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_1  66 happyReduction_133
happyReduction_133 (HappyAbsSyn77  happy_var_1)
	 =  HappyAbsSyn66
		 (YExp happy_var_1
	)
happyReduction_133 _  = notHappyAtAll 

happyReduce_134 = happySpecReduce_1  66 happyReduction_134
happyReduction_134 (HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn66
		 (happy_var_1
	)
happyReduction_134 _  = notHappyAtAll 

happyReduce_135 = happySpecReduce_1  66 happyReduction_135
happyReduction_135 (HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn66
		 (happy_var_1
	)
happyReduction_135 _  = notHappyAtAll 

happyReduce_136 = happySpecReduce_1  66 happyReduction_136
happyReduction_136 (HappyAbsSyn66  happy_var_1)
	 =  HappyAbsSyn66
		 (happy_var_1
	)
happyReduction_136 _  = notHappyAtAll 

happyReduce_137 = happySpecReduce_1  66 happyReduction_137
happyReduction_137 _
	 =  HappyAbsSyn66
		 (YContinue
	)

happyReduce_138 = happySpecReduce_1  66 happyReduction_138
happyReduction_138 _
	 =  HappyAbsSyn66
		 (YBreak
	)

happyReduce_139 = happySpecReduce_1  66 happyReduction_139
happyReduction_139 _
	 =  HappyAbsSyn66
		 (YLeave
	)

happyReduce_140 = happyReduce 5 67 happyReduction_140
happyReduction_140 ((HappyAbsSyn63  happy_var_5) `HappyStk`
	(HappyAbsSyn63  happy_var_4) `HappyStk`
	(HappyAbsSyn77  happy_var_3) `HappyStk`
	(HappyAbsSyn63  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn66
		 (YFor happy_var_2 happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_141 = happyReduce 4 68 happyReduction_141
happyReduction_141 ((HappyAbsSyn71  happy_var_4) `HappyStk`
	(HappyAbsSyn69  happy_var_3) `HappyStk`
	(HappyAbsSyn77  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn66
		 (YSwitch happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_142 = happySpecReduce_2  69 happyReduction_142
happyReduction_142 (HappyAbsSyn69  happy_var_2)
	(HappyAbsSyn70  happy_var_1)
	 =  HappyAbsSyn69
		 (happy_var_1 : happy_var_2
	)
happyReduction_142 _ _  = notHappyAtAll 

happyReduce_143 = happySpecReduce_0  69 happyReduction_143
happyReduction_143  =  HappyAbsSyn69
		 ([]
	)

happyReduce_144 = happySpecReduce_3  70 happyReduction_144
happyReduction_144 (HappyAbsSyn63  happy_var_3)
	(HappyAbsSyn80  happy_var_2)
	_
	 =  HappyAbsSyn70
		 ((happy_var_2, happy_var_3)
	)
happyReduction_144 _ _ _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_2  71 happyReduction_145
happyReduction_145 (HappyAbsSyn63  happy_var_2)
	_
	 =  HappyAbsSyn71
		 (Just happy_var_2
	)
happyReduction_145 _ _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_0  71 happyReduction_146
happyReduction_146  =  HappyAbsSyn71
		 (Nothing
	)

happyReduce_147 = happySpecReduce_3  72 happyReduction_147
happyReduction_147 (HappyAbsSyn63  happy_var_3)
	(HappyAbsSyn77  happy_var_2)
	_
	 =  HappyAbsSyn66
		 (YIf happy_var_2 happy_var_3
	)
happyReduction_147 _ _ _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_3  73 happyReduction_148
happyReduction_148 (HappyAbsSyn74  happy_var_3)
	(HappyAbsSyn76  happy_var_2)
	_
	 =  HappyAbsSyn66
		 (YLet happy_var_2 happy_var_3
	)
happyReduction_148 _ _ _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_2  74 happyReduction_149
happyReduction_149 (HappyAbsSyn77  happy_var_2)
	_
	 =  HappyAbsSyn74
		 (Just happy_var_2
	)
happyReduction_149 _ _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_0  74 happyReduction_150
happyReduction_150  =  HappyAbsSyn74
		 (Nothing
	)

happyReduce_151 = happySpecReduce_3  75 happyReduction_151
happyReduction_151 (HappyAbsSyn77  happy_var_3)
	_
	(HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn66
		 (YAssign happy_var_1 happy_var_3
	)
happyReduction_151 _ _ _  = notHappyAtAll 

happyReduce_152 = happySpecReduce_1  76 happyReduction_152
happyReduction_152 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn76
		 ([happy_var_1]
	)
happyReduction_152 _  = notHappyAtAll 

happyReduce_153 = happySpecReduce_3  76 happyReduction_153
happyReduction_153 (HappyAbsSyn76  happy_var_3)
	_
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn76
		 (happy_var_1 : happy_var_3
	)
happyReduction_153 _ _ _  = notHappyAtAll 

happyReduce_154 = happySpecReduce_1  77 happyReduction_154
happyReduction_154 (HappyAbsSyn80  happy_var_1)
	 =  HappyAbsSyn77
		 (YLit happy_var_1
	)
happyReduction_154 _  = notHappyAtAll 

happyReduce_155 = happySpecReduce_1  77 happyReduction_155
happyReduction_155 (HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn77
		 (YIdent happy_var_1
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_2  77 happyReduction_156
happyReduction_156 (HappyAbsSyn78  happy_var_2)
	(HappyAbsSyn62  happy_var_1)
	 =  HappyAbsSyn77
		 (YCall happy_var_1 happy_var_2
	)
happyReduction_156 _ _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_3  78 happyReduction_157
happyReduction_157 _
	(HappyAbsSyn78  happy_var_2)
	_
	 =  HappyAbsSyn78
		 (happy_var_2
	)
happyReduction_157 _ _ _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_1  79 happyReduction_158
happyReduction_158 (HappyAbsSyn77  happy_var_1)
	 =  HappyAbsSyn78
		 ([happy_var_1]
	)
happyReduction_158 _  = notHappyAtAll 

happyReduce_159 = happySpecReduce_0  79 happyReduction_159
happyReduction_159  =  HappyAbsSyn78
		 ([]
	)

happyReduce_160 = happySpecReduce_3  79 happyReduction_160
happyReduction_160 (HappyAbsSyn78  happy_var_3)
	_
	(HappyAbsSyn77  happy_var_1)
	 =  HappyAbsSyn78
		 (happy_var_1 : happy_var_3
	)
happyReduction_160 _ _ _  = notHappyAtAll 

happyReduce_161 = happySpecReduce_1  80 happyReduction_161
happyReduction_161 (HappyTerminal (Token _ (TNumber happy_var_1)))
	 =  HappyAbsSyn80
		 (YulNumber $ toInteger happy_var_1
	)
happyReduction_161 _  = notHappyAtAll 

happyReduce_162 = happySpecReduce_1  80 happyReduction_162
happyReduction_162 (HappyTerminal (Token _ (TString happy_var_1)))
	 =  HappyAbsSyn80
		 (YulString happy_var_1
	)
happyReduction_162 _  = notHappyAtAll 

happyReduce_163 = happySpecReduce_1  81 happyReduction_163
happyReduction_163 _
	 =  HappyAbsSyn81
		 (()
	)

happyReduce_164 = happySpecReduce_0  81 happyReduction_164
happyReduction_164  =  HappyAbsSyn81
		 (()
	)

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TEOF -> action 125 125 tk (HappyState action) sts stk;
	Token _ (TIdent happy_dollar_dollar) -> cont 82;
	Token _ (TNumber happy_dollar_dollar) -> cont 83;
	Token _ (TString happy_dollar_dollar) -> cont 84;
	Token _ TContract -> cont 85;
	Token _ TImport -> cont 86;
	Token _ TLet -> cont 87;
	Token _ TEq -> cont 88;
	Token _ TDot -> cont 89;
	Token _ TForall -> cont 90;
	Token _ TClass -> cont 91;
	Token _ TInstance -> cont 92;
	Token _ TIf -> cont 93;
	Token _ TFor -> cont 94;
	Token _ TSwitch -> cont 95;
	Token _ TCase -> cont 96;
	Token _ TDefault -> cont 97;
	Token _ TLeave -> cont 98;
	Token _ TContinue -> cont 99;
	Token _ TBreak -> cont 100;
	Token _ TAssembly -> cont 101;
	Token _ TData -> cont 102;
	Token _ TMatch -> cont 103;
	Token _ TFunction -> cont 104;
	Token _ TConstructor -> cont 105;
	Token _ TReturn -> cont 106;
	Token _ TLam -> cont 107;
	Token _ TType -> cont 108;
	Token _ TNoPattersonCondition -> cont 109;
	Token _ TNoCoverageCondition -> cont 110;
	Token _ TNoBoundVariableCondition -> cont 111;
	Token _ TPragma -> cont 112;
	Token _ TSemi -> cont 113;
	Token _ TYAssign -> cont 114;
	Token _ TColon -> cont 115;
	Token _ TComma -> cont 116;
	Token _ TArrow -> cont 117;
	Token _ TWildCard -> cont 118;
	Token _ TDArrow -> cont 119;
	Token _ TLParen -> cont 120;
	Token _ TRParen -> cont 121;
	Token _ TLBrace -> cont 122;
	Token _ TRBrace -> cont 123;
	Token _ TBar -> cont 124;
	_ -> happyError' (tk, [])
	})

happyError_ explist 125 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = ((>>=))
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = (\(tokens, _) -> parseError tokens) tk
parser = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


unitPCon :: Pat 
unitPCon = Pat (Name "()") []

mkTupleTy :: [Ty] -> Ty 
mkTupleTy [] = TyCon (Name "()") []
mkTupleTy ts = foldr1 pairTy ts 

pairExp :: Exp -> Exp -> Exp 
pairExp e1 e2 = ExpName Nothing (Name "pair") [e1, e2]

tupleExp :: [Exp] -> Exp
tupleExp [] = ExpName Nothing (Name "()") []
tupleExp [t1] = t1
tupleExp [t1, t2] = pairExp t1 t2 
tupleExp (t1 : ts) = pairExp t1 (tupleExp ts)

parseError (Token (line, col) lexeme)
  = alexError $ "Parse error while processing lexeme: " ++ show lexeme
                ++ "\n at line " ++ show line ++ ", column " ++ show col

lexer :: (Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
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
