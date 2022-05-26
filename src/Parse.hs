module Parse where

import Text.Parsec

{-
Some phases we definitely want to parse:
*** Assembler:
*** C Compiler:
*** C pre-processor:
*** Called arity analysis [
*** Chasing dependencies:
*** Checking old interface for Lib (use -ddump-hi-diffs for more details):
*** Checking old interface for Main (use -ddump-hi-diffs for more details):
*** Checking old interface for Paths_ghc_build_analyzer (use -ddump-hi-diffs for more details):
*** CodeGen [
*** Common sub-expression [
*** Constructed Product Result analysis [
*** CorePrep [
*** CoreTidy [
*** CoreToStg [
*** Deleting temp dirs:
*** Deleting temp files:
*** Demand analysis [
*** Desugar [
*** Exitification transformation [
*** Float inwards [
*** Linker:
*** Parser [
*** Renamer/typechecker [
*** Simplifier [
*** Specialise [
*** Stg2Stg:
*** Worker Wrapper binds [
*** WriteIface [
*** initializing unit database:
*** otool:
*** systool:
-}

type ModuleName = String

data StructuredMessage = StartModulePass ModulePassType ModuleName
                       | EndModulePass ModulePassType ModuleName PassData

data PassData =
    PassData
        { duration :: ()
        , allocated :: ()
        }

-- data PassType = Assembler
    -- | CPreProcessor
    -- | ChasingDependencies
    -- | DeletingTempFiles
    -- | InitializingUnitDatabase
    -- | Systool String

data ModulePassType = 
                ByteCodeGen
              | CalledArityAnalysis
              | CheckingOldInterface
              | CodeGen
              | CommonSubExpression
              | ConstructedProductResultAnalysis
              | CorePrep
              | CoreTidy
              | CoreToStg
              | DemandAnalysis
              | Desugar
              | ExitificationTransformation
              | FloatInwards
              | FloatOut
              | ParserPass
              | RenamerTypechecker
              | Simplifier
              | Specialize
              | Stg2Stg
              | WorkerWrapperBinds
              | WriteIface

parseModulePassType :: (Monad m, Stream s m Char) => ParsecT s () m ModulePassType
parseModulePassType =
        (string "ByteCodeGen"                         >>= return . const ByteCodeGen)
    <|> (string "Called arity analysis"               >>= return . const CalledArityAnalysis)
    -- Weird format: "Checking old interface for MODULENAME (use -ddump-hi-diffs for more details):"
    <|> (string "Checking old interface"              >>= return . const CheckingOldInterface)
    <|> (string "CodeGen"                             >>= return . const CodeGen)
    <|> (string "Common sub-expression"               >>= return . const CommonSubExpression)
    <|> (string "Constructed Product Result analysis" >>= return . const ConstructedProductResultAnalysis)
    <|> (string "CorePrep"                            >>= return . const CorePrep)
    <|> (string "CoreTidy"                            >>= return . const CoreTidy)
    <|> (string "CoreToStg"                           >>= return . const CoreToStg)
    <|> (string "Demand analysis"                     >>= return . const DemandAnalysis)
    <|> (string "Desugar"                             >>= return . const Desugar)
    <|> (string "Exitification transformation"        >>= return . const ExitificationTransformation)
    <|> (string "Float inwards"                       >>= return . const FloatInwards)
    -- Weird format: "Float out(multiple lines of text) [MODULENAME]"
    --                        ^^ No space here!
    <|> (string "Float out"                           >>= return . const FloatOut)
    <|> (string "Parser"                              >>= return . const ParserPass)
    <|> (string "Renamer/typechecker"                 >>= return . const RenamerTypechecker)
    <|> (string "Simplifier"                          >>= return . const Simplifier)
    <|> (string "Specialise"                          >>= return . const Specialize)
    <|> (string "Worker Wrapper binds"                >>= return . const WorkerWrapperBinds)
    <|> (string "WriteIface"                          >>= return . const WriteIface)

