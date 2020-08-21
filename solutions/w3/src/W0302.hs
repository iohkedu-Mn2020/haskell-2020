-- | W3.2 Stack Language Parser
module W0302
    ( Parser
    , program
    , parseProgramIO
    , runProgramIO
    , fact5'
    ) where

import           Control.Monad              (void)
import           Data.Void                  (Void)
import           Text.Megaparsec            hiding (token)
import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

import           W0301

type Parser a = Parsec Void String a

space :: Parser ()
space = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

token :: String -> Parser ()
token = void . L.symbol space

int :: Parser Int
int = L.signed space $ lexeme L.decimal

newtype Block = Block Instrs
    deriving Show

data Instrs = ISimple Simple Instrs | ICtrl Ctrl
    deriving Show

data Simple = SPush Int
            | SAdd
            | SMul
            | SDup
            | SSwap
            | SNeg
            | SPop
            | SOver
    deriving Show

data Ctrl = CIfZero Block Block
          | CLoop Block
          | CHalt
          | CRet
    deriving Show

block :: Parser Block
block = Block <$> (token "{" *> instrs <* token "}")

instrs :: Parser Instrs
instrs =     ISimple <$> (simple <* token ";") <*> instrs
         <|> ICtrl   <$> ctrl

simple :: Parser Simple
simple =     SPush <$> (token "push " *> int)
         <|> SAdd  <$  token "add"
         <|> SMul  <$  token "mul"
         <|> SDup  <$  token "dup"
         <|> SSwap <$  token "swap"
         <|> SNeg  <$  token "neg"
         <|> SPop  <$  token "pop"
         <|> SOver <$  token "over"

ctrl :: Parser Ctrl
ctrl =     CIfZero <$> (token "ifzero" *> block) <*> block
       <|> CLoop   <$> (token "loop"   *> block)
       <|> CHalt   <$   token "halt"
       <|> CRet    <$   token "ret"

fromBlock :: Block -> Instructions -> Instructions
fromBlock (Block i) = fromInstrs i

fromInstrs :: Instrs -> Instructions -> Instructions
fromInstrs (ISimple s i) = fromSimple s . fromInstrs i
fromInstrs (ICtrl c)     = fromCtrl c

fromSimple :: Simple -> Instructions -> Instructions
fromSimple (SPush n) cont = Push n cont
fromSimple SAdd      cont = Add    cont
fromSimple SMul      cont = Mul    cont
fromSimple SDup      cont = Dup    cont
fromSimple SSwap     cont = Swap   cont
fromSimple SNeg      cont = Neg    cont
fromSimple SPop      cont = Pop    cont
fromSimple SOver     cont = Over   cont

fromCtrl :: Ctrl -> Instructions -> Instructions
fromCtrl (CIfZero b b') ret = IfZero (fromBlock b ret) (fromBlock b' ret)
fromCtrl (CLoop b)      _   = Loop $ fromBlock b
fromCtrl CHalt          _   = Halt
fromCtrl CRet           ret = ret

-- | A parser for Stack programs.
--
-- >>> import Data.Either (isRight)
-- >>> run <$> parse program "" "{push +17; dup; halt}"
-- Right (Just [17,17])
-- >>> isRight $ parse program "" fact5'
-- True
--
program :: Parser Instructions
program = flip fromBlock Halt <$> (block <* eof)

-- |Parses a stack program.
--
-- >>> run <$> parseProgram "" "{push +17; dup; halt}"
-- Right (Just [17,17])
--
parseProgram :: String -- ^source file name (can be "")
             -> String -- ^source code
             -> Either (ParseErrorBundle String Void) Instructions
parseProgram = parse program

-- | Parses a Stack program from a file.
--
-- >>> import Data.Maybe (isJust)
-- >>> isJust <$> parseProgramIO "fact5.stack"
-- True
--
parseProgramIO :: FilePath -> IO (Maybe Instructions)
parseProgramIO fp = do
    s <- readFile fp
    case parseProgram fp s of
        Left bundle -> putStr (errorBundlePretty bundle) >> return Nothing
        Right p     -> return $ Just p

-- | Parses and runs a Stack program from a file.
--
-- >>> runProgramIO "fact5.stack"
-- [120]
--
runProgramIO :: FilePath -> IO ()
runProgramIO fp = do
    mp <- parseProgramIO fp
    case mp of
        Nothing -> return ()
        Just p  -> case run p of
            Nothing -> putStrLn "stack exhausted!"
            Just xs -> print xs

fact5' :: String
fact5' = "{push 5; push 1; swap; loop {dup; ifzero {pop; halt} {swap; over; mul; swap; push 1; neg; add; ret}}}"
