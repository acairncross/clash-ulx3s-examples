{-# LANGUAGE DataKinds #-}

import Prelude

import Clash.Prelude (BitVector, pack, bitCoerce)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Bits
import Data.Word (Word16)
import System.Console.Haskeline
import System.Environment (getArgs)
import System.Hardware.Serialport
import qualified Data.ByteString as BS
import qualified Text.Parsec as Parsec

import StackMachine

-- Parser ---------------------------------------------------------------------

type Parser a = Parsec.Parsec String () a

data Expr
  = MulExpr Expr Expr
  | AddExpr Expr Expr
  | IntLitExpr Int
  deriving (Show, Eq)

spaces :: Parser ()
spaces = Parsec.skipMany (Parsec.char ' ')

parenthesized :: Parser a -> Parser a
parenthesized p = Parsec.char '(' *> spaces *> p <* Parsec.char ')' <* spaces

completeExpression :: Parser Expr
completeExpression = spaces *> expression <* Parsec.eof

intLit :: Parser Expr
intLit = do
  neg <- Parsec.optionMaybe (Parsec.char '-')
  digits <- Parsec.many1 Parsec.digit
  spaces
  case neg of
    Just _ -> return $ IntLitExpr (negate (read digits))
    Nothing -> return $ IntLitExpr (read digits)

expression = term `Parsec.chainl1` addop <* spaces
term = factor `Parsec.chainl1` mulop <* spaces
factor = Parsec.choice [ intLit <* spaces , parenthesized expression ]
addop = AddExpr <$ (Parsec.char '+' *> spaces)
mulop = MulExpr <$ (Parsec.char '*' *> spaces)

-- Code gen -------------------------------------------------------------------

compileCompleteExpr :: Expr -> [Instr]
compileCompleteExpr e = reverse $ Pop : compileExpr e
  where
    compileExpr :: Expr -> [Instr]
    compileExpr e = case e of
      MulExpr lhs rhs -> Mul : compileExpr rhs ++ compileExpr lhs
      AddExpr lhs rhs -> Add : compileExpr rhs ++ compileExpr lhs
      IntLitExpr n -> [Push $ fromIntegral n]

instrsToByteString :: [Instr] -> BS.ByteString
instrsToByteString instrs =
  BS.concat $ map (word16ToByteString . instrToWord16) instrs
  where
    instrToWord16 :: Instr -> Word16
    instrToWord16 = bitCoerce

    word16ToByteString :: Word16 -> BS.ByteString
    word16ToByteString w16 =
      -- Little endian
      BS.pack . map fromIntegral . reverse $ [w16 .&. 0xff, (w16 .&. 0xff00) `shiftR` 8]

-- {{{1
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> case Parsec.parse completeExpression "" input of
          Left err -> do
            outputStrLn "Parse error:"
            outputStrLn $ show err
            loop
          Right expr -> do
            -- outputStrLn $ "Parsed as:"
            -- outputStrLn $ show expr

            let instrs = compileCompleteExpr expr
            -- outputStrLn $ "Compiled to:"
            -- outputStrLn $ show instrs

            let instrsBS = instrsToByteString instrs
            -- let instrsBSs = map (BS.pack . (\x -> [x])) (BS.unpack instrsBS)
            -- liftIO $ mapM_ print (BS.unpack instrsBS)
            -- outputStrLn ""
            liftIO $ withSerial "/dev/ttyUSB0" (defaultSerialSettings { commSpeed = CS115200 }) $ \port -> do
              numBytes <- send port instrsBS
              -- putStrLn $ "Sent " <> show numBytes <> " bytes"
              -- numBytes <- sum <$> traverse (\bs -> send port bs >>= \n -> threadDelay 100 >> return n) instrsBSs
              -- threadDelay 1000
              outputBS <- recv port 1
              mapM_ print (BS.unpack outputBS)
            loop
-- }}}1
