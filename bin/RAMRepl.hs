{-# LANGUAGE DataKinds #-}

import Prelude

import qualified Clash.Prelude as C
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
import System.Hardware.Serialport
import qualified Data.ByteString as BS
import Control.Monad
import Text.Read (readMaybe)

import RAM

parseInput :: String -> Maybe RamOp
parseInput input = case words input of
  [writeS, addrS, dinS] -> do
    guard $ writeS == "write"
    addr <- readMaybe addrS :: Maybe Int
    din <- readMaybe dinS :: Maybe Int
    return defaultRamOp
      { ramClockEnable = True
      , ramReadAddr = fromIntegral addr
      , ramWriteEnable = True
      , ramWriteAddr = fromIntegral addr
      , ramWriteData = fromIntegral din
      }
  [readS, addrS] -> do
    guard $ readS == "read"
    addr <- readMaybe addrS :: Maybe Int
    return defaultRamOp
      { ramClockEnable = True
      , ramReadAddr = fromIntegral addr
      }
  _ -> Nothing

ramOpToByteString :: RamOp -> BS.ByteString
ramOpToByteString op =
  let paddedOp = C.zeroExtend (C.pack op) :: C.BitVector 40
      bytesList = C.toList (C.bitCoerce paddedOp :: C.Vec 5 (C.BitVector 8))
  in BS.pack (map C.unpack bytesList)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just input -> case parseInput input of
          Just op -> do
            liftIO $ withSerial "/dev/ttyUSB0" (defaultSerialSettings { commSpeed = CS115200 }) $ \port -> do
              numBytes <- send port (ramOpToByteString op)
              outputBS <- recv port 1
              mapM_ print (BS.unpack outputBS)
            loop
            
          Nothing -> do
            outputStrLn $ "Usage: read <addr> OR write <addr> <din>"
            loop
