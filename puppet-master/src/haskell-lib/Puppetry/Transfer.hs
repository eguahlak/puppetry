module Puppetry.Transfer where

import Puppetry.State
import Puppetry.Color

import GHC.IO.Handle

import Control.Monad
import Data.List

import Numeric


data StripName
  = StripBack
  | StripMiddle
  | StripFront
  | StripLeft
  | StripRight
  | StripProscenium
  deriving (Show, Eq, Ord)

stripAsChar :: StripName -> Char
stripAsChar s =
  case s of
    StripBack -> 'B'
    StripMiddle -> 'M'
    StripFront -> 'F'
    StripLeft -> 'L'
    StripRight -> 'R'
    StripProscenium -> 'P'

data Lamp = Lamp
  { strip :: StripName
  , index :: Int
  , color :: Color
  } deriving (Show, Eq)


transfer :: Handle -> State -> IO ()
transfer h sts = do
  forM_ (toLampList sts) $ \l ->  do
    hPutStr h $ lampToString l
    hPutStr h "\n"
  hPutStr h "!\n"
  hFlush h

readToBang :: Handle -> IO String
readToBang h = do
  c <- hGetChar h
  if c == '!' then
    return ""
  else do
    str <- readToBang h
    return (c : str)

toLampList :: State -> [ Lamp ]
toLampList s =
  (withName StripBack back)
  ++ (withName StripMiddle middle)
  ++ (withName StripFront front)
  ++ (withName StripLeft left)
  ++ (withName StripRight right)
  ++ (withName StripProscenium proscenium)
  where
    withName n f =
      map (\(i,c) -> Lamp n i c) (toList $ f s)


lampToString :: Lamp -> String
lampToString l =
  intercalate ""
  [ [stripAsChar (strip l)]
  , i2hx (index)
  , i2hx (red . color)
  , i2hx (blue . color)
  , i2hx (green . color)
  ]
  where
    i2hx f =
      exact 2 '0' $ showHex (f l) ""

exact :: Int -> a -> [a] -> [a]
exact i a as =
  fill $ take i (reverse as)
  where
    fill l
      | length l < i = fill (a:l)
      | otherwise = l
