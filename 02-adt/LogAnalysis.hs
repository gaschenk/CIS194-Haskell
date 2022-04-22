{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import           Log

parseMessage :: String -> LogMessage
parseMessage s = case s of
  ('I' : _) -> LogMessage Info timestamp message
  ('W' : _) -> LogMessage Warning timestamp message
  ('E' : _) -> LogMessage (Error errorlevel) timestampError messageError
  _         -> Unknown s
 where
  errorlevel     = read (words s !! 1) :: Int
  timestamp      = read (words s !! 1) :: Int
  timestampError = read (words s !! 2) :: Int
  message        = unwords (drop 2 (words s))
  messageError   = unwords (drop 3 (words s))

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ timestamp _) (Node left message'@(LogMessage _ timestamp' _) right)
  | timestamp > timestamp'
  = Node left message' (insert message right)
  | otherwise
  = Node (insert message' left) message right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Node Leaf msg Leaf ) = [msg]
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right
inOrder _                     = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((LogMessage (Error errorlevel) _ msg) : xs) =
  if errorlevel >= 50 then msg : whatWentWrong xs else whatWentWrong xs
whatWentWrong (_:xs) = whatWentWrong xs
