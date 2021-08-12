import Data.Char

data Cmd =
  C_Input | C_Output
  | C_Left Int
  | C_Right Int
  | C_Plus Int
  | C_Minus Int


data BF = BF { buffer::[Int], ptr::Int}

execute :: BF -> Cmd -> IO BF
execute bf C_Input = do
  c <- getChar
  i <- return (ord c)
  let buf = buffer bf
  let p = ptr bf
  return BF {
    buffer = (take p buf) ++ [i] ++ (drop (p + 1) buf),
    ptr = p
    }
execute bf C_Output = do
  print (chr ((buffer bf)!!(ptr bf)))
  return bf
execute bf (C_Left n) = return BF {buffer = buffer bf, ptr = ptr bf - 1}
execute bf (C_Right n) = return BF {buffer = buffer bf, ptr = ptr bf + 1}
execute bf (C_Plus n) = return BF {
  buffer = (take p buf) ++ [ buf!!p + n] ++ (drop (p + 1) buf),
  ptr = p
  } where buf = buffer bf
          p = ptr bf
execute bf (C_Minus n) = return BF {
  buffer = (take p buf) ++ [ buf!!p - n] ++ (drop (p + 1) buf),
  ptr = p
  } where buf = buffer bf
          p = ptr bf


run :: BF -> [Cmd] -> IO BF
run bf [] = return bf
run bf (x:cmd) = do
  newbf <- execute bf x
  run newbf cmd

testBF = BF {buffer = take 50 [0,0..], ptr=0}
testCmd = [C_Plus 1, C_Output, C_Right 1, C_Plus 12, C_Output, C_Input, C_Plus 1, C_Output]

main :: IO ()
main = do
  print "Hello"
  run testBF testCmd
  return ()
