data Bin = B | I Bin | O Bin deriving (Show, Eq)


toi' :: Bin -> (Int, Int)
toi' B = undefined
toi' (I B) = (1, 1)
toi' (O B) = (0, 1)
toi' (I x) = let (v, c) = toi' x in
               (2 ^ c + v, c + 1)
toi' (O x) = let (v, c) = toi' x in
               (v, c + 1)

toi :: Bin -> Int
toi = fst . toi'

cat :: Bin -> Bin -> Bin
cat B B = B
cat B x = x
cat (I x) y = I $ cat x y
cat (O x) y = O $ cat x y

rev :: Bin -> Bin
rev B = B
rev (I B) = I B
rev (O B) = O B
rev (I x) = cat (rev x) (I B)
rev (O x) = cat (rev x) (O B)

d::Bin
d =  I $ I $ O $ O $ I $ B

main :: IO ()
main = do
  putStrLn $ show d
  putStrLn $ show $ toi d
  putStrLn $ show $ rev d
  putStrLn $ show $ toi $ rev d
