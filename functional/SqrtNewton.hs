module Main where
{-
 - the Newton algorithm in C
 -
public static int sqrt( int n ){
  int x = n;
  while( true ){
    int y = (x + n/x)/2;
    if( y >= x ) return x;
    x = y;
  }
}
-}


sqrtNewton :: Double -> Double
sqrtNewton n = go n n ((n+1)/2) where 
  go n x y
    | y >= x = x
    | y < x = go n y ((y + n/y) / 2)


main = do
  getContents >>=
    putStrLn . show . sqrtNewton . read
