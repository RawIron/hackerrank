{-
There is a diamond hidden on an N*N grid on the square (xD,yD), where xD and yD are integers.
Find the diamond's position with as few guesses as possible.

Every guess, you suggest a pair of coordinates (xG,yG) which represent one of the squares on the grid.
If the diamond isn't there you are given a hint as to where to go to continue looking.
You are told either NW, N, NE, E, SE, S, SW, or W:

W  implies xD<xG and yD=yG
NW implies xD<xG and yD>yG
..

If you can get the diamond with 10 guesses or less (at most 9 wrong guesses and one right one), you get to keep the diamond.
What is the largest N for which you can guarantee success?

2^10-1 = 1023


1*1 = 1

2*2 1*1 = 2
3*3 1*1 = 2

4*4 2*2 = 3
5*5 2*2 = 3
6*6 3*3 = 3
7*7 3*3 = 3

8*8 4*4 = 4
9*9 4*4 = 4
10*10 5*5 = 4
11*11 5*5 = 4
12*12 6*6 = 4
13*13 6*6 = 4
14*14 7*7 = 4
15*15 7*7 = 4

16*16 8*8 = 5

3*3 grid, with the next guess you find the diamond
*--
-x-
---

5*5 grid
x* . --
-- . --
.. x ..
-- . --
-- . --

6*6 grid
*- . ---   -- * ---   -- . ---   
x- . ---   -- x ---   -- . -x*   
-- . ---   -- . ---   -- . ---   
.. x ...   .. x ...   .. x ...   
-- . ---   -- . ---   -- . ---   
-- . ---   -- . ---   -- . ---   

9*9 grid
x*-- . ----   
---- . ----   
--x- . ----   
---- . ----   
.... x ....  
---- . ----  
---- . ----   
---- . ----  
---- . ----  

-}
