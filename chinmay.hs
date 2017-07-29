{-Skill #1-}

{- 0. Define _rules_ to make a fib sequence-}

fib 0 =1
fib 1 = 1
fib n = fib (n-1) + fib(n-2)


{- 1. Rules for factorial. Define the recursive base cases first -}

{- fact 0 = 1 -} {- Ofcourse fact 0 is not defined-}
fact 1 = 1
fact n = n * fact(n-1)

{- For factorial, haskell doesn't have to do the tree-}

{- 2. Defining product. -}

{- Starting with sum -}
_sum [] = 0
_sum (x:xs) = x + _sum xs


{- Same as product-}

{- _product [] = 0 -} {- ofcourse not!-}
{- _product [] = 1 -}
_product [x,y] = x*y
_product (x:xs) = x * _product xs



{- 3. Defining and. -}

{- Same as product-}
{- Base cases are complicated. Truth Table!!!-}
{- _and [True,True] = True
_and [True,False] = False
_and [False,True] = False
_and [False,False] = False -}

_and [x,y] = x && y
_and(x:xs) = x && _and xs

{- Base case for single argument is not considered. That's fine-}


{- 4. Defining zipWith. -}

{- OK. So multiple operators. Deal with them one by one-}

{-Move to other operators-}

 {-
_zipWith (*) [x] [y] = [x*y]
_zipWith (+) [x] [y] = [x+y]
_zipWith (-) [x] [y] = [x-y]
_zipWith (/) [x] [y] = [x/y]-}


_zipWith op [] [] = []
_zipWith op x [] = []
_zipWith op [] x = []
_zipWith op (x:xs) (y:ys) = op x y : _zipWith op xs ys


{- 5. Map -}

{-Same as above just invoking the function on a single variable-}

_map op [] = []
_map op (x:xs) = op x : _map op xs


{- 6. Filter -}

{- Using binary guards. Define like maths-}

_filter op [] = []
_filter op (x:xs) | op x = x: _filter op xs
		 | otherwise = _filter op xs


{- 7. Take -}

{- Same as filter but look for num to be greater than 0-}

_take num [] = []
_take num (x:xs) | (num > 0) = x: _take (num-1) xs
		   | otherwise = []




{- Skill #2-}

{- 1. -}

{-_fibs = _zipWith (+) [0..] [something you've calculated before]
-}


_fibs = 1:_zipWith (+) (0: _fibs) _fibs

{-Most important thing is to somehow define a base case here the 1: -}

{- 2. -}

_facts = 1:zipWith (*) [2..] _facts


isPrime n = and [n `mod` k /= 0 | k <- [2..n-1]]
primes = _filter isPrime [2..]
