type Church a = (a -> a) -> a -> a

church :: Int -> Church Int
church 0 = \s -> \z -> z
church n = \s -> \z -> s (church (n-1) s z)

unchurch :: Church Int -> Int
unchurch ch = ch (+ 1) 0

add :: Church a -> Church a -> Church a
add ch1 ch2 = \s -> \z -> ch1 s (ch2 s z)

times :: Church a -> Church a -> Church a
times ch1 ch2 = \s -> \z -> ch1 (ch2 s) z

powe :: Church a -> Church a -> Church a
powe m n = \s -> \z -> (n m) s z


-- powe :: Church a -> Church a -> Church a
-- powe m n = n (times m)