fib = 0 : 1 : zipWith (+) fib (tail fib)
main = print (take 100 fib)
