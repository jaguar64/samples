
del test.o
del test.abs
del test.abs

rmac -fb -g test.s
rln -v -rq -o test.abs -a 4000 x x test.o
copy test.abs C:\000STUFF\ATARI\JAGUAR\0EMULATORS\virtualjaguar\software\test.abs

