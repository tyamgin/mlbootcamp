library(foreach)
library(doParallel)

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)

my = {}

my$somefunc = function (i) i

make.adder = function (x) {
  function (y) {
    get('my')$somefunc(x) + get('my')$somefunc(y)
  }
}

main = function (adder) {
  print(foreach(i=1:4, .combine=c, .export=c('my', 'make.adder', 'adder')) %dopar% {
    adder(i)
  })
}

main(make.adder(10))

stopCluster(cl)