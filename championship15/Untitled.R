library(foreach)
library(doParallel)

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)

my = {}

my$somefunc = function (i) i

make.adder = function (x) {
  function (y) {
    .GlobalEnv$my$somefunc(x) + .GlobalEnv$my$somefunc(y)
  }
}

main = function (adder) {
  print(foreach(i=1:4, .combine=c, .export=c('my')) %dopar% {
    adder(i)
  })
}

main(make.adder(10))

stopCluster(cl)