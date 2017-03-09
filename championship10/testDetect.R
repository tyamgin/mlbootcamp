sll = function (cnt, pred, size = 1) {
  eps = 1e-15
  pred = min(max(pred, eps), 1 - eps)
  -log(1 - pred) * cnt / size
}

"
n = 10000
cn = 51
act = c(rep(0, n - 1), 1)
pred = c(rep(0, n - cn), rep(1/cn, cn))
print( error.logloss(act, pred) )


n = 10000
cn = 10
act = c(rep(0, n - 1), 1)
pred = c(rep(0, n - cn), rep(1/cn, cn))
print( error.logloss(act, pred) )
"
all0ll = 9.7413490
all1ll = 24.7980015
minDiff = 1e10
for (ksize in 10100:10300) {
  for (ones in 1:4000) {
    d = abs((all0ll - sll(ones, 1, ksize)) - (all1ll - sll(ksize - ones, 1, ksize)))
    if (d < minDiff) {
      minDiff = d
      selOnes = ones
      selSize = ksize
    }
  }
}
print(minDiff)
print(selOnes)
print(selSize)