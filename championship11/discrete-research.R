"
cols = c()
for(i in 1:223) {
  if ( length( unique(round(diff(sort(XX[,i])), 5)) ) < 60 ) {
    cols = c(cols, i)
  }
}
print(length(cols))
X = XX
colnames(X) <- paste0(1:223)

plot(density(my.norm(XX[,77])))
lines(density(my.norm(XX[,201])), col='red')

#png(filename='graph/name2.png', width=2000, height=2000)
corrgram(X[,cols], order=NULL, panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main='Correlogram')
#dev.off()

#########################################
"
my.norm = function (x) {
  (x - mean(x)) / sd(x)
}

my.checkGcd = function (arr, gcd, eps=1e-3) {
  arr = arr - min(arr)
  errStr = ''
  failedCnt = 0
  for (a in arr) {
    if (abs(round(a / gcd) - a / gcd) > eps) {
      failedStr = paste0('failed ', a, ' ', gcd)
      failedCnt = failedCnt + 1
    }
  }
  if (failedCnt == 0) {
    return(T)
  }
  print(failedStr)
  print(paste0(failedCnt, ' / ', length(arr)))
  return(F)
}

my.transformDiscreteFeature = function (arr, gcd) {
  "
  l = length(arr)
  ord = order(arr)
  res = rep(0, l)
  for(i in 2:l) {
    d = arr[ord[i]] - arr[ord[i - 1]]
    res[ord[i]] = res[ord[i - 1]] + round(d / gcd)
  }
  res
  "
  
  arr = arr - min(arr)
  res = round(arr / gcd)
  if (sum(res != round(arr / gcd, 1)) > 0) {
    stop('bad precision')
  }
  res
}


# 12  - 0.0033333333333333
# 77  - 0.028170
# 80  - 0.00166666666            ##########
# 97  - 0.0007666666666666       # погрешность на больших
# 98  - 0.062194
# 116 - 0.004132
# 132 - 0.067202
# 139 - 0.013974
# 157 - 0.001661
# 183 - 0.006024
# 201 - 0.008846

col = 97
"
plot(density(my.norm(my.transformDiscreteFeature(XX[,col], 0.013974))))
lines(density(my.norm(XX[,col])), col='red')
"

sort(unique(diff(sort( c(XX[,col], XXX[,col]) ))))
my.checkGcd( c(XX[,col], XXX[,col]) , 0.0007666666666666, eps=1e-2 )
