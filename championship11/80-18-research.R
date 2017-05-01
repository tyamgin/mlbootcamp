f = function(a) {
  m = mean(a)
  d = max(abs(m))*0.2
  sum(m - d <= a & a <= m + d) > length(a)*0.8
}


sz = ncol(XX)
for(i in 1:(sz-1)) {
  for(j in (i+1):sz) {
    s = XX[, i] * XX[, j]
    #if (f(s)) {
    #  print(c(i, j))
    #}
    
    for(k in 1:sz) {
      if (k != i && k != j) {
        if (f(s - XX[, k])) {
          print(c(i, j, k))
        }
      }
    }
  }
}

"
d = diff(sort(XX[,80]))
sort(unique(round(d, 5)))

e = diff(sort(XX[,18]))
sort(unique(round(e, 5)))

my.norm = function (x) { (x-mean(x))/sd(x)  }

plot(density(my.norm(XX[,80])))
lines(density(my.norm(XX[,18])), col='red')
print('done')
"