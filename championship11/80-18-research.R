d = diff(sort(XX[,80]))
sort(unique(round(d, 5)))

e = diff(sort(XX[,18]))
sort(unique(round(e, 5)))

my.norm = function (x) { (x-mean(x))/sd(x)  }

plot(density(my.norm(XX[,80])))
lines(density(my.norm(XX[,18])), col='red')
print('done')