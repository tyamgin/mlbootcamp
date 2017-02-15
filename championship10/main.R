set.seed(32121)

source("algos.R")

XX = read.csv(file="x_train.csv", head=T, sep=";", na.strings="?")
YY = read.csv(file="y_train.csv", head=F, sep=";", na.strings="?")
XL = as.matrix(unname(cbind(data.matrix(XX), YY)))

sampleIdxes = sample(1:nrow(XL), 5000, T)
XK = XL[-sampleIdxes,]
XL = XL[sampleIdxes,]

#calssifier = ID3.treeClassifier(XL, predicates, 6)
calssifier = ID3.classifier(aggregator=randomForestTreeAggregator, XL=XL, treesDepth=6, partsFactor=200/5000, treesCount=100)

print(paste0('learn error = ', calculateError(XL, calssifier) * 100, '%'))
print(paste0('control error = ', calculateError(XK, calssifier) * 100, '%'))


"
XXX = read.csv(file='x_test.csv', head=T, sep=';', na.strings='?')
XXX = data.matrix(XXX)
results = rep(0, nrow(XXX))
for (i in 1:nrow(XXX)) {
  x = XXX[i,]
  results[i] = calssifier(x)
}
write(results, file='res.txt', sep='\n')
"