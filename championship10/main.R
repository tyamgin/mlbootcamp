set.seed(32121)

debugSource("algos.R")

XX = read.csv(file="x_train.csv", head=T, sep=";", na.strings="?")
YY = read.csv(file="y_train.csv", head=F, sep=";", na.strings="?")
XL = as.matrix(unname(cbind(data.matrix(XX), YY)))

sampleIdxes = sample(1:nrow(XL), 10000, T)
XK = XL[-sampleIdxes,]
XL = XL[sampleIdxes,]

#calssifier = ID3.treeClassifier(XL, predicates, 6)
calssifier = ID3.classifier(aggregator=randomForestTreeFloatAggregator, XL=XL, treesDepth=5, partsFactor=10000/10000, treesCount=1000, colsFactor=4/12)
#calssifier = ID3.classifier(aggregator=adaBoostAggregator, XL=XL, treesDepth=2, partsFactor=200/5000, treesCount=1000, colsFactor=3/12)

print(paste0('learn error = ', calculateError(XL, calssifier)))
print(paste0('control error = ', calculateError(XK, calssifier)))


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