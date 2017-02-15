set.seed(32121)

debugSource("algos.R")

XX = read.csv(file="crx_data_train_x.csv", head=F, sep=",", na.strings="?")
YY = read.csv(file="crx_data_train_y.csv", head=F, sep=",", na.strings="?")
XL = as.matrix(unname(cbind(data.matrix(XX), YY)))

# удаление строк с неполными данными
"
tmp = matrix(NA, 0, ncol(XL))
for (i in 1:nrow(XL)) {
  if (!any(is.na(XL[i,])))
    tmp = rbind(tmp, as.matrix(XL[i,]))
  else
    print(paste0('??? ', XL[i, ncol(XL)]))
}
XL = tmp #TODO
"


#testTree = function() {
predicates = c()
for(j in 1:(ncol(XL)-1)) {
  possibleValues = unique(sort(XL[,j]))
  
  for(k in 2:length(possibleValues)) {
    thr = (possibleValues[k] + possibleValues[k - 1]) / 2
    predicates = c(predicates, local({
      j <- j;
      thr <- thr;
      function(xx) {
        if (is.na(xx[j])) {
          return( NA )
        }
        return( xx[j] < thr )
      }
    }))
  }
}
predicates = predicates[sample(1:length(predicates))]
print(paste('Number of predicates: ', length(predicates)))

treeClassifier = ID3.classifier(adaBoostAggregator, XL, predicates, treesCount=400, treesDepth=2, partsFactor=0.05, predicatesFactor=0.3)
"
treeClassifier = ID3.treeClassifier(XL, predicates, 4)
"
print(paste0('error = ', calculateError(XL, treeClassifier) * 100, '%'))

XXX = read.csv(file='crx_data_test_x.csv', head=F, sep=',', na.strings='?')
XXX = data.matrix(XXX)
results = c()
for (i in 1:nrow(XXX)) {
  x = XXX[i,]
  #x[is.na(x)] = 0
  r = treeClassifier(x)
  results = c(results, r)
}
write(1-results, file='res.txt', sep='\n')

#}