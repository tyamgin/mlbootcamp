print('starting knn 11 check...')
arknn = knnTrainRoundAlgo(XLL, expand.grid(k=11, lol=1), newdata=XXX)
knnResults = arknn(XXX)
for (i in 1:length(knnResults)) {
  if (knnResults[i] != -1 && knnResults[i] != results[i]) {
    print(paste0('[knn] fixed ', i, ', answer is ', knnResults[i], ', not ', results[i]))
    results[i] = knnResults[i]
  }
}