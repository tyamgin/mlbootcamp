X = XLL[, -ncol(XLL)]
colnames(X) <- paste0('X', 1:ncol(X))
Y = factor(XLL[, ncol(XLL)], labels=c('a', 'b', 'c', 'd', 'e'))

trControl = trainControl(method='cv', number=4, repeats=3,  classProbs=T, summaryFunction=defaultSummary)

tuneGrid = expand.grid(
  size=5:7,
  decay=c(0.01, 0.02)
)

capture.output(
  model <- train(X, Y, method='nnet', metric='Accuracy',
               maximize=T, trControl=trControl,MaxNWts=100000,
               tuneGrid=tuneGrid)
)
