j1_sp = readSp('j1', 1:5300)
j1_sp_train = j1_sp[train_answers$cuid,]

r = sapply(1:5000, function (i) {
  mean(train_answers$target[j1_sp_train[, i] > 0])
})

ggplot(data=data.frame(x=1:length(r), y=r), aes(x=x, y=y, width=1)) +
  geom_bar(stat="identity")

