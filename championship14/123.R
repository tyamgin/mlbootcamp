#j1_sp = readSp('j1', 5301:10000)
j1_sp = readSp('j1', 1:5300)
j1_sp_train = j1_sp[train_answers$cuid,]
j1_sp_test = j1_sp[-train_answers$cuid,]

r = colSums(j1_sp_train * train_answers$target) / colSums(j1_sp_train)

ggplot(data=data.frame(x=1:length(r), y=as.vector(r)), aes(x=x, y=y, width=1)) +
  geom_bar(stat="identity")

