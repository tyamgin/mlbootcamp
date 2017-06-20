require(caret)

XLL = read.csv(file="data/train.csv", head=T, sep=";", na.strings="None")
XXX = read.csv(file="data/test.csv", head=T, sep=";", na.strings="None")

X_all = rbind(XLL[,-ncol(XLL)], XXX)

X_good = X_all
X_good = X_good[which(X_good$weight >= 50 & X_good$weight <= 170), ]
X_good = X_good[-which(X_good$height == X_good$ap_hi & X_good$weight == X_good$ap_lo), ]

X_predict = X_all[which(X_all$height == X_all$ap_hi & X_all$weight == X_all$ap_lo), ]

XW = subset(X_good, select=c(age, gender))

weight.model = train(XW, X_good$weight, method='glm', metric='RMSE', maximize=F, 
             trControl=trainControl(method='none'), tuneGrid=NULL)

height.model = train(XW, X_good$height, method='glm', metric='RMSE', maximize=F, 
                     trControl=trainControl(method='none'), tuneGrid=NULL)

X_predict$new_weight = predict(weight.model, subset(X_predict, select=c(age, gender)))
X_predict$new_height = predict(height.model, subset(X_predict, select=c(age, gender)))


write.csv(X_predict, 'predicts/weight-height-predicts1.csv')