ans.distance = function (a, b) {
  df = atan2(a[2], a[1]) - atan2(b[2], b[1])
  
  df = ifelse(df < -pi, df + 2*pi, df)
  df = ifelse(df > pi, df - 2*pi, df)
  abs(df)
}

ang.getCost = function(a, b, y) {
  a = a / max(abs(a))
  b = b / max(abs(b))
  
  mydata = data.frame(a, b)
  # K-Means Cluster Analysis
  #fit <- kmeans(mydata, 5) # 5 cluster solution
  # get cluster means 
  #aggregate(mydata,by=list(fit$cluster),FUN=mean)
  # append cluster assignment
  #plot(mydata, col=fit$cluster)
  
  d <- proxy::dist(mydata, method = ans.distance) # distance matrix
  fit <- hclust(d, method="ward.D2") 
  groups <- cutree(fit, k=5) # cut tree into 5 clusters
  plot(mydata, col=groups)
  
  ang = atan2(a, b)
  s = 0
  for(i in 1:length(a)) {
    df = ang[i] - ang[which(y == y[i])]
    df = ifelse(df < -pi, df + 2*pi, df)
    df = ifelse(df > pi, df - 2*pi, df)
    df = abs(df)
    s = s - sum(df < pi / 20)^2
  }
  s
}


a = XLL$X80
b = XLL$X97
y = XLL$Y

ang.getCost(a + 1, b + 0.05, y)
exit()
minCost = 1e100
for(dx in seq(from=min(a), to=max(a), length.out=5)) {
  for(dy in seq(from=min(b), to=max(b), length.out=5)) {
    cost = ang.getCost(a - dx, b - dy, y)
    if (cost < minCost) {
      minCost = cost
      selCenter = c(dx, dy)
    }
  } 
}
print(selCenter)