"ang.getCost = function(a, b, y) {
  a = a / max(abs(a))
  b = b / max(abs(b))
  
  mydata = data.frame(1:length(a))
  
  ang = atan2(a, b)
  distMat = matrix(NA, length(a), length(a))
  for(i in 1:length(a)) {
    df = ang[i] - ang
    df = ifelse(df < -pi, df + 2*pi, df)
    df = ifelse(df > pi, df - 2*pi, df)
    #df = ifelse(y[i] == y, df, 1e10)
    distMat[i, ] = abs(df)
  }

  d = as.dist(distMat)
  fit <- hclust(d, method='ward.D2') 
  groups <- cutree(fit, k=4) # cut tree into 5 clusters
  plot(a, b, col=groups)
  
  s = 0
  for(i in 1:length(a)) {
    an = distMat[i, groups[i] == groups & y[i] == y]
    s = s + sum(an<pi/40)^4
  }
  -s
}"

ang.getCost = function(a, b, y, draw.plot=F) {
  ang = atan2(a, b)
  s = 0
  pch = rep(0, length(a))
  
  eta = pi / 16
  
  for(cl in 0:4) {
    arr = sort(ang[y == cl])
    
    n = length(arr)
    distMat = matrix(NA, n, n)
    for(i in 1:n) {
      df = arr[i] - arr
      df = ifelse(df < -pi, df + 2*pi, df)
      df = ifelse(df > pi, df - 2*pi, df)
      distMat[i, ] = abs(df)
    }
    d = as.dist(distMat)
    fit = hclust(d, method='ward.D2') 
    clust = cutree(fit, k=2)
    
    lst = c()
    for (j in 1:n) {
      i = j - 1
      if (i == 0) i = n
      k = j + 1
      if (k > n) k = 1
      
      if (distMat[i, j] > eta && distMat[j, k] > eta)
        next
      lst = c(lst, j)
    }
    
    k = 0
    cost = 1
    
    for (idx in 1:length(lst)) {
      i = lst[idx]
      j = lst[idx %% length(lst) + 1]
      if (clust[i] != clust[j]) {
        k = k + 1
        cost = cost * distMat[i, j] / length(arr)
        cost = cost * sum(clust == clust[i]) / length(arr)
      }
    }
    if (k != 2) {
      next
    }
    
    pch[y == cl] = clust + 20
    s = s + cost * sqrt(length(arr))
  }
  
  if (draw.plot) {
    print(
      ggplot(NULL, aes(x=cos(ang)*(y+1), y=sin(ang)*(y+1), fill=as.factor(y), color=as.factor(y))) 
      + geom_point(alpha=.3) 
      + m.fill 
      + m.color
    )
    plot(a, b, col=y, pch=pch)
    points(0, 0, cex=10)
  }
  
  -s
}

labels = c('a', 'b', 'c', 'd', 'e')
Y_Y = factor(X_X$Y, labels=labels)

cols = intCols


cl <- makeCluster(4)
registerDoParallel(cl)

ang.result = foreach(c2=2:length(cols), .combine=rbind, .packages='doParallel') %dopar% {
  foreach(c1=1:(c2-1), .combine=rbind) %do% {
    a = XLL[, cols[c1]]
    b = XLL[, cols[c2]]
    y = XLL$Y
    
    bins = 16
    selCenter = NULL
    
    minCost = 1e100
    for(dx in seq(from=min(a), to=max(a), length.out=bins)) {
      for(dy in seq(from=min(b), to=max(b), length.out=bins)) {
        cost = ang.getCost(a - dx, b - dy, y)
        if (cost < minCost) {
          minCost = cost
          selCenter = c(dx, dy)
        }
      } 
    }
    
    cost = ang.getCost(a - selCenter[1], b - selCenter[2], y, draw.plot=F)
    
    data.frame(
      idx1 = c1,
      idx2 = c2,
      col1 = cols[c1],
      col2 = cols[c2],
      x = selCenter[1],
      y = selCenter[2],
      cost = cost
    )
  }
}

stopCluster(cl)


gg = ggpairs(XLL[, cols], aes(alpha=0.4, colour=Y_Y), upper=NULL, diag=NULL)

for (idx in 1:nrow(ang.result)) {
  i = ang.result[idx, ]
  gg[i$idx2, i$idx1] = (gg[i$idx2, i$idx1] 
                          + m.fill + m.color 
                          + geom_point(data=NULL, aes(x=i$y, y=i$x), size=10, colour='black')
  )
}

png(filename=paste0('graph/ggpairs3.png'), width=3000, height=3000)
print(gg)
dev.off()
