
"my.donskoiInform = function(x, y) {
  x = sort(x)
  y = sort(y)
  xit = 0
  yit = 0
  z = unique(sort(c(x, y)))
  r = 0
  selThr = 0
  maxInform = -1
  for (i in 0:length(z)) {
    if (i == 0)
      thr = z[1] - 1e-5
    else if (i == length(z))
      thr = z[length(z)] + 1e-5
    else
      thr = (z[i] + z[i + 1]) / 2
    
    while (xit < length(x) && x[xit + 1] < thr)
      xit = xit + 1
    while (yit < length(y) && y[yit + 1] < thr)
      yit = yit + 1
    
    inform = xit * (length(y) - yit) + yit * (length(x) - xit)
    if (inform > maxInform) {
      maxInform = inform
      selThr = thr
    }
  }
  selThr
}"

my.simpleInform = function(x, y) {
  x = sort(x)
  y = sort(y)
  xit = 0
  yit = 0
  z = unique(sort(c(x, y)))
  r = 0
  selThr = 0
  maxInform = -1
  for (i in 0:length(z)) {
    if (i == 0)
      thr = z[1] - 1e-5
    else if (i == length(z))
      thr = z[length(z)] + 1e-5
    else
      thr = (z[i] + z[i + 1]) / 2
    
    while (xit < length(x) && x[xit + 1] < thr)
      xit = xit + 1
    while (yit < length(y) && y[yit + 1] < thr)
      yit = yit + 1
    
    xx = xit / length(x)
    yy = yit / length(y)
    
    alpha = 0.03
    
    inform = max(
      ifelse(yy < alpha, xx - yy, 0),
      ifelse(xx < alpha, yy - xx, 0),
      ifelse(1 - yy < alpha, (1 - xx) - (1 - yy), 0),
      ifelse(1 - xx < alpha, (1 - yy) - (1 - xx), 0)
    )
    
    if (inform > maxInform) {
      maxInform = inform
      selThr = thr
      selXX=xx
      selYY=yy
    }
  }
  list(
    threshold = selThr,
    inform = maxInform
  )
}


m.fill = scale_fill_manual(values=1:5)
m.color = scale_color_manual(values=1:5)
X_X = data.frame(XLL)
labels = c('a', 'b', 'c', 'd', 'e')
Y_Y = factor(X_X$X224, labels=labels)


mat = foreach(cl=1:4, .combine=rbind) %do% {
  foreach(i=1:ncol(XX), .combine=rbind) %do% {
    x = XX[which(XLL[, ncol(XLL)] == cl), i]
    y = XX[which(XLL[, ncol(XLL)] == cl - 1), i]
    
    dt = my.simpleInform(x, y)
    data.frame(inform = dt$inform, threshold = dt$threshold, col = i, class=cl)
  }
}
gl.mat = mat[rev(order(mat$inform))[1:200], ]

"
Y = YY[, 1]

for(i in 1:ncol(XX)) {
  
  pl = (
    ggplot(NULL, aes(x=XX[, i], y=Y, fill=as.factor(Y), color=as.factor(Y))) 
    + geom_point(alpha=.3) 
    + m.fill 
    + m.color
  )
  
  segs = foreach(cl=1:4, .combine=rbind) %do% {
    x = XX[which(XLL[, ncol(XLL)] == cl), i]
    y = XX[which(XLL[, ncol(XLL)] == cl - 1), i]
  
    thr = my.simpleInform(x, y)$threshold
 
    data.frame(x=thr, y1=cl-1, y2=cl)
    #+ geom_vline(aes(xintercept=thr), colour=3, linetype='dashed')
  }
  pl1 = pl + geom_segment(aes(x=x, y=y1, xend=x, yend=y2, fill=as.factor(0)), data=segs, colour='black', linetype='dashed')
  
  png(filename=paste0('graph/ycor/', i, '.png'), width=1200, height=900)
  print(pl1)
  dev.off()
}

"