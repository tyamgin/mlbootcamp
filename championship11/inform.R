my.simpleInform = function(x, y, alpha = 0.03) {
  x = sort(x)
  y = sort(y)
  xsz = length(x)
  ysz = length(y)
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
    
    while (xit < xsz && x[xit + 1] < thr)
      xit = xit + 1
    while (yit < ysz && y[yit + 1] < thr)
      yit = yit + 1
    
    xx = xit / xsz
    yy = yit / ysz
    
    inform = max(
      ifelse(yy < alpha, (xit - yit) / (xsz + ysz), 0),
      ifelse(xx < alpha, (yit - xit) / (xsz + ysz), 0),
      ifelse(1 - yy < alpha, ((xsz - xit) - (ysz - yit)) / (xsz + ysz), 0),
      ifelse(1 - xx < alpha, ((ysz - yit) - (xsz - xit)) / (xsz + ysz), 0)
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