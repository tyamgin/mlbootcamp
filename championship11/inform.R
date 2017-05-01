
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
    
    inform = max(
      xit / (yit + 1),
      yit / (xit + 1),
      (length(x) - xit) / (length(y) - yit + 1),
      (length(y) - yit) / (length(x) - xit + 1)
    )
    if (inform > maxInform) {
      maxInform = inform
      selThr = thr
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
Y_Y = factor(X_X$X224, labels=c('a', 'b', 'c', 'd', 'e'))

col = 1
x = XX[which(XLL[, ncol(XLL)] == 2), col]
y = XX[which(XLL[, ncol(XLL)] == 1), col]

thr = my.simpleInform(x, y)$threshold

print(ggplot(NULL, aes(x=X_X[, col], y=Y_Y, fill=Y_Y, color=Y_Y)) + geom_point(alpha=.3) + m.fill + m.color + geom_vline(xintercept=thr))


mat = foreach(i = 1:ncol(XX), .combine = rbind) %do% {
  x = XX[which(XLL[, ncol(XLL)] == 2), i]
  y = XX[which(XLL[, ncol(XLL)] == 1), i]
  
  dt = my.simpleInform(x, y)
  data.frame(inform = dt$inform, threshold = dt$threshold, col = i)
}


gl.mat = mat[rev(order(mat$inform))[1:4], ]

"
for(i in 1:ncol(XX)) {
  png(filename=paste0('graph/ycor/', i, '.png'), width=1200, height=900)
  
  x = XX[which(XLL[, ncol(XLL)] == 2), i]
  y = XX[which(XLL[, ncol(XLL)] == 1), i]
  
  thr = my.simpleInform(x, y)$threshold
  
  print(
    ggplot(X_X, aes(x=X_X[, i], y=Y_Y, fill=Y_Y, color=Y_Y)) 
    + geom_point(alpha=.3) 
    + m.fill 
    + m.color
    + geom_vline(xintercept=thr)
  )
  dev.off()
}
"