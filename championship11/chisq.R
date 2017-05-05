r = foreach(i=1:ncol(XX), .combine=c) %do% {
  y = XLL[,ncol(XLL)]
  x = XX[, i]
  #x = (x - min(x))
  #x = x / max(x)
  #x = round(x, 3)
  
  #X = table(x, y)
  #r = chisq.test(X)
  r = chisq.test(x, y)
  r$p.value
}

X_X = XX
colnames(X_X) <- paste0('X', 1:ncol(XX))

corrgram(X_X[, order(r)[1:30]], order=NULL, panel=panel.shade, text.panel=panel.txt, main='Correlogram')

#as.data.frame(table(cut(x, breaks=seq(0,1, by=0.1))))


