"
for(i in 1:ncol(XX)) {
  png(filename=paste0('graph/density/', i, '.png'), width=500, height=500)
  plot(density(my.norm(XX[,i])))
  dev.off()
}
"
# 'длинные графики'
cols = c(15, 57, 66, 81, 150, 152, 157, 174, 190, 195, 204, ncol(XLL))
X = XLL
colnames(X) <- c(paste0(1:ncol(XX)), 'A')
corrgram(X[, cols], order=NULL, panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main='Correlogram')