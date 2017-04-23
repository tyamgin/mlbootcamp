cols = c()
for(i in 1:223) {
  if ( length( unique(round(diff(sort(XX[,i])), 5)) ) < 60 ) {
    cols = c(cols, i)
  }
}
print(length(cols))
X = XX
colnames(X) <- paste0(1:223)

plot(density(my.norm(XX[,77])))
lines(density(my.norm(XX[,201])), col='red')

#png(filename='graph/name2.png', width=2000, height=2000)
corrgram(X[,cols], order=NULL, panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt, main='Correlogram')
#dev.off()