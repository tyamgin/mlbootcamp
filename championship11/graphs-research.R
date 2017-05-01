# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



m.fill = scale_fill_manual(values=1:5)
m.color = scale_color_manual(values=1:5)

X_X = data.frame(XLL)
Y_Y = factor(X_X$X224, labels=c('a', 'b', 'c', 'd', 'e'))

for(i in 1:ncol(XX)) {
  png(filename=paste0('graph/hist/', i, '.png'), width=1200, height=900)
  
  #ggplot(data=X_X, aes(x=X_X[,col], y=YY[,1]))+geom_histogram(aes(y=..density..), bins=30)+geom_density(col=42)
  
  limits = scale_x_continuous(limits = c(min(X_X[, i]), max(X_X[, i])))
  pa = ggplot(X_X, aes(x=X_X[, i], fill=Y_Y, color=Y_Y))+geom_histogram(bins=30)+m.fill + m.color
  
  plots = list(pa)
  for (cls in c('a', 'b', 'c', 'd', 'e')) {
    p2 = local({
      Y = Y_Y[which(Y_Y == cls)]
      X = X_X[which(Y_Y == cls), ]
      
      ggplot(X, aes(x=X[, i]))+geom_histogram(bins=30, fill=as.integer(Y[1]))+limits
    })
    
    plots[[length(plots) + 1]] = p2
  }
  
  multiplot(plotlist=plots, cols=2)
  
  dev.off()
}

ggplot(X_X, aes(x=X_X[,3], y=X_X[,2], fill=Y_Y, color=Y_Y)) + geom_point(alpha=.3) + m.fill + m.color


png(filename=paste0('graph/ggpairs.png'), width=3000, height=3000)
gg = ggpairs(X_X[,1:25], aes(alpha=0.4, colour=Y_Y), upper=NULL)
for(ii in 1:gg$nrow) for(jj in 1:gg$ncol) gg[ii, jj] = gg[ii, jj] + m.fill + m.color
print(gg)
dev.off()


pc = princomp(XX)
XX2 = XX %*% solve(t(pc$loadings))
X_X2 = data.frame(XX2)
ggplot(X_X2, aes(x=X_X2[,1], y=X_X2[,2], fill=Y_Y, color=Y_Y)) + geom_point(alpha=.3) + m.fill + m.color

gg = ggpairs(X_X2[,1:5], aes(alpha=0.4, colour=Y_Y), upper=NULL)
for(ii in 1:gg$nrow) for(jj in 1:gg$ncol) gg[ii, jj] = gg[ii, jj] + m.fill + m.color
print(gg)

