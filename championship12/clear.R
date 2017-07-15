my.fixData = function (XA, remove=F) {
  Y = NULL
  if ('cardio' %in% colnames(XA)) {
    Y = XA$cardio
    XA = subset(XA, select=-c(cardio))
  }
  
  m = function (a, b, A, B) {
    which.replace = which(XA$ap_hi == a & XA$ap_lo == b)
    if (length(which.replace) > 0) {
      XA[which.replace, ]$ap_hi <<- A
      XA[which.replace, ]$ap_lo <<- B
    }
  }
  source('fix.R', local=T)
  
  if (sum(XA$weight < 20) > 0)
    XA[XA$weight < 20, ]$weight = 50
  
  if (length(Y) > 1 || !is.null(Y)) {
    XA$cardio = Y
  }
  
  if (remove) {
    which.remove = which(XA$ap_hi == 0 | XA$ap_lo == 0)
    XA = XA[-which.remove, ]
  } else {
    if (any(XA$ap_hi == 0))
      XA[XA$ap_hi == 0, ]$ap_hi = 130
    if (any(XA$ap_lo == 0))
      XA[XA$ap_lo == 0, ]$ap_lo = 80
  }
  
  XA
}

#XA = rbind(XLL[,-ncol(XLL)], XXX)
#XA1 = my.fixData(XA)
#XA = my.fixData(XA)
#my.fixData(XLL)