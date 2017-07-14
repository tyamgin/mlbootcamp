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
  source('fix.txt', local=T)
  
  
  #weight_height_fix = read.csv('predicts/weight-height-predicts1.csv')
  #XA[which(XA$id %in% weight_height_fix$id), ]$height = weight_height_fix$new_height[which(weight_height_fix$id %in% XA$id)]
  #XA[which(XA$id %in% weight_height_fix$id), ]$weight = weight_height_fix$new_weight[which(weight_height_fix$id %in% XA$id)]
  
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
#mean(XLL[XLL$gender == 2, ]$age)
#XA = rbind(XLL[,-ncol(XLL)], XXX)
#XA1 = my.fixData(XA)
#XA = my.fixData(XA)
#my.fixData(XLL)