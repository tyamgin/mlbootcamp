my.fixData = function (XA, remove=F) {
  XA_prev = XA
  XA$ap_hi = abs(XA$ap_hi)
  XA$ap_lo = abs(XA$ap_lo)
  
  for (. in 1:2) {
    XA[which(XA$ap_lo > 200), ]$ap_lo = XA[which(XA$ap_lo > 200), ]$ap_lo / 10
  }
  
  for (. in 1:2) {
    XA[which(XA$ap_hi > 300), ]$ap_hi = XA[which(XA$ap_hi > 300), ]$ap_hi / 10
  }
  
  XA[which(XA$ap_hi < 25), ]$ap_hi = XA[which(XA$ap_hi < 25), ]$ap_hi * 10
  
  XA[which(XA$ap_lo <= 12), ]$ap_lo = XA[which(XA$ap_lo <= 12), ]$ap_lo * 10 # TODO: остались ещё 20-ки
  
  swap.idxes = which(XA$ap_lo > XA$ap_hi)
  tmp = XA[swap.idxes, ]$ap_lo
  XA[swap.idxes, ]$ap_lo = XA[swap.idxes, ]$ap_hi
  XA[swap.idxes, ]$ap_hi = tmp
  
  XA[which(XA$ap_lo < 40), ]$ap_lo = 80
  
  #weight_height_fix = read.csv('predicts/weight-height-predicts1.csv')
  #XA[which(XA$id %in% weight_height_fix$id), ]$height = weight_height_fix$new_height[which(weight_height_fix$id %in% XA$id)]
  #XA[which(XA$id %in% weight_height_fix$id), ]$weight = weight_height_fix$new_weight[which(weight_height_fix$id %in% XA$id)]
  
  if (remove) {
    r = rep(NA, nrow(XA))
    for (i in 1:nrow(XA))
      r[i] = any(XA[i, ] != XA_prev[i, ])
    return( XA[which(!r), ] )
  }
  
  XA
}

#XA = rbind(XLL[,-ncol(XLL)], XXX)
#XA = my.fixData(XA)