my.fixData = function (XA) {
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
  
  XA[which(XA$ap_lo < 30), ]$ap_lo = 80
  
  XA
}

#XA = rbind(XLL[,-ncol(XLL)], XXX)
#XA = my.fixData(XA)