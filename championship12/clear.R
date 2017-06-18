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
  
  XA
}

#XA = rbind(XLL[,-ncol(XLL)], XXX)
#XA = my.fixData(XA)