act.last.3 = function (ACT, CONTACT_DATE, SNAP_DATE, metric) {
  CONTACT_DATE = as.character(CONTACT_DATE)
  SNAP_DATE = as.character(SNAP_DATE)
  
  CONTACT_DATE = CONTACT_DATE[1]
  ord = order(sapply(SNAP_DATE, parse.date), decreasing=T)
  SNAP_DATE = SNAP_DATE[ord]
  ACT = ACT[ord]
  
  if (length(grep('.04', CONTACT_DATE)) > 0) {
    if (length(grep('.05.02', SNAP_DATE)) > 0) {
      print(CONTACT_DATE)
      print(SNAP_DATE)
      stop('asdsa')
      
    }
  }
  
  if (metric == 'cz') {
    r = rle(ACT)
    v = r$lengths[r$values == 0]
    if (length(v) == 0) return(-1)
    return(max(v))
  }
  if (metric == 'm3') {
    return(mean(ACT[1:3], na.rm=T))
  }
  stop('invalid metric')
}
zz2343 = left_join(subs_features_test, subs_csi_test, 'SK_ID') %>% group_by(SK_ID) %>% summarise(
  ACT_CZ=act.last.3(ACT, CONTACT_DATE, SNAP_DATE, 'cz')
)


zz = left_join(subs_features_train, subs_csi_train, 'SK_ID') %>% group_by(SK_ID) %>% summarise(
  ACT1=mean(ACT),
  ACT_CZ=act.last.3(ACT, CONTACT_DATE, SNAP_DATE, 'cz'),
  ACT_M3=act.last.3(ACT, CONTACT_DATE, SNAP_DATE, 'm3'),
  count=n(), 
  cn=0
)


for (i in 1:nrow(zz)) zz[i,]$cn = sum(zz$count == zz[i,]$count & zz$ACT1 == zz[i,]$ACT1)

p = ggplot(zz %>% filter(count!=12|ACT1!=1), aes(count, ACT1))

p + geom_point(aes(size = cn)) + scale_size_continuous(range = c(1, 40))



zz_test = subs_features_test %>% group_by(SK_ID) %>% summarise(ACT1=(function (ACT) {
  mean(ACT)
})(ACT), count=n(), cn=0)

zz_test = left_join(zz_test, subs_csi_test, 'SK_ID')

for (i in 1:nrow(zz_test)) zz_test[i,]$cn = sum(zz_test$count == zz_test[i,]$count & zz_test$ACT1 == zz_test[i,]$ACT1)

p = ggplot(zz_test %>% filter(count!=12|ACT1!=1), aes(count, ACT1))

p + geom_point(aes(size = cn)) + scale_size_continuous(range = c(1, 40))