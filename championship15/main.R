library(lightgbm)
library(plyr)  
library(dplyr)
library(foreach)
library(caret)
library(e1071)
library(nnet)
library(extraTrees)
library(doParallel)

debugSource("cv.R")
debugSource("tune.R")
debugSource("models.R")
debugSource("feat-select.R")

set.seed(888)

parse.date = function (s, n.d=31, n.m=12) {
  x = strsplit(s, '.', fixed=T)[[1]]
  if (length(x) < 2 || length(x) > 3)
    stop(paste0('Unsupported date format ', x))
  if (nchar(x[1]) != 2)
    stop(paste0('Unsupported day ', x[1]))
  if (nchar(x[2]) != 2)
    stop(paste0('Unsupported month ', x[2]))
  
  if (length(x) == 2) {
    return((as.integer(x[2])-1) * n.d + (as.integer(x[1])-1))
  }
  if (length(x) == 3) {
    if (nchar(x[3]) != 2)
      stop(paste0('Unsupported year ', x[3]))
    return((as.integer(x[3])-1) * n.d * n.m + (as.integer(x[2])-1) * n.d + (as.integer(x[1])-1))
  }
}

most.freq = function (vec) {
  vec[is.na(vec)] = 0
  as.integer(names(sort(table(vec), decreasing=T))[[1]])
}

not.most.freq.count = function (vec) {
  sum(vec != most.freq(vec))
}

bs_avg_kpi_1_12 = readRDS('cache/bs_avg_kpi_1_12')
bs_chnn_kpi_1_12 = readRDS('cache/bs_chnn_kpi_1_12')

subs_features_train = readRDS("cache/subs_features_train")
subs_csi_train = readRDS("cache/subs_csi_train")
subs_bs_data_session_train = readRDS("cache/subs_bs_data_session_train")
subs_bs_voice_session_train = readRDS("cache/subs_bs_voice_session_train")
subs_bs_consumption_train = readRDS("cache/subs_bs_consumption_train")

subs_features_test = readRDS("cache/subs_features_test")
subs_csi_test = readRDS("cache/subs_csi_test")
subs_bs_data_session_test = readRDS("cache/subs_bs_data_session_test")
subs_bs_voice_session_test = readRDS("cache/subs_bs_voice_session_test")
subs_bs_consumption_test = readRDS("cache/subs_bs_consumption_test")

subs_features_train = subs_features_train %>% arrange(SK_ID)
subs_csi_train = subs_csi_train %>% arrange(SK_ID)
subs_bs_data_session_train = subs_bs_data_session_train %>% arrange(SK_ID)
subs_bs_voice_session_train = subs_bs_voice_session_train %>% arrange(SK_ID)
subs_bs_consumption_train = subs_bs_consumption_train %>% arrange(SK_ID)

bs_avg_kpi_1_12 = bs_avg_kpi_1_12 %>% arrange(CELL_LAC_ID) 

na3g = is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_3G)
bs_avg_kpi_1_12$CELL_AVAILABILITY_3G[na3g] = bs_avg_kpi_1_12$CELL_AVAILABILITY_4G[na3g]
ls3g = !is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_3G) & !is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_4G) & bs_avg_kpi_1_12$CELL_AVAILABILITY_3G < bs_avg_kpi_1_12$CELL_AVAILABILITY_4G
bs_avg_kpi_1_12$CELL_AVAILABILITY_3G[ls3g] = bs_avg_kpi_1_12$CELL_AVAILABILITY_4G[ls3g]

na2g = is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_2G)
bs_avg_kpi_1_12$CELL_AVAILABILITY_2G[na2g] = bs_avg_kpi_1_12$CELL_AVAILABILITY_3G[na2g]
ls2g = !is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_2G) & !is.na(bs_avg_kpi_1_12$CELL_AVAILABILITY_3G) & bs_avg_kpi_1_12$CELL_AVAILABILITY_2G < bs_avg_kpi_1_12$CELL_AVAILABILITY_3G
bs_avg_kpi_1_12$CELL_AVAILABILITY_2G[ls2g] = bs_avg_kpi_1_12$CELL_AVAILABILITY_3G[ls2g]


avg1 = bs_avg_kpi_1_12 %>% group_by(CELL_LAC_ID) %>% summarise(
  CELL_AVAILABILITY_2G = mean(CELL_AVAILABILITY_2G, na.rm=T),
  CELL_AVAILABILITY_3G = mean(CELL_AVAILABILITY_3G, na.rm=T),
  CELL_AVAILABILITY_4G = mean(CELL_AVAILABILITY_4G, na.rm=T),
  CSSR_2G = mean(CSSR_2G, na.rm=T),
  CSSR_3G = mean(CSSR_3G, na.rm=T),
  ERAB_PS_BLOCKING_RATE_LTE = mean(ERAB_PS_BLOCKING_RATE_LTE, na.rm=T),
  ERAB_PS_BLOCKING_RATE_PLMN_LTE = mean(ERAB_PS_BLOCKING_RATE_PLMN_LTE, na.rm=T),
  ERAB_PS_DROP_RATE_LTE = mean(ERAB_PS_DROP_RATE_LTE, na.rm=T)
)

chnn1 = bs_chnn_kpi_1_12 %>% group_by(CELL_LAC_ID) %>% summarise(
  AVEUSERNUMBER = mean(AVEUSERNUMBER, na.rm=T),
  AVEUSERNUMBER_PLMN = mean(AVEUSERNUMBER_PLMN, na.rm=T),
  AVR_DL_HSPA_USER_3G = mean(AVR_DL_HSPA_USER_3G, na.rm=T),
  AVR_DL_R99_USER_3G = mean(AVR_DL_R99_USER_3G, na.rm=T),
  AVR_DL_USER_3G = mean(AVR_DL_USER_3G, na.rm=T),
  AVR_DL_USER_LTE = mean(AVR_DL_USER_LTE, na.rm=T),
  AVR_TX_POWER_3G = mean(AVR_TX_POWER_3G, na.rm=T),
  AVR_UL_HSPA_USER = mean(AVR_UL_HSPA_USER, na.rm=T),
  AVR_UL_R99_USER = mean(AVR_UL_R99_USER, na.rm=T),
  AVR_UL_USER_3G = mean(AVR_UL_USER_3G, na.rm=T)
)

avg1_d_train = left_join(subs_bs_data_session_train, avg1, 'CELL_LAC_ID')
avg1_d_test = left_join(subs_bs_data_session_test, avg1, 'CELL_LAC_ID')
avg1_v_train = left_join(subs_bs_voice_session_train, avg1, 'CELL_LAC_ID')
avg1_v_test = left_join(subs_bs_voice_session_test, avg1, 'CELL_LAC_ID')

chnn1_d_train = left_join(subs_bs_data_session_train, chnn1, 'CELL_LAC_ID')
chnn1_d_test = left_join(subs_bs_data_session_test, chnn1, 'CELL_LAC_ID')
chnn1_v_train = left_join(subs_bs_voice_session_train, chnn1, 'CELL_LAC_ID')
chnn1_v_test = left_join(subs_bs_voice_session_test, chnn1, 'CELL_LAC_ID')


create_features = function (subs_features, subs_csi, avg1_d, avg1_v, chnn1_d, chnn1_v, subs_bs_consumption) {
  ss = subs_features %>% group_by(SK_ID) %>% summarise(
    REVENUE = sum(REVENUE),
    C1_FREQ = most.freq(COM_CAT.1),
    C2_FREQ = most.freq(COM_CAT.2),
    C3 = mean(COM_CAT.3),
    C3_FREQ = most.freq(COM_CAT.3),
    BASE_TYPE_MEAN = mean(BASE_TYPE),
    ACT_MEAN = mean(ACT),
    ARPU_GROUP = mean(ARPU_GROUP),
    ARPU_GROUP_FREQ = most.freq(ARPU_GROUP),
    C7_FREQ = most.freq(COM_CAT.7),
    C7_3456 = most.freq(COM_CAT.7) >= 3 & most.freq(COM_CAT.7) <= 6,
    C8_FREQ = most.freq(COM_CAT.8),
    DEVICE_TYPE_ID_FREQ = most.freq(DEVICE_TYPE_ID),
    INTERNET_TYPE_ID_1 = mean(INTERNET_TYPE_ID == 1, na.rm=T),
    INTERNET_TYPE_ID_2 = mean(INTERNET_TYPE_ID == 2 | is.na(INTERNET_TYPE_ID), na.rm=T),
    INTERNET_TYPE_ID_NOT_1 = mean(INTERNET_TYPE_ID != 1, na.rm=T),
    INTERNET_TYPE_ID_3 = mean(INTERNET_TYPE_ID == 3, na.rm=T),
    
    REVENUE = sum(REVENUE),
    ITC = sum(ITC),
    VAS = sum(VAS),
    
    REVENUE_M = mean(REVENUE),
    ITC_M = mean(ITC),
    VAS_M = mean(VAS),
    
    RENT_CHANNEL = sum(RENT_CHANNEL),
    ROAM = sum(ROAM),
    COST = mean(COST),
    
    RENT_CHANNEL_M = mean(RENT_CHANNEL),
    ROAM_M = mean(ROAM),
    COST_S = sum(COST),
    
    C17 = mean(COM_CAT.17),
    C18 = mean(COM_CAT.18),
    C19 = mean(COM_CAT.19),
    C20 = mean(COM_CAT.20),
    C21 = mean(COM_CAT.21),
    C22 = mean(COM_CAT.22),
    C23 = mean(COM_CAT.23),
    C25 = mean(COM_CAT.25),#01
    C26 = mean(COM_CAT.26),#01
    C27 = mean(COM_CAT.27),
    C28 = mean(COM_CAT.28),
    C29 = mean(COM_CAT.29),
    C30 = mean(COM_CAT.30),
    C31 = mean(COM_CAT.31),
    C32 = mean(COM_CAT.32),
    C33 = mean(COM_CAT.33),
    
    C17S = sum(COM_CAT.17),
    C18S = sum(COM_CAT.18),
    C19S = sum(COM_CAT.19),
    C20S = sum(COM_CAT.20),
    C21S = sum(COM_CAT.21),
    C22S = sum(COM_CAT.22),
    C23S = sum(COM_CAT.23),
    C27S = sum(COM_CAT.27),
    C28S = sum(COM_CAT.28),
    C29S = sum(COM_CAT.29),
    C30S = sum(COM_CAT.30),
    C31S = sum(COM_CAT.31),
    C32S = sum(COM_CAT.32),
    C33S = sum(COM_CAT.33),
    
    C34 = most.freq(COM_CAT.34),
    count=n()
  )

  dd = avg1_d %>% group_by(SK_ID) %>% summarise(
    DATA_VOL_MB_MEAN = mean(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_SUM = sum(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_MIN = min(DATA_VOL_MB, na.rm=T),
    DATA_VOL_MB_MAX = max(DATA_VOL_MB, na.rm=T),
    
    CELL_AVAILABILITY_2G_D_MEAN = mean(CELL_AVAILABILITY_2G, na.rm=T),
    CELL_AVAILABILITY_3G_D_MEAN = mean(CELL_AVAILABILITY_3G, na.rm=T),
    CELL_AVAILABILITY_4G_D_MEAN = mean(CELL_AVAILABILITY_4G, na.rm=T),
    CSSR_2G_D_MEAN = mean(CSSR_2G, na.rm=T),
    CSSR_3G_D_MEAN = mean(CSSR_3G, na.rm=T),
    
    ERAB_PS_BLOCKING_RATE_LTE_D_MEAN = mean(ERAB_PS_BLOCKING_RATE_LTE, na.rm=T),
    ERAB_PS_BLOCKING_RATE_PLMN_LTE_D_MEAN = mean(ERAB_PS_BLOCKING_RATE_PLMN_LTE, na.rm=T),
    ERAB_PS_DROP_RATE_LTE_D_MEAN = mean(ERAB_PS_DROP_RATE_LTE, na.rm=T),
    
    count_d = n()
  )
  
  ddd = chnn1_d %>% group_by(SK_ID) %>% summarise(
    AVEUSERNUMBER_D_MEAN = mean(AVEUSERNUMBER, na.rm=T),
    AVEUSERNUMBER_PLMN_D_MEAN = mean(AVEUSERNUMBER_PLMN, na.rm=T),
    AVR_DL_HSPA_USER_3G_D_MEAN = mean(AVR_DL_HSPA_USER_3G, na.rm=T),
    AVR_DL_R99_USER_3G_D_MEAN = mean(AVR_DL_R99_USER_3G, na.rm=T),
    AVR_DL_USER_3G_D_MEAN = mean(AVR_DL_USER_3G, na.rm=T),
    AVR_DL_USER_LTE_D_MEAN = mean(AVR_DL_USER_LTE, na.rm=T),
    AVR_TX_POWER_3G_D_MEAN = mean(AVR_TX_POWER_3G, na.rm=T),
    AVR_UL_HSPA_USER_D_MEAN = mean(AVR_UL_HSPA_USER, na.rm=T),
    AVR_UL_R99_USER_D_MEAN = mean(AVR_UL_R99_USER, na.rm=T),
    AVR_UL_USER_3G_D_MEAN = mean(AVR_UL_USER_3G, na.rm=T)
  )
  
  vv = avg1_v %>% group_by(SK_ID) %>% summarise(
    VOICE_DUR_MIN_MEAN = mean(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_SUM = sum(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_MIN = min(VOICE_DUR_MIN, na.rm=T),
    VOICE_DUR_MIN_MAX = max(VOICE_DUR_MIN, na.rm=T),
    
    CELL_AVAILABILITY_2G_V_MEAN = mean(CELL_AVAILABILITY_2G, na.rm=T),
    CELL_AVAILABILITY_3G_V_MEAN = mean(CELL_AVAILABILITY_3G, na.rm=T),
    CELL_AVAILABILITY_4G_V_MEAN = mean(CELL_AVAILABILITY_4G, na.rm=T),
    CSSR_2G_V_MEAN = mean(CSSR_2G, na.rm=T),
    CSSR_3G_V_MEAN = mean(CSSR_3G, na.rm=T),
    
    ERAB_PS_BLOCKING_RATE_LTE_V_MEAN = mean(ERAB_PS_BLOCKING_RATE_LTE, na.rm=T),
    ERAB_PS_BLOCKING_RATE_PLMN_LTE_V_MEAN = mean(ERAB_PS_BLOCKING_RATE_PLMN_LTE, na.rm=T),
    ERAB_PS_DROP_RATE_LTE_V_MEAN = mean(ERAB_PS_DROP_RATE_LTE, na.rm=T),
    
    count_v = n()
  )
  
  vvv = chnn1_v %>% group_by(SK_ID) %>% summarise(
    AVEUSERNUMBER_V_MEAN = mean(AVEUSERNUMBER, na.rm=T),
    AVEUSERNUMBER_PLMN_V_MEAN = mean(AVEUSERNUMBER_PLMN, na.rm=T),
    AVR_DL_HSPA_USER_3G_V_MEAN = mean(AVR_DL_HSPA_USER_3G, na.rm=T),
    AVR_DL_R99_USER_3G_V_MEAN = mean(AVR_DL_R99_USER_3G, na.rm=T),
    AVR_DL_USER_3G_V_MEAN = mean(AVR_DL_USER_3G, na.rm=T),
    AVR_DL_USER_LTE_V_MEAN = mean(AVR_DL_USER_LTE, na.rm=T),
    AVR_TX_POWER_3G_V_MEAN = mean(AVR_TX_POWER_3G, na.rm=T),
    AVR_UL_HSPA_USER_V_MEAN = mean(AVR_UL_HSPA_USER, na.rm=T),
    AVR_UL_R99_USER_V_MEAN = mean(AVR_UL_R99_USER, na.rm=T),
    AVR_UL_USER_3G_V_MEAN = mean(AVR_UL_USER_3G, na.rm=T)
  )
  
  cc = subs_bs_consumption %>% group_by(SK_ID) %>% summarise(
    SUM_MINUTES_SUM = sum(SUM_MINUTES, na.rm=T),
    SUM_DATA_MB_SUM = sum(SUM_DATA_MB, na.rm=T),
    SUM_DATA_MIN_SUM = sum(SUM_DATA_MIN, na.rm=T),
    SUM_MINUTES_MEAN = mean(SUM_MINUTES, na.rm=T),
    SUM_DATA_MB_MEAN = mean(SUM_DATA_MB, na.rm=T),
    SUM_DATA_MIN_MEAN = mean(SUM_DATA_MIN, na.rm=T),
    count_c = n()
  )
  
  XL = data.frame(
    SK_ID = subs_csi$SK_ID,
    CONTACT_DATE = sapply(as.character(subs_csi$CONTACT_DATE), parse.date, USE.NAMES=F)
  )
  if ("CSI" %in% colnames(subs_csi)) {
    XL$Y = subs_csi$CSI
  }

  XL1 = left_join(ss, dd, 'SK_ID')
  XL1 = left_join(XL1, vv, 'SK_ID')
  XL1 = left_join(XL1, cc, 'SK_ID')
  XL1 = left_join(XL1, vvv, 'SK_ID')
  XL1 = left_join(XL1, ddd, 'SK_ID')
  XL = left_join(XL1, XL, 'SK_ID')
  
  XL
}

my.extendedColsTrain = function (XL, trainFunc, feats) {
  model = trainFunc(extendXYCols(XL, feats))
  function (X) {
    model(extendXYCols(X, feats))
  }
}

XL = create_features(subs_features_train, subs_csi_train, avg1_d_train, avg1_v_train, chnn1_d_train, chnn1_v_train, subs_bs_consumption_train)

algo1 = function (XL) {
  my.extendedColsTrain(XL, function (XL) {
    my.train.lgb(XL, lgbParams)
  }, feats);
}

feats = c('C1_FREQ','ACT_MEAN','C7_FREQ','INTERNET_TYPE_ID_2','RENT_CHANNEL','ROAM','C17','C25','C27','DATA_VOL_MB_MAX',
          'count_v','BASE_TYPE_MEAN','C30S','ITC','C29S','RENT_CHANNEL_M','C33','count_c','DATA_VOL_MB_MEAN',
          'CELL_AVAILABILITY_3G_D_MEAN', 'CELL_AVAILABILITY_4G_D_MEAN', 'CSSR_3G_V_MEAN')

feats = c('C1_FREQ','ACT_MEAN','C7_FREQ','INTERNET_TYPE_ID_2','RENT_CHANNEL','ROAM','C17','C25','C27','DATA_VOL_MB_MAX',
          'count_v','BASE_TYPE_MEAN','C30S','ITC','C29S','RENT_CHANNEL_M','CSSR_3G_V_MEAN','C33',
          'ERAB_PS_BLOCKING_RATE_LTE_D_MEAN','C2_FREQ')

feats = c('C1_FREQ','ACT_MEAN','C7_FREQ','INTERNET_TYPE_ID_2','RENT_CHANNEL','ROAM','C17','C25','C27','count_v','C30S',
          'ITC','C29S','RENT_CHANNEL_M','CSSR_3G_V_MEAN','C33','C2_FREQ','C23S','DATA_VOL_MB_MEAN','COST_S','C19S')

feats = c('C1_FREQ','ACT_MEAN','C7_FREQ','INTERNET_TYPE_ID_2','RENT_CHANNEL','ROAM','C17','C25','C27','count_v','C30S',
          'ITC','C29S','RENT_CHANNEL_M','CSSR_3G_V_MEAN','C33','C2_FREQ','C23S','DATA_VOL_MB_MEAN','COST_S')

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)
my.gridSearch(XL, function (params) {
  function (XL) {
    my.extendedColsTrain(XL, function (XL) {
      my.train.lgb(XL, params)
    }, feats);
  }
}, expand.grid(lgbParams), verbose=T, iters=10, folds=7, resample.seed=2707, algo.seed=449)
stopCluster(cl)

stopCluster(cl)
cl = makeCluster(4)
registerDoParallel(cl)
addRemoveSelect(400, XL, function (XL) {
  my.train.lgb(XL, lgbParams)
}, startFeatures=feats)
stopCluster(cl)

XX = create_features(subs_features_test, subs_csi_test, avg1_d_test, avg1_v_test, chnn1_d_test, chnn1_v_test, subs_bs_consumption_test)
model = algo1(XL)
XX$Y = model(XX)
resulted_table = left_join(subs_csi_test, XX, "SK_ID")
YY = resulted_table$Y
YY[is.na(YY)] = mean(YY[!is.na(YY)])
write(YY, file="res/result.txt", sep='\n')

#set.seed(888)
#validation.tqfold(XL, algo1, folds=7, iters=4, verbose=T, seed=2707); asdasd()