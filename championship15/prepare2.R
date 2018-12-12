if (F) {
  bs_avg_kpi_1_12 = readRDS('cache/bs_avg_kpi_1_12')
  bs_chnn_kpi_1_12 = readRDS('cache/bs_chnn_kpi_1_12')
  
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
  
  saveRDS(avg1, 'cache/avg1', compress=F)
  saveRDS(chnn1, 'cache/chnn1', compress=F)
} else {
  avg1 = readRDS('cache/avg1')
  chnn1 = readRDS('cache/chnn1')
}

if (F) {
  bs_avg_kpi_2 = readRDS('cache/bs_avg_kpi_2')
  bs_chnn_kpi_2 = readRDS('cache/bs_chnn_kpi_2')
  
  avg2 = bs_avg_kpi_2 %>% group_by(CELL_LAC_ID) %>% summarise(
    PART_CQI_QPSK_LTE = mean(PART_CQI_QPSK_LTE, na.rm=T),
    PART_MCS_QPSK_LTE = mean(PART_MCS_QPSK_LTE, na.rm=T),
    #PROC_LOAD_3G = mean(PROC_LOAD_3G, na.rm=T),                      #пустая колонка
    PSSR_2G = mean(PSSR_2G, na.rm=T),
    PSSR_3G = mean(PSSR_3G, na.rm=T),
    PSSR_LTE = mean(PSSR_LTE, na.rm=T),
    RAB_CS_BLOCKING_RATE_3G = mean(RAB_CS_BLOCKING_RATE_3G, na.rm=T),
    RAB_CS_DROP_RATE_3G = mean(RAB_CS_DROP_RATE_3G, na.rm=T),
    RAB_PS_BLOCKING_RATE_3G = mean(RAB_PS_BLOCKING_RATE_3G, na.rm=T),
    RAB_PS_DROP_RATE_3G = mean(RAB_PS_DROP_RATE_3G, na.rm=T),
    RBU_AVAIL_DL = mean(RBU_AVAIL_DL, na.rm=T)
    #RBU_AVAIL_DL_LTE = mean(RBU_AVAIL_DL_LTE, na.rm=T)               #пустая колонка
  )
  
  # aaa=as.data.frame(chnn3); for (ccc in colnames(aaa)) if (sum(!is.nan(aaa[,ccc]))==0) print(ccc)
  
  chnn2 = bs_chnn_kpi_2 %>% group_by(CELL_LAC_ID) %>% summarise(
    AVR_UL_USER_LTE = mean(AVR_UL_USER_LTE, na.rm=T),
    DL_AVR_THROUGHPUT_3G = mean(DL_AVR_THROUGHPUT_3G, na.rm=T),
    DL_AVR_THROUGHPUT_LTE = mean(DL_AVR_THROUGHPUT_LTE, na.rm=T),
    DL_AVR_THROUGHPUT_R99 = mean(DL_AVR_THROUGHPUT_R99, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_LTE = mean(DL_MEAN_USER_THROUGHPUT_LTE, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_DL_2G = mean(DL_MEAN_USER_THROUGHPUT_DL_2G, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_HSPA3G = mean(DL_MEAN_USER_THROUGHPUT_HSPA3G, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_PLTE = mean(DL_MEAN_USER_THROUGHPUT_PLTE, na.rm=T),
    DL_MEAN_USER_THROUGHPUT_REL93G = mean(DL_MEAN_USER_THROUGHPUT_REL93G, na.rm=T),
    #HSDPA_USERS_3G = mean(HSDPA_USERS_3G, na.rm=T),    #пустая колонка
    #HSUPA_USERS_3G = mean(HSUPA_USERS_3G, na.rm=T),    #пустая колонка
    RBU_USED_DL = mean(RBU_USED_DL, na.rm=T)
  )
  saveRDS(avg2, 'cache/avg2', compress=F)
  saveRDS(chnn2, 'cache/chnn2', compress=F)
} else {
  avg2 = readRDS('cache/avg2')
  chnn2 = readRDS('cache/chnn2')
}

if (F) {
  bs_avg_kpi_3 = readRDS('cache/bs_avg_kpi_3')
  bs_avg_kpi_4 = readRDS('cache/bs_avg_kpi_4')
  bs_chnn_kpi_3 = readRDS('cache/bs_chnn_kpi_3')
  
  avg3 = bs_avg_kpi_3 %>% group_by(CELL_LAC_ID) %>% summarise(
    RBU_AVAIL_UL = mean(RBU_AVAIL_UL, na.rm=T),
    RBU_OTHER_DL = mean(RBU_OTHER_DL, na.rm=T),
    RBU_OTHER_UL = mean(RBU_OTHER_UL, na.rm=T),
    RBU_OWN_DL = mean(RBU_OWN_DL, na.rm=T),
    RBU_OWN_UL = mean(RBU_OWN_UL, na.rm=T),
    RRC_BLOCKING_RATE_3G = mean(RRC_BLOCKING_RATE_3G, na.rm=T),
    RRC_BLOCKING_RATE_LTE = mean(RRC_BLOCKING_RATE_LTE, na.rm=T),
    #RTWP_3G = mean(RTWP_3G, na.rm=T),            ####
    SHO_FACTOR = mean(SHO_FACTOR, na.rm=T),
    TBF_DROP_RATE_2G = mean(TBF_DROP_RATE_2G, na.rm=T),
    TCH_DROP_RATE_2G = mean(TCH_DROP_RATE_2G, na.rm=T)
    #UTIL_BRD_CPU_3G = mean(UTIL_BRD_CPU_3G, na.rm=T)  ####
  )
  
  avg4 = bs_avg_kpi_4 %>% group_by(CELL_LAC_ID) %>% summarise(
    UTIL_CE_DL_3G = mean(UTIL_CE_DL_3G, na.rm=T),
    #UTIL_CE_HW_DL_3G = mean(UTIL_CE_HW_DL_3G, na.rm=T),  #####
    #UTIL_CE_UL_3G = mean(UTIL_CE_UL_3G, na.rm=T),        #####
    #UTIL_SUBUNITS_3G = mean(UTIL_SUBUNITS_3G, na.rm=T),  #####
    UL_VOLUME_LTE = mean(UL_VOLUME_LTE, na.rm=T),
    DL_VOLUME_LTE = mean(DL_VOLUME_LTE, na.rm=T),
    TOTAL_DL_VOLUME_3G = mean(TOTAL_DL_VOLUME_3G, na.rm=T),
    TOTAL_UL_VOLUME_3G = mean(TOTAL_UL_VOLUME_3G, na.rm=T)
  )
  
  chnn3 = bs_chnn_kpi_3 %>% group_by(CELL_LAC_ID) %>% summarise(
    RBU_USED_UL = mean(RBU_USED_UL, na.rm=T),
    RELATIVE_RBU_USED_DL = mean(RELATIVE_RBU_USED_DL, na.rm=T),
    RELATIVE_RBU_USED_UL = mean(RELATIVE_RBU_USED_UL, na.rm=T),
    RELATIVE_TX_POWER_3G = mean(RELATIVE_TX_POWER_3G, na.rm=T),
    UL_AVR_THROUGHPUT_3G = mean(UL_AVR_THROUGHPUT_3G, na.rm=T),
    UL_AVR_THROUGHPUT_LTE = mean(UL_AVR_THROUGHPUT_LTE, na.rm=T),
    UL_AVR_THROUGHPUT_R99 = mean(UL_AVR_THROUGHPUT_R99, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_LTE = mean(UL_MEAN_USER_THROUGHPUT_LTE, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_HS3G = mean(UL_MEAN_USER_THROUGHPUT_HS3G, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_PLTE = mean(UL_MEAN_USER_THROUGHPUT_PLTE, na.rm=T),
    UL_MEAN_USER_THROUGHPUT_REL93G = mean(UL_MEAN_USER_THROUGHPUT_REL93G, na.rm=T)
  )
  saveRDS(avg3, 'cache/avg3', compress=F)
  saveRDS(avg4, 'cache/avg4', compress=F)
  saveRDS(chnn3, 'cache/chnn3', compress=F)
} else {
  avg3 = readRDS('cache/avg3')
  avg4 = readRDS('cache/avg4')
  chnn3 = readRDS('cache/chnn3')
}

avg1 = left_join(avg1, avg2, 'CELL_LAC_ID')
avg1 = left_join(avg1, avg3, 'CELL_LAC_ID')
avg1 = left_join(avg1, avg4, 'CELL_LAC_ID')
chnn1 = left_join(chnn1, chnn2, 'CELL_LAC_ID')
chnn1 = left_join(chnn1, chnn3, 'CELL_LAC_ID')

bd = full_join(avg1, chnn1, 'CELL_LAC_ID')
saveRDS(bd, 'cache/bd')
