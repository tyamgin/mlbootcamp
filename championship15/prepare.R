read_data = function (name) {
  read.csv(file=paste0("data/", name, ".csv"), head=T, sep=";", dec=",")
}

subs_features_train = read_data("subs_features_train")
subs_csi_train = read_data("subs_csi_train")
subs_bs_data_session_train = read_data("subs_bs_data_session_train")
subs_bs_voice_session_train = read_data("subs_bs_voice_session_train")
subs_bs_consumption_train = read_data("subs_bs_consumption_train")

subs_features_test = read_data("subs_features_test")
subs_csi_test = read_data("subs_csi_test")
subs_bs_data_session_test = read_data("subs_bs_data_session_test")
subs_bs_voice_session_test = read_data("subs_bs_voice_session_test")
subs_bs_consumption_test = read_data("subs_bs_consumption_test")

saveRDS(subs_features_train, 'cache/subs_features_train', compress=F)
saveRDS(subs_csi_train, 'cache/subs_csi_train', compress=F)
saveRDS(subs_bs_data_session_train, 'cache/subs_bs_data_session_train', compress=F)
saveRDS(subs_bs_voice_session_train, 'cache/subs_bs_voice_session_train', compress=F)
saveRDS(subs_bs_consumption_train, 'cache/subs_bs_consumption_train', compress=F)

saveRDS(subs_features_test, 'cache/subs_features_test', compress=F)
saveRDS(subs_csi_test, 'cache/subs_csi_test', compress=F)
saveRDS(subs_bs_data_session_test, 'cache/subs_bs_data_session_test', compress=F)
saveRDS(subs_bs_voice_session_test, 'cache/subs_bs_voice_session_test', compress=F)
saveRDS(subs_bs_consumption_test, 'cache/subs_bs_consumption_test', compress=F)

bs_avg_col_classes = c(
  'character',       #T_DATE
  'integer',         #CELL_LAC_ID
  'double',          #CELL_AVAILABILITY_2G
  'double',          #CELL_AVAILABILITY_3G
  'double',          #CELL_AVAILABILITY_4G
  'double',          #CSSR_2G
  'double',          #CSSR_3G
  'double',          #ERAB_PS_BLOCKING_RATE_LTE
  'double',          #ERAB_PS_BLOCKING_RATE_PLMN_LTE
  'double',          #ERAB_PS_DROP_RATE_LTE
  'double',          #HSPDSCH_CODE_UTIL_3G    ???
  'double',          #NODEB_CNBAP_LOAD_HARDWARE    ???
  rep('NULL', 44-12)
)

bs_avg_kpi_1_12 = read.csv(file="data/bs_avg_kpi.csv", head=T, sep=";", dec=",", colClasses=bs_avg_col_classes)
saveRDS(bs_avg_kpi_1_12, 'cache/bs_avg_kpi_1_12')       
  
   
bs_avg_col_classes = c(
  'NULL',            #T_DATE
  'integer',         #CELL_LAC_ID
  rep('NULL', 10),
  rep('double', 12), #PART_CQI_QPSK_LTE, PART_MCS_QPSK_LTE, PROC_LOAD_3G, PSSR_2G, PSSR_3G, PSSR_LTE, 
                     #RAB_CS_BLOCKING_RATE_3G, RAB_CS_DROP_RATE_3G, RAB_PS_BLOCKING_RATE_3G, RAB_PS_DROP_RATE_3G, RBU_AVAIL_DL, RBU_AVAIL_DL_LTE
  rep('NULL', 44-12-12)
)

bs_avg_kpi_2 = read.csv(file="data/bs_avg_kpi.csv", head=T, sep=";", dec=",", colClasses=bs_avg_col_classes)
saveRDS(bs_avg_kpi_2, 'cache/bs_avg_kpi_2') 

bs_avg_col_classes = c(
  'NULL',            #T_DATE
  'integer',         #CELL_LAC_ID
  rep('NULL', 10+12),
  rep('double', 12), #RBU_AVAIL_UL, RBU_OTHER_DL, RBU_OTHER_UL, RBU_OWN_DL, RBU_OWN_UL, RRC_BLOCKING_RATE_3G        
                     #RRC_BLOCKING_RATE_LTE, RTWP_3G, SHO_FACTOR, TBF_DROP_RATE_2G, TCH_DROP_RATE_2G, UTIL_BRD_CPU_3G
  rep('NULL', 44-12-12-12)
)

bs_avg_kpi_3 = read.csv(file="data/bs_avg_kpi.csv", head=T, sep=";", dec=",", colClasses=bs_avg_col_classes)
saveRDS(bs_avg_kpi_3, 'cache/bs_avg_kpi_3')

bs_avg_col_classes = c(
  'NULL',            #T_DATE
  'integer',         #CELL_LAC_ID
  rep('NULL', 10+12+12),
  rep('double', 8)   #UTIL_CE_DL_3G, UTIL_CE_HW_DL_3G, UTIL_CE_UL_3G, UTIL_SUBUNITS_3G, UL_VOLUME_LTE, DL_VOLUME_LTE, TOTAL_DL_VOLUME_3G, TOTAL_UL_VOLUME_3G
)

bs_avg_kpi_4 = read.csv(file="data/bs_avg_kpi.csv", head=T, sep=";", dec=",", colClasses=bs_avg_col_classes)
saveRDS(bs_avg_kpi_4, 'cache/bs_avg_kpi_4')


bs_chnn_kpi_col_classes = c(
  'character',      #T_DATE
  'integer',        #CELL_LAC_ID
  'double',         #AVEUSERNUMBER
  'double',         #AVEUSERNUMBER_PLMN
  'double',         #AVR_DL_HSPA_USER_3G
  'double',         #AVR_DL_R99_USER_3G
  'double',         #AVR_DL_USER_3G
  'double',         #AVR_DL_USER_LTE
  'double',         #AVR_TX_POWER_3G
  'double',         #AVR_UL_HSPA_USER
  'double',         #AVR_UL_R99_USER
  'double',         #AVR_UL_USER_3G
  rep('NULL', 35-12)
)

bs_chnn_kpi_1_12 = read.csv(file="data/bs_chnn_kpi.csv", head=T, sep=";", dec=",", colClasses=bs_chnn_kpi_col_classes)
saveRDS(bs_chnn_kpi_1_12, 'cache/bs_chnn_kpi_1_12') 
           
bs_chnn_kpi_col_classes = c(
  'NULL',           #T_DATE
  'integer',        #CELL_LAC_ID
  rep('NULL', 10),
  rep('double', 12),#AVR_UL_USER_LTE, DL_AVR_THROUGHPUT_3G, DL_AVR_THROUGHPUT_LTE,DL_AVR_THROUGHPUT_R99,DL_MEAN_USER_THROUGHPUT_LTE,DL_MEAN_USER_THROUGHPUT_DL_2G
                    #DL_MEAN_USER_THROUGHPUT_HSPA3G, DL_MEAN_USER_THROUGHPUT_PLTE, DL_MEAN_USER_THROUGHPUT_REL93G,HSDPA_USERS_3G,HSUPA_USERS_3G,RBU_USED_DL
  rep('NULL', 35-12-12)
)

bs_chnn_kpi_2 = read.csv(file="data/bs_chnn_kpi.csv", head=T, sep=";", dec=",", colClasses=bs_chnn_kpi_col_classes)
saveRDS(bs_chnn_kpi_2, 'cache/bs_chnn_kpi_2') 

bs_chnn_kpi_1_12 = read.csv(file="data/bs_chnn_kpi.csv", head=T, sep=";", dec=",", colClasses=bs_chnn_kpi_col_classes)
saveRDS(bs_chnn_kpi_1_12, 'cache/bs_chnn_kpi_1_12') 

bs_chnn_kpi_col_classes = c(
  'NULL',           #T_DATE
  'integer',        #CELL_LAC_ID
  rep('NULL', 10+12),
  rep('double', 11) #RBU_USED_UL,RELATIVE_RBU_USED_DL,RELATIVE_RBU_USED_UL,RELATIVE_TX_POWER_3G,UL_AVR_THROUGHPUT_3G,UL_AVR_THROUGHPUT_LTE"         
                    #UL_AVR_THROUGHPUT_R99, UL_MEAN_USER_THROUGHPUT_LTE, UL_MEAN_USER_THROUGHPUT_HS3G, UL_MEAN_USER_THROUGHPUT_PLTE, UL_MEAN_USER_THROUGHPUT_REL93G
)

bs_chnn_kpi_3 = read.csv(file="data/bs_chnn_kpi.csv", head=T, sep=";", dec=",", colClasses=bs_chnn_kpi_col_classes)
saveRDS(bs_chnn_kpi_3, 'cache/bs_chnn_kpi_3')
