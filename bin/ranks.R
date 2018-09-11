mhm_dv_sca <- mhm_dv[PT_ID %in% sca_sp, .(DTM, variable, value)] 
mhm_dv_sca[, sdv := sum(value, na.rm = T), .(DTM, variable)]
mhm_dv_sca[, mdv := mean(value, na.rm = T), .(DTM, variable)]

dv_sca <- merge(unique(mhm_dv_sca[, .(DTM, mdv, variable)]), 
                unique(mhm_dv_sca[, .(DTM, sdv, variable)]), 
                c('DTM', 'variable'))

dv_sca[, mrank := rank(-mdv), variable]
dv_sca[, srank := rank(-sdv), variable]
dv_sca[, mexceed := mrank/.N, variable]
dv_sca[, sexceed := srank/.N, variable]
dv_sca[DTM >= '2018-01-01']
dv_sca[mexceed < 0.005, .(DTM, mexceed), variable]
dv_sca[sexceed < 0.005, .(DTM, mexceed), variable]

mhm_dv_ger <- mhm_dv[PT_ID %in% ger_sp, .(DTM, variable, value)] 
mhm_dv_ger[, sdv := sum(value, na.rm = T), .(DTM, variable)]
mhm_dv_ger[, mdv := mean(value, na.rm = T), .(DTM, variable)]

dv_ger <- merge(unique(mhm_dv_ger[, .(DTM, mdv, variable)]), 
                unique(mhm_dv_ger[, .(DTM, sdv, variable)]), 
                c('DTM', 'variable'))

dv_ger[, mrank := rank(-mdv), variable]
dv_ger[, srank := rank(-sdv), variable]
dv_ger[, mexceed := mrank/.N, variable]
dv_ger[, sexceed := srank/.N, variable]
dv_ger[DTM >= '2018-01-01']
dv_ger[mexceed < 0.005, .(DTM, mexceed), variable]
dv_ger[sexceed < 0.005, .(DTM, mexceed), variable]

mhm_dv_cz <- mhm_dv[PT_ID %in% cz_sp, .(DTM, variable, value)] 
mhm_dv_cz[, sdv := sum(value, na.rm = T), .(DTM, variable)]
mhm_dv_cz[, mdv := mean(value, na.rm = T), .(DTM, variable)]

dv_cz <- merge(unique(mhm_dv_cz[, .(DTM, mdv, variable)]), 
                unique(mhm_dv_cz[, .(DTM, sdv, variable)]), 
                c('DTM', 'variable'))

dv_cz[, mrank := rank(-mdv), variable]
dv_cz[, srank := rank(-sdv), variable]
dv_cz[, mexceed := mrank/.N, variable]
dv_cz[, sexceed := srank/.N, variable]
dv_cz[DTM >= '2018-01-01']
dv_cz[mexceed < 0.005, .(DTM, mexceed), variable]
dv_cz[sexceed < 0.005, .(DTM, mexceed), variable]
