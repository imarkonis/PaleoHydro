library(data.table); library(ggplot2)

add_prv_yr <- function(drought, dataset = dta){
  aa <- unique(drought[, .(PT_ID, yr)]) 
  aa_prv <- aa
  aa$event = factor('cur_yr')
  aa_prv$event = factor('prv_yr')
  aa_prv[, yr := yr - 1] 
  aa <- rbind(aa, aa_prv)
  out <- merge(aa, dataset)
  return(out)
}

add_nxt_yr <- function(drought, dataset = dta){
  aa <- unique(drought[, .(PT_ID, yr)]) 
  aa_nxt <- aa
  aa$event = factor('cur_yr')
  aa_nxt$event = factor('nxt_yr')
  aa_nxt[, yr := yr + 1] 
  aa <- rbind(aa, aa_nxt)
  out <- merge(aa, dataset)
  return(out)
}
