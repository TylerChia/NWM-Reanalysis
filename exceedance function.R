exceedance = function(site){
  
  final = final_df(site)
  l = build_l(site)
  
  dfnew = final %>% 
    na.omit() %>% 
    select(date, nwm_cms) %>% 
    group_by(year = lubridate::year(date)) %>% 
    summarize(nwm2 = sum(nwm_cms >= l[[2]][1]),
              nwm10 = sum(nwm_cms >= l[[2]][2]),
              nwm25 = sum(nwm_cms >= l[[2]][3]),
              nwm100 = sum(nwm_cms >= l[[2]][4]))
  
  dfnew1 = final %>% 
    na.omit() %>% 
    select(date, nwis_cms) %>% 
    group_by(year = lubridate::year(date)) %>% 
    summarise(nwis2 = sum(nwis_cms >= l[[1]][1]),
              nwis10 = sum(nwis_cms >= l[[1]][2]),
              nwis25 = sum(nwis_cms >= l[[1]][3]),
              nwis100 = sum(nwis_cms >= l[[1]][4]))
  
  dfnew = merge(x = dfnew, y = dfnew1, by = "year", all.x = TRUE)
  
  dfnew[is.na(dfnew)] = 0
  
  return(dfnew)
}
