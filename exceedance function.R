# exceedance = function(site){
#   
#   final = final_df(site)
#   l = build_l(site)
#   
#   dfnew = final %>% 
#     na.omit() %>% 
#     select(date, nwm_cms) %>% 
#     group_by(year = lubridate::year(date)) %>% 
#     summarize(nwm2 = sum(nwm_cms >= l[[2]][1]),
#               nwm10 = sum(nwm_cms >= l[[2]][2]),
#               nwm25 = sum(nwm_cms >= l[[2]][3]),
#               nwm100 = sum(nwm_cms >= l[[2]][4]))
#   
#   dfnew1 = final %>% 
#     na.omit() %>% 
#     select(date, nwis_cms) %>% 
#     group_by(year = lubridate::year(date)) %>% 
#     summarise(nwis2 = sum(nwis_cms >= l[[1]][1]),
#               nwis10 = sum(nwis_cms >= l[[1]][2]),
#               nwis25 = sum(nwis_cms >= l[[1]][3]),
#               nwis100 = sum(nwis_cms >= l[[1]][4]))
#   
#   dfnew = merge(x = dfnew, y = dfnew1, by = "year", all.x = TRUE)
#   
#   dfnew[is.na(dfnew)] = 0
#   
#   return(dfnew)
# }


#correction to exceedance function to fix edge cases
exceedance = function(site){
  
  final = final_df(site)
  df = data.frame(final)
  if(is.null(df[1,1])){
    return(NULL)
  } else{
    l = build_l(site)
    df1 = data.frame(l)
    if(is.null(df1[1,1])){
      return(NULL)
    } else{
      dfnew = final %>% 
        na.omit() %>% 
        select(date, nwm_cms) %>% 
        group_by(year = lubridate::year(date)) %>% 
        summarize(nwmBF = sum(nwm_cms >= l[[2]][1]),
                  nwm2 = sum(nwm_cms >= l[[2]][2]),
                  nwm5 = sum(nwm_cms >= l[[2]][3]),
                  nwm10 = sum(nwm_cms >= l[[2]][4]),
                  nwm25 = sum(nwm_cms >= l[[2]][5]),
                  nwm50 = sum(nwm_cms >= l[[2]][6]),
                  nwm100 = sum(nwm_cms >= l[[2]][7]),
                  nwm250 = sum(nwm_cms >= l[[2]][8]),
                  nwm500 = sum(nwm_cms >= l[[2]][9]))
      
      dfnew1 = final %>% 
        na.omit() %>% 
        select(date, nwis_cms) %>% 
        group_by(year = lubridate::year(date)) %>% 
        summarise(nwisBF = sum(nwis_cms >= l[[1]][1]),
                  nwis2 = sum(nwis_cms >= l[[1]][2]),
                  nwis5 = sum(nwis_cms >= l[[1]][3]),
                  nwis10 = sum(nwis_cms >= l[[1]][4]),
                  nwis25 = sum(nwis_cms >= l[[1]][5]),
                  nwis50 = sum(nwis_cms >= l[[1]][6]),
                  nwis100 = sum(nwis_cms >= l[[1]][7]),
                  nwis250 = sum(nwis_cms >= l[[1]][8]),
                  nwis500 = sum(nwis_cms >= l[[1]][9]))
      
      dfnew = merge(x = dfnew, y = dfnew1, by = "year", all.x = TRUE)
      
      dfnew[is.na(dfnew)] = 0
      
      return(dfnew)
    }
  }
}
