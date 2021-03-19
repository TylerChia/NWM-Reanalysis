recurrence_int = function(site) {
  final = final_df(site)
  df1 = data.frame(final)
  if(is.null(df1[1,1])){
    return(NULL)
  } else {
    l = build_l(site)
    df2 = data.frame(l)
    if(is.null(df2[1,1])){
      return(NULL)
    } else {
      df = data.frame()
      
      nwis_int = l[[1]]
      nwm_int = l[[2]]
      change_int = 100 * ((l[[1]] - l[[2]] ) / l[[1]])
      
      df = df %>% 
        rbind(df, nwis_int) %>% 
        rbind(df, nwm_int) %>% 
        rbind(df, change_int) 
      
      colnames(df) = c(
        "yrBF",
        "yr002",
        "yr005",
        "yr010",
        "yr025",
        "yr050",
        "yr100",
        "yr250",
        "yr500"
      )
      
      df = cbind(Model = c("NWIS", "NWM", "Norm"), df)
      df$COMID = mapping$feature_id[site]
      df$NWIS_ID = mapping$site_no[site]
      
      df = df[, c(11,12,1,2,3,4,5,6,7,8,9,10)]
      
      df = df %>% 
        tidyr::pivot_longer(
          cols = starts_with("yr"),
          names_to = "Type",
          values_to = "Value") 
      
      exceed = exceedance(site)
      exceed1 = exceed %>%
        filter(year <= 2005) %>% 
        select(-year) %>%
        colSums() %>% 
        as.vector() 
      
      exceed1 = exceed1[c(10,11,12,13,14,15,16,17,18,1,2,3,4,5,6,7,8,9)]
      exceed1 = append(exceed1, c(NA,NA,NA,NA,NA,NA,NA,NA,NA))
      df$exceed1 = exceed1
      
      exceed2 = exceed %>% 
        filter(year > 2005) %>% 
        select(-year) %>% 
        colSums() %>% 
        as.vector()
      
      exceed2 = exceed2[c(10,11,12,13,14,15,16,17,18,1,2,3,4,5,6,7,8,9)]
      exceed2 = append(exceed2, c(NA,NA,NA,NA,NA,NA,NA,NA,NA))
      df$exceed2 = exceed2
      
      (test.site = mapping[site,])
      
      nwm_flow <- data.frame(
        time = as.POSIXct(var.get.nc(nwm, "time"), origin = "1970-01-01", tz = 'UTC'),
        Q    = var.get.nc(nwm, "streamflow",
                          start = c(1, test.site$nwm_retro_id), 
                          count = c(NA, 1),
                          unpack = TRUE)) %>%
        group_by(date   = as.Date(time)) %>%
        summarise(nwm_cms = mean(Q), comid = test.site$feature_id)
      
      usgs_flow = data.frame(
        date = as.Date(var.get.nc(nwis, "time"), origin = "1970-01-01", tz = 'UTC'),
        nwis_cms    = var.get.nc(nwis, "streamflow",
                                 start = c(1, test.site$nwis_retro_id), 
                                 count = c(NA, 1),
                                 unpack = TRUE),
        site_id = test.site$site_no)
      
      nwm_count = nrow(na.omit(nwm_flow))
      nwis_count = nrow(na.omit(usgs_flow))

      totalcount = c(rep(as.numeric(nwis_count), times = 9), rep(as.numeric(nwm_count), times = 9), rep(NA, times = 9))
      df$total_count = totalcount
      
      minyear = min(format(final$date, format = "%Y"))
      maxyear = max(format(final$date, format = "%Y"))
      minyear1 = c(rep(minyear, times = 9), rep("1993", times = 9), rep(NA, times = 9)) 
      maxyear1 = c(rep(maxyear, times = 9), rep("2018", times = 9), rep(NA, times = 9))

      df$minyear = minyear1
      df$maxyear = maxyear1
      return(df)
    }
  }
}

# set up the interval
ints = 1:nrow(mapping)

# run function over the interval and add each element to a list
vec = list()
for(i in 1:length(ints)){
  vec[[i]] = recurrence_int((ints[i]))
}

# row bind the list into a dataframe
dataframe1 = dplyr::bind_rows(vec)

# export the dataframe
write.csv(dataframe, "recurrence_int.csv", row.names = F)
