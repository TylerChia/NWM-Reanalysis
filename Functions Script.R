library(dplyr)
# NWIS download
library(dataRetrieval)  
# time series helpers
library(xts)
library(tsbox)
library(tseries)
# Trends and plotting
library(trend)
library(dygraphs)
library(RNetCDF)
mapping    = readRDS("/Users/TylerC/Desktop/Research_Assistant_Stuff/mapping.rds")
nwis       = open.nc("/Users/TylerC/Desktop/Research_Assistant_Stuff/nwis_v20_daily.nc")
nwm        = open.nc("/Users/TylerC/Desktop/Research_Assistant_Stuff/nwm_v20_hourly.nc")

#gamma function
Ky_gamma <- function(g, aep){
  
  if(abs(g) < 1e-8) { # Use the Wilson-Hilferty approximation to avoid numerical
    # issues near zero
    z <- qnorm(1-aep)
    b <- g/6
    return(z + (z^2 - 1)*b + (1/3)*(z^3 - 6*z)*b^2 - 
             (z^2 - 1)*b^3 + z*b^4 - (1/3)*(b)^5 )
  }
  
  if(g < 0) aep  <- 1 - aep
  a  <- 4/(g^2)
  b  <- 1/sqrt(a)
  sign(g)*(qgamma(1 - aep, shape = a, scale = b) - a*b)
}

#set years
yr = c(2,10,25,100)

#function for slope used in recurrence_int
slopediff <- function(i, xx, yy, n){
  start = 1:(n - i)
  end   = (i + 1):n
  (yy[start] - yy[end])/as.numeric(xx[start] - xx[end])
}

#function for final dataframe used in recurrence_int and exceedance
final_df  = function(site) {
  
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
  
  merge(nwm_flow, usgs_flow, by = "date") %>% 
    select(comid, date, nwm_cms, nwis_cms) %>% 
    na.omit()
}

#function for l used in recurrence_int and exceedance
build_l = function(site) {
  final = final_df(site)
  finalPeak = final %>% 
    group_by(year  = format(date, "%Y")) %>% 
    summarize(NWM  = log(max(nwm_cms)), 
              NWIS = log(max(nwis_cms))) %>% 
    tidyr::pivot_longer(-year) %>% 
    group_by(name) %>% 
    summarise(m = mean(value), s = sd(value), g = e1071::skewness(value, type=2))
  
  lapply(1:2, function(x){ exp(finalPeak$m[x] + finalPeak$s[x] * Ky_gamma(finalPeak$g[x], 1/yr))}) 
}



