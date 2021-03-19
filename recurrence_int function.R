# recurrence_int = function(site) {
# 
#   final = final_df(site)
#   l = build_l(site)
# 
#   df = data.frame()
# 
#   nwis_int = l[[1]]
#   nwm_int = l[[2]]
#   change_int = 100 * ((l[[1]] - l[[2]] ) / l[[1]])
# 
#   df = df %>%
#     rbind(df, nwis_int) %>%
#     rbind(df, nwm_int) %>%
#     rbind(df, change_int)
# 
#   colnames(df) = c(
#     "yr002",
#     "yr010",
#     "yr025",
#     "yr100"
#   )
# 
#   df = cbind(Model = c("NWIS", "NWM", "Norm"), df)
#   df$COMID = mapping$feature_id[site]
#   df$NWIS_ID = mapping$site_no[site]
# 
#   mannKendall <- trend::mk.test(final$nwis_cms, alternative = c("two.sided"), continuity = TRUE)
#   mannKendall1 <- trend::mk.test(final$nwm_cms, alternative = c("two.sided"), continuity = TRUE)
# 
#   df = cbind(Z_score = c(mannKendall$statistic, mannKendall1$statistic, NA), df)
#   df = cbind(p_value = c(mannKendall$p.value, mannKendall1$p.value, NA), df)
# 
#   x <- final$date
#   y <- final$nwm_cms
#   n <- length(x)
#   slopes <- unlist(pbapply::pblapply(1:(n - 1), slopediff, x, y, n))
#   slope  <- median(slopes[is.finite(slopes)])
# 
#   x <- final$date
#   y <- final$nwis_cms
#   n <- length(x)
#   slopes <- unlist(pbapply::pblapply(1:(n - 1), slopediff, x, y, n))
#   slope1  <- median(slopes[is.finite(slopes)])
# 
#   df = cbind(sens = c(slope1, slope, NA), df)
#   df = df[, c(6,7,1,2,3,4,5)]
# 
#   df1 = df %>%
#     select(COMID, NWIS_ID, Model, yr002, yr010, yr025, yr100)
#   df1 = df1 %>%
#     tidyr::pivot_longer(
#       cols = starts_with("yr"),
#       names_to = "Type",
#       values_to = "Value")
# 
#   df2 = df %>%
#     select(COMID, NWIS_ID, Model, Z_score) %>%
#     na.omit()
#   df2 = df2 %>%
#     tidyr::pivot_longer(
#       cols = ends_with("score"),
#       names_to = "Type",
#       values_to = "Value")
# 
#   df3 = df %>%
#     select(COMID, NWIS_ID, Model, p_value) %>%
#     na.omit()
#   df3 = df3 %>%
#     tidyr::pivot_longer(
#       cols = ends_with("value"),
#       names_to = "Type",
#       values_to = "Value")
# 
#   df4 = df %>%
#     select(COMID, NWIS_ID, Model, sens) %>%
#     na.omit()
#   df4 = df4 %>%
#     tidyr::pivot_longer(
#       cols = ends_with("sens"),
#       names_to = "Type",
#       values_to = "Value")
# 
#   df5 = rbind(df1, df2, df3, df4)
# 
#   return(df5)
# }

#correction to recurrence_int function to fix edge cases
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
        "yr001",
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
      
      # mannKendall <- trend::mk.test(final$nwis_cms, alternative = c("two.sided"), continuity = TRUE)
      # mannKendall1 <- trend::mk.test(final$nwm_cms, alternative = c("two.sided"), continuity = TRUE)
      
      # df = cbind(Z_score = c(mannKendall$statistic, mannKendall1$statistic, NA), df)
      # df = cbind(p_value = c(mannKendall$p.value, mannKendall1$p.value, NA), df)
      
      # x <- final$date
      # y <- final$nwm_cms
      # n <- length(x)
      # slopes <- unlist(pbapply::pblapply(1:(n - 1), slopediff, x, y, n))
      # slope  <- median(slopes[is.finite(slopes)])
      # 
      # x <- final$date
      # y <- final$nwis_cms
      # n <- length(x)
      # slopes <- unlist(pbapply::pblapply(1:(n - 1), slopediff, x, y, n))
      # slope1  <- median(slopes[is.finite(slopes)])
      # 
      # df = cbind(sens = c(slope1, slope, NA), df)
      df = df[, c(11,12,1,2,3,4,5,6,7,8,9,10)]
      
      # df1 = df %>% 
      #   select(COMID, NWIS_ID, Model, yr002, yr010, yr025, yr100)
      df = df %>% 
        tidyr::pivot_longer(
          cols = starts_with("yr"),
          names_to = "Type",
          values_to = "Value") 
      
      # df2 = df %>% 
      #   select(COMID, NWIS_ID, Model, Z_score) %>% 
      #   na.omit()
      # df2 = df2 %>% 
      #   tidyr::pivot_longer(
      #     cols = ends_with("score"),
      #     names_to = "Type",
      #     values_to = "Value")
      # 
      # df3 = df %>% 
      #   select(COMID, NWIS_ID, Model, p_value) %>% 
      #   na.omit()
      # df3 = df3 %>% 
      #   tidyr::pivot_longer(
      #     cols = ends_with("value"),
      #     names_to = "Type",
      #     values_to = "Value")
      # 
      # df4 = df %>% 
      #   select(COMID, NWIS_ID, Model, sens) %>% 
      #   na.omit()
      # df4 = df4 %>% 
      #   tidyr::pivot_longer(
      #     cols = ends_with("sens"),
      #     names_to = "Type",
      #     values_to = "Value")
      # 
      # df5 = rbind(df1, df2, df3, df4)
      
      return(df)
    }
  }
}
