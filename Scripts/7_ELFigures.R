#Plotting Data
  #Set up
    if(Sys.info()["sysname"]=="Windows"){sdrive <- "S:/"}
    if(Sys.info()["sysname"]=="Linux"){sdrive <- "/work/sf/internal/l1werp20.sf.frb.org/Shared/"}
    setwd(paste0(sdrive,"./Noah K/Tatevik/ISO"))

    Sys.setenv(http_proxy  =  "http://p1web5.frb.org:8080")
    Sys.setenv(https_proxy =  "https://p1web5.frb.org:8080")
    Sys.setenv(HTTP_PROXY  = "http://p1web5.frb.org:8080")
    Sys.setenv(HTTPS_PROXY = "https://p1web5.frb.org:8080")
    Sys.setenv(NO_PROXY    =  ".frb.org,.frb.gov,.frbres.org")
    
    
    source(paste0(sdrive,"Monroe/ERcharts/r_backend/functions.R"));load_colors()
    source(paste0(sdrive,"Noah K/Functions.R"))
    
    library(tidyverse)
    library(lubridate)
    library(seasonal)
    library(zoo)
    library(gridExtra)
  
#Lets load the data
  load("./Data/ca_daily.Rdata")  
  #load("./Data/ca_work_daily.Rdata")  
  #load("./Data/ca_weekly.Rdata")  
  #load("./Data/ca_work_weekly.Rdata")  
  #load("./Data/ca_monthly.Rdata")  
  #load("./Data/ca_work_monthly.Rdata")  
  
  #ca_macro <- read.csv("./Data/california_macro_indicators.csv")
  #ca_macro$eop_date <- as.Date(ca_macro$eop_date)
  
  edison <- read.csv("./Data/Edison.csv", skip = 13,header = FALSE)%>%
    select(date = V2, pac_sw = V3, us_tot = V4)%>%
    mutate(date= as.Date(as.character(date),"%Y%m%d"))%>%
    pivot_longer(-date)
    
#Set Key date
  ca_sip <- as.Date("2020-03-19")
  
#EL Figure 1
  start_date <- as.Date("2020-01-01")
  end_date   <- Sys.Date()
  
  df_1 <- ca_daily
  df_1$MW_sa_lm_MA7 <- rollapply(df_1$MW_sa_lm, width = 7, align = 'right', fill = NA, FUN = mean, na.rm=TRUE)
  df_1 <- df_1 %>% select(date, MW_sa_lm, MW_sa_lm_MA7) %>% pivot_longer(-date)
  #df_1 <- bind_rows(df_1, edison)

  df_1 <- df_1 %>% group_by(name) %>% 
    mutate(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(percent_of_mean = (value/mean)*100)
  
  EL_1 <- ggplot(data = df_1)+
    get_recessions(window_start=start_date, 
                   window_end = end_date)+
    geom_vline(xintercept = ca_sip, col = "red")+
    #annotate("text", x = ca_sip, y = max(y), label = "CA S.I.P.", col = "red")+
    geom_line(aes(x=date,y=percent_of_mean, col = name), size = 1.25)+
    scale_x_date(limits = c(start_date, end_date))+
    scale_y_continuous(limits = c(85, 110), labels = function(x){paste0(x-100,"%")}, name = "deviation from mean")+
    scale_color_manual(labels = c("Weather/Weekday Corrected","7 day MA of corrected"),
                         values = c(col4, col1))+
    labs(title = "Recent Change in California Electric Demand", caption = "Note: Date of statewide shelter in place order shown in red.\nSource: California ISO" )+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="top", legend.direction = "horizontal")
  
  ggsave("./figures/EL_Fig_1_Recent_Centered_Demand_Series.png", EL_1,
         width = 12, height = 8)

  
  
#El Figure 2: Whole Daily Series: Raw, Corrected, MA
  start_date <- as.Date("2009-04-01")
  end_date   <- Sys.Date()
  df_2 <- ca_daily
  df_2$MW_sa_lm_MA7 <- rollapply(df_2$MW_sa_lm, width = 7, align = 'right', fill = NA, FUN = mean, na.rm=TRUE)
  df_2 <- df_2 %>% select(date, MW, MW_sa_lm, MW_sa_lm_MA7) %>% pivot_longer(-date)
  
  EL_2 <- ggplot(data = df_2)+
    get_recessions(window_start=start_date, 
                   window_end = end_date)+
    geom_vline(xintercept = ca_sip, col = "red")+
    #annotate("text", x = ca_sip, y = max(df_2$value[]), label = "CA S.I.P.", col = "red")+
    geom_line(aes(x=date,y=value/1000, col = name))+
    labs(title = "California Electric Usage", 
         caption = "Note: Date of statewide shelter in place order shown in red.\nSource: California ISO",
         y = "Gigawatts")+
    scale_color_manual(labels = c("Raw Demand","Weather/Weekday Corrected","7 day MA of corrected"),
                       values = c(col3,col4,col1))+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="top", legend.direction = "horizontal")
  
  ggsave("./figures/EL_Fig_2_Long_Demand_Series.png", EL_2, width = 12, height = 8)
    
#FIGURE 6: Adjusted Across Years
  start_date <- as.Date("2020-01-01")
  end_date   <- as.Date("2020-12-31")
  
  df_3 <- ca_daily
  df_3$year <- year(df_3$date)
  df_3 <- df_3[df_3$year > year(Sys.Date())-5,]
  df_3$month <- month(df_3$date)
  df_3$day   <- day(df_3$date)
  df_3$date <- as.Date(paste0("2020-",df_3$month,"-",df_3$day))
  df_3$year <- as.character(df_3$year)
  
  EL_3 <- ggplot(data = df_3)+
    geom_vline(xintercept = ca_sip, col = "red")+
    geom_line(aes(x=date,y=MW_sa_lm_MA7/1000, col = year), size = 1.25)+
    labs(title = "CA ISO Electricity Usage: Comparing Recent Years",
         y = "Gigawatts",
         caption = "Note: Date of statewide shelter in place order shown in red.\nNote: Series are 7 day moving averages of weather/weekday corrected electric demand.\nSource: California ISO")+
    scale_x_date(limits = c(start_date, end_date))+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="top", legend.direction = "horizontal")+
    scale_color_manual(values = c(col1,col2,col3,col4,col5))
  
  ggsave("./figures/EL_Fig_3_Adjusted_Demand_Over_Recent_Years.png", EL_3,
         width = 12, height = 8)
  
#Figure 4: MIDAS
  library("midasr")
  
  ca_gdp <- read.csv("./Data/ca_rgdp_q.csv")%>%
    mutate(date = as.Date(paste0(substr(Date,1,4),"-",as.numeric(substr(Date,7,7))*3-2,"-01")))
  
  df_4 <-merge(ca_gdp, ca_daily, all.y = TRUE)
  
  df_4 <- df_4 %>%
    mutate(yq = paste0(year(date),":",quarter(date)))%>%
    group_by(yq)%>%
    summarize(date = first(date), MW_sa_lm = sum(MW_sa_lm), rGDP = mean(rGDP, na.rm = TRUE))%>%
    mutate(l1_rGDP = lag(rGDP,1),
           l2_rGDP = lag(rGDP,2),
           l1_mw_sa_lm = lag(MW_sa_lm,1),
           l2_mw_sa_lm = lag(MW_sa_lm,2))

  model <- lm(rGDP ~ MW_sa_lm + l1_mw_sa_lm + l2_mw_sa_lm + l1_rGDP + l2_rGDP,
              data = df_4)
  model
  
  midas <- midas_r(rGDP ~ MW_sa_lm + l1_mw_sa_lm + l2_mw_sa_lm + l1_rGDP + l2_rGDP,
              data = df_4)
  midas
  
  