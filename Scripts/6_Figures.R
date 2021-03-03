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
  load("./Data/ca_work_daily.Rdata")  
  load("./Data/ca_weekly.Rdata")  
  load("./Data/ca_work_weekly.Rdata")  
  load("./Data/ca_monthly.Rdata")  
  load("./Data/ca_work_monthly.Rdata")  
  ca_macro <- read.csv("./Data/california_macro_indicators.csv")
  ca_macro$eop_date <- as.Date(ca_macro$eop_date)
  
#Lets mark some key dates
  sfba_sip <-as.Date("2020-03-16")
  ca_sip   <- as.Date("2020-03-19")
  
  
#FIGURE 1: Whole Daily Series: Raw, Corrected, MA
  start_date <- as.Date("2008-11-01")
  end_date   <- Sys.Date()
  data_1 <- ca_daily
  data_1$MW_sa_lm_MA7 <- rollapply(data_1$MW_sa_lm, width = 7, align = 'right', fill = NA, FUN = mean, na.rm=TRUE)
  data_1 <- data_1 %>% select(date, MW, MW_sa_lm, MW_sa_lm_MA7) %>% pivot_longer(-date)
  
  figure_1 <- ggplot(data = data_1)+
    get_recessions(window_start=start_date, 
                   window_end = end_date)+
    geom_vline(xintercept = ca_sip, col = "red")+
    annotate("text", x = ca_sip, y = max(data_1$value[]), label = "CA S.I.P.", col = "red")+
    geom_line(aes(x=date,y=value, col = name))+
    labs(title = "CA ISO Electricity Demand" )+
    scale_color_discrete(labels = c("Raw MW Demanded","Weather/Weekday Corrected Demand","Corrected Demand: 7 day moving average"))+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="bottom", legend.direction = "horizontal")
  ggsave("./figures/1_Long_Demand_Series.png", figure_1, width = 12, height = 8)
    
#FIGURE 2: Recent Daily Series: Raw, Corrected, MA, recent
  start_date <- as.Date("2020-01-01")
  end_date   <- Sys.Date()
  
  data_2 <- data_1
  data_2 <- data_2[data_2$date > start_date & data_2$date < end_date,]
  
  figure_2 <- ggplot(data = data_2)+
    get_recessions(window_start=start_date, 
                   window_end = end_date)+
    geom_vline(xintercept = ca_sip, col = "red")+
    annotate("text", x = ca_sip, y = max(data_2$value[]), label = "CA S.I.P.", col = "red")+
    geom_line(aes(x=date,y=value, col = name), size = 1.25)+
    scale_color_discrete(labels = c("Raw MW Demanded","Weather/Weekday Corrected Demand","Corrected Demand: 7 day moving average"))+
    labs(title = "CA ISO Electricity Demand" )+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="bottom", legend.direction = "horizontal")
  ggsave("./figures/2_Recent_Demand_Series.png", figure_2,
         width = 12, height = 8)

#FIGURE 3: Centered Daily Series: Raw, Corrected, MA
  start_date <- as.Date("2008-11-01")
  end_date   <- Sys.Date()
  
  data_3 <- data_1
  
  data_3 <- data_3 %>% group_by(name) %>% 
              mutate(mean = mean(value, na.rm = TRUE)) %>%
              ungroup() %>%
              mutate(percent_of_mean = (value/mean)*100)
  
  figure_3 <- ggplot(data = data_3)+
    get_recessions(window_start=start_date, 
                   window_end = end_date)+
    geom_vline(xintercept = ca_sip, col = "red")+
    #annotate("text", x = ca_sip, y = max(y), label = "CA S.I.P.", col = "red")+
    geom_line(aes(x=date,y=percent_of_mean, col = name))+
    scale_x_date(limits = c(start_date, end_date))+
    scale_y_continuous(labels = function(x){paste0(x-100,"%")}, name = "deviation from mean")+
    scale_color_discrete(labels = c("Raw","Weather/Weekday Corrected","7 day MA of Corrected"))+
    labs(title = "CA ISO Electricity Demand" )+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="bottom", legend.direction = "horizontal")
  ggsave("./figures/3_Long_Centered_Demand_Series.png", figure_3,
         width = 12, height = 8)
  
#FIGURE 4: Recent Centered Daily Series: Raw, Corrected, MA, recent
  start_date <- as.Date("2020-01-01")
  end_date   <- Sys.Date()
  
  data_4 <- data_1
  
  data_4 <- data_4 %>% group_by(name) %>% 
    mutate(mean = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(percent_of_mean = (value/mean)*100)
  
  figure_4 <- ggplot(data = data_4)+
    get_recessions(window_start=start_date, 
                   window_end = end_date)+
    geom_vline(xintercept = ca_sip, col = "red")+
    #annotate("text", x = ca_sip, y = max(y), label = "CA S.I.P.", col = "red")+
    geom_line(aes(x=date,y=percent_of_mean, col = name), size = 1.25)+
    scale_x_date(limits = c(start_date, end_date))+
    scale_y_continuous(limits = c(75, 110), labels = function(x){paste0(x-100,"%")}, name = "deviation from mean")+
    scale_color_discrete(labels = c("Raw","Weather/Weekday Corrected","7 day MA of Corrected"))+
    labs(title = "CA ISO Electricity Demand" )+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="bottom", legend.direction = "horizontal")
  
  ggsave("./figures/4_Recent_Centered_Demand_Series.png", figure_4,
         width = 12, height = 8)
  
  
#FIGURE 5: Raw Across Years
  start_date <- as.Date("2020-01-01")
  end_date   <- Sys.Date()
  
  data_5 <- ca_daily
  
  data_5$year <- year(data_5$date)
  data_5 <- data_5[data_5$year > year(Sys.Date())-5,]
  data_5$year <- as.character(data_5$year)
  
  year(data_5$date) <- 2020
  
  figure_5 <- ggplot(data = data_5)+
    geom_vline(xintercept = ca_sip, col = "red")+
    #annotate("text", x = ca_sip, y = max(y), label = "CA S.I.P.", col = "red")+
    geom_line(aes(x=date,y=MW, col = year), size = 1.25)+
    labs(title = "CA ISO Electricity Recent Years", y = "Raw MW Load")+
    scale_x_date(limits = c(as.Date("2020-01-01"), Sys.Date()))+
    scale_y_continuous(limits = c(400000, 700000))+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="bottom", legend.direction = "horizontal")
  
  ggsave("./figures/5_Raw_Demand_Over_Recent_Years.png", figure_5,
         width = 12, height = 8)
  
  
#FIGURE 6: Adjusted Across Years
  start_date <- as.Date("2020-01-01")
  end_date   <- Sys.Date()
  
  data_6 <- data_5
  
  figure_6 <- ggplot(data = data_6)+
    geom_vline(xintercept = ca_sip, col = "red")+
    #annotate("text", x = ca_sip, y = max(y), label = "CA S.I.P.", col = "red")+
    geom_line(aes(x=date,y=MW_sa_lm_MA7, col = year), size = 1.25)+
    labs(title = "CA ISO Electricity Load: Comparing Recent Years", y = "Weather Adjusted MW Load (7 Day MA)" )+
    scale_x_date(limits = c(as.Date("2020-01-01"), Sys.Date()))+
    #scale_y_continuous(limits = c(400000, 700000))+
    get_bod_theme()+
    theme(axis.title.y = element_text())+
    theme(legend.position="bottom", legend.direction = "horizontal")
  
  
  
  ggsave("./figures/6_Adjusted_Demand_Over_Recent_Years.png", figure_6,
         width = 12, height = 8)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#   
#   #Centered Daily Series: Raw, Corrected, with UR
#   start_date <- as.Date("2008-11-01")
#   end_date   <- Sys.Date()
#   
#   ca_macro$date <- ca_macro$eop_date
#   chart_4 <- bind_rows(chart_1, ca_macro)
#   
#   
#   chart_4 <- chart_4 %>% group_by(name) %>% 
#     mutate(mean = mean(value, na.rm = TRUE)) %>%
#     ungroup() %>%
#     mutate(percent_of_mean = (value/mean)*100)
#   
#   chart_4 <- chart_4[chart_4$name %in% c("MW_sa_lm_MA7","Unemployment_Rate_SA"),]
#   
#   ggplot(data = chart_4)+
#     get_recessions(window_start=start_date, 
#                    window_end = end_date)+
#     geom_vline(xintercept = ca_sip, col = "red")+
#     #annotate("text", x = ca_sip, y = max(y), label = "CA S.I.P.", col = "red")+
#     geom_line(aes(x=date,y=percent_of_mean, col = name))+
#     scale_x_date(limits = c(start_date, end_date))+
#     scale_color_discrete(labels = c("Corrected Demand: 7 day moving average","Unemployment Rate"))+
#     
#     labs(title = "CA ISO Electricity Demand and Unemployment" )+
#     get_bod_theme()  
#   
#   ggsave("./figures/5_Long_Centered_Demand_Series_w_UR.png",
#          width = 8, height = 5)
#   
# 
# #Growth Daily Series: Raw, Corrected, with UR
#   start_date <- as.Date("2008-11-01")
#   end_date   <- Sys.Date()
#   
#   ca_macro$date <- ca_macro$eop_date
#   chart_5 <- bind_rows(chart_1, ca_macro)
#   
#   
#   chart_5 <- chart_5 %>% group_by(name) %>% 
#     mutate(lag = lag(value,1)) %>%
#     ungroup() %>%
#     mutate(growth = ((value-lag)/lag)*100)
#   
#   chart_5 <- chart_5[chart_5$name %in% c("MW_sa_lm_MA7","Unemployment_Rate_SA"),]
#   
#   ggplot(data = chart_5)+
#     get_recessions(window_start=start_date, 
#                    window_end = end_date)+
#     geom_vline(xintercept = ca_sip, col = "red")+
#     #annotate("text", x = ca_sip, y = max(y), label = "CA S.I.P.", col = "red")+
#     geom_line(aes(x=date,y=growth, col = name))+
#     scale_x_date(limits = c(start_date, end_date))+
#     labs(title = "CA ISO Electricity Demand and Unemployment" )+
#     get_bod_theme()  
#   
#   ggsave("./figures/6_Long_Growth_Demand_Series_w_UR.png",
#          width = 8, height = 5)
#   

# #   
# #   
# #   
# # ###################################################################################################################
# # #Plot 0: Daily Frequency #############################################
# #   #RAW data
# #   ggplot()+
# #         get_recessions(window_start=min(ca_daily$date), 
# #                    window_end = max(ca_daily$date))+
# #         geom_vline(xintercept = ca_sip, col = "red")+
# #         annotate("text", x = ca_sip, y = 850000, label = "CA S.I.P.", col = "red")+
# #         geom_line(data=ca_daily, aes(x=date,y=MW))+
# #         ggtitle("Average Daily MW Load: Raw Data")+
# #       get_bod_theme()
# #   ggsave("./figures/daily_data.png",
# #          width = 6, height = 4)
# #   
# #   #Raw and recent
# #   ggplot()+
# #     get_recessions(window_start=min(ca_daily$date), 
# #                    window_end = max(ca_daily$date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 850000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_daily, aes(x=date,y=MW))+
# #     ggtitle("Average Daily MW Load: Raw Data")+
# #     scale_x_date(limits = as.Date(c("2020-01-01",NA)))+
# #     scale_y_continuous(limits = c(450000,650000))+
# #     get_bod_theme()
# #   ggsave("./figures/daily_data.png",
# #          width = 6, height = 4)
# #       
# #   #SA by Loess
# #   ggplot()+
# #     get_recessions(window_start=min(ca_daily$date), 
# #                    window_end = max(ca_daily$date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 850000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_daily, aes(x=date,y=MW_sa_loess))+
# #     ggtitle("Average Daily MW Load: SA with Loess")+
# #     get_bod_theme()
# #   ggsave("./figures/daily_data_sa_loess.png",
# #          width = 6, height = 4)
# #   
# #   #SA by lm
# #   ggplot()+
# #     get_recessions(window_start=min(ca_daily$date), 
# #                    window_end = max(ca_daily$date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 850000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_daily, aes(x=date,y=MW_sa_loess))+
# #     ggtitle("Average Daily MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()
# #   ggsave("./figures/daily_data_sa_lm.png",
# #          width = 6, height = 4)
# #   
# #   #SA by lm, recent
# #   ggplot()+
# #     get_recessions(window_start=min(ca_daily$date), 
# #                    window_end = max(ca_daily$date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 850000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_daily, aes(x=date,y=MW_sa_lm))+
# #     ggtitle("Average Daily MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()+
# #     scale_x_date(limits = as.Date(c("2020-01-01",NA)))
# #   ggsave("./figures/daily_data_sa_lm_recent.png",
# #          width = 6, height = 4)
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# #   
# # #################################################################################################################
# # #Plot 1: Weekly Frequency #############################################
# #   #RAW data
# #   ggplot()+
# #     get_recessions(window_start=min(ca_weekly$eop_date), 
# #                    window_end = max(ca_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 850000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_weekly, aes(x=eop_date,y=MW))+
# #     ggtitle("Average weekly MW Load: Raw Data")+
# #     get_bod_theme()
# #   ggsave("./figures/weekly_data.png",
# #          width = 6, height = 4)
# #   
# #   #SA by Loess
# #   ggplot()+
# #     get_recessions(window_start=min(ca_weekly$eop_date), 
# #                    window_end = max(ca_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 850000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_weekly, aes(x=eop_date,y=MW_sa_loess))+
# #     ggtitle("Average weekly MW Load: SA with Loess")+
# #     get_bod_theme()
# #   ggsave("./figures/weekly_data_sa_loess.png",
# #          width = 8, height = 5)
# #   
# #   #SA by lm
# #   ggplot()+
# #     get_recessions(window_start=min(ca_weekly$eop_date), 
# #                    window_end = max(ca_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 850000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_weekly, aes(x=eop_date,y=MW_sa_loess))+
# #     ggtitle("Average weekly MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()
# #   ggsave("./figures/weekly_data_sa_lm.png",
# #          width = 6, height = 4)
# #   
# #   # Raw, recent
# #   ggplot()+
# #     get_recessions(window_start=min(ca_weekly$eop_date), 
# #                    window_end = max(ca_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 650000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_weekly, aes(x=eop_date,y=MW))+
# #     ggtitle("Average weekly MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()+
# #     scale_x_date(limits = as.Date(c("2020-01-01",NA)))+
# #     scale_y_continuous(limits = c(500000,600000))
# #   ggsave("./figures/weekly_data_raw_recent.png",
# #          width = 6, height = 4)
# #   
# #   #SA by lm, recent
# #   ggplot()+
# #     get_recessions(window_start=min(ca_weekly$eop_date), 
# #                    window_end = max(ca_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 650000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_weekly, aes(x=eop_date,y=MW_sa_lm))+
# #     ggtitle("Average weekly MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()+
# #     scale_x_date(limits = as.Date(c("2020-01-01",NA)))+
# #     scale_y_continuous(limits = c(500000,600000))
# #   ggsave("./figures/weekly_data_sa_lm_recent.png",
# #          width = 6, height = 4)
# #   
# #   
# # #Work weekly
# #   #RAW data
# #   ggplot()+
# #     get_recessions(window_start=min(ca_work_weekly$eop_date), 
# #                    window_end = max(ca_work_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 550000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_work_weekly, aes(x=eop_date,y=MW))+
# #     ggtitle("Average work weekly MW Load: Raw Data")+
# #     get_bod_theme()
# #   ggsave("./figures/work_weekly_data.png",
# #          width = 6, height = 4)
# #   
# #   #SA by Loess
# #   ggplot()+
# #     get_recessions(window_start=min(ca_work_weekly$eop_date), 
# #                    window_end = max(ca_work_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 450000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_work_weekly, aes(x=eop_date,y=MW_sa_loess))+
# #     ggtitle("Average work weekly MW Load: SA with Loess")+
# #     get_bod_theme()
# #   ggsave("./figures/work_weekly_data_sa_loess.png",
# #          width = 6, height = 4)
# #   
# #   #SA by lm
# #   ggplot()+
# #     get_recessions(window_start=min(ca_work_weekly$eop_date), 
# #                    window_end = max(ca_work_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 450000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_work_weekly, aes(x=eop_date,y=MW_sa_loess))+
# #     ggtitle("Average work weekly MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()
# #   ggsave("./figures/work_weekly_data_sa_lm.png",
# #          width = 6, height = 4)
# #   
# #   # Raw, recent
# #   ggplot()+
# #     get_recessions(window_start=min(ca_work_weekly$eop_date), 
# #                    window_end = max(ca_work_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 350000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_work_weekly, aes(x=eop_date,y=MW))+
# #     ggtitle("Average work weekly MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()+
# #     scale_x_date(limits = as.Date(c("2020-01-01",NA)))+
# #     scale_y_continuous(limits = c(250000,350000))
# #   ggsave("./figures/work_weekly_data_raw_recent.png",
# #          width = 6, height = 4)
# #   
# #   #SA by lm, recent
# #   ggplot()+
# #     get_recessions(window_start=min(ca_work_weekly$eop_date), 
# #                    window_end = max(ca_work_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 350000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=ca_work_weekly, aes(x=eop_date,y=MW_sa_lm))+
# #     ggtitle("Average work weekly MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()+
# #     scale_x_date(limits = as.Date(c("2020-01-01",NA)))+
# #     scale_y_continuous(limits = c(250000,350000))
# #   ggsave("./figures/work_weekly_data_sa_lm_recent.png",
# #          width = 6, height = 4)
# #   
# # 
# # #Plot with macro indicators
# #   ca_weekly <- ca_weekly[!names(ca_weekly) %in% "p_type"]
# #   ca_weekly <- pivot_longer(ca_weekly, -eop_date)
# #   weekly_ic_scatter <- bind_rows(ca_macro[ca_macro$name =="Initial_Unemployment_Claims",],ca_weekly)
# #   weekly_ic_scatter_mw <- weekly_ic_scatter[weekly_ic_scatter$name %in% c("Initial_Unemployment_Claims","MW"),]
# #   weekly_ic_scatter_mw$value[weekly_ic_scatter_mw$name=="Initial_Unemployment_Claims"]<- 200000+100*weekly_ic_scatter_mw$value[weekly_ic_scatter_mw$name=="Initial_Unemployment_Claims"]
# #   ggplot()+
# #     get_recessions(window_start=min(ca_work_weekly$eop_date), 
# #                    window_end = max(ca_work_weekly$eop_date))+
# #     geom_vline(xintercept = ca_sip, col = "red")+
# #     annotate("text", x = ca_sip, y = 350000, label = "CA S.I.P.", col = "red")+
# #     geom_line(data=weekly_ic_scatter_mw, aes(x=eop_date,y=value, color = name))+
# #     ggtitle("Average work weekly MW Load: adjusted for weekends & degree days")+
# #     get_bod_theme()+
# #     scale_y_continuous(sec.axis = sec_axis(~./100-200000, name = "Unemployment Claims"))+
# #     scale_x_date(limits = as.Date(c("2009-01-01",NA)))+
# #     #scale_y_continuous(limits = c(250000,350000))
# #   ggsave("./figures/MW_and_IC.png",
# #          width = 6, height = 4)
# #   
# #   weekly_ic_scatter <- weekly_ic_scatter[-c("","","")]
# #   weekly_ic_scatter <- pivot_longer(weekly_ic_scatter,-eop_date)
# #     
# #   monthly_UR_scatter <- 
# #    
# # 
# #   