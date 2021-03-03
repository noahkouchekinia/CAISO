#Treat series for cyclicality

#Set Up
  if(Sys.info()["sysname"]=="Windows"){sdrive <- "S:/"}
  if(Sys.info()["sysname"]=="Linux"){sdrive <- "/work/sf/internal/l1werp20.sf.frb.org/Shared/"}
  setwd(paste0(sdrive,"./Noah K/Tatevik/ISO"))

  Sys.setenv(http_proxy  =  "http://p1web5.frb.org:8080")
  Sys.setenv(https_proxy =  "https://p1web5.frb.org:8080")
  Sys.setenv(HTTP_PROXY  = "http://p1web5.frb.org:8080")
  Sys.setenv(HTTPS_PROXY = "https://p1web5.frb.org:8080")
  Sys.setenv(NO_PROXY    =  ".frb.org,.frb.gov,.frbres.org")  
  
  library(tidyverse)
  library(lubridate)
  library(seasonal)
  library(BBmisc)
  library(zoo)
  library(chron)
  
#Load the data
  load_data <- read.csv("./Data/Actual Hourly Integrated Load by Region.csv",stringsAsFactors = FALSE)
  load_data$OPR_DT <- as.Date(load_data$OPR_DT)
  
#It seems like the begining of the data is messy and full of missing values. Lets chop it off
  load_data <- load_data[load_data$OPR_DT > as.Date("2009-04-01"),]
  
#Create Time series of varying frequencies
  ca_daily <- load_data %>%
    filter(load_data$TAC_AREA_NAME =="CA ISO-TAC") %>% 
    group_by(OPR_DT) %>%
    summarize(MW=sum(MW),hours = n())%>%
    rename(date=OPR_DT)
  

  ca_work_daily <- load_data %>%
    filter(load_data$TAC_AREA_NAME =="CA ISO-TAC" & load_data$OPR_HR >= 6 & load_data$OPR_HR <=18 & wday(load_data$OPR_DT)<6) %>% 
    group_by(OPR_DT) %>%
    summarize(MW=sum(MW), hours = n())%>%
    rename(date=OPR_DT)
  
  ca_weekly <- ca_daily %>% 
    group_by(year(date), week(date)) %>%
    summarize(eop_date = max(date), MW=mean(MW), hours = sum(hours)) %>%
    ungroup() %>%
    select(eop_date,MW, hours)%>%
    mutate(p_type = "w")
  
  ca_work_weekly <- ca_work_daily %>% 
    group_by(year(date),week(date)) %>%
    summarize(eop_date = max(date), MW=mean(MW), hours = sum(hours)) %>%
    ungroup() %>%
    select(eop_date,MW, hours)%>%
    mutate(p_type = "w")

  ca_monthly <- ca_daily %>% 
    group_by(year(date),month(date)) %>%
    summarize(eop_date = max(date), MW=mean(MW), hours = sum(hours)) %>%
    ungroup() %>%
    select(eop_date,MW, hours)%>%
    mutate(p_type = "m")
  
  ca_work_monthly <- ca_work_daily %>% 
    group_by(year(date),month(date)) %>%
    summarize(eop_date = max(date), MW=mean(MW), hours = sum(hours)) %>%
    ungroup() %>%
    select(eop_date,MW, hours)%>%
    mutate(p_type = "m")
  
  #Write to file project directory
    write.csv(ca_daily, "./Data/ca_daily.csv")
    write.csv(ca_work_daily, "./Data/ca_work_daily.csv")
    write.csv(ca_weekly, "./Data/ca_weekly.csv")
    write.csv(ca_work_weekly, "./Data/ca_work_weekly.csv")
    write.csv(ca_monthly, "./Data/ca_monthly.csv")
    write.csv(ca_work_monthly, "./Data/ca_work_monthly.csv")


#Daily Corrected
  #Some days are missing hours, lets eliminate those missing too many
    tolerence <- 1 #how many hours can be missing and not exclude the day from the serried
    ca_daily$MW[abs(24-ca_daily$hours) > tolerence] <- NA
  
  #Method 1: loess + Datebreak
    #Datebreak
      #There is a large structural break around 2012. Lets find it
        plot(ca_daily$date, ca_daily$MW, type = "l")
        ca_daily_zoom <-ca_daily[year(ca_daily$date) >=2011 & year(ca_daily$date) <2012,]
        plot(ca_daily_zoom$date, ca_daily_zoom$MW, type = "l") 
        #Looks like the structural break is at may 1st 20011.
        ca_daily$datebreak <-ifelse(ca_daily$date < as.Date('2011-05-01'),1,0)
      #Now lets correct for it
        model <- lm(MW~datebreak, data = ca_daily, na.action = na.exclude)
        summary(model)
        plot(ca_daily$date, ca_daily$MW, type="l", col="grey")
        lines(ca_daily$date,residuals(model)+model$coefficients["(Intercept)"])
      ca_daily$MW_dbc <- residuals(model)+model$coefficients["(Intercept)"]
    #Loess STL Seasonal Correction
      ca_daily_ts <- ts(ca_daily$MW_dbc, 
                        start=c(year(ca_daily$date[1]),day(ca_daily$date[1])),
                        freq = 365.25)
      ca_daily_decomposition <- stl((ca_daily_ts), s.window = 7,  na.action = na.approx)
      plot(ca_daily_decomposition)
      seasonal <- ca_daily_decomposition$time.series[,1]
      trend <- ca_daily_decomposition$time.series[,2]
      remainder <- ca_daily_decomposition$time.series[,3]
      corrected<- trend+remainder
      ca_daily$MW_sa_loess <- as.numeric(NA)
      ca_daily$MW_sa_loess[(dim(ca_daily)[1]-length(corrected)+1):(dim(ca_daily)[1])] <- corrected
      plot(ca_daily$date, ca_daily$MW, type="l", col="grey")
      lines(ca_daily$date,ca_daily$MW_sa_loess)
    
  #Method 2: Regression (Degree Days, Structural Break)
    #Degree Days + datebreak
      degree_days <- read.csv("./Data/degree_days.csv", stringsAsFactors=F)
      degree_days$date <- as.Date(degree_days$date)
      degree_days <- degree_days %>%
                      filter(state == "CA") %>%
                      select(date, value, type) %>%
                      pivot_wider(id_cols = date, names_from= type, values_from=value)
      
      ca_daily <- merge(ca_daily, degree_days, by = "date", keep.x = T)
      
      model <- lm(MW~heat+cool+datebreak+date, data = ca_daily, na.action = na.exclude )
      summary(model)
      plot(ca_daily$date, ca_daily$MW, type="l", col="grey")
      lines(ca_daily$date,residuals(model)+model$coefficients["(Intercept)"])

    #Degree Days + Datebreak + Weekday Effects + Bank holidays?
      ca_daily$weekday <- factor(wday(ca_daily$date, label = TRUE, week_start = 2), ordered = FALSE)
      ca_daily$holiday <- is.holiday(ca_daily$date)
      model <-  lm(MW~heat+cool+datebreak+weekday+holiday, data = ca_daily, na.action = na.exclude)
      summary(model)
      plot(ca_daily$date, ca_daily$MW, type="l", col="grey")
      lines(ca_daily$date,residuals(model)+model$coefficients["(Intercept)"])
    #Degree Days + Datebreak + Weekday Effects + Bank holidays? + trend
      model <-  lm(MW~heat+cool+datebreak+weekday+holiday+date, data = ca_daily, na.action = na.exclude)
      summary(model)
      plot(ca_daily$date, ca_daily$MW, type="l", col="grey")
      lines(ca_daily$date,residuals(model)+model$coefficients["(Intercept)"])
    #lets save this model sa_lm
      ca_daily$MW_sa_lm <- residuals(model)+model$coefficients["(Intercept)"]
    #Lets add a moving average
      ca_daily$MW_sa_lm_MA7 <- rollapply(ca_daily$MW_sa_lm, width = 7, align = 'right', fill = NA, FUN = mean, na.rm=TRUE)
      
#Daily work Corrected
  #Some days are missing hours, lets eliminate those missing too many
    tolerence <- 1 #how many hours can be missing and not exclude the day from the serried
    ca_work_daily$MW[abs(13-ca_work_daily$hours) > tolerence] <- NA
      
  #Method 1: loess + Datebreak
    #Datebreak
      ca_work_daily$datebreak <-ifelse(ca_work_daily$date < as.Date('2011-05-01'),1,0)
      model <- lm(MW~datebreak, data = ca_work_daily, na.action = na.exclude)
      summary(model)
      plot(ca_work_daily$date, ca_work_daily$MW, type="l", col="grey")
      lines(ca_work_daily$date,residuals(model)+model$coefficients["(Intercept)"])
      ca_work_daily$MW_dbc <- residuals(model)+model$coefficients["(Intercept)"]
    #Loess STL Seasonal Correction
      ca_work_daily_ts <- ts(ca_work_daily$MW_dbc, 
                        start=c(year(ca_work_daily$date[1]),day(ca_work_daily$date[1])),
                        freq = 365.25-2*52)
      ca_work_daily_decomposition <- stl((ca_work_daily_ts), s.window = 7,  na.action = na.approx)
      plot(ca_work_daily_decomposition)
      seasonal <- ca_work_daily_decomposition$time.series[,1]
      trend <- ca_work_daily_decomposition$time.series[,2]
      remainder <- ca_work_daily_decomposition$time.series[,3]
      corrected<- trend+remainder
        ca_work_daily$MW_sa_loess <- as.numeric(NA)
      ca_work_daily$MW_sa_loess[(dim(ca_work_daily)[1]-length(corrected)+1):(dim(ca_work_daily)[1])] <- corrected
      plot(ca_work_daily$date, ca_work_daily$MW, type="l", col="grey")
      lines(ca_work_daily$date,ca_work_daily$MW_sa_loess)
      
  #Method 2: Regression (Degree Days, Structural Break)
    #Degree Days + datebreak + weekdays + holidays + trend
      ca_work_daily$weekday <- factor(wday(ca_work_daily$date, label = TRUE, week_start = 2), ordered = FALSE)
      ca_work_daily$holiday <- is.holiday(ca_work_daily$date)
      ca_work_daily <- merge(ca_work_daily, degree_days, by = "date", keep.x = TRUE)
      model <- lm(MW~heat+cool+datebreak+weekday+holiday+date, data = ca_work_daily, na.action = na.exclude )
      summary(model)
      plot(ca_work_daily$date, ca_work_daily$MW, type="l", col="grey")
      lines(ca_work_daily$date,residuals(model)+model$coefficients["(Intercept)"])
      
    #lets save model to sa_lm
      ca_work_daily$MW_sa_lm <- residuals(model)+model$coefficients["(Intercept)"]


#Weekly Corrected
  #Some days are missing hours, lets eliminate those missing too many
    tolerence <- 2 #how many hours can be missing and not exclude the day from the serried
    ca_weekly$MW[abs((7*24)-ca_weekly$hours) > tolerence] <- NA
  
  #Method 1: loess + Datebreak
    #Datebreak
      ca_weekly$datebreak <-ifelse(ca_weekly$eop_date < as.Date('2011-05-01'),1,0)
      model <- lm(MW~datebreak, data = ca_weekly, na.action = na.exclude)
      summary(model)
      plot(ca_weekly$eop_date, ca_weekly$MW, type="l", col="grey")
      lines(ca_weekly$eop_date,residuals(model)+model$coefficients["(Intercept)"])
      ca_weekly$MW_dbc <- residuals(model)+model$coefficients["(Intercept)"]
    #Loess STL Seasonal Correction
      ca_weekly_ts <- ts(ca_weekly$MW_dbc, 
                             start=c(year(ca_weekly$eop_date[1]),week(ca_weekly$eop_date[1])),
                             freq = 52)
      ca_weekly_decomposition <- stl((ca_weekly_ts), s.window = 7,  na.action = na.approx)
      plot(ca_weekly_decomposition)
      seasonal <- ca_weekly_decomposition$time.series[,1]
      trend <- ca_weekly_decomposition$time.series[,2]
      remainder <- ca_weekly_decomposition$time.series[,3]
      corrected<- trend+remainder
      ca_weekly$MW_sa_loess <- as.numeric(NA)
      ca_weekly$MW_sa_loess[(dim(ca_weekly)[1]-length(corrected)+1):(dim(ca_weekly)[1])] <- corrected
      plot(ca_weekly$eop_date, ca_weekly$MW, type="l", col="grey")
      lines(ca_weekly$eop_date,ca_weekly$MW_sa_loess)
      
  #Method 2: Regression (Degree Days, Structural Break)
    #Degree Days + datebreak
      degree_days_weekly <- degree_days %>% 
        group_by(year(date), week(date)) %>%
        summarize(eop_date = max(date), heat=mean(heat), cool = mean(cool)) %>%
        ungroup() %>%
        select(eop_date, heat, cool)

      ca_weekly <- merge(ca_weekly, degree_days_weekly, by = "eop_date", keep.x = T)
      model <- lm(MW~heat+cool+datebreak, data = ca_weekly, na.action = na.exclude )
      summary(model)
      plot(ca_weekly$eop_date, ca_weekly$MW, type="l", col="grey")
      lines(ca_weekly$eop_date,residuals(model)+model$coefficients["(Intercept)"])
      
      #lets save model to sa_lm
      ca_weekly$MW_sa_lm <- residuals(model)+model$coefficients["(Intercept)"]
      
      
      
#Work Weekly Corrected
  #Some days are missing hours, lets eliminate those missing too many
    tolerence <- 2 #how many hours can be missing and not exclude the day from the serried
    ca_work_weekly$MW[abs((5*13)-ca_work_weekly$hours) > tolerence] <- NA
      
  #Method 1: loess + Datebreak
    #Datebreak
      ca_work_weekly$datebreak <-ifelse(ca_work_weekly$eop_date < as.Date('2011-05-01'),1,0)
      model <- lm(MW~datebreak, data = ca_work_weekly, na.action = na.exclude)
      summary(model)
      plot(ca_work_weekly$eop_date, ca_work_weekly$MW, type="l", col="grey")
      lines(ca_work_weekly$eop_date,residuals(model)+model$coefficients["(Intercept)"])
      ca_work_weekly$MW_dbc <- residuals(model)+model$coefficients["(Intercept)"]
    #Loess STL Seasonal Correction
      ca_work_weekly_ts <- ts(ca_work_weekly$MW_dbc, 
                         start=c(year(ca_work_weekly$eop_date[1]),week(ca_work_weekly$eop_date[1])),
                         freq = 52)
      ca_work_weekly_decomposition <- stl((ca_work_weekly_ts), s.window = 7,  na.action = na.approx)
      plot(ca_work_weekly_decomposition)
      seasonal <- ca_work_weekly_decomposition$time.series[,1]
      trend <- ca_work_weekly_decomposition$time.series[,2]
      remainder <- ca_work_weekly_decomposition$time.series[,3]
      corrected<- trend+remainder
      ca_work_weekly$MW_sa_loess <- as.numeric(NA)
      ca_work_weekly$MW_sa_loess[(dim(ca_work_weekly)[1]-length(corrected)+1):(dim(ca_work_weekly)[1])] <- corrected
      plot(ca_work_weekly$eop_date, ca_work_weekly$MW, type="l", col="grey")
      lines(ca_work_weekly$eop_date,ca_work_weekly$MW_sa_loess)
      
  #Method 2: Regression (Degree Days, Structural Break)
    #Degree Days + datebreak
      degree_days_work_weekly <- degree_days[wday(degree_days$date)<6,] %>% 
        group_by(year(date), week(date)) %>%
        summarize(eop_date = max(date), heat=mean(heat, na.rm = T), cool = mean(cool, na.rm = T)) %>%
        ungroup() %>%
        select(eop_date, heat, cool)
        
      ca_work_weekly <- merge(ca_work_weekly, degree_days_work_weekly, by = "eop_date", all.x = T)
      model <- lm(MW~heat+cool+datebreak, data = ca_work_weekly, na.action = na.exclude )
      summary(model)
      
      #Plot Result
      plot(ca_work_weekly$eop_date, ca_work_weekly$MW, type="l", col="grey")
      lines(ca_work_weekly$eop_date,residuals(model)+model$coefficients["(Intercept)"])
      
      #lets save model to sa_lm
      ca_work_weekly$MW_sa_lm <- residuals(model)+model$coefficients["(Intercept)"]
      

#############################################     Monthly   ##########################################################################
      
      
#monthly Corrected
  #Some days are missing hours, lets eliminate those missing too many
    tolerence <- 2.5*24 #how many hours can be missing and not exclude the day from the serried
    ca_monthly$MW[abs((30*24)-ca_monthly$hours) > tolerence] <- NA
      
    #Method 1: loess + Datebreak
      #Datebreak
        ca_monthly$datebreak <-ifelse(ca_monthly$eop_date < as.Date('2011-05-01'),1,0)
        model <- lm(MW~datebreak, data = ca_monthly, na.action = na.exclude)
        summary(model)
        plot(ca_monthly$eop_date, ca_monthly$MW, type="l", col="grey")
        lines(ca_monthly$eop_date,residuals(model)+model$coefficients["(Intercept)"])
        ca_monthly$MW_dbc <- residuals(model)+model$coefficients["(Intercept)"]
      #Loess STL Seasonal Correction
      ca_monthly_ts <- ts(ca_monthly$MW_dbc, 
                         start=c(year(ca_monthly$eop_date[1]),month(ca_monthly$eop_date[1])),
                         freq = 12)
      ca_monthly_decomposition <- stl((ca_monthly_ts), s.window = 7,  na.action = na.approx)
      plot(ca_monthly_decomposition)
      seasonal <- ca_monthly_decomposition$time.series[,1]
      trend <- ca_monthly_decomposition$time.series[,2]
      remainder <- ca_monthly_decomposition$time.series[,3]
      corrected<- trend+remainder
      ca_monthly$MW_sa_loess <- as.numeric(NA)
      ca_monthly$MW_sa_loess[(dim(ca_monthly)[1]-length(corrected)+1):(dim(ca_monthly)[1])] <- corrected
      plot(ca_monthly$eop_date, ca_monthly$MW, type="l", col="grey")
      lines(ca_monthly$eop_date,ca_monthly$MW_sa_loess)
      
    #Method 2: Regression (Degree Days, Structural Break)
      #Degree Days + datebreak
      degree_days_monthly <- degree_days %>% 
        group_by(year(date), month(date)) %>%
        summarize(eop_date = max(date), heat=mean(heat), cool = mean(cool)) %>%
        ungroup() %>%
        select(eop_date, heat, cool)
        
      ca_monthly <- merge(ca_monthly, degree_days_monthly, by = "eop_date", keep.x = T)
      model <- lm(MW~heat+cool+datebreak, data = ca_monthly, na.action = na.exclude )
      summary(model)
      plot(ca_monthly$eop_date, ca_monthly$MW, type="l", col="grey")
      lines(ca_monthly$eop_date,residuals(model)+model$coefficients["(Intercept)"])
      
      #lets save model to sa_lm
      ca_monthly$MW_sa_lm <- residuals(model)+model$coefficients["(Intercept)"]
      
    #Method 3: X13-Arima-Seats
      #Commented out while there is no x13 on cluster
      ca_monthly_x13<- seas(ca_monthly_ts, na.action = na.approx)
      
      plot(ca_monthly_x13)
      
      ca_monthly$MW_sa_x13 <- as.data.frame(ca_monthly_x13$data)$final

      
      
#Work monthly Corrected
      #Some days are missing hours, lets eliminate those missing too many
      tolerence <- 2*24 #how many hours can be missing and not exclude the day from the serried
      ca_work_monthly$MW[abs(((5/7)*30*13)-ca_work_monthly$hours) > tolerence] <- NA
      
      #Method 1: loess + Datebreak
      #Datebreak
      ca_work_monthly$datebreak <-ifelse(ca_work_monthly$eop_date < as.Date('2011-05-01'),1,0)
      model <- lm(MW~datebreak, data = ca_work_monthly, na.action = na.exclude)
      summary(model)
      plot(ca_work_monthly$eop_date, ca_work_monthly$MW, type="l", col="grey")
      lines(ca_work_monthly$eop_date,residuals(model)+model$coefficients["(Intercept)"])
      ca_work_monthly$MW_dbc <- residuals(model)+model$coefficients["(Intercept)"]
      #Loess STL Seasonal Correction
      ca_work_monthly_ts <- ts(ca_work_monthly$MW_dbc, 
                              start=c(year(ca_work_monthly$eop_date[1]),month(ca_work_monthly$eop_date[1])),
                              freq = 12)
      ca_work_monthly_decomposition <- stl((ca_work_monthly_ts), s.window = 7,  na.action = na.approx)
      plot(ca_work_monthly_decomposition)
      seasonal <- ca_work_monthly_decomposition$time.series[,1]
      trend <- ca_work_monthly_decomposition$time.series[,2]
      remainder <- ca_work_monthly_decomposition$time.series[,3]
      corrected<- trend+remainder
      ca_work_monthly$MW_sa_loess <- as.numeric(NA)
      ca_work_monthly$MW_sa_loess[(dim(ca_work_monthly)[1]-length(corrected)+1):(dim(ca_work_monthly)[1])] <- corrected
      plot(ca_work_monthly$eop_date, ca_work_monthly$MW, type="l", col="grey")
      lines(ca_work_monthly$eop_date,ca_work_monthly$MW_sa_loess)
      
      #Method 2: Regression (Degree Days, Structural Break)
      #Degree Days + datebreak
      degree_days_work_monthly <- degree_days[wday(degree_days$date)<6,] %>% 
        group_by(year(date), month(date)) %>%
        summarize(eop_date = max(date), heat=mean(heat, na.rm = T), cool = mean(cool, na.rm = T)) %>%
        ungroup() %>%
        select(eop_date, heat, cool)
      
      ca_work_monthly <- merge(ca_work_monthly, degree_days_work_monthly, by = "eop_date", all.x = T)
      model <- lm(MW~heat+cool+datebreak, data = ca_work_monthly, na.action = na.exclude )
      summary(model)
      
      #Plot Result
      plot(ca_work_monthly$eop_date, ca_work_monthly$MW, type="l", col="grey")
      lines(ca_work_monthly$eop_date,residuals(model)+model$coefficients["(Intercept)"])
      
      #lets save model to sa_lm
      ca_work_monthly$MW_sa_lm <- residuals(model)+model$coefficients["(Intercept)"]
      
      #Method 3: X13-Arima-Seats
        # ca_work_monthly_x13<- seas(ca_work_monthly_ts, na.action = na.approx)
        # 
        # plot(ca_work_monthly_x13)
        # 
        # ca_work_monthly$MW_sa_x13 <- as.data.frame(ca_work_monthly_x13$data)$final
      
    
#Write to file project directory
      save(ca_daily,file = "./Data/ca_daily.Rdata")  
      save(ca_work_daily,file ="./Data/ca_work_daily.Rdata")  
      save(ca_weekly,file = "./Data/ca_weekly.Rdata")  
      save(ca_work_weekly,file = "./Data/ca_work_weekly.Rdata")  
      save(ca_monthly,file = "./Data/ca_monthly.Rdata")  
      save(ca_work_monthly,file ="./Data/ca_work_monthly.Rdata")  
      
      write.csv(ca_daily, "./Data/ca_daily.csv")
      write.csv(ca_work_daily, "./Data/ca_work_daily.csv")
      write.csv(ca_weekly, "./Data/ca_weekly.csv")
      write.csv(ca_work_weekly, "./Data/ca_work_weekly.csv")
      write.csv(ca_monthly, "./Data/ca_monthly.csv")
      write.csv(ca_work_monthly, "./Data/ca_work_monthly.csv")
      
      
  