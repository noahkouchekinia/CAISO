#Set Up
  setwd("~/Documents/Job Search/Code Sample/CAISO/")
  library(tidyverse)
  library(lubridate)

#Get California ISO Demand Data from Oasis API
  #Set query parameters
    start_date     <- as.Date("2009-01-01")
    end_date       <- Sys.Date()
    market_process <- "ACTUAL"              #Select from: 2DA, 7DA, DAM, ACTUAL, RTM
    timezone_offset <- function(date,timezone){date%>%as.character()%>%paste("12:00:00")%>%as.POSIXct(tz = timezone)%>%with_tz()%>%hour()}
    #offset <- date_to_hour(start_date, "America/Los_Angeles"), tz= "Europe/London")) - hour(as.POSIXct(paste(as.character(start_date), "12:00:00"), tz = "America/Los_Angeles"))
    offset <- hour(with_tz(as.POSIXct(paste(as.character(start_date), "12:00:00"), tz = "America/Los_Angeles"), tz= "Europe/London")) - hour(as.POSIXct(paste(as.character(start_date), "12:00:00"), tz = "America/Los_Angeles"))
  #Check for existing data
      extant_data <- read.csv("./Data/Actual Hourly Integrated Load by Region.csv")
      extant_data$X <- NULL
      extant_data$OPR_DT <- as.Date(extant_data$OPR_DT)
      start_date <- max(start_date, max(extant_data$OPR_DT-1))
    
  #Prepare to loop through portions of days
    all_dates <- as.Date(start_date:end_date,origin="1970-01-01")
    start_dates <- c(start_date, all_dates[1:length(all_dates)%%30==0])
    end_dates   <- unique(c(start_dates[2:length(start_dates)],end_date))
    if(length(start_dates)==1){end_dates <- end_date}
        
    start_offset <- hour(with_tz(as.POSIXct(paste(as.character(start_dates), "01:00:00"), tz = "America/Los_Angeles"), tz= "Europe/London")) - 
      hour(as.POSIXct(paste(as.character(start_dates), "01:00:00"), tz = "America/Los_Angeles"))
    end_offset <- hour(with_tz(as.POSIXct(paste(as.character(end_dates), "12:00:00"), tz = "America/Los_Angeles"), tz= "Europe/London")) - 
      hour(as.POSIXct(paste(as.character(end_dates), "12:00:00"), tz = "America/Los_Angeles"))
    length(end_dates)==length(start_dates) 
    
    data_list <- vector("list",length(start_dates))
    for(i in 1:length(start_dates)){
      #Build query one block at a time
        A <- "http://oasis.caiso.com/oasisapi/SingleZip?queryname=SLD_FCST"
        B <-  paste0("market_run_id=",market_process)
        C <-  "resultformat=6"
        D <-  paste0("startdatetime=",format(start_dates[i], "%Y%m%d"),"T0",start_offset[i],":00-0000")
        E <-  paste0("enddatetime=",format(end_dates[i], "%Y%m%d"),"T0", end_offset[i],":00-0000")
        F <- "version=1"
        query <-paste(A,B,C,D,E,F,sep="&")    #Query URL for the OASIS API
      #Call Query
        tf = tempfile()
        download.file(query,tf) # extra = c("use_proxy=on","http_proxy=l1proxy.frb.org:8080"))
        fname = unzip(tf,list=T)$Name[1]
        data_list[[i]] <- read.csv(unz(tf, fname))
        Sys.sleep(5)
    }
    data_list <- data_list[unlist(lapply(data_list,ncol))==14]
    data <- bind_rows(data_list)

#Clean and export choice cuts of data
  simplified <- data[c("OPR_DT","OPR_HR","TAC_AREA_NAME","LABEL","POS","MW")]
  simplified$OPR_DT <- as.Date(simplified$OPR_DT)
  simplified$TAC_AREA_NAME <- as.character(simplified$TAC_AREA_NAME)
  simplified <- rbind(extant_data, simplified)
  simplified <- simplified %>% group_by(OPR_DT,OPR_HR,TAC_AREA_NAME) %>% summarize(LABEL = last(LABEL), POS = last(POS),MW = last(MW))
  write.csv(simplified, "./Data/Actual Hourly Integrated Load by Region.csv")

