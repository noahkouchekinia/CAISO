#Lets get degree days

#Set up  
  if(Sys.info()["sysname"]=="windows"){sdrive <- "S:/"}
  if(Sys.info()["sysname"]=="Linux"){sdrive<- "/work/sf/internal/l1werp20.sf.frb.org/Shared/"}
  setwd(paste0(sdrive,"./Noah K/Tatevik/ISO"))

  Sys.setenv(http_proxy  =  "http://p1web5.frb.org:8080")
  Sys.setenv(https_proxy =  "https://p1web5.frb.org:8080")
  Sys.setenv(HTTP_PROXY  = "http://p1web5.frb.org:8080")
  Sys.setenv(HTTPS_PROXY = "https://p1web5.frb.org:8080")
  Sys.setenv(NO_PROXY    =  ".frb.org,.frb.gov,.frbres.org")

  library(tidyverse)
  
#Build Query
  url_start <- "https://ftp.cpc.ncep.noaa.gov/htdocs/degree_days/weighted/daily_data/"
  years <- 2008:2021
  categories <-c("/StatesCONUS.Cooling","/StatesCONUS.Heating")
  extension <- ".txt"

#Pull Data with loop
heat_tables <- cool_tables <-vector("list", length=length(years))
i = 1
for(year in years){
  for(category in categories){
    api_call <- paste0(url_start,year,category,extension)
    table <- read.table(api_call, sep = "|", skip = 3, stringsAsFactors = FALSE)
    table <- t(table)
    colnames(table) <- table[1,]
    colnames(table)[1] <- "date"
    table <- table[-1,]
    table <- as.data.frame(table, stringsAsFactors = FALSE)
    if(grepl("Heat",category)) heat_tables[[i]] <- table
    if(grepl("Cool",category)) cool_tables[[i]] <- table
  }
  i = i+1
}
heat_data <- bind_rows(heat_tables)
cool_data <- bind_rows(cool_tables)

heat_data <-pivot_longer(heat_data, -date)
cool_data <-pivot_longer(cool_data, -date)

heat_data$type <- "heat"
cool_data$type <- "cool"

degree_days <- bind_rows(heat_data, cool_data)

degree_days$date <- as.Date(degree_days$date,format = "%Y%m%d")
degree_days$value <- as.numeric(degree_days$value)
names(degree_days)[2]<- "state"
write.csv(degree_days, "./Data/degree_days.csv")
