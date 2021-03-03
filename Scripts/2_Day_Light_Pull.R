#Day light pull

#Set Up
  if(Sys.info()["sysname"]=="windows"){sdrive <- "S:/"}
  if(Sys.info()["sysname"]=="Linux"){sdrive <- "/work/sf/internal/l1werp20.sf.frb.org/Shared/"}
  setwd(paste0(sdrive,"./Noah K/Tatevik/ISO"))

  Sys.setenv(http_proxy  =  "http://p1web5.frb.org:8080")
  Sys.setenv(https_proxy =  "https://p1web5.frb.org:8080")
  Sys.setenv(HTTP_PROXY  = "http://p1web5.frb.org:8080")
  Sys.setenv(HTTPS_PROXY = "https://p1web5.frb.org:8080")
  Sys.setenv(NO_PROXY    =  ".frb.org,.frb.gov,.frbres.org")

  library(jsonlite)

#Get appropriate long, lat values: https://www2.census.gov/geo/docs/reference/cenpop2010/CenPop2010_Mean_ST.txt?#
  ca_pop_centroid <- c(+35.463595,-119.325359)
  date_range <- as.Date(as.Date("2008-01-01"):as.Date(Sys.Date()+365), origin ="1970-01-01")

#Generate API calls
  api_calls <- paste0("https://api.sunrise-sunset.org/json?lat=",ca_pop_centroid[1],"&lng=",ca_pop_centroid[2],"&date=",date_range,"&formatted=0")

#Set vectors to store results
  sunrise <- character(length=length(api_calls))
  sunset <- character(length=length(api_calls))
  day_length <- character(length=length(api_calls))

#Loop through calls
  for(i in 1:length(api_calls)){
      results <-fromJSON(url(api_calls[i]))[[1]]
      sunrise[i]    <-results$sunrise[1]
      sunset[i]     <-results$sunset[1]
      day_length[i] <-results$day_length[1]
  }

#create data.frame
  data <- data.frame(date = date_range, sunrise, sunset, day_length )
  write.csv(data,"./Data/day_light.csv")