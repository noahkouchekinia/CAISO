#California Macro Data
  #Set up
    if(Sys.info()["sysname"]=="Windows"){sdrive <- "S:/"}
    if(Sys.info()["sysname"]=="Linux"){sdrive <- "/work/sf/internal/l1werp20.sf.frb.org/Shared/"}
    setwd(paste0(sdrive,"Noah K/Tatevik/ISO"))

    Sys.setenv(http_proxy  =  "http://p1web5.frb.org:8080")
    Sys.setenv(https_proxy =  "https://p1web5.frb.org:8080")
    Sys.setenv(HTTP_PROXY  = "http://p1web5.frb.org:8080")
    Sys.setenv(HTTPS_PROXY = "https://p1web5.frb.org:8080")
    Sys.setenv(NO_PROXY    =  ".frb.org,.frb.gov,.frbres.org")  
    
    
#Lets see if this data can be pulled from fred instead
    names <- c("Nominal_Personal_Income","Nonfarm_Payroll_Employment_SA","Household_Employment_SA", "Unemployment_Rate_SA")
    codes <- c("CAOTOT"                 ,"CANA"                         ,"EMPLOYCA"               , "CAUR"                )
    
    fred <- function(series_codes, series_names = NULL, fred_url = "https://fred.stlouisfed.org/graph/fredgraph.csv?id="){
      list <- lapply(paste0(fred_url,series_codes), read.csv, stringsAsFactors = FALSE)
      list <- lapply(list, function(x){x$series_id<- names(x)[2]; return(x)})
      if(!is.null(series_names)){list <- mapply(function(x,y){x$series_names <- y; return(x)},list,series_names, SIMPLIFY = FALSE)}
      list <- lapply(list, function(x){names(x)[1:3] <- c("eop_date", "value", "series_id"); return(x)})
      #Maybe add frequency detection at some point
      data <- do.call(rbind,list)
      data$eop_date <- as.Date(data$eop_date)
      return(data)
      }
    
   ca_macro_indicators <-  fred(codes, names)


#Lets export
  write.csv(ca_macro_indicators, file = "./Data/california_macro_indicators.csv", row.names = FALSE)
  
  
