#import libraries
# library(shiny)
# library(dplyr)
# library(rgdal)
# library(leaflet)
# library(sp)

if(!require(shiny)) install.packages("shiny")
if(!require(dplyr)) install.packages("dplyr")
if(!require(rgdal)) install.packages("rgdal")
if(!require(leaflet)) install.packages("leaflet")
if(!require(sp)) install.packages("sp")

Sys.setlocale("LC_CTYPE", "Thai")

load("data.Rdata")


joinTbl <- function(map,freqt, lvl, rm_na = FALSE){
  # By using `setNames`, the order of the arguments is inverted 
  # with respect to the original use in `inner_join`    
  if(lvl == "Country"){
    mapcol <- "PROV_CODE"
    idcol <- "ProvinceID"
  }
  else if(lvl == "Province"){
    mapcol <- "CC_2"
    idcol <- "DistrictID"
  }
  else if(lvl == "Amphoe"){
    mapcol <- "TAM_CODE"
    idcol <- "TambonID"
  }
  
  map@data <- left_join(map@data, freqt,
                        by = setNames(nm = mapcol,idcol), keep = TRUE)
  
  if (rm_na){
    map@data %>%
      mutate_at(vars(Freq), ~replace_na(., 0)) -> map@data
  }
  return(map)
}


filterSp <- function(sp, lvl, popDf){
  if(lvl == "Country") return(sp)
  else if(lvl == "Province"){
    mapcol <- "CC_2"
    idcol <- "ProvinceID"
    stop <- 2
  }
  else if(lvl == "Amphoe"){
    mapcol <- "TAM_CODE"
    idcol <- "DistrictID"
    stop <- 4
  }
  code <- popDf[idcol] %>% unique() %>% unlist() %>% as.vector()
  
  sp <-  subset(sp, substr(sp@data[[mapcol]], 1, stop) == code)
  
  return(sp)
}

filtertbl <- function(rawtbl, lvl, prov = NULL, amp = NULL){
  if(lvl == "Country") {
    tmp <- rawtbl
  }
  else{
    tmp <- rawtbl %>% 
      subset(ProvinceThai == prov)
    if(lvl == "Amphoe"){
      tmp <- tmp %>% subset(DistrictThaiShort == amp)
    }
  }
  
  return(tmp)
}