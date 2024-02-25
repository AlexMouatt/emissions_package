#' Read NAEI Data
#'
#' This function reads NAEI (National Atmospheric Emissions Inventory) data 
#'
#' @return The NAEI data as a data frame.
#' @export
read_naei <- function() {
  library(readxl)
  library(tidyverse)
  
  file_path <- "naei.xlsx"
  url <- "https://naei.beis.gov.uk/resources/PivotTableViewer_2023_AQ_Final_v1.1.xlsx"
  
  download.file(url, file_path, mode = "wb")
  
  gases <- read_excel(file_path, sheet = "AirPollutants", skip = 10)%>% 
    as_tibble() %>%
    pivot_longer(cols = "1970":latest_year, names_to = "year",
                 values_to = "Kt", values_drop_na = TRUE) %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(nfr=nfr_code) %>% 
    mutate(emission=kt) %>%
    mutate(activity=activity_name) %>%
    mutate(source=source_name) %>%
    select(pollutant,nfr,source,activity,year,emission)
  
  particulates <- read_excel(file_path, sheet = "PM", skip = 10)  %>%
    as_tibble() %>%
    pivot_longer(cols = "1970":latest_year, names_to = "Year",
                 values_to = "Kt", values_drop_na = TRUE)  %>% 
    janitor::clean_names() %>% 
    mutate(nfr=nfr_code) %>% 
    mutate(emission=kt) %>%
    mutate(activity=activity_name) %>%
    mutate(source=source_name) %>%
    mutate(pollutant=particle_size) %>% 
    select(pollutant,nfr,source,activity,year,emission)
  
  # NO2 - have to calculate no2_compliance to exclude agriculture
  no2 <- subset(gases, gases$pollutant == "Nitrogen Oxides as NO2") 
  
  no2_comp <- no2 %>% filter(!str_detect(nfr, "^3"))%>%
    mutate(pollutant='NO2 Compliance') 
  
  # VOC's - have to calculate voc_compliance to exclude agriculture
  voc <- subset(gases, gases$pollutant == "Non Methane VOC")
  
  # Change this to be 3B + 3D
  voc_comp <- voc %>% filter(!str_detect(nfr, "^3"))%>%
    mutate(pollutant='NMVOC Compliance') 
  
  # NH3 - have to calculate nh3_compliance to exclude non manure digestates
  # and nh3_agri to include just agriculture
  nh3 <- subset(gases, gases$pollutant == "Ammonia")
  
  nh3_comp <- nh3 %>% 
    filter(!str_detect(source, "Crop Digestates - TAN")) %>%
    filter(!str_detect(source, "Food Digestates - TAN")) %>%
    filter(!str_detect(source, "Other organic residue Digestates - TAN")) %>%
    mutate(pollutant='Ammonia Compliance') 
  
  # Change this to be 3B + 3D
  nh3_agri <- nh3 %>% filter(str_detect(nfr, "^3")) %>%
    mutate(pollutant='Ammonia Agriculture')
  
  # Other pollutants compliance is same as national total
  so2  <- subset(gases, gases$pollutant == "Sulphur Dioxide")
  pm25 <- subset(particulates, particulates$pollutant == "PM2.5")
  pm10 <- subset(particulates, particulates$pollutant == "PM10")
  
  # Bind all the pollutants together into one tidy tibble
  # Ensure that emission and year columns are numeric
  
  naei <- rbind(no2_comp,no2,voc_comp,voc,so2,nh3,nh3_comp,nh3_agri,pm25,pm10)%>% 
    mutate_at(c('emission', 'year'), as.numeric)
  
  # Assign the combined data to an object called "naei"
  assign("naei", naei, envir = .GlobalEnv)
  
}