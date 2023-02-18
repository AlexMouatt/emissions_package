

read_naei <- function() {

library(readxl)

file_path <- "naei.xlsx"
url <- "https://naei.beis.gov.uk/resources/PivotTableViewer_2022_AQ_Final_v1.xlsx"

download.file(url, file_path, mode = "wb")

air_pollutants <- read_excel(file_path, sheet = "AirPollutants", skip = 10)
pm <- read_excel(file_path, sheet = "PM", skip = 10)

colnames(pm) <- c("Pollutant", "NFRCode", "Emission Unit", c(1970:2020))

# Combine the two sheets into a single data frame

naei <- rbind(air_pollutants, pm)

# Assign the combined data to an object called "naei"
assign("naei", naei, envir = .GlobalEnv)

}

read_naei()


#mosaic plot

# mosaic plot with percentage on the x axis - for urban/rural split and % over threshold
