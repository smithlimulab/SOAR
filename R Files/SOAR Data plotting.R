# Load relevant packages ------
library(tidyverse)
library(scales)
library(ggpubr)

# Set working directory and import files -----
#setwd('C:/Users/smith/Documents/SOAR Working Folder/')
timeseries <- read.csv("Timeseries_20220605.csv", header = T)

 
# Cleaning Function ------
cleaner <- function(time_choice) {
  timeseries$Date.Time <- as.POSIXct(timeseries$Date.Time, format = "%m/%d/%Y %H:%M")
  timeseries$day <- format(timeseries$Date.Time, format = "%d")
  timeseries$month <- format(timeseries$Date.Time, format = "%m")
  timeseries$year <- format(timeseries$Date.Time, format = "%Y")
  if(time_choice == "daily") {
    timeseries <- aggregate(.~day+month+year,timeseries,mean)
    timeseries$date <- paste(timeseries$year, timeseries$month, timeseries$day, sep = "-")
    timeseries$date <- as.POSIXct(timeseries$date, format = "%Y-%m-%d")
  } else if(time_choice == "monthly") {
    timeseries <- timeseries %>% 
      select(month, year, pH, Temperature)
    timeseries <- aggregate(.~month+year,timeseries,mean)
    timeseries$day <- 1
    timeseries$date <- paste(timeseries$year, timeseries$month, timeseries$day, sep = "-")
    timeseries$date <- as.POSIXct(timeseries$date, format = "%Y-%m-%d")
  } else if(time_choice == "original") {
    timeseries$date <- paste(timeseries$year, timeseries$month, timeseries$day, sep = "-")
    timeseries$date <- as.POSIXct(timeseries$date, format = "%Y-%m-%d")
  } else {
    timeseries <- "invalid choice"
  }
  return(timeseries)
}
  

# plotting function ------

plotter <- function(var_choice){
  if(var_choice == "pH") {
    timeseries %>%
    ggplot(aes(x = date, y = pH))+
    geom_path()+
    coord_cartesian(xlim = as.POSIXct(c("2012-03-01", "2022-06-05")), ylim = c(6,10))+
    scale_x_datetime(labels = date_format("%Y"), breaks = date_breaks("1 year"))+
    theme_classic()
  } else if(var_choice == "oxygen") {
    timeseries %>%
      ggplot(aes(x = date, y = oxygen))+
      geom_path()+
      coord_cartesian(xlim = as.POSIXct(c("2012-03-01", "2022-06-05")), ylim = c(6,10))+
      scale_x_datetime(labels = date_format("%Y"), breaks = date_breaks("1 year"))+
      theme_classic()
  } else if(var_choice == "temperature") {
    timeseries %>%
      ggplot(aes(x = date, y = temperature))+
      geom_path()+
      coord_cartesian(xlim = as.POSIXct(c("2012-03-01", "2022-06-05")), ylim = c(6,10))+
      scale_x_datetime(labels = date_format("%Y"), breaks = date_breaks("1 year"))+
      theme_classic()
  }
}



# Input Choices ------

# Choose time: monthly, daily, original 
time_choice <- "monthly"

# Chose variable: pH, temperature, oxygen 
var_choice <- c("pH")

timeseries <- cleaner(time_choice)
plotter(var_choice)





