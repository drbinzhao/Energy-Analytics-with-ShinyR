
#RunApp command will run the following commands to load libraries and data

if(!require(devtools)) install.packages("devtools", dependencies = T)
if(!require(leaflet)) install_github("rstudio/leaflet")
if(!require(Rcpp)) install.packages("Rcpp", dependencies = T)
if(!require(ggplot2)) install.packages("ggplot2", dependencies = T)
if(!require(reshape)) install.packages("reshape", dependencies = T)
if(!require(DT)) install.packages("DT", dependencies = T)
if(!require(rCharts)) install_github('ramnathv/rCharts')
library(rCharts)
if(!require(htmlwidgets)) install_github('ramnathv/htmlwidgets')
if(!require(shinydashboard)) install.packages("shinydashboard", dependencies = T)
if(!require(dygraphs)) install.packages("dygraphs", dependencies = T)
if(!require(xts)) install.packages("xts", dependencies = T)
if(!require(dplyr)) install.packages("dplyr", dependencies = T)
if(!require(doBy)) install.packages("doBy", dependencies = T)


#read data from csv; make sure file name is correct, and directory is set in RunnApp.r code
fulldata <- read.csv("new_data.csv", header = TRUE)

#create year and month columns
fulldata$year<-substr(fulldata$date, 1, 4)
fulldata$month<-as.numeric(substr(fulldata$date, 5, 6))
fulldata$month<-month.abb[fulldata$month]

#dplyr code for use in map tab
fulldata_map <- group_by(fulldata, acc_num, group_name, cust_name, meterlatitude, meterlongitude) %>%
  summarise(max_usage = max(peak_max), present_bill = mean(present_bill), 
            proposed_bill = mean(proposed_bill), percent_diff = (mean(percent_diff)))

#icon used in map tab
lightbulb <- makeIcon("lightbulb.png")
