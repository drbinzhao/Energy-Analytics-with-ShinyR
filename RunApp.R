####set to appropriate directory with all R files
setwd("/Users/ShireeXue/GitHub/Energy-Analytics-with-ShinyR")

#need to run to load shiny package
if(!require(shiny)) install.packages("shiny", dependencies = T)

#initialize app by running this command
runApp()

