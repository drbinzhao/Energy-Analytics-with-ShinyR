####set to appropriate directory with all R files
setwd("C:/Users/Gavin/Desktop/CapstoneProject")

#need to run to load shiny package
if(!require(shiny)) install.packages("shiny", dependencies = T)

#initialize app by running this command
runApp()

