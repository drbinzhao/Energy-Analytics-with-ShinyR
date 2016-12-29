# Energy-Analytics-with-ShinyR


###Background
The account management team at a local energy company faces increasing challenges in managing and utilizing the large amount of utilization data relevant to their commercial customers. The team would like a proof-of-concept solution to address the following pain points: 

      -  Growing volume of data decreases efficiency in data management
      -  Limited visibility into usage pattern and insights due to data volume
      -  Lack of capability of forecasting and planning 

###Dataset
The data is NOT real-world representatino of customers of the energy company. The information categories are similar but the data is randomly generated with code. Key info includes hourly energy usage, billing details, industry, geo-location of sites, etc.


### Solution
Designed and developed an interactive dashboard using ShinyApp in R to visualize energy usage pattern of over 100 commercial accounts providing the team easy access to the large amount of underlying dataset. Built advanced features into the dashboard such as bill forecasting and geo-location mapping to support the planning process.

###Methodology
Shiny is an open-source application framework for R to turn analyses into interactive web applications. Building BI dashboards as an web application was the main use case for this project. In order to develope a fully functional dashboard, three R files were built: 
     -  UI : layout of the dashboard
     -  Server : backend logics and data
     -  RunApp : lauch the app 
Multiple html widgets and packages were used to enhance usability and functionality , such as leaflet, dygraphs, DataTables, etc.

