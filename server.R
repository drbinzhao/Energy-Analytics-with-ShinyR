


shinyServer(function(input,output){

  
# Date formatting

  fulldata$date<-paste(substr(fulldata$date, 1, 4), "-", substr(fulldata$date,5, nchar(fulldata$date)), sep = "")
  fulldata$date<-as.Date(as.yearmon(fulldata$date))
  
###########################################
# Overview Tab
###########################################
  
#Values for Boxes on top of overview screen  
  totaldis<-sum(fulldata$total_distribution)
  totalengery<-sum(fulldata$total_energy)
  totalbi<-sum(fulldata$present_bill)
  totalgroup<-length(unique(fulldata$group_code))
  
#Value Boxes in Overview
# Box 1
  output$totalBill<-renderInfoBox({
    infoBox(
      "Total Present Bill",totalbi,icon=icon("dollar"),color="yellow",fill=TRUE)
  })
# Box 2
  output$totalEnergy<-renderInfoBox({
    infoBox(
      "Total Energy Used",totalengery,icon=icon("calculator"),color="blue",fill=TRUE)
  })
# Box 3
  output$totalGroup<-renderInfoBox({
    infoBox(
      "Total Groups",totalgroup,icon=icon("users"),color="purple",fill=TRUE)
  })

#Time Series graph for overview panel
#dygraphs package makes the graph, reshape2 (melt function) is needed to munge the data
#xts package changes to a time series object
output$usageplot<-renderDygraph({
  rate_class_data<-melt(fulldata,id=c("date","rate_class"),measure.vars="total_energy")
  rate_class_data2<-cast(rate_class_data, date~ rate_class,sum)
  xtsdata<-data.frame(xts(rate_class_data2[,c(2:ncol(rate_class_data2))],rate_class_data2[,1]))
  colnames(xtsdata)<- unique(as.character(fulldata$rate_class))
  dygraph(xtsdata,
          main="Overall Usage Trend for All Rate Classes") %>% 
    dyAxis("y", label = "Total Energy Used (kWh)", valueRange =c(0,NULL)) %>%
    dyRangeSelector(dateWindow = c("2014-01-01", "2014-12-01")) %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, 
                highlightSeriesBackgroundAlpha = 0.2) %>%
    dyOptions(colors=RColorBrewer::brewer.pal(ncol(rate_class_data2), "Set2"))
})

#Top 10 usage groups graph
output$toptable <- renderChart2({
  nn <- nPlot( total_energy ~ group_name, group = "year", data = fulldata, type = "multiBarHorizontalChart")
  nn$chart(
    color=c("purple","blue"),
    margin=list( left=170),
    showControls=FALSE,
    width = 300
  )
  return(nn)
})

###############################################################
#Account Lookup Tab
###############################################################

#subset data for searching by group code/group name/acct code
reactdata<-reactive({switch(input$searchby,
                            "accnum"=subset(fulldata,fulldata$acc_num == input$accid & fulldata$year==input$year),
                            "gcode"=subset(fulldata,fulldata$group_code==input$group_code & fulldata$year==input$year),
                            "gname"=subset(fulldata,fulldata$group_name==input$group_name & fulldata$year==input$year))
                   })

# Raw data output for lookup tab
output$table<-renderDataTable({
  if (input$searchby=="accnum") {
    rawtable1<-aggregate(cbind(total_energy,total_distribution,present_bill,proposed_bill) ~ date, data=reactdata(), sum)
    
  }
  if (input$searchby=="gcode" | input$searchby=='gname') {
    
    rawtable1<-aggregate(cbind(total_energy,total_distribution,present_bill,proposed_bill) ~ date, data=reactdata(), sum)

  }
  return(rawtable1)

},options = list(width=300,lengthMenu = c(12, 24), pageLength = 8))


# Plot One -Dygraph (time series)
output$plot<-renderDygraph({
  if (length(input$accid) <1 && length(input$group_name)<1 && length(input$group_code)<1) {
    return(NULL)}
  else {
     plotdata<-as.data.frame(aggregate(get(input$usage) ~ date,data=reactdata(),sum))
   dygraph(xts(plotdata[,2],plotdata[,1]),main="Usage Trend") %>% 
     dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
      dyOptions(colors=RColorBrewer::brewer.pal(3, "Set2"))}
  
})


# Plot Two - GGPlot of usage over year
output$plot2<-renderPlot({
  if(is.null(input$billing)) {plot<-NULL}
  
  else if  (length(input$billing)>1) {
    data1=melt(reactdata(), id.vars =c("group_code","group_name","cust_name","acc_num","month"), measure.vars = c("present_bill", "proposed_bill"))
    plot<-ggplot(data1, aes(month, y=value,fill=variable)) + geom_bar(stat="identity", position=position_dodge())+theme(legend.position="bottom")+ ggtitle("Present and Future Bill Comparison")+scale_y_continuous(expand=c(0.15,0)) }
  
  else  {
    plot<-ggplot(data = reactdata(), aes_string('month', y=input$billing))+  ggtitle("12-Month Billing Trend") +stat_summary(fun.y = sum, geom="bar",fill="#33CCCC")+scale_y_continuous(expand=c(0.15,0))
  }
  print(plot)
  
})

#Output for summary statistics tab
output$summary<-renderTable({
  data<-select(reactdata(),total_distribution,total_energy,present_bill,proposed_bill)
  summary(data)
  
})

#########################################
## Rate Class Profile Tab
#########################################

#HD Tab
output$usage_var<-renderUI({selectInput("hd_var1","Select variable", 
                            choices=c("Distribution Demand"="total_distribution","Energy"="total_energy","Peak Demand"="peak_max","Non-Peak Demand"="offpeak_max"))
})

output$bill_var<-renderUI({
selectInput("hd_var2","Select variable", choices=c("Present Bills"="present_bill","Proposed Bill"="proposed_bill"))
})

reactive_hd1<-reactive({
  data<-fulldata[fulldata$rate_class=="HD",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$hd_var1)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

reactive_hd2<-reactive({
  data<-fulldata[fulldata$rate_class=="HD",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$hd_var2)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

output$profile_usage1<-renderDygraph({
  dygraph(reactive_hd1()) %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
  dyOptions(colors="purple")
})

output$profile_bill1<-renderDygraph({
  dygraph(reactive_hd2()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="blue")
  
})


#PD Tab
output$usage_var2<-renderUI({selectInput("pd_var1","Select variable", 
                                        choices=c("Distribution Demand"="total_distribution","Energy"="total_energy","Peak Demand"="peak_max","Non-Peak Demand"="offpeak_max"))
})

output$bill_var2<-renderUI({
  selectInput("pd_var2","Select variable", choices=c("Present Bills"="present_bill","Proposed Bill"="proposed_bill"))
})

reactive_pd1<-reactive({
  data<-fulldata[fulldata$rate_class=="PD",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$pd_var1)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

reactive_pd2<-reactive({
  data<-fulldata[fulldata$rate_class=="PD",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$pd_var2)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

output$profile_usage2<-renderDygraph({
  dygraph(reactive_pd1()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="purple")
})

output$profile_bill2<-renderDygraph({
  dygraph(reactive_pd2()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="blue")
  
})



#GS 100-500 Tab
output$usage_var3<-renderUI({selectInput("gs1_var1","Select variable", 
                                         choices=c("Distribution Demand"="total_distribution","Energy"="total_energy","Peak Demand"="peak_max","Non-Peak Demand"="offpeak_max"))
})

output$bill_var3<-renderUI({
  selectInput("gs1_var2","Select variable", choices=c("Present Bills"="present_bill","Proposed Bill"="proposed_bill"))
})

reactive_gs11<-reactive({
  data<-fulldata[fulldata$rate_class=="GS 100-500 kW",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$gs1_var1)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

reactive_gs12<-reactive({
  data<-fulldata[fulldata$rate_class=="GS 100-500 kW",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$gs1_var2)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

output$profile_usage3<-renderDygraph({
  dygraph(reactive_gs11()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="purple")
})

output$profile_bill3<-renderDygraph({
  dygraph(reactive_gs12()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="blue")
  
})


#GS >500 Tab

output$usage_var4<-renderUI({selectInput("gs2_var1","Select variable", 
                                         choices=c("Distribution Demand"="total_distribution","Energy"="total_energy","Peak Demand"="peak_max","Non-Peak Demand"="offpeak_max"))
})

output$bill_var4<-renderUI({
  selectInput("gs2_var2","Select variable", choices=c("Present Bills"="present_bill","Proposed Bill"="proposed_bill"))
})

reactive_gs21<-reactive({
  data<-fulldata[fulldata$rate_class=="GS >500 kW",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$gs2_var1)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

reactive_gs22<-reactive({
  data<-fulldata[fulldata$rate_class=="GS >500 kW",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$gs2_var2)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

output$profile_usage4<-renderDygraph({
  dygraph(reactive_gs21()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="purple")
})

output$profile_bill4<-renderDygraph({
  dygraph(reactive_gs22()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="blue")
  
})


#TLC Tab

output$usage_var5<-renderUI({selectInput("tlc_var1","Select variable", 
                                         choices=c("Distribution Demand"="total_distribution","Energy"="total_energy","Peak Demand"="peak_max","Non-Peak Demand"="offpeak_max"))
})

output$bill_var5<-renderUI({
  selectInput("tlc_var2","Select variable", choices=c("Present Bills"="present_bill","Proposed Bill"="proposed_bill"))
})

reactive_tlc1<-reactive({
  data<-fulldata[fulldata$rate_class=="TLC",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$tlc_var1)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

reactive_tlc2<-reactive({
  data<-fulldata[fulldata$rate_class=="TLC",]
  energy_data<-melt(data,id=c("date","group_name"),measure.vars=input$tlc_var2)
  energy_data2<-cast(energy_data, date~ group_name,sum)
  xtsdata<-data.frame(xts(energy_data2[,2:ncol(energy_data2)],energy_data2[,1]))
  colnames(xtsdata)<-c(unique(as.character(data$group_name)))
  return(xtsdata)
})

output$profile_usage5<-renderDygraph({
  dygraph(reactive_tlc1()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="purple")
})

output$profile_bill5<-renderDygraph({
  dygraph(reactive_tlc2()) %>% 
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 2),highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2)  %>% 
    dyOptions(colors="blue")
  
})


#######################################
# Forecast Tab
#######################################

# Define dataset 
reactdata2<-reactive({switch(input$forecast_select1,
                            "accnum2"=subset(fulldata,fulldata$acc_num == input$accid2 & fulldata$year==input$year2),
                            "gcode2"=subset(fulldata,fulldata$group_code==input$group_code2 & fulldata$year==input$year2),
                            "gname2"=subset(fulldata,fulldata$group_name==input$group_name2 & fulldata$year==input$year2))
})

# Forecasting based on selected range
output$forecastoutput<- renderDataTable({
if (input$forecast_select1=="gname2" | input$forecast_select1=="gcode2"){
  newdata<-aggregate(cbind(total_energy,total_distribution,present_bill) ~ date, data=reactdata2(), sum)
  proposed_bill<-
    input$fixed_charge*12+input$vdc_kwh*newdata$total_energy+input$vdc_kw*newdata$total_distribution+input$eec*newdata$total_energy+input$genc*newdata$total_energy+input$tsc*newdata$total_distribution
  dollar_diff<-  (reactdata2()$proposed_bill-reactdata2()$present_bill)
  percent_diff<- (reactdata2()$proposed_bill-reactdata2()$present_bill)/reactdata2()$present_bill 
  forecast.data<-cbind(newdata,data.frame(proposed_bill),data.frame(dollar_diff),data.frame(percent_diff))
  colnames(forecast.data)<-c("Date", "Total Energy","Total Distribution Demand","Present Bill","Proposed Bill","$ Difference","% Difference")
}
if (input$forecast_select1=="accnum2")  
  {
   newdata<-select(reactdata2(),date,total_energy,total_distribution,present_bill)
                                    proposed_bill<-
input$fixed_charge*12+input$vdc_kwh*reactdata2()$total_energy+input$vdc_kw*reactdata2()$total_distribution+input$eec*reactdata2()$total_energy+input$genc*reactdata2()$total_energy+input$tsc*reactdata2()$total_distribution
dollar_diff<-  (reactdata2()$proposed_bill-reactdata2()$present_bill)
percent_diff<- (reactdata2()$proposed_bill-reactdata2()$present_bill)/reactdata2()$present_bill                                   
forecast.data<-cbind(newdata,data.frame(proposed_bill),data.frame(dollar_diff),data.frame(percent_diff))}
colnames(forecast.data)<-c("Date", "Total Energy","Total Distribution Demand","Present Bill","Proposed Bill","$ Difference","% Difference")

if (is.null(input$group_name2) | is.null(input$group_code2) | is.null(input$accid2)) {
  forecast.data<-NULL
}
  
                                    print(forecast.data) 
}
,options = list(width=300,lengthMenu = c(12, 24), pageLength = 10)

)


####################################################
#map tab
####################################################

#define color pallete
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = fulldata$max_usage)

#marker popup code
p2 <- paste("<b>", as.character(fulldata_map$cust_name), "</b><br>",
            "<b>Acct:</b>", as.character(fulldata_map$acc_num), "<br>",
            "<b>Max On Peak Usage:</b>", as.character(fulldata_map$max_usage), "<br>",
            "<b>Current Bill:</b>", as.character(sprintf("$ %3.2f", fulldata_map$present_bill)), "<br>",
            "<b>Proposed Bill:</b>", as.character(sprintf("$ %3.2f", fulldata_map$proposed_bill)), "<br>",
            "<b>Percent Diff:</b>", as.character(sprintf("%.1f %%", 100*fulldata_map$percent_diff)), "<br>"
)

#create map with all locations
# addCircles command to create heatmap has bug; would like to include if resolved

map <- leaflet(fulldata_map) %>%
  addTiles() %>%
  setView(lng = -75.1626236, lat = 39.9600265, zoom = 12) %>%
  #addCircles(lng = ~ meterlongitude, lat = ~meterlatitude, radius = ~max_usage, weight = 1, color = "#777777",
  #          fillColor = ~pal(max_usage), fillOpacity = 0.7) %>%
  #addLegend(position = "bottomleft", pal = pal, values = ~max_usage, opacity = 1)
  addMarkers(lng = ~ meterlongitude, lat = ~meterlatitude, popup = p2, icon = lightbulb)

output$map <- renderLeaflet({map})



#subset data based on user input in dropdown
filteredData <- reactive ({
  
  if(input$group == ""){
    fulldata
  } else{
    fulldata[fulldata$group_name == input$group,]  
  }
  
})

filteredData_map <- reactive ({
  if (input$group == "") {
    fulldata_map
  } else{
    fulldata_map[fulldata_map$group_name == input$group,]
  }
})

#re-plot markers based upon user inputs
observe ({  
  leafletProxy("map", data = filteredData_map()) %>%
    #clearShapes() %>%
    #addCircles(lng = ~ meterlongitude, lat = ~meterlatitude, radius = ~max_usage, weight = 1, color = "#777777",
    #fillColor = ~pal(max_usage), fillOpacity = 0.7, popup = p2) 
    clearMarkers() %>%
    addMarkers(lng = ~ meterlongitude, lat = ~meterlatitude, popup = p2, icon = lightbulb)
  
})



#render plots to show Group usage plots in side panel
output$OnPeakPlot <- renderPlot ({
  OnPeakPlot <- ggplot(filteredData(), aes(x=month, y=peak_max)) + 
    geom_bar(position="dodge", stat="identity", aes(fill=factor(year))) + 
    scale_x_discrete("Month", limit = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                                        "Sep", "Oct", "Nov", "Dec")) +
    ylab("On Peak Energy Usage")
  print(OnPeakPlot)
})

output$OffPeakPlot <- renderPlot ({
  OffPeakPlot <- ggplot(filteredData(), aes(x=month, y=offpeak_max)) + 
    geom_bar(position="dodge", stat="identity", aes(fill=factor(year))) + 
    scale_x_discrete("Month", limit = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                                        "Aug", "Sep", "Oct", "Nov", "Dec")) +
    ylab("Off Peak Energy Usage")
  print(OffPeakPlot)
})

#close server function code
})
