
#Define dashboard color and title
dashboardPage(
    skin="green",
  dashboardHeader(title="EnergyX Dashboard"),
  
#Define Dashboard Sidebar  
  dashboardSidebar(
    
    #menu items
    sidebarMenu(
      menuItem("Overview",tabName="overview",icon=icon("dashboard")),
      menuItem("Customers LookUp",icon=icon("search"),tabName="lookup") ,
      menuItem("Rate Class Profile",icon=icon("bar-chart"),tabName="tabs",
               menuSubItem("GS 100-500",tabName="gs1"),
               menuSubItem("GS >500",tabName="gs2"),
               menuSubItem("HD",tabName="hd"),
               menuSubItem("PD",tabName="pd"),
               menuSubItem("TLC",tabName="tlc")
               ),
      menuItem("Bill Forecast",icon=icon("line-chart"),tabName="forecast"),
      menuItem("Geographic Trend",icon=icon("globe"),tabName="map"))),
  
  #UI for overview page
  dashboardBody(
    tags$head(tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
              tags$title("EnergyX")),
    tabItems(
    tabItem(tabName="overview",
    fluidRow(
    infoBoxOutput("totalEnergy"),
    infoBoxOutput("totalBill"),
    infoBoxOutput("totalGroup")),
    fluidRow(
         box(
        title = "Usage Overview", width =8,height=500 ,solidHeader = TRUE, status="info",dygraphOutput("usageplot",width = "100%", height = "350px")
        ),
         box(
        title = "Top 10 Usage Groups", width = 4,height=500, solidHeader = TRUE, status = "success",showOutput("toptable","nvd3")
      ))),

#Account Lookup tab    
 tabItem("lookup",
         sidebarPanel(width=3,
           selectInput("searchby","Search By",choices=c("Account Number"="accnum","Group Name"="gname","Group Code"="gcode"),selected="accnum"),
           conditionalPanel(
             condition="input.searchby =='accnum'",
             textInput("accid","Type in Account Number")),
           conditionalPanel(
             condition="input.searchby=='gname'",
             selectInput("group_name","Select Group Name",as.character(fulldata$group_name),selected=NULL)),
          conditionalPanel(
             condition="input.searchby=='gcode'",
             textInput("group_code","Type in Group Code")),
          selectInput("year", "Select year", unique(fulldata$year),selected=NULL),
          selectInput("usage","Usage Insight: Select variable", choices=c("Distribution Demand"="total_distribution","Energy"="total_energy","Peak Demand"="peak_max","Non-Peak Demand"="offpeak_max")),
          checkboxGroupInput("billing","Billing Insight: Select variable", choices=c("Present Bill"="present_bill","Proposed Bill"="proposed_bill"),selected='present_bill')),
         mainPanel(
           tabBox(width=20,
                          tabPanel("Plot",
                                   dygraphOutput("plot",height=300),plotOutput("plot2",height=300)
                                   ),
                          tabPanel("Summary Statistics",tableOutput("summary")),
                          tabPanel("Raw Data",
                                   div(style = 'overflow-x: scroll;font-size:80%',dataTableOutput("table")))                  
                                  )))
 
#Forecasting Tab
  ,tabItem("forecast",
    sidebarPanel(width=3,h4("Groups/Accounts Selection"),br(),
      selectInput("forecast_select1", "Select Data by",choices=c("Group Name"="gname2","Group Code"="gcode2","Account Number"="accnum2")),
      conditionalPanel(
          condition="input.forecast_select1=='gname2'",
          selectInput("group_name2","Select Group Name",as.character(fulldata$group_name),selected=NULL)),
      conditionalPanel(
          condition="input.forecast_select1=='gcode2'",
          textInput("group_code2","Insert Group Code")),
      conditionalPanel(
          condition="input.forecast_select1=='accnum2'",
          textInput("accid2","Insert Account Number")),
        selectInput("year2", "Select year", unique(fulldata$year),selected=NULL)) ,
      mainPanel(
        tabBox(width=15,
          tabPanel("Customize Rate",
                   numericInput("fixed_charge","Fixed Charge",0,min=NA,max=NA,step=NA,width=NULL),
                   numericInput("vdc_kwh","VDC - kWh",0,min=NA,max=NA,step=NA,width=NULL),
                   numericInput("vdc_kw","VDC - kW",0,min=NA,max=NA,step=NA,width=NULL),
                   numericInput("eec","EEC",0,min=NA,max=NA,step=NA,width=NULL),
                   numericInput("genc","Gen C",0,min=NA,max=NA,step=NA,width=NULL),
                   numericInput("tsc","TSC",0,min=NA,max=NA,step=NA,width=NULL)
                   ),
          tabPanel("View Forecast Results",
                   div(style = 'overflow-x: scroll;font-size:80%',dataTableOutput("forecastoutput")))
                  
      ))
)

#Rate Class Profile tabs
  ,tabItem("hd",
              fluidRow(
              box(title = "Usage Trend for HD", width = 12,height=400,solidHeader = FALSE, status="warning",uiOutput("usage_var"),dygraphOutput("profile_usage1",width = "100%", height = "270px")),
              box(title = "Billing Trend for HD", width = 12,height=400 ,solidHeader = FALSE, status="info",uiOutput("bill_var"),dygraphOutput("profile_bill1",width = "100%", height = "270px"))))
,tabItem("pd",
         fluidRow(
           box(title = "Usage Trend for PD", width = 12,height=400,solidHeader = FALSE, status="warning",uiOutput("usage_var2"),dygraphOutput("profile_usage2",width = "100%", height = "270px")),
           box(title = "Billing Trend for PD", width = 12,height=400 ,solidHeader = FALSE, status="info",uiOutput("bill_var2"),dygraphOutput("profile_bill2",width = "100%", height = "270px"))             
         ))
 ,tabItem("gs1",
         fluidRow(
           box(title = "Usage Trend for GS 100-500 kWh", width = 12,height=400,solidHeader = FALSE, status="warning",uiOutput("usage_var3"),dygraphOutput("profile_usage3",width = "100%", height = "270px")),
           box(title = "Billing Trend for GS 100-500 kWh", width = 12,height=400 ,solidHeader = FALSE, status="info",uiOutput("bill_var3"),dygraphOutput("profile_bill3",width = "100%", height = "270px"))             
         ))
,
  tabItem("gs2",
         fluidRow(
           box(title = "Usage Trend for GS >500 kWh", width = 12,height=400,solidHeader = FALSE, status="warning",uiOutput("usage_var4"),dygraphOutput("profile_usage4",width = "100%", height = "270px")),
           box(title = "Billing Trend for GS >500 kWh", width = 12,height=400 ,solidHeader = FALSE, status="info",uiOutput("bill_var4"),dygraphOutput("profile_bill4",width = "100%", height = "270px"))             
         ))
,
  tabItem("tlc",
         fluidRow(
           box(title = "Usage Trend for TLC", width = 12,height=400,solidHeader = FALSE, status="warning",uiOutput("usage_var5"),dygraphOutput("profile_usage5",width = "100%", height = "270px")),
           box(title = "Billing Trend for TLC", width = 12,height=400 ,solidHeader = FALSE, status="info",uiOutput("bill_var5"),dygraphOutput("profile_bill5",width = "100%", height = "270px"))             
         ))


#Map tab
,tabItem("map",
         mainPanel(
           leafletOutput("map"),
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 400, height = "auto", style = "opacity: 0.92",
                         h3(" Account Selector"), br(),
                         selectizeInput("group"," Select Group Name", as.character(fulldata$group_name), selected = NULL, multiple = FALSE),
                         plotOutput("OnPeakPlot", height = 200, width = 400),
                         plotOutput("OffPeakPlot", height = 250, width = 400)
                         ))))
)
 

 )



