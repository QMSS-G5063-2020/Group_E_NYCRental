library(psych)
library(stringr)
library(tmap)
library(leaflet)
library(rgdal)
library(purrr)
library(readr)
library(dplyr)
library(shiny)
library(tidyr)
library(reshape)
library(zoo)
library(xts)
library(dygraphs)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(devtools)
library(readr)
library(RColorBrewer)
library(geosphere)
library(rgeos)
library(mapview)
library(ggthemes)
library(scales)




dateInput <- function(inputId, label, value, min, max, format="yyyy-mm", minview = "days", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, value, min, max, format, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}


clean_up_rent<-function(rent){

  rent <- rent%>%subset(areaType == "neighborhood"&Borough=="Manhattan")

  rent$areaName <- rent$areaName%>%as.character()
  rent$areaName[rent$areaName == "Soho"] = "SoHo"
  rent$areaName[rent$areaName == "Central Harlem"] = "Harlem"
  rent$areaName[rent$areaName == "Flatiron"] = "Flatiron District"
  rent$areaName[rent$areaName == "Gramercy Park"] = "Gramercy"
  rent$areaName[rent$areaName == "Stuyvesant Town/PCV"] = "Stuyvesant Town"
  rent$areaName[rent$areaName == "Midtown West"] = "Hell's Kitchen"
  rent$areaName[rent$areaName == "Midtown"] = "Theater District"
  rent$areaName[rent$areaName == "Midtown East"] = "Midtown"
  kp<-rep(rent[rent$areaName == "Midtown South",])%>%as.data.frame()
  rent$areaName[rent$areaName == "Midtown South"] = "Murray Hill"

  kp$areaName <- as.character(kp$areaName)
  kp$areaName <- "Kips Bay"
  rent <- rbind(rent, kp)
  return (rent)

}

clean_up_invent <- function(invent){
  invent <- invent%>%subset(areaType == "neighborhood"&Borough=="Manhattan")

  invent$areaName <- invent$areaName%>%as.character()
  invent$areaName[invent$areaName == "Soho"] = "SoHo"
  invent$areaName[invent$areaName == "Central Harlem"] = "Harlem"
  invent$areaName[invent$areaName == "Flatiron"] = "Flatiron District"
  invent$areaName[invent$areaName == "Gramercy Park"] = "Gramercy"
  invent$areaName[invent$areaName == "Stuyvesant Town/PCV"] = "Stuyvesant Town"
  invent$areaName[invent$areaName == "Midtown West"] = "Hell's Kitchen"
  invent$areaName[invent$areaName == "Midtown"] = "Theater District"
  invent$areaName[invent$areaName == "Midtown East"] = "Midtown"
  kp<-rep(invent[invent$areaName == "Midtown South",])%>%as.data.frame()
  invent$areaName[invent$areaName == "Midtown South"] = "Murray Hill"

  kp$areaName <- as.character(kp$areaName)
  kp$areaName <- "Kips Bay"
  invent <- rbind(invent, kp)
  return (invent)
}



nybr <- readOGR("neigborhoods_bound.geojson", verbose= FALSE)
nybr<-nybr%>%subset(as.numeric(nybr@data$borough)==3)
nybr@data$neighborhood<- as.character(nybr@data$neighborhood)
nybr<-nybr%>%subset(!nybr@data$neighborhood%in%c("Randall's Island", "Ellis Island", "Liberty Island", "Governors Island"))

rent_all <- read.csv("medianRent.csv")
invent_all <- read.csv("rentalInventory_All.csv")
rent_all_all <- rent_all 
rent_all<-clean_up_rent(rent_all)
invent_all <- clean_up_invent(invent_all)

rent_studio <- read.csv("medianAskingRent_Studio.csv")
invent_studio <- read.csv("rentalInventory_Studio.csv")
rent_studio_all <- rent_studio 
rent_studio<-clean_up_rent(rent_studio)
invent_studio <- clean_up_invent(invent_studio)

rent_1b <- read.csv("medianAskingRent_OneBd.csv")
invent_1b <- read.csv("rentalInventory_OneBd.csv")
rent_1b_all <- rent_1b 
rent_1b<-clean_up_rent(rent_1b)
invent_1b <- clean_up_invent(invent_1b)


rent_2b <- read.csv("medianAskingRent_TwoBd.csv")
invent_2b <- read.csv("rentalInventory_TwoBd.csv")
rent_2b_all <- rent_2b 
rent_2b<-clean_up_rent(rent_2b)
invent_2b <- clean_up_invent(invent_2b)


rent_3b <- read.csv("medianAskingRent_ThreePlusBd.csv")
invent_3b <- read.csv("rentalInventory_ThreePlusBd.csv")
rent_3b_all <- rent_3b
rent_3b <-clean_up_rent(rent_3b)
invent_3b <- clean_up_invent(invent_3b)

PAGE_TITLE <- "Rental Genius|Manhattan, NY"

condo<- read.csv("DOF__Condominium_Comparable_Rental_Income___Manhattan___FY_2011_2012.csv")

geo_condo <- condo%>%
  filter(!is.na(Latitude))

content1 <- paste("Address:",geo_condo$MANHATTAN.CONDOMINIUMS.COMPARABLE.PROPERTIES.Address,"<br/>",
                  "Neighborhood:",geo_condo$MANHATTAN.CONDOMINIUMS.COMPARABLE.PROPERTIES.Neighborhood,"<br/>",
                  "Building Classification:",geo_condo$COMPARABLE.RENTAL.1.Building.Classification,"<br/>",
                  "Rental per SqFt:",geo_condo$MANHATTAN.CONDOMINIUMS.COMPARABLE.PROPERTIES.Market.Value.per.SqFt,"<br/>")

pal1 = colorQuantile("dodgerblue", domain = geo_condo$MANHATTAN.CONDOMINIUMS.COMPARABLE.PROPERTIES.Market.Value.per.SqFt%>%as.numeric())
color_neighborhood1 = pal1(geo_condo$MANHATTAN.CONDOMINIUMS.COMPARABLE.PROPERTIES.Market.Value.per.SqFt%>%as.numeric())

coop<- read.csv("DOF__Cooperative_Comparable_Rental_Income___Manhattan__FY_2011_2012.csv")

geo_coop <- coop%>%
  filter(!is.na(Latitude))

content2 <- paste("Address:",geo_coop$MANHATTAN.COOPERATIVES.COMPARABLE.PROPERTIES.Address,"<br/>",
                  "Neighborhood:",geo_coop$MANHATTAN.COOPERATIVES.COMPARABLE.PROPERTIES.Neighborhood,"<br/>",
                  "Building Classification:",geo_coop$MANHATTAN.COOPERATIVES.COMPARABLE.PROPERTIES.Building.Classification,"<br/>",
                  "Rental per SqFt:",geo_coop$MANHATTAN.COOPERATIVES.COMPARABLE.PROPERTIES.Gross.Income.per.SqFt,"<br/>")

pal2 = colorQuantile("dodgerblue", domain = geo_coop$MANHATTAN.COOPERATIVES.COMPARABLE.PROPERTIES.Gross.Income.per.SqFt%>%as.numeric())
color_neighborhood2 = pal2(geo_coop$MANHATTAN.COOPERATIVES.COMPARABLE.PROPERTIES.Gross.Income.per.SqFt%>%as.numeric())


ui =  navbarPage(
  title = div(PAGE_TITLE, img(src = "city.png", height="30px", 
                              style = "position: relative; top: -3px")),
  theme = shinytheme("sandstone"),
  
  tabPanel("About",
            fluidRow(
             column(width=12, h2(strong("Rental Apartments in Manhattan"), style = "font-size:30px; color:black;text-align:center;position:relative"))
           ),
            fluidRow(
             column(width=8, offset = 2, uiOutput("preface"), 
                    fluidRow(
                      column(12, uiOutput("nycstreet"))
                    ))
             )
           
              
           
           ),
  
  tabPanel( "Overview",
  
  
    tags$head(tags$style(
    type = "text/css",
      "#controlPanel {background-color: rgba(255,255,255,0.8);}",
      ".leaflet-top.leaflet-right .leaflet-control {
        margin-top: 580px;
        margin-right: 160px
      }",
      ".dygraph-title {
      color: white;
      font-weight: bold;
      }",
      ".dygraph-axis-label {
      color:white;
      }", 
      ".dygraph-legend {
      color:black;
      }",
      "#date{
      color:white
      }",
      ".h1 {
      margin-bottom: 100px;
      }",
      "#h1 {
      margin-bottom: 100px;
      }"
    )),


  
    fixedPanel(
      id = "fullscreen",
      top = 60,
      left = 0,
      width = "100%",
      height = "100%",
      leafletOutput("nyplot", width = "100%", height = "100%")
    ),
  
    absolutePanel(
      id = "controls",
      draggable = FALSE,
      top = 65,
      right = 2,
      width = 400,
      height = "auto",
      id = "input_panel",
      style = "background-color:rgba(0, 0, 0, 0.5); padding:10px;",
    
    
      tabsetPanel(
        tabPanel(
          shiny::HTML("<p><span style='color: black'>Overview</span></p>"),
          dateInput("date", 
                    shiny::HTML("<p><span style='color: white'>Select a Date</span></p>"),
                    "2010-01","2010-01","2020-03", 
                    startview = "year", minview = "months", maxview = "decades"),
          selectInput(
            "bedroom",
            shiny::HTML("<p><span style='color: white'>Bedroom Count</span></p>"),
            c("All", "Studio", "One Bedroom", "Two Bedroom", "Three Bedroom+"),
            selected = "All"
          ),
        
          textOutput("date"),
          uiOutput("rentsource")
          ),
        
        tabPanel(
          shiny::HTML("<p><span style='color: black'>Neighborhood</span></p>"),
          selectInput(
            'neighborhood',
            shiny::HTML("<p><span style='color: white'>Select a Neighborhood</span></p>"),
            rent_all$areaName,
            selected = 'Midtown'
          ),
          selectInput(
            "bedroom2",
            shiny::HTML("<p><span style='color: white'>Bedroom Count</span></p>"),
            c("All", "Studio", "One Bedroom", "Two Bedroom", "Three Bedroom+"),
            selected = "All"
          ),
          dygraphOutput("chart", width = "100%")
        )
      )
    ),
  
  ),
  tabPanel("Time Series",
           titlePanel("Manhattan NYC"),
           fluidRow(
                    sidebarPanel(
                      selectInput("input1","Select a Year",choices = c(2010:2020)),
                      selectInput(
                        "bedroom3",
                        shiny::HTML("Bedroom Count"),
                        c("All", "Studio", "One Bedroom", "Two Bedroom", "Three Bedroom+"),
                        selected = "All"
                      )
                      
                    ),
                    mainPanel(
                      plotOutput(outputId = "plot1", width = 635,height = 400),
                      br(),
                      br(),
                      plotOutput(outputId = "plot2", width = 635,height = 400)
                    )
                  )
          ),
  tabPanel("Renters",
    tabsetPanel(
      tabPanel("Characteristics",
               fluidRow(
                 column(width=12, h2(strong("New York City Rental Housing & Renter Characteristics"), style = "font-size:30px; color:black;margin-left:1.5em")
               )),
               fluidRow(
                 column(width=10, offset=1,uiOutput("nyccha"))
               )
               ),
      tabPanel("Review", 
               fluidRow(
                 column(width=12, h2(strong("Renter Review"), style = "font-size:30px; color:black;margin-left:1.5em"))
               ),
               fluidRow(
                 column(width=10, offset=1,uiOutput("nycrev"))
               )
               )

    )
  ),
  
  tabPanel("Condo",
           tabsetPanel(
             tabPanel("Summary",
                uiOutput("condo")),
               tabPanel("Detail",
                        fluidRow(
                          column(width=12,h2(strong("Leaflet Map"), style = "font-size:30px; color:black;margin-left:0.3em")
                        )),
                        fluidRow(
                          column(width=11, offset=1,h2(strong("Condo"), style = "font-size:15px; color:black")
                        )),
                        fluidRow(
                          column(8, offset=1, leafletOutput("condoleaf"))
                        ),
                        fluidRow(
                          column(width=11, offset=1,h2(strong("Coop"), style = "font-size:15px; color:black")
                        )),
                        fluidRow(
                          column(8, offset=1, leafletOutput("condoleaf2"))
                        ),
                        fluidRow(
                          column(width=11, offset=1,h2(strong("Comparison"), style = "font-size:15px; color:black")
                        )),
                        fluidRow(
                          column(10, offset=1, uiOutput("condoleaf3"))
                        )
                )
           ))
)


  
server = function(input, output) {
  
  rent <- reactive({
    if (input$bedroom =="All"){
      rent <- rent_all
    }
    else if (input$bedroom =="Studio"){
      rent <- rent_studio
    }
    else if (input$bedroom =="One Bedroom"){
      rent <- rent_1b
    }
    else if (input$bedroom =="Two Bedroom"){
      rent <- rent_2b
    }
    else if (input$bedroom =="Three Bedroom+"){
      rent <- rent_3b
    }
  })
  
  rent2 <- reactive({
    if (input$bedroom2 =="All"){
      rent <- rent_all
    }
    else if (input$bedroom2 =="Studio"){
      rent <- rent_studio
    }
    else if (input$bedroom2 =="One Bedroom"){
      rent <- rent_1b
    }
    else if (input$bedroom2 =="Two Bedroom"){
      rent <- rent_2b
    }
    else if (input$bedroom2 =="Three Bedroom+"){
      rent <- rent_3b
    }
  })
  
  rent3 <- reactive({
    if (input$bedroom3 =="All"){
      rent <- rent_all
    }
    else if (input$bedroom3 =="Studio"){
      rent <- rent_studio
    }
    else if (input$bedroom3 =="One Bedroom"){
      rent <- rent_1b
    }
    else if (input$bedroom3 =="Two Bedroom"){
      rent <- rent_2b
    }
    else if (input$bedroom3 =="Three Bedroom+"){
      rent <- rent_3b
    }
  })
  
  rent4 <- reactive({
    if (input$bedroom3 =="All"){
      rent <- rent_all_all
    }
    else if (input$bedroom3 =="Studio"){
      rent <- rent_studio_all
    }
    else if (input$bedroom3 =="One Bedroom"){
      rent <- rent_1b_all
    }
    else if (input$bedroom3 =="Two Bedroom"){
      rent <- rent_2b_all
    }
    else if (input$bedroom3 =="Three Bedroom+"){
      rent <- rent_3b_all
    }
  })
  
  invent <- reactive({
    if (input$bedroom =="All"){
      invent <- invent_all
    }
    else if (input$bedroom =="Studio"){
      invent <- invent_studio
    }
    else if (input$bedroom =="One Bedroom"){
      invent <- invent_1b
    }
    else if (input$bedroom =="Two Bedroom"){
      invent <- invent_2b
    }
    else if (input$bedroom =="Three Bedroom+"){
      invent <- invent_3b
    }
  })

  output$nyplot <- renderLeaflet({
    
    rent <- rent()
    invent <- invent()

    dt <- input$date%>%format("%Y.%m")%>%as.character()
    dt <- str_c("X", dt)
    df <- rent%>%select(c(areaName, Borough, areaType, dt))
    
    colnames(df)[1] = "neighborhood"
    avg <- df%>%group_by(neighborhood)%>%summarise_at(.vars = names(.)[4], mean)
    avg <- avg%>%subset(neighborhood%in%nybr@data$neighborhood)%>%na.omit()
    colnames(avg)[2]<-"mean"
    
    df2 <- invent%>%select(c(areaName, Borough, areaType, dt))
    
    colnames(df2)[1] = "neighborhood"
    summ <- df2%>%group_by(neighborhood)%>%summarise_at(.vars = names(.)[4], sum, round=1)
    summ <- summ%>%subset(neighborhood%in%nybr@data$neighborhood)%>%na.omit()
    colnames(summ)[2]<-"sum"
    
    nybr@data<-merge(nybr@data, avg, by = "neighborhood", all = T)
    nybr@data<-merge(nybr@data, summ, by = "neighborhood", all = T)
    
    tm1 <- tm_shape(nybr) + tm_fill("mean", title = str_c("Median Rent"," [",input$date%>%format("%B, %Y")%>%as.character(),"]"),
                                            popup.vars=c("Median:$"="mean", "Inventory: "="sum"), style = "pretty") +
      tm_text("neighborhood", shadow=TRUE, bg.color="white", bg.alpha=.25,size=.6, remove.overlap=TRUE) 
    
    tmap_leaflet(tm1)
    
    
    })
  
    output$date <- renderText({
      input$date%>%format("%B, %Y")%>%as.character()
    })
    
   
    output$preface <- renderUI({
      HTML("<br><center><p style='font-size:18px;font-family:Lucida Sans Unicode'>We use multiple datasets of rental information of Manhattan, NY, to closely
      examine several different aspects of these apartments. Hopefully, this 
      website can help future renters to get a clear picture of the overall rental 
      market in Manhattan, NY, and select the one that suits them the most with ease.</p><br>
      <i><h4 style='font-weight:bold;font-color:black;'>OVERVIEW</h4></i>
      <p style='font-size:18px;font-family:Lucida Sans Unicode'>Under the overview tab, renters can access the median rent price of different neighborhoods
      and different room types across time. Users can also find the price trends of every neighborhood in
      Manhattan under the neighborhood tab.</p> <br>
      <i><h4 style='font-weight:bold;font-color:black;'>TIME SERIES</h4></i>
      <p style='font-size:18px;font-family:Lucida Sans Unicode'>The time series tab includes average monthly rent in Manhattan in a 
       selected year and a comparison between Manhattan and other boroughs in NYC with regard to rent price. </p> <br>
       <i><h4 style='font-weight:bold;font-color:black;'>RENTERS</h4></i>
      <p style='font-size:18px;font-family:Lucida Sans Unicode'>The renters tab contains two sub-tabs. The characteristics tab contains rental housing characteristics and renter
      characteristics. The review tab includes text analysis of renter reviews.</p> <br>
       <i><h4 style='font-weight:bold;font-color:black;'>CONDO</h4></i>
      <p style='font-size:18px;font-family:Lucida Sans Unicode'>
      The condo tab also contains two sub-tabs. The summary tab shows a
      comparison between the rental per square feet value of condos and cooperatives. The detail tab includes 
      two leaflet maps that displays all the condos and cooperatives valued by NY State government in Manhattan.</p>
      <br>
      <a href='https://github.com/QMSS-G5063-2020/Group_E_NYCRental'>Link to Github</a>
      <br>
      <br>
      <a href='https://www.loom.com/share/e4a12b9731c24d59a47b34f852d64aad'>Link to Presentation</a>
      <br>
           </p></center>"                   
              )
    })
    
    output$nyccha <- renderUI({
      HTML('<br>
      Data source: American Community Survey (ACS) - Census Bureau <br>
           <br>
            <a href="https://data.census.gov/cedsci/table?d=ACS%205-Year%20Estimates%20Data%20Profiles&table=DP04&tid=ACSDP5Y2018.DP04&g=
           0400000US36_1600000US3651000">Link to Data</a><br />
        
           <br>
            The American Community Survey (ACS) contains data profiles of social, 
           economic, housing, and demographic characteristics of various geography type. 
           The most specific geography type that we concern is New York City, New York. 
           The data from the website of the United States Census Bureau are ACS 5-Year 
           Estimates Data Profiles that are available from 2010 to 2018.<br>
           <br>
            The first plot is about housing occupancy. Overall, the rental vacancy rate is 
           lowering from 2010 to 2018, which means more rental housing units are being occupied.<br>
           <br>
           
           <img src="Renter/unnamed-chunk-1-1.png" style="width:650px;height:450px;"> <br>
           <br>
           The second plot shows that the percentage of renter-occupied unit among all occupied housing 
           unit experience a peak in 2015. Throughout the years, this number is always over 67%. Not surprisingly, 
           renters consistently outnumber owners in NYC.<br>
           <br>
           <img src="Renter/unnamed-chunk-1-2.png" style="width:650px;height:450px;"> <br> 
           <br>
           The average household size of renter-occupied unit shows an increasing trend. Each renter-occupied 
           unit is usually occupied by 2.5 renters.<br>
           <br>
           <img src="Renter/unnamed-chunk-1-3.png" style="width:650px;height:450px;"> <br> 
           <br>
           The median gross rent, in dollars, keeps increasing from 2010 to 2018. This increase is over 300 dollars.<br>
           <br>
           <img src="Renter/unnamed-chunk-1-4.png" style="width:650px;height:450px;"> <br> 
           <br>
           When take a closer look at the gross rent as a percentage of household income, more than 40% of the renters 
           spend over 35% of their income on rent.<br>
           <br>
           <img src="Renter/unnamed-chunk-5-1.png" style="width:680px;height:450px;"> <br> 
           <br>
           As for the cross-year-within-group comparison, two groups fluctuate most: the group that spend less than 15% 
           of their income on rent and the one that spend 35% or higher. The four middle groups experience less fluctuation 
           over years. In general, NYC renters spend a large portion of their household income on gross rent and the burden
           for the less privileged has an increasing trend.<br>
           <br>
           <img src="Renter/unnamed-chunk-6-1.png" style="width:660px;height:450px;"> <br> 
           <br>')
    })
    
    output$nycrev <- renderUI({
      HTML('<br>
      Reviews are obained from the apartmentratings.com.<br>
           <br>
           <a href="https://www.apartmentratings.com/ny/new-york/">Link to Data</a><br/>
           <h3 style = "font-weight:bold; color:black">Good Review</h3>
           We clean up the reviews for text analysis. This is the wordcloud for the good reviews. 
           "Great", "staff", "friend", and "nice" are some popular words used by renters. <br>
           <img src="Review/unnamed-chunk-2-1.png" style="width:650px;height:450px;"> <br> 
           <br>
           <h3 style = "font-weight:bold; color:black">Bad Review</h3>
           The wordcloud for bad reviews contains words like "manage", "rent", "tenant", "move", and "lease". 
           These are potentially common complaints or issues raised by residents.<br>
            <img src="Review/unnamed-chunk-3-1.png" style="width:650px;height:450px;"> <br> 
           <br>
           <h3 style = "font-weight:bold; color:black">Pyramid Plot</h3>
           This pyramid plot compares words used in common in good reviews and bad reviews. 
           "Live", "staff", and "nice" stand out in the good reviews. "Rent", "tenant", and 
           "lease" are more frequently used in bad reviews as previously shown in the wordcloud.<br>
           <br>
           <img src="Review/unnamed-chunk-4-1.png" style="width:700px;height:450px;"> <br> 
           <br>
           <h3 style = "font-weight:bold; color:black">Comparison Cloud</h3>
           This comparison cloud is based on the positive words in good reviews and negative words in bad reviews.
           The words in red on the top half of the comparison cloud are very positive. It is reasonable to infer 
           that residents "love" their apartment and have "fun" living there. The words in blue on the bottom half 
           are very negative and problematic. "Dirt", "bug", "rust", "damage", and "allergies" appear in this part. 
           Adjectives like "aloof", "annoying", "aggressive", and "atrocious" demonstrate the unpleasant experience 
           and feelings of the residents.<br>
           <br>
           <img src="Review/unnamed-chunk-5-1.png" style="width:650px;height:450px;"> <br> 
           <br>
           <h3 style = "font-weight:bold; color:black">Sentiment Analysis</h3>
           This sentiment analysis is based on the NRC dictionary. It categorizes words into ten emotional categories.
           Bad review is more sentimental in all categories except "joy", which is slightly higher in good review. 
           Bad review has very distinctive emotions like "negative", "sadness", "fear", "disgust", and "anger".<br>
           <img src="Review/unnamed-chunk-6-1.png" style="width:750px;height:550px;"
           <br>')
      
    })
    
    output$nycstreet <- renderUI({
      HTML('<br>
      <br>
           <center><img src="nycb.png" style="width:340px;height:340px;
  position:relative"></center> <br> 
           ')
    })
    
    output$rentsource <- renderUI({
      HTML("<a href='https://streeteasy.com/blog/data-dashboard/
           ?agg=Total&metric=Inventory&type=Rentals&bedrooms=Three%20or%20More%20Bedrooms&property
           =Any%20Property%20Type&minDate=2010-01-01&maxDate=2020-01-01&area=Flatiron,Brooklyn%20Heights'>
           Link to Data</a><br/>")
    })
    
    output$condo <- renderUI({
      HTML("<h2 style = 'font-weight:bold; color:black'>Year Built & Market value Per square 
           Feet </h2>
           <br>
           <h4 style = 'font-weight:bold; color:black'>Condominium</h4>
           <img src='condo1.png' style='width:650px;height:450px;'>
           <h4 style = 'font-weight:bold; color:black'>Cooperative</h4>
           <img src='condo2.png' style='width:650px;height:450px;'>")
    })
    
    average <- reactive({
      rent_t <-rent3()
      year_data <- rent_t[,c(2,4:126)]
      plotdata_month <- year_data%>%gather(time,value,-Borough)
      plotdata_month$time <- gsub("X","",plotdata_month$time)
      
      plotdata_month$month <- lapply(plotdata_month$time,function(x){
        temp <- unlist(strsplit(x,split = ".",fixed = TRUE))
        temp[2]
      })
      
      plotdata_month$year <- lapply(plotdata_month$time,function(x){
        temp <- unlist(strsplit(x,split = ".",fixed = TRUE))
        temp[1]
      })
      plotdata_month$month <- as.numeric(plotdata_month$month)
      plotdata_month$year <- as.numeric(plotdata_month$year)
      
      average <- plotdata_month %>% group_by(year,month,Borough) %>% 
        summarise(price = mean(value,na.rm = TRUE))
      
      average[average==""] <- NA
      average <- na.omit(average)
      
      average
    })
    
    average2 <- reactive({
      rent_t <-rent4()
      year_data <- rent_t[,c(2,4:126)]
      plotdata_month <- year_data%>%gather(time,value,-Borough)
      plotdata_month$time <- gsub("X","",plotdata_month$time)
      
      plotdata_month$month <- lapply(plotdata_month$time,function(x){
        temp <- unlist(strsplit(x,split = ".",fixed = TRUE))
        temp[2]
      })
      
      plotdata_month$year <- lapply(plotdata_month$time,function(x){
        temp <- unlist(strsplit(x,split = ".",fixed = TRUE))
        temp[1]
      })
      plotdata_month$month <- as.numeric(plotdata_month$month)
      plotdata_month$year <- as.numeric(plotdata_month$year)
      
      average <- plotdata_month %>% group_by(year,month,Borough) %>% 
        summarise(price = mean(value,na.rm = TRUE))
      
      average[average==""] <- NA
      average <- na.omit(average)
      
      average
    })
    
    
    output$plot1 <- renderPlot({
      
      average <- average()
      
      plotdata1 <- subset(average,average$year==input$input1)
      plotdata1$label <- paste0(plotdata1$year,'/',plotdata1$month)
      
      titles <- paste0('Mean Rent Price in ',input$input1)
      
      ggplot(plotdata1, aes(x = month, y = price))+ 
        geom_line() + geom_point(size=2.5, color="orangered") + 
        theme_fivethirtyeight()+scale_x_continuous('Date',breaks = seq(1,12,1), 
                                                  label= c("Jan", "Feb","Mar","Apr","May","Jun","Jul","Aug", 
                                                           "Sept","Oct","Nov","Dec") )+
        geom_text(aes(label= sprintf("$%s", round(price,0))), vjust=-0.6) +  
        xlab("Date") + ylab("Average Median Asking") + 
        ggtitle(titles) +
        theme(plot.title = element_text(hjust = 0.5, size=14, color="grey25"),
              panel.grid.minor = element_blank(),panel.background = element_blank(), panel.grid.major.x = element_blank())  
    })
    
    output$plot2 <- renderPlot({
      
      average <- average2()
      
      temp <- subset(average,average$year==input$input1)
      titles <- paste0('Borough Comparison ',input$input1)
      temp <- temp %>% group_by(Borough) %>% summarise(price = mean(price,na.rm = TRUE))
      ggplot(temp,aes(Borough,price))+geom_bar(stat = "identity",fill="skyblue")+
        ggtitle(titles)+xlab("Borough")+
        ylab("Average price")+ theme_fivethirtyeight()+
        theme(plot.title = element_text(hjust = 0.5, size=14, color="grey25"),
              panel.grid.minor = element_blank(),panel.background = element_blank())
    })
    
    output$condoleaf <- renderLeaflet({
      
      
      map_condo <- leaflet(geo_condo)%>%
        addTiles() %>%  
        addCircleMarkers(lng = ~Longitude, 
                   lat = ~Latitude,
                   popup = content1,
                   color = color_neighborhood1,
                   clusterOptions = markerClusterOptions())
      map_condo
    })
    
    output$condoleaf2 <- renderLeaflet({
      
      map_coop <- leaflet(geo_coop)%>%
        addTiles() %>%  
        addCircleMarkers(lng = ~Longitude, 
                   lat = ~Latitude,
                   popup = content2,
                   color = color_neighborhood2, 
                   clusterOptions = markerClusterOptions())
      
      map_coop
      
    })
    
    output$condoleaf3<-renderUI({

      map_condo <- leaflet(geo_condo)%>%
        addTiles() %>%  
        addCircleMarkers(lng = ~Longitude, 
                   lat = ~Latitude,
                   popup = content1,
                   color = color_neighborhood1,
                   clusterOptions = markerClusterOptions())
      
      
      map_coop <- leaflet(geo_coop)%>%
        addTiles() %>%  
        addCircleMarkers(lng = ~Longitude, 
                   lat = ~Latitude,
                   popup = content2,
                   color = color_neighborhood2,
                   clusterOptions = markerClusterOptions())
      
      sync(map_condo, map_coop)
      
    })
    

    
    
    
    output$chart <- renderDygraph({
      rent <- rent2()
      
      nb <- input$neighborhood
      df3 <- rent%>%subset(areaName==nb)
      df3 <- df3%>%select(-c(Borough, areaType))

      df3<-melt(df3)
      df3$variable <- df3$variable%>%as.character()%>%str_replace("X","")%>%str_replace("\\.","-")
      df3$variable <- as.Date(as.yearmon(df3$variable))
      colnames(df3)[3] <- "Median Asking Rent"

      xts(df3, order.by = df3$variable)

      dygraph(xts(df3, order.by = df3$variable),
        main = str_c("Median Asking Rent in ", input$neighborhood),
        xlab = "Year")%>%
        dyOptions(colors = c("red", "black")) %>%
        dyRangeSelector(height = 20) %>%
        dyHighlight(
          highlightCircleSize = 5,
          highlightSeriesBackgroundAlpha = 0.4,
          hideOnMouseOut = FALSE
          )
      
    })
    

}

shinyApp(ui=ui, server=server)


























