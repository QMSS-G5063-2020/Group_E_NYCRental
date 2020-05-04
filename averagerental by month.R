library(ggplot2)
library(tidyverse)
library(shiny)

mydata <- read.csv('medianAskingRent_All.csv')
year_data <- mydata[,c(4:126)]
plotdata_month <- year_data%>%gather(time,value)
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

average <- plotdata_month %>% group_by(year,month) %>% 
  summarise(price = mean(value,na.rm = TRUE))

ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
    selectInput("input1","Select year:",choices = c(2010:2020))
    ),
    mainPanel(
      plotOutput(outputId = "plot1")
    )
  )
)

server <- function(input, output) {
  
  plotdata <- reactive({
    temp <- subset(average,average$year==input$input1)
    temp$label <- paste0(temp$year,'/',temp$month)
    temp
  })

  output$plot1 <- renderPlot({
    plotdata1 <-plotdata()
    ggplot(plotdata1,aes(month,price))+geom_bar(stat = "identity",fill="skyblue")+
      scale_x_continuous(breaks = plotdata1$month,labels = plotdata1$label)+xlab("")+
      theme(axis.text.x = element_text(angle =45))
  })
  
}

shinyApp(ui, server)




