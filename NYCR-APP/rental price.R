library(ggplot2)
library(tidyverse)
library(shiny)

mydata <- read.csv('medianAskingRent_All.csv')
year_data <- mydata[,c(2,4:126)]
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

ui <- fluidPage(
  titlePanel("Manhattan NYC"),
  fluidRow(
    column(12,
    sidebarPanel(
      selectInput("input1","Select year:",choices = c(2010:2020))
    ),
    mainPanel(
      plotOutput(outputId = "plot1")
    )),
    column(12,
    sidebarPanel(
      selectInput("input2","Select year:",choices = c(2010:2020))
    ),
    mainPanel(
      plotOutput(outputId = "plot2")
      )
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
    titles <- paste0('The mean rent price in ',input$input1)
    ggplot(plotdata1,aes(month,price))+geom_bar(stat = "identity",fill="skyblue")+
      scale_x_continuous(breaks = plotdata1$month,labels = plotdata1$label)+
      theme(axis.text.x = element_text(angle =45))+ggtitle(titles)+xlab("Date")+
      ylab("Average price")+
      theme(axis.text.x = element_text(size = 16),axis.title.x = element_text(size = 14),
                                    axis.title.y = element_text(size = 14),
                                    plot.title = element_text(size = 20, face = "bold"))
  })
  
  output$plot2 <- renderPlot({
    temp <- subset(average,average$year==input$input2)
    titles <- paste0('The mean rent price of boroughs in ',input$input2)
    temp <- temp %>% group_by(Borough) %>% summarise(price = mean(price,na.rm = TRUE))
    ggplot(temp,aes(Borough,price))+geom_bar(stat = "identity",fill="skyblue")+
      ggtitle(titles)+xlab("Borough")+
      ylab("Average price")+
      theme(axis.text.x = element_text(size = 16),axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            plot.title = element_text(size = 20, face = "bold"))
  })
  
}

shinyApp(ui, server)




