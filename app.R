library(shiny) #The gist of R Shiny
library(tidyverse)#Actually a combination of packages that make R Code more legible
library(readxl)#To make it easier for me to use an excel file
library(kableExtra)#I used this to help create the table
library(scales) #I used this to make the line chart easier to construct


#This is the UI which only comprises of 3 elements.
ui <- fluidPage(
  
  titlePanel("Cumulative Paid Claims"),
  
  splitLayout(fileInput("data","Upload your claims data here:"),
              numericInput("t_f","Tail Factor",1.1),
              tableOutput(outputId = "cpc_Table"),
              plotOutput(outputId = "cpc_Graph"))
)
#This is the Server part. ipc = Incremental Paid Claims, cpc = Cumulative Paid Claims
server <- function(input, output){
  data <- reactive({
    req(input$data)
    read_excel(input$data$datapath)
  })
  observe({
    Claims <- data()
    
    tf<-input$t_f
    ipc11 <- Claims[Claims$`Loss Year`==2017&Claims$`Development Year`<2,]
    cpc11 <- sum(ipc11[,3])
    ipc12 <- Claims[Claims$`Loss Year`==2017&Claims$`Development Year`<3,]
    cpc12 <- sum(ipc12[,3])
    ipc13 <- Claims[Claims$`Loss Year`==2017&Claims$`Development Year`<4,]
    cpc13 <- sum(ipc13[,3])
    
    cpc14 <- cpc13*tf
    
    ipc21 <- Claims[Claims$`Loss Year`==2018&Claims$`Development Year`<2,]
    cpc21 <- sum(ipc21[,3])
    ipc22 <- Claims[Claims$`Loss Year`==2018&Claims$`Development Year`<3,]
    cpc22 <- sum(ipc22[,3])
    cpc23 <- (cpc13/cpc12)*cpc22
    cpc24 <- cpc23*tf
    
    ipc31 <- Claims[Claims$`Loss Year`==2019&Claims$`Development Year`<2,]
    cpc31 <- sum(ipc31[,3])
    cpc32 <- sum(cpc12,cpc22)/sum(cpc11,cpc21)*cpc31
    cpc33 <- (cpc13/cpc12)*cpc32
    cpc34 <- cpc33*tf
    
    
    cpc <- data.frame(Loss_Year = c(2017, 2018, 2019),
                      dy1=c(cpc11, cpc21, cpc31),
                      dy2=c(cpc12, cpc22, cpc32),
                      dy3=c(cpc13, cpc23, cpc33),
                      dy4=c(cpc14, cpc24, cpc34))
    
    cpc$dy1 <- prettyNum(cpc$dy1,big.mark = ",")
    cpc$dy2 <- prettyNum(cpc$dy2,big.mark = ",")
    cpc$dy3 <- prettyNum(cpc$dy3,big.mark = ",")
    cpc$dy4 <- prettyNum(cpc$dy4,big.mark = ",")
    
    # This is the Output Table
    output$cpc_Table <- function(){
      kable(cpc, col.names = c("Loss Year", "1", "2", "3", "4"),
            caption = "Cumulative Paid Claims ($)",
            format = "html") %>%
        add_header_above(c(" " = 1, "Development Year" = 4)) %>%
        kable_styling(latex_options = "hold_position", position = "center")
    }
    
    ly1 <- c(cpc11, cpc12, cpc13, cpc14)
    ly2 <- c(cpc21, cpc22, cpc23, cpc24)
    ly3 <- c(cpc31, cpc32, cpc33, cpc34)
    dy <- c(1, 2, 3, 4)
    Cpc <- data.frame(dy,ly1,ly2,ly3)
    Cpc_group <- data.frame(x = Cpc$dy,
                            y = c(Cpc$ly1,Cpc$ly2,Cpc$ly3),
                            group = c(rep("2017", nrow(Cpc)),rep("2018", nrow(Cpc)),rep("2019", nrow(Cpc))))
    
    # This is the Output Line Chart
    output$cpc_Graph <- renderPlot({
      ggplot(Cpc_group, aes(x, y, col = group, label = y)) + geom_smooth(size = 1) + geom_point(size = 2) + geom_text(aes(label = comma(y)),
                                                                                                                      subset(Cpc_group)) + ggtitle("Cumulative Paid Claims vs Development Year") + scale_x_continuous(name = "Development Year") + scale_y_continuous(name = "Cumulative Claims Paid") 
    })
  })
}

shinyApp(ui = ui, server = server)
