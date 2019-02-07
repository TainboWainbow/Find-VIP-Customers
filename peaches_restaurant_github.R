library(shiny) #shiny web application
library(plotly) #for interactive plot
library(datasets)
library(dplyr)
library(lubridate) #to format date and time values
library(tidyr) #to separate a column into two
library(tidyverse)
library(ggplot2)
library(DT) #interactive datatable
library(rsconnect) #to host the application on website
library(shinythemes) #to choose different themes

setwd("C:/Users/User/Desktop/for github")
custdat<-read.table("sample_customer_data_github.txt",stringsAsFactors = FALSE)

################## CLEAN & EXTRACT DATA ######################
colnames(custdat)<-c("UserID","Restaurant","Subtotal","Tip","Date_of_activity","Time_of_activity",
                     "fav_dish","fav_drnk","focus_gr")
#adjusting the format of date, time data for ggplot2
custdat$"Date_of_activity"<-format(custdat$"Date_of_activity", format="%Y-%m-%d")

#setting data in order of date (necessary for faster programming later on)
custdat<-custdat[order(custdat$"Date_of_activity"),]

#making Restaurant variable as factor
custdat$"Restaurant"<-as.factor(custdat$"Restaurant")

#making additional columns for facility of filtering
custdat$"Month"<-c(rep(0,times=nrow(custdat)))
custdat$"Year"<-c(rep(0,times=nrow(custdat)))
for (i in 1:nrow(custdat)){
  custdat$Month[i]<-month(custdat$Date_of_activity[i],label=TRUE) %>% as.character()
  custdat$Year[i]<-year(custdat$Date_of_activity[i]) %>% as.character()
  
}

#making a column that represent #visits made the month at that restaurant
#for that user
custdat$"Freq"<-c(rep(0,times=nrow(custdat)))
result<-c()#will contain resulting dataframe

for (i in unique(custdat$UserID)){
  u_indices <- which(custdat$UserID==i)
  userdf <- custdat[u_indices,]
  for (j in unique(userdf$Month)){#for each month
    m_indices <- which(userdf$Month==j)
    userm <- userdf[m_indices,]
    for (k in unique(userdf$Restaurant)){#for each restaurant user visited
      r_indices <- which(userm$Restaurant==k)
      userr<-userm[r_indices,]
      #NOTE: userr dataframe represents data of one restaurant in a given month
      
      userr$"Freq"<-c(rep(nrow(userr),times=nrow(userr)))
      result<-rbind(result,userr)
    }
  }
}
result$Month<-as.factor(result$Month)
result$Year<-as.factor(result$Year)
result$Date_of_activity<-format(result$"Date_of_activity", format="%Y-%m-%d")

################## UI & SERVER ######################
ui<-fluidPage(
  
  titlePanel(tags$strong("Welcome to",
                         img(src="peaches_cleanfont.jpg", height="70px")),
             windowTitle="Peaches"),

  theme=shinytheme("united"),
  
  h3("View As: Restaurant Management Team"),
  
  #### FIRST ROW ####
  fluidRow(
    column(width=12,
        selectInput(inputId = "chooserest", h5("Select your restaurant: "), 
                    choices=levels(result$Restaurant),selected="Father and Sons")
      )
  ),
  hr(),
  
  #### SECOND ROW ####
  fluidRow(
    column(width=4,
        h3("Select Time Period"), hr(),
        selectInput(inputId = "year", "Select year of data: ", 
                    choices=levels(result$Year), selected="2018"),
        selectInput(inputId="month","Select month of data: ",
                    choices=levels(result$Month), selected="Sep"),
        #button
        actionButton("plotbutton","Update Time")
      ),
    column(width=4,  
        h3("Select Plot Variables"), hr(),
        selectInput(inputId="yvar", "Select variable: ",
                  choices=c("Subtotal","Tip"),selected="Subtotal"),
      
        selectInput(inputId="xvar", "Against time : ",
                  choices=c("Throughout the selected month" = "Date_of_activity",
                            "Of day" = "Time_of_activity")),
        
        radioButtons(inputId="pcolor","Plot colour by: ",
                      choices=c("Number of visits made this month", "Focus group"),
                      selected="Focus group")

    ),
    column(width=4,
           h3("Find Customer Segment"), hr(),
           numericInput("fsubtotal","Find customers whose subtotal($) were at least: ",min=0,value=0),
           numericInput("ftip","Find customers whose tip($) were at least: ",min=0,value=0),
           numericInput("fvisit","Find customers whose numbers of visits this month were at least: ",min=0,value=0),
           #button
           actionButton("tabbutton","Refresh Table")
    
    )
  ),

  #### THIRD ROW ####

  fluidRow(
    #PLOT & TABLE outputs
    column(width=10,
      #TEXT output
      textOutput(outputId="info"),
      br(),
      tabsetPanel(type="pills",
        tabPanel("Plot", plotlyOutput("plotit")),
        tabPanel("Table", DT::dataTableOutput("tab"))
        ))
      ),
  br(),
  downloadButton("downloadMonth","Download this month's data"),
  br(),br(),
  downloadButton("downloadFilter","Download filtered customer group data"),
  hr()
)

server<-function(input, output, session) {

  #extracting data for selected restaurant
  getrest<- eventReactive(input$chooserest,{
    indices<-which(result$Restaurant==input$chooserest)
    result[indices,]
  })
  
  #selecting data with correct year and month
  getyear <- eventReactive( input$plotbutton, {
    indices <- which(getrest()$Year==input$year)
    getrest()[indices,]
    })
  getmonth <- eventReactive( input$plotbutton, {
    indices <- which(getyear()$Month==input$month)
    getyear()[indices,]
    })
      ## NOTE: use getmonth() for plot ##
   
  filter1 <- eventReactive( c(input$tabbutton,input$plotbutton), {
    indices <- which(getmonth()$Subtotal >= input$fsubtotal)
    getmonth()[indices,]
    })
  
  filter2 <- eventReactive( c(input$tabbutton, input$plotbutton), {
    indices <- which(filter1()$Tip >= input$ftip)
    filter1()[indices,]
    })
   
  filter3 <- eventReactive( c(input$tabbutton, input$plotbutton), {
    indices <- which(filter2()$Freq >= input$fvisit)
    filter2()[indices,]
    })
      ## NOTE: use filter3() for datatable ##

  
  num_data<-reactive(nrow(getmonth()))
  output$info<-renderText(
               print(paste("A total of",num_data(),
                           "transactions made in",input$month,input$year))
               )
   
  getplottitle<- eventReactive( input$plotbutton, {
    return(paste("Customers in",input$month,input$year))
  })

  output$plotit<-renderPlotly({
    if (input$pcolor == "Focus group"){
      p<-ggplot(data=getmonth(),aes_string(x=input$xvar, y=input$yvar)) +
        geom_point(aes(color=factor(getmonth()$"focus_gr"))) +
        labs(title=getplottitle(), x=input$xvar, color="Focus Group")
    } else{
      p<-ggplot(data=getmonth(),aes_string(x=input$xvar, y=input$yvar)) +
        geom_point(aes(color=factor(getmonth()$Freq))) +
        labs(title=getplottitle(), x=input$xvar, color="#Visits")
    }
    
    p+
        theme(plot.title=element_text(size=10,face="bold"),
              axis.title=element_text(size=8),
              legend.title=element_text(size=8, face="bold"),
              legend.position= "bottom")
  })

  output$tab<-DT::renderDataTable({
    DT::datatable(filter3()[1:9],options=list(pageLength=8),rownames = FALSE)
    })
  
  ## downloading this months' data ##
  output$downloadMonth <- downloadHandler(
    filename = function() {
      paste(input$month,"_",input$year,".csv", sep="")
    },
    content = function(file) {
      write.csv(getmonth()[1:9], file, row.names = FALSE)
    }
    )
  
  ## downloading the filtered customer data ##
  output$downloadFilter <- downloadHandler(
    filename = function() {
      paste("mycustomer.csv")
    },
    content = function(file) {
      write.csv(filter3()[1:9], file, row.names = FALSE)
    }
    )
  
}
  
shinyApp(ui=ui,server=server)

########## DEPLOYING APP #################
# rsconnect::setAccountInfo(name='yujijeong',
#                           token='5189AB8BB1624910AA5A13BC28F2620F',
#                           secret='I4iz5KWIFR2gURZYk2DcsaemYvbHXmzWHmuX3MgM')
# rsconnect::deployApp('C:/Users/User/Desktop')
