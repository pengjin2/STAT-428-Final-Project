library(tidyverse)
library(reshape2)
library(shiny)
library(quantmod)

ui <- fluidPage(
  titlePanel("Guaranteed Minimum Maturity Benefit"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine. 
               Information will be collected from Yahoo finance."),
      
      textInput("symb", "Symbol", "SPY"),
      
      dateRangeInput("dates", 
                     "Insured Period",
                     start = "2013-01-01", 
                     end = as.character(Sys.Date())),
      
      numericInput("num", "Premium Invested", value = 100),
      
      checkboxInput("log","Plot y Axis on Log Scale"),
      
      checkboxInput("St", "Real Time Stock Value (St)", 
                    value = TRUE),
      checkboxInput("Ft","Market Value of the policy holder's subaccounts (Ft)",
                    value = FALSE),
      checkboxInput("Gt","Guranteed Value (Gt)", value=FALSE)),
    
    mainPanel(plotOutput("plot"),h3(textOutput("final")))
    )
)
# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    a_1<-getSymbols(input$symb, src = "yahoo",
                    from = input$dates[1],
                    to = input$dates[2],
                    auto.assign = FALSE)
    a<-as.data.frame(a_1)
    n<-length(a[,1])
    Ft_col<-Gt_val<-numeric(n)
    for (i in 1:length(Ft_col)){
      Ft_col[i]<-input$num*(a[,2][i]/a[,2][1])*exp(-i*0.02/365)
      Gt_val[i]<-max(Ft_col[i],input$num*exp(i*0.01/365))
    }
    
    ind<-c()
    if(input$St==TRUE){ind<-c(ind,2)}
    if(input$Ft==TRUE){ind<-c(ind,3)}
    if(input$Gt==TRUE){ind<-c(ind,4)}
    ind<-c(1,ind)
    dates=index(a_1)
    St=a[,4]
    Ft=Ft_col
    Gt=Gt_val
    all_data<-data.frame(dates,St,Ft,Gt)[,ind]
    final_payout<-max(Gt_val[n],Ft_col[n])
    if(input$log==FALSE){
      list(all_data,final_payout)}
    else{
      list(data.frame(dates,log(St),log(Ft),log(Gt))[,ind],final_payout)
    }
  })
  output$plot <- renderPlot({
    df <- melt(dataInput()[[1]], "dates")
    X<-as.Date(dataInput()[[1]][n,1])
    Y<-dataInput()[[2]]
    df2<-data.frame(x=X,y=Y)
    ggplot(df, aes(x=dates, y=value, color=variable)) + 
      geom_line()
    
    
  })
  output$final<-renderPrint({
    
    paste("Your Initial Premium Investment is",input$num," and your account balance will be",round(dataInput()[[2]],2),"after the designated period")
  })
  
}

# Run the app
shinyApp(ui, server)