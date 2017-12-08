source("decrement.R")
source("investment.R")
source("discount rate.R")
# require("DT")
require("shiny")
require("tidyverse")

ui <- fluidPage(
  # App title ----
  titlePanel("Profit Testing of Variable Annuity Products via Simulation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Instructions ----
      helpText("Set assumptions below."),
      
      # Decrement Modeling Section ---
      wellPanel(
        
        # Instructions ----
        helpText("Decrement Section"),
        
        # Input: Select a file ----
        fileInput(inputId = "life.table", 
                  label = "Upload CSV File: Empirical Life Table",
                  multiple = FALSE,
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), 
        numericInput(inputId = "life.num", label = "Number of Policyholders", 
                     value = 20, step = 1), # need to be an integer
        radioButtons(inputId = "life.gender", label = "Gender", 
                     choices = c("Male" = "Male", "Female" = "Female"), 
                     selected = "Male"), 
        numericInput(inputId = "life.start.age", label = "Age at Policy Issuance", 
                     value = 60, step = 1)
      
        # # Input: Enter a number ----
        # numericInput(inputId = "lapse.const", label = "Enter Periodic Lapse Rate (in decimal)", 
        #              value = 0.03, step = 0.01), 
        # 
        # # Input: Enter a number ----
        # numericInput(inputId = "ann.const", label = "Enter Periodic Annuitization Rate (in decimal)", 
        #              value = 0.01, step = 0.01)
    
        # --- Decrement Modeling Section
      ),
      
      # Equity Investment Section ---
      wellPanel(

        # Instructions ----
        helpText("Equity Investment Section"),
        
        # Input: controler for the following uiOutput element
        selectInput(inputId = "inv.select", label = "Approach",
                    choices = c("Calibrate a New Model" = "calibrate", 
                                "Setup a Constant Model" = "const", 
                                "Upload an Existing Prediction" = "upload"), 
                    selected = "calibrate"), 
        
        # Output: dynamic ui element for discount rate section
        uiOutput(outputId = "inv.section")

        # --- Equity Investment Section
      ),
      
      # Discount Rate Section ---
      wellPanel(
        
        # Instructions ----
        helpText("Discount Rate Section"),
        
        # Input: controler for the following uiOutput element
        selectInput(inputId = "disc.select", label = "Approach",
                    choices = c("Calibrate a New Model" = "calibrate", 
                                "Setup a Constant Model" = "const", 
                                "Upload an Existing Prediction" = "upload"), 
                    selected = "calibrate"), 
        
        # Output: dynamic ui element for discount rate section
        uiOutput(outputId = "disc.section")
        
        # --- Discount Rate Section
      ),
      
      
      # Input: Slider for the number of observations to generate ----
      sliderInput(inputId = "sim.num", label = "Number of Simulations:",
                  min = 1, max = 500, 
                  # initial value of the slider
                  value = 100), 
      
      # Input: data range for prediction
      dateRangeInput(inputId = "pred.dates", 
                     label = "Prediction Horizon",
                     start = as.character(Sys.Date()), 
                     end = as.character(Sys.Date() + lubridate::years(15))), 
      
      # Input: time increment for analysis
      radioButtons(inputId = "dt", label = "Frequency of Analysis", 
                   choices = c("Monthly" = "monthly", 
                               "Quarterly" = "quarterly", 
                               "Annually" = "annually"), 
                   selected = "monthly"), 
      
      width = 3 # a quarter of the screen
    # --- Sidebar panel for inputs
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      navbarPage("SUBSECTION",
                  tabPanel("Dashboard", plotOutput("plot")),
                  navbarMenu(title = "Modeling", 
                    tabPanel("Decrements", 
                             titlePanel("Decrements"), 
                             DT::dataTableOutput(
                               outputId = "life.table.out"), 
                             fluidRow(
                               column(
                                 width = 10, 
                                 plotOutput(
                                   outputId = "mortality.plot", 
                                   hover = hoverOpts(id = "mortality.plot.hover"))), 
                               column(
                                 width = 2, 
                                 htmlOutput(outputId = "mortality.plot.info"))
                             )),
                    tabPanel("Equity Return", 
                             titlePanel("Equity Investment Model"), 
                             tabsetPanel(type = "tabs", 
                                         # tabPanel("Parameter Estimations"), 
                                         tabPanel("Return Predicitons", 
                                                  fluidRow(
                                                      fluidRow(
                                                        column(
                                                          width = 10, 
                                                          plotOutput(
                                                            outputId = "inv.plot.fix", 
                                                            brush = 
                                                              brushOpts(
                                                                id = "inv.plot.fix.brush",
                                                                resetOnNew = TRUE) # set up brush for interactive plot
                                                            )
                                                          )
                                                        ), 
                                                      fluidRow(
                                                        column(
                                                          width = 10, 
                                                          plotOutput(
                                                            outputId = "inv.plot.zoom", 
                                                            hover = hoverOpts(id = "inv.plot.zoom.hover"))), 
                                                        column(
                                                          width = 2, 
                                                          htmlOutput(outputId = "inv.plot.zoom.info"))
                                                        ))))), 
                 tabPanel("Discount Rate", 
                          titlePanel("Discount Rate Model"), 
                          tabsetPanel(type = "tabs", 
                                      # tabPanel("Parameter Estimations"), 
                                      tabPanel("Rate Predicitons", 
                                               fluidRow(
                                                 fluidRow(
                                                   column(
                                                     width = 10, 
                                                     plotOutput(
                                                       outputId = "disc.plot.fix", 
                                                       brush = 
                                                         brushOpts(
                                                           id = "disc.plot.fix.brush",
                                                           clip = TRUE, 
                                                           resetOnNew = TRUE) # set up brush for interactive plot
                                                     )
                                                   )
                                                 ), 
                                                 fluidRow(
                                                   column(
                                                     width = 10, 
                                                     plotOutput(
                                                       outputId = "disc.plot.zoom", 
                                                       hover = hoverOpts(id = "disc.plot.zoom.hover"))), 
                                                   column(
                                                     width = 2, 
                                                     htmlOutput(outputId = "disc.plot.zoom.info"))
                                                 ), 
                                                 fluidRow(
                                                   column(width = 6, 
                                                          plotOutput(outputId = "disc.plot.ZCBondPrice")), 
                                                   column(width = 6, 
                                                          plotOutput(outputId = "disc.plot.YieldCurve"))
                                                 )
                                                 ))))), 
                  navbarMenu(title = "Profit Testing", 
                    tabPanel("Projected Account Values"), 
                    tabPanel("Projected Cash Flows"), 
                    tabPanel("Projected Distributable Earnings"), 
                    tabPanel("Profit Testing"))
      )
    
    # --- Main panel for displaying outputs  
    )
    
  # --- Sidebar layout with input and output definitions  
  )
)
# Server logic
server <- function(input, output) {
  
  ##############################################################################
  ### DECREMENT SECTION
  ##############################################################################
  
  life.table <- reactive({
    inFile <- input$life.table
    
    if (is.null(inFile))
      return(NULL)
    
    decrement.clean.data(
      path.to.file = inFile$datapath, 
      source = "mortality", 
      model = "life-table")
  })
  
  mortality.model <- reactive({
    decrement.calibrate(
      hist.data = life.table(), 
      source = "mortality", 
      model = "life-table", 
      model.param = list(assumption = "UDD"))
  })
  
  mortality.input.data <- reactive({
    data.frame(count = c(as.integer(input$life.num)), 
               gender = c(input$life.gender), 
               start.age = c(input$life.start.age))
  })
  
  mortality.model.pred <- reactive({
    decrement.predict(
      model.fit = mortality.model(), 
      t.now = as.Date(input$pred.dates[1]), dt = input$dt, t.end = as.Date(input$pred.dates[2]), 
      n = input$sim.num, new.data = mortality.input.data())
  })
  
  output$life.table.out <- DT::renderDataTable({
    DT::datatable(data = life.table() %>% mutate(gender = as.factor(gender)), 
                  filter = "top", rownames = FALSE, 
                  colnames = c("Gender", "Remaining Lives (lx)", "Annual Number of Death (dx)", "Age (x)"),
                  options = list(pageLength = 5))
  })
  
  output$mortality.plot <- renderPlot({
    decrement.plot(input.data = mortality.input.data(), 
                   model.pred.df = mortality.model.pred(), 
                   t.now = as.Date(input$pred.dates[1]), 
                   t.end = as.Date(input$pred.dates[2]), 
                   dt.char = input$dt)
  })
  
  output$mortality.plot.info <- renderUI({
    HTML(paste(
      "<em>Mouse Hover On ...</em>", 
      sprintf("Time: %s", 
              ifelse(is.null(input$mortality.plot.hover$x), "NULL", 
                     as.character(as.Date(input$mortality.plot.hover$x, origin = "1970-01-01")))), 
      sprintf("Remaining Lives: %s", 
              ifelse(is.null(input$mortality.plot.hover$y), "NULL", 
                     format(round(input$mortality.plot.hover$y, digits = 2)))), 
      sep = "<br/>"))
  })
  
  ##############################################################################
  ### INVESTMENT SECTION
  ##############################################################################
  
  # Output: render equity investment section UI --- 
  output$inv.section <- renderUI({
    
    if (is.null(input$inv.select))
      return()
    
    switch(input$inv.select, 
           "calibrate" = tagList(
             # Input: select model type --- 
             radioButtons(inputId = "inv.model", label = "Select Model Type", 
                          choices = c("Geometric Brownian Motion" = "GBM"), 
                          selected = "GBM"), 
             # Input: index symbol for equity investment ---- 
             textInput(inputId = "inv.index", label = "Equity Investment Index", 
                       value = "SPY"),
             # Input: data range for calibration
             dateRangeInput(inputId = "inv.dates", 
                            label = "Equity Return Calibration Period",
                            start = "2013-01-01", 
                            end = as.character(Sys.Date()))
           ), 
           "upload" = 
             # Input: select a file ---- 
           fileInput(inputId = "inv.pred.file", 
                     label = "Upload CSV File: Projected Equity Returns", 
                     multiple = FALSE, 
                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), 
           "const" =
             # Input: input a constant number percentage as future equity return
             numericInput(inputId = "inv.const", 
                          label = "Input Constant Future Return (in decimal)", 
                          value = 1.00, step = 0.01)
    )
    
    # --- Depending on input$inv.select, we'll generate different inv.section 
  })
  
  
  data.inv <- reactive({
    inv.clean.data(ticker = input$inv.index, 
                   src = "yahoo", 
                   dt = input$dt, 
                   start_date = input$inv.dates[1], 
                   end_date = input$inv.dates[2])
  })
  
  param.est.inv <- reactive({
    inv.calibrate(hist.data = data.inv(), model = input$inv.model)
  })
  
  model.predict.inv <- reactive({
    inv.predict(hist.data = data.inv(), 
                model = input$inv.model, 
                param = param.est.inv(), 
                sim.length = 
                  as.numeric(substr(input$pred.dates[2], start = 1, stop = 4)) - 
                  as.numeric(substr(input$pred.dates[1], start = 1, stop = 4)), 
                n = input$sim.num, 
                dt = input$dt)
  })
  
  output$inv.plot.fix <- renderPlot({
    inv.plot(hist.data = data.inv(), 
             model.pred = model.predict.inv(), 
             conf = 0.95, 
             x.coord.interact = NULL, 
             y.coord.interact = NULL)
  })
  
  inv.plot.zoom.ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$inv.plot.zoom <- renderPlot({
    inv.plot(hist.data = data.inv(), 
             model.pred = model.predict.inv(), 
             conf = 0.95, 
             x.coord.interact = inv.plot.zoom.ranges$x, 
             y.coord.interact = inv.plot.zoom.ranges$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$inv.plot.fix.brush
    if (!is.null(brush)) {
      inv.plot.zoom.ranges$x <- c(brush$xmin, brush$xmax)
      inv.plot.zoom.ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      inv.plot.zoom.ranges$x <- NULL
      inv.plot.zoom.ranges$y <- NULL
    }
  })
  
  output$inv.plot.zoom.info <- renderUI({
    HTML(paste(
      "<em>Mouse Hover On ...</em>", 
      sprintf("Time: %s", 
              ifelse(is.null(input$inv.plot.zoom.hover$x), "NULL", 
                     as.character(as.Date(input$inv.plot.zoom.hover$x, origin = "1970-01-01")))), 
      sprintf("Value: %s", format(input$inv.plot.zoom.hover$y)), 
      sep = "<br/>"))
  })
  
  
  ##############################################################################
  ### DISCOUNT RATE SECTION
  ##############################################################################
  
  # Output: render discount rate section UI --- 
  output$disc.section <- renderUI({
    
    if (is.null(input$disc.select))
      return()
    
    switch(input$disc.select, 
           "calibrate" = tagList(
             # Input: select model type --- 
             radioButtons(inputId = "disc.model", label = "Select Model Type", 
                          choices = c("Vasicek" = "vasicek"), 
                          selected = "vasicek"), 
             # Input: index symbol for discount rate ---- 
             textInput(inputId = "disc.index", label = "Discount Rate Index", 
                       value = "DGS10"),
             # Input: data range for calibration
             dateRangeInput(inputId = "disc.dates", 
                            label = "Discount Rate Calibration Period",
                            start = "2013-01-01", 
                            end = as.character(Sys.Date()))
           ), 
           "upload" = 
             # Input: select a file ---- 
           fileInput(inputId = "disc.pred.file", 
                     label = "Upload CSV File: Projected Discount Rate", 
                     multiple = FALSE, 
                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), 
           "const" =
             # Input: input a constant number percentage as future discount rate
             numericInput(inputId = "disc.const", 
                          label = "Input Constant Future Discount Rate (in decimal)", 
                          value = 0.05, step = 0.01)
    )
    
    # --- Depending on input$disc.select, we'll generate different disc.section 
  })
  
  data.disc <- reactive({
    disc.clean.data(
      ticker = input$disc.index, 
      src = "FRED", 
      dt = input$dt, 
      start.date = input$disc.dates[1], 
      end.date = input$disc.dates[2])
  })
  
  model.fit.disc <- reactive({
    disc.calibrate(hist.data = data.disc(), model = input$disc.model, dt = input$dt)
  })
  
  model.predict.disc <- reactive({
    disc.predict(
      model.fit = model.fit.disc(), 
      sim.length = 
        as.numeric(substr(input$pred.dates[2], start = 1, stop = 4)) - 
        as.numeric(substr(input$pred.dates[1], start = 1, stop = 4)), 
      n = input$sim.num)
  })
  
  output$disc.plot.fix <- renderPlot({
    disc.plot.short.rate.helper(
      hist.data = data.disc(), 
      model.pred = model.predict.disc(), 
      conf = 0.95, 
      x.coord.interact = NULL, 
      y.coord.interact = NULL)
  })
  
  disc.plot.zoom.ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$disc.plot.zoom <- renderPlot({
    disc.plot.short.rate.helper(
      hist.data = data.disc(), 
      model.pred = model.predict.disc(), 
      conf = 0.95, 
      x.coord.interact = disc.plot.zoom.ranges$x, 
      y.coord.interact = disc.plot.zoom.ranges$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$disc.plot.fix.brush
    if (!is.null(brush)) {
      disc.plot.zoom.ranges$x <- c(brush$xmin, brush$xmax)
      disc.plot.zoom.ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      disc.plot.zoom.ranges$x <- NULL
      disc.plot.zoom.ranges$y <- NULL
    }
  })
  
  output$disc.plot.zoom.info <- renderUI({
    digits <- 4
    HTML(paste(
      "<em>Mouse Hover On ...</em>", 
      sprintf("Time: %s", 
              ifelse(is.null(input$disc.plot.zoom.hover$x), "NULL", 
                     as.character(as.Date(input$disc.plot.zoom.hover$x, origin = "1970-01-01")))), 
      sprintf("Rate: %s %%", format(round(input$disc.plot.zoom.hover$y*(10^(digits+2)))/(10^digits))), 
      sep = "<br/>"))
  })
  
  output$disc.plot.ZCBondPrice <- renderPlot({
    disc.plot(
      hist.data = data.disc(), 
      model.pred = model.predict.disc(), 
      plot.type = c("zero-coupon-bond-price"), 
      conf = 0.95, 
      x.coord.interact = NULL, 
      y.coord.interact = NULL)
  })
  
  output$disc.plot.YieldCurve <- renderPlot({
    disc.plot(
      hist.data = data.disc(), 
      model.pred = model.predict.disc(), 
      plot.type = c("yield-curve"), 
      conf = 0.95, 
      x.coord.interact = NULL, 
      y.coord.interact = NULL)
  })
  
}

# Run the app
shinyApp(ui, server)
