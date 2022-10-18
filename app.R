
library(shinythemes)
#library(shiny.info)
library(shiny)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(svglite)
library(glue)
library(bslib)
library(shinyWidgets)
library(scales)
library(ggplot2)
library(markdown)
library(shinyalert)
library(shinybrowser)
library(feather)


source("fun_sim_match.R") ## includes loading of "scores" so accessible in global.env
#source("fun_results_table.R")
source("plot_results.R")

new_order <- c("#E6CED4","#76908E","#F1FAFE","#C8B7E4")

choices <- levels(scores$handicap_f)[5:44]

choices <- str_replace(choices, "([+])([1-9])","\\1 \\2") ##\\1 means first element matched 

my_theme <- bs_theme(bootswatch = "flatly", 
                     bg = "#fff", 
                     fg = "#576B6A",
                     primary = "#576B6A",
                     secondary = "#576B6A",
                     success = "#89B6AD",
                     info = "#576B6A",
                     spacer = "2rem")


ui <- 

navbarPage(
  shinybrowser::detect(),
  title = "Golf Match Simulator",
  #header = display(p(style = "margin: 0;", "Created by ", a(href = "https://www.seanbock.com",                                                                                         target = "_blank", "Sean Bock")), type = "powered_by"),
  theme = my_theme,
  tabPanel("Simulator",fluidPage(theme = my_theme,
                                 tags$style('.alert {background-color: transparent}; 
                                      .alert {color: white};'),
                                 tags$head(
                                   tags$meta(name="Author", content = "Sean Bock"),
                                   tags$style(type = "text/css",'.well {
                                                                   background-color: #00244a;
                                                                   }'
                                   )
                                 ),
                                 
                                 # Application title
                                 titlePanel("Simulate a match"),
                                 
                                 inputPanel(
                                   # style = "background-color: #576B6A",
                                   sliderTextInput("p1_hdc",
                                                   "Player 1 Handicap",
                                                   choices = choices,
                                                   selected = "5"
                                   ),
                                   sliderTextInput("p2_hdc",
                                                   "Player 2 Handicap",
                                                   selected = "15",
                                                   choices = choices),
                                   
                                   radioButtons(
                                     "net",
                                     "Scoring Type",
                                     choices = c("Gross","Net"),
                                     selected = "Gross"
                                   )
                                   
                                   
                                 ),
                                 
                                 
                                 mainPanel(
                                   
                                   br(),
                                   actionButton("click", "Run Simulation!"),
                                   br(),
                                   br(),
                                   tabPanel(title = "Output",      
                                            htmlOutput("summary"),
                                            #br(),
                                            plotOutput("plot")
                                                       #width = "400px") 
                                                       #height = 500)
                                            
                                   )
                                 ))
  ),
  tabPanel("About", 
           includeMarkdown("about"))
)


# Define server 
server <- function(input, output, session) {
  
  shinyalert(#title = span("Welcome!", style = "color:white"),
    #title = HTML('<p style = "color: white; font-size = 20;"><b>Welcome!</b></p>'),
    text = p(HTML("<b> Welcome to the Golf Match Simulator App! To get started, first drag the Handicap levels to your desired matchup. Next, select if Gross or Net scoring should be used. Once you've made your selections, click the 'Run Simulation!' button.</b>"), style = "color:white; size = 20; text-align = left;"), 
    type = "info",
    size = "s",
    className = "alert",
    closeOnClickOutside = TRUE,
    confirmButtonCol = "#576B6A",
    html = TRUE,
    #imageUrl = 'https://i.imgur.com/ybvcvdR.jpg',
    #imageUrl = "http://deanattali.com/img/deanimg.jpeg",
    imageWidth = 100,
    imageHeight = 100,
    timer = 20000)
  
  results <- eventReactive(input$click,{
    
    score_type <- if_else(input$net == "Net", TRUE, FALSE)
    
    results <- sim_match(data = scores,
                         p1_hdc = str_replace(input$p1_hdc, "([+]) ([1-9])","\\1\\2"), 
                         p2_hdc = str_replace(input$p2_hdc, "([+]) ([1-9])","\\1\\2"),
                         n_sims = 10000,
                         net_scores = score_type)
    
    
  })
  
  # output$table <-
  #   gt::render_gt(
  #     expr = results()[[1]],
  #     height = pct(50),
  #     width = pct(100),
  #     align = "left"
  #   )
 #width <- observe(shinybroswer::get_width()) 
 
  output$summary <-
    renderText(results()$summary)
  
  window_width <- function(x){shinybrowser::get_width()}
  window_height <- function(x){shinybrowser::get_height()}

  ratio <- function(x) {window_width() / window_height()}
  
  height <- function(x) {if_else(ratio() > 1, 500, 400)}
  
  
  output$plot <- 
    renderPlot(
      #height = function(x){shinybrowser::get_height() * .50},
      height = function(x) {height()},
      #width = "auto",
      #
      #height = function(x) {session$clientData[["output_plot_height"]]*1.5},
      #width = function(x) {session$clientData[["output_plot_width"]]*1},
      width = function(x){shinybrowser::get_width() * .85},
      res = 95,
      execOnResize = TRUE,
    #plot_results2(results(), base_size = window_width() * .02))
  # execOnResize = TRUE, 
   {
    plot_results(results(),constant = ratio() * 500)
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
