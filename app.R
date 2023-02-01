
source("global.R")

ui <- 
  
  navbarPage(
    shinybrowser::detect(),
    title = "Golf Match Simulator",
    header = display(p(style = "margin: 0;", "Created by ", a(href = "https://www.seanbock.com", 
                                                              target = "_blank", "Sean Bock")), type = "powered_by"),
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
                                              plotOutput("plot")
      
                                     )
                                   ))
    ),
    tabPanel("About", 
             includeMarkdown("about"))
  )


# Define server 
server <- function(input, output, session) {
  
  shinyalert(
    text = p(HTML("<b> Welcome to the Golf Match Simulator App! To get started, first drag the Handicap levels to your desired matchup. Next, select if Gross or Net scoring should be used. Once you've made your selections, click the 'Run Simulation!' button.</b>"), style = "color:white; size = 20; text-align = left;"), 
    type = "info",
    size = "s",
    className = "alert",
    closeOnClickOutside = TRUE,
    confirmButtonCol = "#576B6A",
    html = TRUE,
    imageWidth = 100,
    imageHeight = 100,
    timer = 20000)
  
  results <- eventReactive(input$click,{
    
    score_type <- if_else(input$net == "Net", "net", "gross")
    
    results <- get_results(results = sim_results,
                         hdc_1 = str_replace(input$p1_hdc, "([+]) ([1-9])","\\1\\2"), 
                         hdc_2 = str_replace(input$p2_hdc, "([+]) ([1-9])","\\1\\2"),
                         scoring = score_type)
    
    
  })
  

  output$summary <-
    renderText(results()$summary)
  
  ## making plot size dependent on browser window size 
  window_width <- function(x){shinybrowser::get_width()}
  window_height <- function(x){shinybrowser::get_height()}
  
  ratio <- function(x) {window_width() / window_height()}
  
  height <- function(x) {if_else(ratio() > 1, 500, 400)}
  
  
  output$plot <- 
    renderPlot(
      height = function(x) {height()},
      width = function(x){shinybrowser::get_width() * .85},
      res = 95,
      execOnResize = TRUE,
      {
        plot_results(results(), constant = ratio() * 500)
      }
      )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
