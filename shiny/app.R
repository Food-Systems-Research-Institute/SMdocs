pacman::p_load(
  shiny,
  DBI,
  RMariaDB,
  DT
)


# Shiny UI
ui <- fluidPage(
  titlePanel("Test Database Access"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Choose Variable", choices = NULL),
      actionButton("show_data", "Show Data")
    ),
    mainPanel(
      DT::dataTableOutput("metric_table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  con <- dbConnect(
    RMariaDB::MariaDB(),
    user = Sys.getenv('WEBDB_CDONOV12_READER'),
    password = Sys.getenv('WEBDB_CDONOV12_READER_PW'),
    host = Sys.getenv('WEBDB_HOST'),
    dbname = 'CDONOV12_sm_repo'
  )
  
  db_data <- reactiveVal(dbReadTable(con, "metrics"))
  
  # Populate filter column choices dynamically
  observe({
    variable_names <- unique(db_data()$variable_name)
    updateSelectInput(session, "variable", choices = variable_names)
  })
  
  # Query database 
  filtered_data <- reactive({
    req(input$variable)
    
    # Check column names
    column_names <- dbListFields(con, 'metrics')
    print(column_names)
   
    # Dynamic query
    query <- "SELECT * FROM metrics WHERE variable_name = ?"
    print(query)
    dbGetQuery(con, query, params = list(input$variable))
  })
  
  # Show table button to filter and show table
  observeEvent(input$show_data, {
    output$metric_table <- DT::renderDT({
      req(filtered_data())
      filtered_data()
    })
  })
  
  # Disconnect from the database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)