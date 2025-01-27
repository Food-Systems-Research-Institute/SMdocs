pacman::p_load(
  shiny,
  DBI,
  RMariaDB
)


# Shiny UI
ui <- fluidPage(
  titlePanel("Access Database in Quarto Shiny"),
  sidebarLayout(
    sidebarPanel(
      actionButton("load", "Load Data")
    ),
    mainPanel(
      tableOutput("data_table")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Database connection (modify for your database)
  con <- dbConnect(
    RMariaDB::MariaDB(),
    user = Sys.getenv('WEBDB_CDONOV12_READER'),
    password = Sys.getenv('WEBDB_CDONOV12_READER_PW'),
    host = Sys.getenv('WEBDB_HOST'),
    dbname = 'CDONOV12_sm_repo'
  )
  
  # Observe button click to load data
  observeEvent(input$load, {
    query <- "SELECT * FROM metrics LIMIT 10"
    data <- dbGetQuery(con, query)
    output$data_table <- renderTable(data)
  })
  
  # Disconnect from the database when session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)
