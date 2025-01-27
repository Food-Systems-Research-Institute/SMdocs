# Shiny UI
ui <- fluidPage(
  titlePanel("Database Filter App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("filter_col", "Select Column to Filter:", choices = NULL),
      textInput("filter_val", "Filter Value:", placeholder = "Enter a value to filter"),
      actionButton("apply_filter", "Apply Filter")
    ),
    mainPanel(
      dataTableOutput("table_output")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  # Database connection
  con <- dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    dbname = Sys.getenv("DB_NAME")
  )
  
  # Reactive value for storing the data
  db_data <- reactiveVal(dbReadTable(con, "your_table_name"))
  
  # Populate filter column choices dynamically
  observe({
    columns <- colnames(db_data())
    updateSelectInput(session, "filter_col", choices = columns)
  })
  
  # Filter data when the button is clicked
  filtered_data <- reactive({
    req(input$filter_col, input$filter_val)
    db_data() %>%
      filter(.data[[input$filter_col]] == input$filter_val)
  })
  
  # Render the table
  output$table_output <- renderDataTable({
    if (input$apply_filter > 0) {
      filtered_data()
    } else {
      db_data()
    }
  })
  
  # Disconnect from the database on session end
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the app
shinyApp(ui, server)
