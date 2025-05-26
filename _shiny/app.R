# Housekeeping ------------------------------------------------------------

pacman::p_load(
  shiny,
  DBI,
  RMariaDB,
  DT,
  purrr,
  shinyWidgets,
  readr,
  bslib
)

# Load sm_data metadata and fips/state key
metadata <- readRDS('sm_metadata.rds')
key <- readRDS('complete_key.rds')
source('../dev/get_reactable.R')



# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(
    preset = 'lumen',
    info = '#2F4F4F',
    primary = '#2F4F4F',
    font_scale = 1
  ),
  
  titlePanel("Test Database Access"),
  
    
  ## Sidebar Panel -----------------------------------------------------------
    
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "variable",
        "Choose Variable",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = 'Select a variable',
          allowClear = TRUE
        )
      ),
      sliderTextInput(
        inputId = "year_range",
        label = "Choose a Range of Years", 
        choices = 2000:2025,
        selected = c(2000, 2025)
      ),
      tagList(
        tags$div(
          style = "display: flex; justify-content: center; align-items: center; gap: 30px;",
          actionButton(
            "show_data", 
            "Show Data",
            style = 
              "color: #fff;
               background-color: #243f3f;
               border-color: #243f3f;
               border-radius: 10px;
               border-width: 2px;
               width: 50%;",
            icon = icon(
              name = 'rotate-left',
              lib = 'font-awesome'
            )
          ),
          downloadButton(
            'download',
            'Download',
            style = 
              "color: #fff;
               background-color: #243f3f;
               border-color: #243f3f;
               border-radius: 10px;
               border-width: 2px;
               width: 50%;",
            icon = icon(
              name = 'download',
              lib = 'font-awesome'
            )
          )
        )
      )
    ),
    

    ## Main Panel --------------------------------------------------------------
    
    mainPanel(reactableOutput("metric_table"))
    
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

  # Connection --------------------------------------------------------------
  
  con <- dbConnect(
    RMariaDB::MariaDB(),
    user = Sys.getenv('WEBDB_CDONOV12_READER'),
    password = Sys.getenv('WEBDB_CDONOV12_READER_PW'),
    host = Sys.getenv('WEBDB_HOST'),
    dbname = 'CDONOV12_sm_repo'
  )
  
  db_data <- reactiveVal(dbReadTable(con, "metrics"))
  
  
  # Dropdown Values ---------------------------------------------------------
  
  # Get variable names
  observe({
    variable_names <- metadata$variable_name %>% 
      unique() %>% 
      sort()
    updateSelectizeInput(
      session, 
      "variable", 
      choices = variable_names, 
      selected = NULL
    )
  })
 
  
  # Query -------------------------------------------------------------------
  
  # Show table button to filter and show table
  result_reactive <- reactiveVal()
  
  observeEvent(input$show_data, {
    
    # Dynamic query
    query <- 'SELECT * FROM metrics WHERE 1=1'
    params <- list()
    
    if (!is.null(input$variable) & length(input$variable) >= 1) {
      query <- paste(query, 'AND variable_name IN (', paste(rep('?', length(input$variable)), collapse = ', '), ')')
      params <- append(params, input$variable)
    }
     
    if (!is.null(input$year_range) & length(input$year_range) == 2) {
      query <- paste(query, 'AND year BETWEEN ? AND ?')
      params <- append(params, c(input$year_range[1], input$year_range[2]))
    }
    
    # Make query to get results. Make it reactive to use elsewhere
    result <- dbGetQuery(con, query, params = params)
    result_reactive(result)
    
    # Sanity checks
    print(query)
    print(params)
    print(dim(result))
  
    # Render output table with result
    output$metric_table <- renderReactable({
      get_reactable(
        result_reactive(),
        fullWidth = FALSE,
        columns = list(
          fips = colDef(minWidth = 50), 
          year = colDef(minWidth = 50),
          variable_name = colDef(minWidth = 125),
          value = colDef(minWidth = 75)
        ),
        defaultColDef = colDef(align = 'left')
      )
    })
  })
  

  # Download ----------------------------------------------------------------
  
  output$download <- downloadHandler(
    filename = function() {
      paste(
        Sys.Date(), 
        "_metrics.csv", 
        sep = ""
      )
    },
    content = function(file) {
      write_csv(result_reactive(), file)
    }
  )

  
  # Disconnect --------------------------------------------------------------
  
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui, server)