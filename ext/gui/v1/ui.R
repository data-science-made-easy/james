library(shinydashboard)

dashboardPage(
  dashboardHeader(
    title = "James"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Upload xlsx", tabName = "upload", icon = icon("upload")),
      menuItem("Edit", tabName = "edit", icon = icon("edit")),
      menuItem("Download", tabName = "download", icon = icon("download")),
      menuItem("Manual", icon = icon("info-circle"), href = url_to_manual),
      conditionalPanel("output.panelStatus", selectInput("select", h4("Select figure"), choices = list())),
      conditionalPanel("output.panelStatus", actionButton("create_fig", "Create figure"))
      
      # ,
#       HTML('<BR/><center><img src="james-small.png"></center><BR/>')
    )
  ),
  dashboardBody(
    tags$style("
      .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #e6006e;
      }
    "),
    
    tags$head(tags$style('h1 {color:#e6006e;}')),
    tags$head(tags$style('h2 {color:#e6006e;}')),
    tags$head(tags$style('h3 {color:#e6006e;}')),
    tags$head(tags$style('h4 {color:#e6006e;}')),
    
    tags$head(tags$style(HTML('
     /* tabBox background */                    
     .nav-tabs-custom > .nav-tabs {
         color: #e6006e;
     }
     .nav-tabs-custom > .nav-tabs > li.header {
       font-weight: bold;
       color: #e6006e;
     }'))),
    
    tabItems(
      tabItem(
        tabName = "home",
        h2("Getting you started with James")
      ),
      tabItem(
        tabName = "upload",
        h2("Upload your data"),
        
        fileInput("uploadxlsx", "Upload a xlsx-file...", multiple = FALSE, accept = ".xlsx"),
        tableOutput("meta")
      ),
      
      tabItem(
        tabName = "edit",
        verbatimTextOutput("xlsx"),
        tabBox(
          # title = "EDIT",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset-metadata", height = "250px",
          tabPanel("ALL",
            rHandsontableOutput("hot")
          ),
          tabPanel("TEXT",
            textInput("title", "Title", value = NULL, placeholder = "First upload xlsx-file...")
          ),
          tabPanel("DATA", "Tab content 2"),
          tabPanel("TIME SERIES", ""),
          tabPanel("DIMENSIONS", ""),
          tabPanel("AXES", ""),
          tabPanel("NUMBERS", ""),
          tabPanel("MISC", "")
        ),
        HTML('<center>'),
        imageOutput("plot_file"),
        HTML('</center>')
      )
    )
  )
)



