library(shiny)
library(rhandsontable)


for (f in list.files('~/Dropbox/cpb/git/james/R', pattern = "*.R", full.names = T)) source(f)

#
## INIT: READ CONFIG FILE FROM PACKAGE
#

server <- function(input, output, session) {
  rv <- reactiveValues()

  output$meta <- renderTable({
    req(input$uploadxlsx)
    
    rv$xlsx <- input$uploadxlsx$datapath
    DF <- openxlsx::read.xlsx(rv$xlsx, sheet = "meta", colNames = F, rowNames = T)

    if (!is.element("id", rownames(DF))) {
      DF <- rbind(id = 1:ncol(DF), DF)
      wb <- openxlsx::loadWorkbook(rv$xlsx)
      openxlsx::writeData(wb, sheet = "meta", DF, colNames = F, rowNames = T)      
      openxlsx::saveWorkbook(wb, rv$xlsx, overwrite = T)
    }
    ids <- as.character(DF["id", ])
    rv$DF <- DF
    
    updateSelectInput(
      session = session,
      inputId = "select",
      choices = ids,
      selected = ids[1]
    )

    updateTextInput(session, "title", value = DF["title", 1])

    return(DF)
  })
  
  observeEvent(input$select, {
    req(input$uploadxlsx)
    rv$plot_file <- plot(input$uploadxlsx$datapath, id = input$select, open = F)
    index <- which(input$select == rv$DF["id", ])
    updateTextInput(session, "title", value = rv$DF["title", index])
  })
  
  observeEvent(input$create_fig, {
    index <- which(input$select == rv$DF["id", ])
    updateTabItems(session, "sidebarMenu", "edit")
    rv$DF["title", index] <- input$title
    wb <- openxlsx::loadWorkbook(rv$xlsx)
    openxlsx::writeData(wb, sheet = "meta", rv$DF, colNames = F, rowNames = T)      
    openxlsx::saveWorkbook(wb, rv$xlsx, overwrite = T)
    rv$plot_file <- plot(rv$xlsx, id = input$select, open = F)
  })

    
  output$plot_file <- renderImage({
    if (is.null(rv$plot_file)) rv$plot_file <- "www/james-small.png"
    list(src = rv$plot_file, contentType = 'image/png')
  }, deleteFile = FALSE)
  
  
  output$panelStatus <- reactive({
    !is.null(input$uploadxlsx$datapath)
  })
  outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)

  ## Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF <- hot_to_r(input$hot)
    } else {
      if (is.null(rv[["DF"]]))
        DF <- rv[["DF"]]#WorldPhones # DF
      else
        DF <- rv[["DF"]]
    }
    rv[["DF"]] <- DF
  })  

  output$hot <- renderRHandsontable({
    DF <- rv[["DF"]]
    if (!is.null(DF))
      rhandsontable(DF, useTypes = F, stretchH = "all")
  })


}






























