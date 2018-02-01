
# Load packages
library(shiny)

# Load source
source("sourceLearn.R")

# Load data
content <- read.csv("content.csv", stringsAsFactors=FALSE)
nrRows <- nrow(content)

################
### TERMINAL ###
################

# Terminal item check
#reqTags <- list("ch01","ch01","ch02")
#itemNr <- PickItem(reqTags)
#GetItem(itemNr)
#GetAnswer(itemNr)

#############
### SHINY ### 
#############

# Shiny
i <- 1
itemPhase <- TRUE
shinyTool <- shinyApp(
  ui = fluidPage(
    fluidRow(
      column(10,textInput("tags", "Tags", "")),
      column(1 ,actionButton("doTag", "Go!"))
    ),
    fluidRow(
      column(5,textInput("EN","","")),
      column(5,textInput("SP","","")),
      column(1,actionButton("doUpdate", "Update"))
    ),
    fluidRow(
      column(5,textInput("TAG","","")),
      column(1,actionButton("doDelete","delete"))
    ),
    hr(),
    fluidRow(
      column(5,textInput("newEN", "", "")),
      column(5,textInput("newSP", "", "")),
      column(1, actionButton("addNew", "Add"))
    ),
    fluidRow(
      column(5,textInput("newTag","",""))
    ),
    hr()
    ),
  server = function(input, output, session) { 
    
    # Request or check item
    observeEvent(input$doTag, {
      print(itemPhase)
      if(itemPhase) {
        reqTags <- as.list(unlist(strsplit(input$tags, ";")))
        itemNr <- PickItem(reqTags)
        enText <- GetItem(itemNr)
        updateTextInput(session,"EN",value=enText)
        updateTextInput(session,"SP",value="")
      } else {
        spText <- GetAnswer(itemNr)    
        updateTextInput(session,"SP",value=spText)
      }
      itemPhase <- !itemPhase
      assign("itemNr", itemNr, envir = .GlobalEnv)
      assign("itemPhase", itemPhase, envir = .GlobalEnv)
    })
    
    # Update
    observeEvent(input$doUpdate, {
      updateEN <- input$EN
      updateSP <- input$SP
      if(updateEN!="") { content$EN[itemNr] <- updateEN }
      if(updateSP!="") { content$SP[itemNr] <- updateSP }
      print(content$SP[itemNr])
      assign("content", content, envir = .GlobalEnv)
      # Write csv file
      write.csv(content,file="content.csv",row.names = FALSE)
    })
    
    # Add item
    observeEvent(input$addNew, {
      content <- rbind(content,c(input$newSP, input$newEN, input$newTag,0))
      assign("content", content, envir = .GlobalEnv)
      output$output <- renderText({ "Added!" })
      updateTextInput(session,"newSP",value="")
      updateTextInput(session,"newEN",value="")
      updateTextInput(session,"newTag",value="")
      # Write csv file
      write.csv(content,file="content.csv",row.names = FALSE)
    })
    
  }
)

runApp(shinyTool)




