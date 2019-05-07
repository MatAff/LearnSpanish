
# Load packages
library(shiny)

# Load source
source("sourceLearn.R")

# Load data
content <- read.csv("content.csv", stringsAsFactors=FALSE); head(content, 20)
content$Count <- as.numeric(content$Count)

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
      column(12,textInput("tags", "Required tags", "",width='100%'))
    ),
    fluidRow(
      column(12,textInput("EN","EN","",width='100%'))
    ),
    fluidRow(
      column(12,textInput("SP","SP","",width='100%'))
    ),
    fluidRow(
      column(12,textInput("TAG","Tags","",width='100%'))
    ),
    fluidRow(
      column(1,actionButton("doTag", "Go!")),
      column(1,actionButton("doUpdate", "Update")),
      column(1,actionButton("doDelete","Delete")),
      column(1,actionButton("doClear", "Clear")),
      column(1,actionButton("doAdd", "Add"))
    ),
    fluidRow(
      column(12,textInput("answer", "Enter answer", "",width='100%'))
    ),
    fluidRow(
      column(1,actionButton("doCheck", "Check!"))
    ),
    fluidRow(
      column(12,textInput("answerResult", "", "",width='100%'))
    )
  ),
  server = function(input, output, session) { 
    
    # Go - Request or check item
    observeEvent(input$doTag, {
      print(itemPhase)
      if(itemPhase) {
        reqTags <- as.list(unlist(strsplit(input$tags, ";")))
        itemNr <- PickItem(reqTags)
        enText <- GetItem(itemNr)
        
        updateTextInput(session,"EN",value=enText)
        updateTextInput(session,"SP",value="")
        updateTextInput(session,"TAG",value="")
        print(enText)
        content[itemNr,4] <- content[itemNr,4] + 1
        print(enText)
        assign("content", content, envir=.GlobalEnv)
        print("Increasing counter")
        write.csv(content,file="content.csv",row.names = FALSE)
      } else {
        spText <- GetAnswer(itemNr)    
        updateTextInput(session,"SP",value=spText)
        updateTextInput(session,"TAG",value=content[itemNr,3])
      }
      itemPhase <- !itemPhase
      assign("itemNr", itemNr, envir = .GlobalEnv)
      assign("itemPhase", itemPhase, envir = .GlobalEnv)
    })
    
    # Update
    observeEvent(input$doUpdate, {
      updateEN <- input$EN
      updateSP <- input$SP
      updateTAG <- input$TAG
      if(updateEN!="") { content$EN[itemNr] <- updateEN }
      if(updateSP!="") { content$SP[itemNr] <- updateSP }
      if(updateTAG!="") { content$Tag[itemNr] <- updateTAG }
      print(content$SP[itemNr])
      assign("content", content, envir = .GlobalEnv)
      # Write csv file
      write.csv(content,file="content.csv",row.names = FALSE)
    })
    
    # Delete
    observeEvent(input$doDelete, {
      content[itemNr,3] <- paste(content[itemNr,3],"delete",sep=";")
    })
    
    # Clear
    observeEvent(input$doClear, {
      updateTextInput(session,"SP",value="")
      updateTextInput(session,"EN",value="")
      updateTextInput(session,"TAG",value="")
    })
    
    # Add item
    observeEvent(input$doAdd, {
      content <- rbind(content,c(input$SP, input$EN, input$TAG,0))
      content$Count <- as.numeric(content$Count)
      assign("content", content, envir = .GlobalEnv)
      output$output <- renderText({ "Added!" })
      updateTextInput(session,"SP",value="")
      updateTextInput(session,"EN",value="")
      updateTextInput(session,"TAG",value="")
      # Write csv file
      write.csv(content,file="content.csv",row.names = FALSE)
    })
    
    # Check 
    observeEvent(input$doCheck, {
      userEntered <- input$answer
      spText <- GetAnswer(itemNr)    
      updateTextInput(session,"SP",value=spText)
      if (tolower(userEntered) == tolower(spText)) {
        updateTextInput(session,"answerResult",value="Correct")
        AddCheckResult(itemNr, "1")
      } else {
        updateTextInput(session,"answerResult",value="Wrong")
        AddCheckResult(itemNr, "0")
      }
      write.csv(content,file="content.csv",row.names = FALSE)
    })
    
  }
)

runApp(shinyTool)




