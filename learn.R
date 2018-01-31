
# Load packages
library(shiny)

# Function to select content based on tag
SelectTag <- function(tag) {
  searchString <- gsub(" ","",paste("\\",tag,"\\b",collapse=""))
  rNrs <- c(1:nrow(content))[grepl(searchString,content$Tag)]
  return(rNrs)
}

# Function to select content
SelectMultiTags <- function(reqTags) {
  if (length(reqTags)==0) {
    return(c(1:nrow(content)))  
  } else {
    nrsList <- lapply(c(1:length(reqTags)), function(y) SelectTag(reqTags[[y]]))
    return(unique(unlist(nrsList)))
  }
}

# Pick item/row
PickItem <- function(reqTags) {
  rowNrs <- SelectMultiTags(reqTags)
  return(sample(rowNrs, 1))
}

# Function to get item
GetItem <- function(rowNr) {
  return(content[rowNr,"EN"]) 
}

# Function to get answer
GetAnswer <- function(rowNr) {
  return(content[rowNr,"SP"]) 
}

# Function to add item
AddItem <- function(item, answer, tags) {
  content <- rbind(content,c(item, answer, tags,0))
  assign("content",content,envir=.GlobalEnv)
}

# Load data
content <- read.csv("content.csv", stringsAsFactors=FALSE)
nrRows <- nrow(content)

# Set content selection requirements
reqTags <- list("ch01","ch01","ch02")

# Pick item
itemNr <- PickItem(reqTags)

# Show item
GetItem(itemNr)

# Show answer
GetAnswer(itemNr)

##########################
### PERSON X INF ITEMS ###
##########################

# Function to create personXInf items
CreateItemPersonXInf <- function() {
  p1 <- PickItem("personX")
  p2 <- PickItem("inf") 
  itemText <- paste(GetItem(p1), gsub("to ","(to) ",tolower(GetItem(p2))))
  answerText <- paste(GetAnswer(p1), tolower(GetAnswer(p2)))
  return(c(itemText,answerText))
}

for(i in 1:100) {
  item <- CreateItemPersonXInf() 
  AddItem(item[2],item[1],"personXInf;generated")
}

tail(content)

# Write csv file
write.csv(content,file="content.csv",row.names = FALSE)

#################
### CONJUGATE ###
#################

Conjugate <- function(infSP, infEN) {
  
  # Trim
  inf <- trimws(infSP)
  baseEN <- gsub("^To ","",trimws(infEN))
  
  # Get ending
  end <- substr(inf,nchar(inf)-1,nchar(inf))
  base <- substr(inf,1,nchar(inf)-2)
  
  # Compile
  if(end=="ar") {
    answer <- paste("Yo ", base, "o; Té ", base, "as; El ", base, "a", sep="")
    item <- paste("I ", baseEN, "; You ", baseEN, "; He ", baseEN, "s", sep="")
  } else {
    answer <- paste("Yo ", base, "o; Té ", base, "es; El ", base, "e", sep="")
    item <- paste("I ", baseEN, "; You ", baseEN, "; He ", baseEN, "s", sep="")
  }
  
  return(c(item,answer))
}

content <- read.csv("content.csv", stringsAsFactors=FALSE)
head(content)

SelectTag("something")
SelectTag("greetings")
SelectTag("regverb")

grepl("*regverb*",content$Tag)
grepl("\\ch01\\b",content$Tag[1:3])

CreateConjItems <- function() {
 
  # Get item nrs
  itemNrs <- 
    
    
    
  
}
  
  

Conjugate("Hablar", "To talk")

paste("a", "b", sep="")

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
      column(1, actionButton("doTag", "Go!")) #,
      #column(1, actionButton("doCont", "Cont!"))
    ),
    fluidRow(
      column(5,textInput("newSP", "", "")),
      column(5,textInput("newEN", "", "")),
      column(1, actionButton("addNew", "Add"))
    ),
    hr(),
    textOutput("output")
    ),
  server = function(input, output, session) { 
    
    # Request or check item
    observeEvent(input$doTag, {
      print(itemPhase)
      if(itemPhase) {
        reqTags <- as.list(unlist(strsplit(input$tags, ";")))
        itemNr <- PickItem(reqTags)
        itemText <- GetItem(itemNr)
      } else {
        itemText <- GetAnswer(itemNr)    
      }
      itemPhase <- !itemPhase
      assign("itemNr", itemNr, envir = .GlobalEnv)
      assign("itemPhase", itemPhase, envir = .GlobalEnv)
      output$output <- renderText({ itemText })
    })
    
    # Add item
    observeEvent(input$addNew, {
      content <- rbind(content,c(input$newSP, input$newEN, "sentences",0))
      assign("content", content, envir = .GlobalEnv)
      output$output <- renderText({ "Added!" })
      
      # Write csv file
      write.csv(content,file="content.csv",row.names = FALSE)
    })
    
    # Continuous mode
    observeEvent(input$doCont, {
      
      while(TRUE) {
        output$output <- renderText({ i })
        Sys.sleep(1)
        i <- i + 1
        print(i)
        output$output <- renderText({ i })
        #invalidateLater(100,session)
      }
      output$output <- renderText({ itemText })
    })
  }
)

runApp(shinyTool)

# Loop and display content continuously to provide an emmersive experience.





