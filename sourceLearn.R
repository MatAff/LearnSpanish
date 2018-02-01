

# Function to select content based on tag
SelectTag <- function(tag) {
  searchString <- gsub(" ","",paste("\\b",tag,"\\b",collapse=""))
  rNrs <- c(1:nrow(content))[grepl(searchString,content$Tag)]
  return(rNrs)
}

# Function to select content
SelectMultiTags <- function(reqTags) {
  print(reqTags)
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
