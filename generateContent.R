
# Load source
source("sourceLearn.R")

# Load data
content <- read.csv("content.csv", stringsAsFactors=FALSE)
nrRows <- nrow(content)

##########################
### PERSON X INF ITEMS ###
##########################

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

itemNrs <- SelectTag("regverb")
for(itemNr in itemNrs) {
  item <- Conjugate(GetAnswer(itemNr),GetItem(itemNr))
  print(item)
  AddItem(item[2],item[1],"conj;generated")
}

tail(content)

# Write csv file
write.csv(content,file="content.csv",row.names = FALSE)

# Test update
AddItem("a","a","test")
tail(content)
SelectTag("test")

################
### THIS ETC ###
################

# Function to determine gender of a noun
GetGender <- function(sp) {
  if(tolower(substr(sp,1,2))=="el") {
    return("male")
  } else {
    return("female")
  }
}

NounPlural <- function(itemNr) {
  en <- GetItem(itemNr)
  #enPl <- paste(substr(en,4,nchar(en)),"s",sep="")
  enPl <- paste(en,"s",sep="")
  sp <- GetAnswer(itemNr)
  gender <- GetGender(sp)
  if(gender=="male") {
    spPl <- paste("Los ", substr(sp,4,nchar(sp)),"s",sep="")
  } else {
    spPl <- paste("Las ", substr(sp,4,nchar(sp)),"s",sep="")
  }
  return(c(enPl,spPl))
}

ReplaceFirstWord <- function(text,start) {
  clean <- gsub("^.* ","",text)  
  if(start!="") {
    new <- paste(start,clean)
    
  } else {
    new <- paste(start,clean,sep="")
  }
  return(new)
}

GenerateThisEtc <- function() {
  
  pos <- sample(c("close","far"),1)
  num <- sample(c("single","plural"),1)
  
  objectNr <- SelectTag("object")
  en <- GetItem(objectNr)
  sp <- GetAnswer(objectNr)
  gen <- GetGender(sp)
  
  if(num=="plural") {
    plural <- NounPlural(objectNr)
    en <- plural[1]
    sp <- plural[2]
  } 
  
  if(pos=="close" && num=="single" && gen=="male") {
    en <- ReplaceFirstWord(en,"This")
    sp <- ReplaceFirstWord(sp,"Este")
  }
  
  if(pos=="close" && num=="single" && gen=="female") {
    en <- ReplaceFirstWord(en,"This")
    sp <- ReplaceFirstWord(sp,"Esta")
  }
  
  if(pos=="far" && num=="single" && gen=="male") {
    en <- ReplaceFirstWord(en,"That")
    sp <- ReplaceFirstWord(sp,"Ese")
  }
  
  if(pos=="far" && num=="single" && gen=="female") {
    en <- ReplaceFirstWord(en,"That")
    sp <- ReplaceFirstWord(sp,"Esa")
  }

  if(pos=="close" && num=="plural" && gen=="male") {
    en <- ReplaceFirstWord(en,"These")
    sp <- ReplaceFirstWord(sp,"Estos")
  }
  
  if(pos=="close" && num=="plural" && gen=="female") {
    en <- ReplaceFirstWord(en,"These")
    sp <- ReplaceFirstWord(sp,"Estas")
  }
  
  if(pos=="far" && num=="plural" && gen=="male") {
    en <- ReplaceFirstWord(en,"Those")
    sp <- ReplaceFirstWord(sp,"Esos")
  }
  
  if(pos=="far" && num=="plural" && gen=="female") {
    en <- ReplaceFirstWord(en,"Those")
    sp <- ReplaceFirstWord(sp,"Esas")
  }

  return(c(en,sp))
}

GeneratePersonXThisEtc <- function() {
  personX <- PickItem("personX")
  base <- GenerateThisEtc()
  en <- paste(GetItem(personX), tolower(base[1]))
  sp <- paste(GetAnswer(personX), tolower(base[2]))
  # Hack to fix gusta(n)
  if(substr(sp,nchar(sp),nchar(sp))=="s") {
    gsub("gusta","gustan",sp)
  }
  return(c(en,sp))
}  

GenerateThisEtc()
GeneratePersonXThisEtc()
















