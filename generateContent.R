
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
