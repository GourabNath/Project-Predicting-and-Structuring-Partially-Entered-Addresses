##==================================================================================================================================
## REQUIRED LIBRARIES
##==================================================================================================================================

library(bitops) #requires to load RCurl
library(RCurl)
library(RJSONIO)
library(plyr)


##===================================================================================================================================
## BUILD AN URL TO PARSE THE DATA
##===================================================================================================================================

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}


##===================================================================================================================================
## FUNCTION TO PARSE THE RESULT
##===================================================================================================================================

geoCode <- function(address,verbose=FALSE) 
{
  
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  
  if(x$status=="OK") 
  {
    
    #For a given address identify the number of components
    
    comp <- length(x$results[[1]]$address_components)
    
    #For each components get their types
    
    types <- matrix(NA,comp,3)
    
    for(i in 1:comp)
    {
      for(j in 1:length(x$results[[1]]$address_components[[i]]$types))
      {
        types[i,j] <- x$results[[1]]$address_components[[i]]$types[[j]]
      }
    }
    
    types[is.na(types)] <- "0"
    
    if(any(types == "premise") == TRUE)
    {
      indx <- as.numeric(which(types == "premise", TRUE))
      House <- x$results[[1]]$address_components[[indx[1]]]$long_name
    }
    else {House <- NA}
    
    if(any(types == "street_number") == TRUE)
    {
      indx <- as.numeric(which(types == "street_number", TRUE))
      House <- x$results[[1]]$address_components[[indx[1]]]$long_name
    }
    else {House <- NA}
    
    if(any(types == "route") == TRUE)
    {
      indx <- as.numeric(which(types == "route", TRUE))
      House <- x$results[[1]]$address_components[[indx[1]]]$long_name
    }
    else {House <- NA}
    
    if(any(types == "sublocality_level_2") == TRUE)
    {
      indx <- as.numeric(which(types == "sublocality_level_2", TRUE))
      Locality <- x$results[[1]]$address_components[[indx[1]]]$long_name
    }
    else {Locality <- NA}
    
    if(any(types == "sublocality_level_1") == TRUE)
    {
      indx <- as.numeric(which(types == "sublocality_level_1", TRUE))
      Area <- x$results[[1]]$address_components[[indx[1]]]$long_name
    }
    else {Area <- NA}
    
    if(any(types == "locality") == TRUE)
    {
      indx <- as.numeric(which(types == "locality", TRUE))
      City <- x$results[[1]]$address_components[[indx[1]]]$long_name
    }
    else {City <- NA}
    
    if(any(types == "postal_code") == TRUE)
    {
      indx <- as.numeric(which(types == "postal_code", TRUE))
      Pincode <- x$results[[1]]$address_components[[indx[1]]]$long_name
    }
    else {Pincode <- NA}
    
    
    return(c(House, Locality, Area, City, Pincode))
    Sys.sleep(0.5)
  } 
  else 
  {
    return(c(NA, NA, NA, NA, NA))
  }
  
}



##=====================================================================================================================================
## MAKING PREDICTION
##=====================================================================================================================================

address <- read.csv("C:/Users/user/Desktop/test_address.csv", header = F)
address <- as.character(address$V1)

locations <- ldply(address, function(x) geoCode(x))

names(locations) <- c("Predicted_House_OR_Street_No", "Predicted_Locality_OR_Colony", "Predicted_Area_OR_Ward", 
                      "Predicted_City", "Predicted_Pincode")

loc <- locations 
loc[is.na(loc)] <- ""

loc <- as.data.frame(cbind(Address=address, loc))
View(loc)



##======================================================================================================================================
## CHECKING THE PREDICTION ACCURACY
##======================================================================================================================================
loc$Outcome <- NA
loc$Outcome <- ifelse(#loc$Predicted_House_OR_Street_No == "" & 
                        loc$Predicted_Locality == "" & loc$Predicted_Area == "" & 
                        loc$Predicted_City == "" & loc$Predicted_Pincode == "", "Negative", "Positive")

View(loc)
table(loc$Outcome)

#positive prediction rate
181/204



##======================================================================================================================================
## 
##======================================================================================================================================

addr <- loc$Address[which(loc$Outcome == "Negative")]
head(addr)

#Cleaning 1 - Remove near or behind locations
library(qdap)
addr <- genX(tolower(addr), "near", ",")
addr <- genX(tolower(addr), "behind", ",")

#Cleaning 2 - Remove Breakets and its contents
addr <- genX(tolower(addr), "(", ")")


#Cleaning 3 - Remove punctuations & unnecessary white spaces from the address
#install.packages("stringr")
library("stringr")

#Remove punctuations
addr <- str_replace_all(addr, "[[:punct:]]", " ")


#Remove unnecessary whitespaces
addr <-gsub("\\s+"," ",addr)



#Cleaning 4 - Remove some common words

cmmn <- c("number", "nbr",  "no", "street", "storey", "flats", "floor", "flat", "type", "str", "sr", "p o", "&", "#",
          "one", "two", "three", "four")

addr <- genX(tolower(addr), cmmn, rep("", length(cmmn)))
addr <- toupper(addr)
head(addr)


#Cleaning 5 - Remove Opp locations







#TESTING
test_loc <- ldply(addr, function(x) geoCode(x))

names(test_loc) <- c("Predicted_House_OR_Street_No", "Predicted_Locality_OR_Colony", "Predicted_Area_OR_Ward", 
                      "Predicted_City", "Predicted_Pincode")

test_loc <- cbind(addr, test_loc)

test_loc[is.na(test_loc)] <- ""

test_loc$Outcome <- ifelse(#loc$Predicted_House_OR_Street_No == "" & 
  test_loc$Predicted_Locality == "" & test_loc$Predicted_Area == "" & 
    test_loc$Predicted_City == "" & test_loc$Predicted_Pincode == "", "Negative", "Positive")


View(test_loc)



##=================================================================================================
## Merging this predictions
##=================================================================================================
loc1 <- loc
loc1[which(loc$Outcome == "Negative"), -1] <- test_loc[,-1]
View(loc1)
table(loc1$Outcome)
loc <- loc1
remove(loc1)



##=================================================================================================
##
##=================================================================================================

addr2 <- loc$Address[which(loc$Outcome == "Negative")]
length(addr2)
addr2
split <- str_split(addr2, " ", simplify = T)
split
#removing the first column
split <- split[,-1]
split


addr3 <- gsub("\\s+", " ", apply(split, 1, function(x){paste(x, collapse = " ")}))
addr3


test_loc2 <- ldply(addr3, function(x) geoCode(x))

names(test_loc2) <- c("Predicted_House_OR_Street_No", "Predicted_Locality_OR_Colony", "Predicted_Area_OR_Ward", 
                     "Predicted_City", "Predicted_Pincode")

test_loc2 <- cbind(addr3, test_loc2)
test_loc2[is.na(test_loc2)] <- ""

test_loc2$Outcome <- ifelse(#loc$Predicted_House_OR_Street_No == "" & 
  test_loc2$Predicted_Locality == "" & test_loc2$Predicted_Area == "" & 
    test_loc2$Predicted_City == "" & test_loc2$Predicted_Pincode == "", "Negative", "Positive")


View(test_loc2)


##=================================================================================================
## Merging this predictions
##=================================================================================================
loc1 <- loc
loc1[which(loc$Outcome == "Negative"), -1] <- test_loc2[,-1]
View(loc1)
table(loc1$Outcome)
loc <- loc1
remove(loc1)


write.csv(loc, "C:/Users/user/Desktop/predict_address.csv")

