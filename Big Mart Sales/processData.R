# include libraries
library(ggplot2)
library(ggbiplot)
library(gridExtra)
library(corrplot)
library(plyr)
library(dplyr)
library(caret)
library(mice)
library(VIM)

# load files
setwd("./")
train <- read.csv("Train.csv")
test <- read.csv("Test.csv")

# add label to test set
test$Item_Outlet_Sales <- 0

# append test data set to train data set
combinedData <- rbind(train, test)

# Item_Fat_Content has misleading data, fix it
combinedData$Item_Fat_Content <- revalue(combinedData$Item_Fat_Content, c("LF"="Low Fat", "low fat"="Low Fat", "reg"="Regular"))


# assigning a fat content to non-food items makes no sense, replace with "None"
levels(combinedData$Item_Fat_Content) <- c(levels(combinedData$Item_Fat_Content), "None")

combinedData[ which(combinedData$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content <- "None"
combinedData[ which(combinedData$Item_Type == "Household") ,]$Item_Fat_Content <- "None"
combinedData[ which(combinedData$Item_Type == "Others") ,]$Item_Fat_Content <- "None"
combinedData$Item_Fat_Content <- factor(combinedData$Item_Fat_Content)

# output item list, sort by Item_Type, check each item's Item_Fat_Content
fat <- as.data.frame(setNames(
  aggregate(
    combinedData$Item_Fat_Content, 
    by=list(Category=combinedData$Item_Type,
            Category=combinedData$Item_Fat_Content), 
    FUN= length),
  c("Item_Type", "Item_Fat_Content", "number")
))

fat

# some entries for Outlet_Size are empty, assign "Other"..... levels(combinedData$Outlet_Size)
levels(combinedData$Outlet_Size)[1] <- "Other"

## take care of missing values
# find missing values column
colSums(is.na(combinedData))

# assuming that each item identifier actually identifies a unique item,
# hence a unique weight, let's create a dataframe containing the mean
# weights and standard deviations by item identifier
weightsByItem <- as.data.frame( ddply(na.omit(combinedData), 
                                      ~Item_Identifier, 
                                      summarise, 
                                      mean=mean(Item_Weight), 
                                      sd=sd(Item_Weight)))

# fill in the missing weight values with mean score
combinedData$Item_Weight <- ifelse(is.na(combinedData$Item_Weight), 
                            weightsByItem$mean[
                              match(combinedData$Item_Identifier, weightsByItem$Item_Identifier)], combinedData$Item_Weight)

# replace Outlet_Establishment_Year with number of years it has existed till 2013
combinedData$Year <- as.factor(2013 - combinedData$Outlet_Establishment_Year)

# drop column Outlet_Establishment_Year since we have a new column Year
combinedData <- select(combinedData, -c(Outlet_Establishment_Year))

# convert Item_MRP (continuous) to categorical
combinedData$MRP_Level <- as.factor(
  ifelse(combinedData$Item_MRP < 69, "Low",
         ifelse(combinedData$Item_MRP < 136, "Medium",
                ifelse(combinedData$Item_MRP < 203, "High", "Very_High"))))

# reorder the dataset such that the label Item_Outlet_Sales comes last
combinedData <- select( combinedData, c(Item_Identifier,
                          Item_Weight,
                          Item_Fat_Content,
                          Item_Visibility,
                          Item_Type,
                          Item_MRP,
                          Outlet_Identifier,
                          Outlet_Size,
                          Outlet_Location_Type,
                          Outlet_Type,
                          Year,
                          MRP_Level,
                          Item_Outlet_Sales
))

# find out how often does each Outlet_Identifier appear in the data
aggregate(combinedData$Outlet_Identifier, by=list(Category=combinedData$Outlet_Identifier), FUN=length)
aggregate(combinedData$Item_Identifier, by=list(Category=combinedData$Outlet_Identifier), FUN= length)

# from Sales vs Outlet Identifier.png and Sales vs Outlet Type.png, 
# we learn that outlet 10 and 19 has far less sales
# replace Outlet_Size of "Other" to "Small"
combinedData[ which(combinedData$Outlet_Identifier == "OUT010") ,]$Outlet_Size <- "Small"
combinedData[ which(combinedData$Outlet_Identifier == "OUT017") ,]$Outlet_Size <- "Small"
combinedData[ which(combinedData$Outlet_Identifier == "OUT045") ,]$Outlet_Size <- "Small"

# OutletSize still contains "Other" where count=0, use factor to drop it
combinedData$Outlet_Size <- factor(combinedData$Outlet_Size)

# Some entries in Visibility is 0, impute them
combiNonZeroVis <- subset(combinedData, Item_Visibility > 0)

# replace 0 by NA, so that MICE works
outletIdentifiers <- levels(combinedData$Outlet_Identifier)
itemTypes <- levels(combinedData$Item_Type)
for (outName in outletIdentifiers) {
  for (itemName in itemTypes) {
    combinedData[ which(combinedData$Outlet_Identifier == outName &
                          combinedData$Item_Type == itemName),]$Item_Visibility <-
      ifelse(
        combinedData[ which(combinedData$Outlet_Identifier == outName &
                       combinedData$Item_Type == itemName), ]$Item_Visibility == 0 ,
        NA ,
        combinedData[ which(combinedData$Outlet_Identifier == outName &
                              combinedData$Item_Type == itemName),]$Item_Visibility
      )
  }
}

# use MICE to impute value for Item_Visibility
newCombinedData <- mice(combinedData,m=1,maxit=1,meth='pmm',seed=0)

# replace NA (formally zero) with imputed values from MICE
combinedData <- complete(newCombinedData,1)

# show total visibility per shop
shopSum <- as.data.frame(setNames(
  aggregate(combinedData$Item_Visibility, by=list(Category=combinedData$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

# normalize all vibilities so that total visibilities per shop becomes 100
for (outName in outletIdentifiers) {
  combinedData[ which(combinedData$Outlet_Identifier == outName),]$Item_Visibility <-
    combinedData[ which(combinedData$Outlet_Identifier == outName),]$Item_Visibility *
    100/shopSum[ which(shopSum$Outlet_Identifier == outName),]$TotVis
}

shopSum <- as.data.frame(setNames(
  aggregate(combinedData$Item_Visibility, by=list(Category=combinedData$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))

# plot a correlation matrix graph to show relations between visibility and other data
corMatrix <- cor(combinedData[1:nrow(train),][sapply(combinedData[1:nrow(train),], is.numeric)])
corrplot::corrplot(corMatrix, method="number", type="upper")
corrplot::corrplot(corMatrix, method="number", type="upper", order="hclust")

write.csv(combinedData, file="ProcessedData.csv")

gc(verbose = TRUE)
ls(all = TRUE)
rm(list = ls(all = TRUE)) 
ls(all = TRUE)
gc(verbose = TRUE)
