# edit column names
combinedData$Item_Fat_Content <- revalue(combinedData$Item_Fat_Content, c("LF"="Low Fat", "low fat"="Low Fat", "reg"="Regular"))

# check missing values
table(is.na(combinedData))

# where exactly is missing values
colSums(is.na(combinedData))

# output data count, group by Outlet_Identifier
aggregate(combinedData$Outlet_Identifier, by=list(Category=combinedData$Outlet_Identifier), FUN=length)

# output data count, group by Outlet_Identifier
aggregate(combinedData$Item_Identifier, by=list(Category=combinedData$Outlet_Identifier), FUN= length)

# output item list, sort by Item_Type, check each item's Item_Fat_Content
fat <- as.data.frame(setNames(
  aggregate(
    combinedData$Item_Fat_Content, 
    by=list(Category=combinedData$Item_Type,
            Category=combinedData$Item_Fat_Content), 
    FUN= length),
  c("Item_Type", "Item_Fat_Content", "number")
))

# write csv file
write.csv(combinedData, file="CleanedFile.csv")

# Item_Weight has most missing values, plot a graph of Item_Weight vs Item_Type
ggplot(combinedData, aes(Item_Type, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item Type") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Item Type")

# boxplot of weights vs. Outlet Identifier
ggplot(combinedData, aes(Outlet_Identifier, Item_Weight)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet_Identifier") + 
  ylab("Item Weight") + 
  ggtitle("Item Weight vs Outlet identifier")

# plot a graph of Item_MRP vs density
ggplot(combinedData, aes(x=Item_MRP)) + 
  geom_density(color = "blue", adjust=1/5) +
  geom_vline(xintercept = 69, color="red")+
  geom_vline(xintercept = 136, color="red")+
  geom_vline(xintercept = 203, color="red") + 
  ggtitle("Density of Item MRP")

# plot a graph of sales vs Outlet_Identifier
ggplot(combinedData[1:nrow(train),], aes(Outlet_Identifier, Item_Outlet_Sales)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet identifier") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet Identifier")

# plot a graph of Outlet_Type vs Outlet_Identifier
ggplot(combinedData[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Outlet Type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Outlet Type")

# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
  aggregate(
    combinedData$Outlet_Size, 
    by=list(Category=combinedData$Outlet_Identifier, 
            Category=combinedData$Outlet_Type,
            Category=combinedData$Outlet_Location_Type,
            Category=combinedData$Outlet_Size), 
    FUN= length),
  c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops

# use MICE to impute missing values
newCombinedData <- mice(combinedData,m=1,maxit=1,meth='pmm',seed=0)

# show total visibility per shop
shopSum <- as.data.frame(setNames(
  aggregate(combinedData$Item_Visibility, by=list(Category=combinedData$Outlet_Identifier), FUN=sum),
  c("Outlet_Identifier", "TotVis")))
shopSum

# normalize all vibilities so that total visibilities per shop becomes 100
for (outName in outletIdentifiers) {
  combinedData[ which(combinedData$Outlet_Identifier == outName),]$Item_Visibility <-
    combinedData[ which(combinedData$Outlet_Identifier == outName),]$Item_Visibility *
    100/shopSum[ which(shopSum$Outlet_Identifier == outName),]$TotVis
}

# plot a correlation matrix graph to show relations between visibility and other data
corMatrix <- cor(combinedData[1:nrow(train),][sapply(combinedData[1:nrow(train),], is.numeric)])
corrplot::corrplot(corMatrix, method="number", type="upper")
corrplot::corrplot(corMatrix, method="number", type="upper", order="hclust")

# do a PCA check
subData <- as.data.frame(cbind(
  combinedData[1:nrow(train),]$Item_Visibility, 
  combinedData[1:nrow(train),]$Item_MRP, 
  combinedData[1:nrow(train),]$Item_Outlet_Sales))

names(subData) <- c("Item_Visibility",
                    "Item_MRP",
                    "Item_Outlet_Sales")

sub.groupby <- combinedData[1:nrow(train),]$Outlet_Type

subData.pca <- prcomp(subData,
                      center = TRUE,
                      scale. = TRUE) 
summary(subData.pca)

g <- ggbiplot(subData.pca, 
              obs.scale = 1, 
              var.scale = 1, 
              groups = sub.groupby, 
              ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

# boxplot of  Sales vs. Item type
ggplot(combinedData[1:nrow(train),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Type)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
  xlab("Item type") + 
  ylab("Sales") + 
  ggtitle("Sales vs Item type")