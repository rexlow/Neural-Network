library(neuralnet)
library(ggplot2)

setwd("./")
data = read.csv("ProcessedData.csv")

# methods
normalizeData <- function(x) {
  (x -min(x))/ (max(x)- min(x))
}
denormalizeData <- function(x) {
  return (x * (max(x) - min(x)) + min(x))
}
softplus <- function(x) log(1+exp(x))

# Outlet sales is greatly affected by Maximum Retail Price
# and also grocery store sells less than supermarket
# ggplot(data, aes(x=data$Item_MRP, y=data$Item_Outlet_Sales, color=data$Outlet_Type)) +
#   geom_point(shape=1) +
#   xlab("Maximum Retail Price") +
#   ylab("Item Outlet Sales") +
#   ggtitle("Item Sales vs Item type")
# # 
# # # this shows that how much each store sells (volume) is the linear function
# ggplot(data, aes(x=data$Item_MRP, y=log(data$Item_Outlet_Sales), color=data$Outlet_Type)) +
#   geom_point(shape=1) +
#   xlab("Maximum Retail Price") +
#   ylab("Item Outlet Sales") +
#   ggtitle("Item Sales vs Item type (logged transformed)")
# 
# head(data$Item_Outlet_Sales)
# 
# # solve this by averaging sells volume per store
# data$Item_Outlet_Sales <- data$Item_Outlet_Sales / data$Item_MRP
# 
# # result
# ggplot(data, aes(x=data$Item_MRP, y=data$Item_Outlet_Sales, color=data$Outlet_Type)) +
#   geom_point(shape=1) +
#   xlab("Maximum Retail Price") +
#   ylab("Item Outlet Sales")

# save ori price for scaling purpose
train_o <- data$Item_Outlet_Sales

# normalize data
data$Year <- normalizeData(data$Year)
data$Item_MRP <- normalizeData(data$Item_MRP)
data$Item_Weight <- normalizeData(data$Item_Weight)
data$Item_Outlet_Sales <- normalizeData(data$Item_Outlet_Sales)

# splitting data
train <- data[1:8523,]
test <- data[8524:14204,]
test$Item_Outlet_Sales <- NULL
test_item_identifier <- data[8524:14204,]$Item_Identifier
test_outlet_identifier <- data[8524:14204,]$Outlet_Identifier

# remove excess attributes
data$X <- NULL
data$Item_Identifier <- NULL
data$Outlet_Identifier <- NULL

# neuralnet model transform categorical data for us
m_train <- model.matrix(
  ~ Item_Outlet_Sales + 
    Item_Weight +
    Item_Fat_Content +
    Item_Visibility +
    Item_MRP +
    Outlet_Size + 
    Outlet_Location_Type +
    Outlet_Type +
    Year +
    MRP_Level,
  data = train
)

m_test <- model.matrix(
  ~ Item_Weight +
    Item_Fat_Content +
    Item_Visibility +
    Item_MRP +
    Outlet_Size +
    Outlet_Location_Type +
    Outlet_Type +
    Year +
    MRP_Level,
  data = test
)

# build a net
net = neuralnet(
  Item_Outlet_Sales ~
    Item_Weight +
    Item_Fat_ContentNone +
    Item_Fat_ContentRegular +
    Item_Visibility +
    Item_MRP +
    Outlet_SizeSmall +
    Outlet_SizeMedium +
    Outlet_Location_TypeTier_2 +
    Outlet_Location_TypeTier_3 +
    Outlet_TypeSupermarket_Type1 +
    Outlet_TypeSupermarket_Type2 +
    Outlet_TypeSupermarket_Type3 +
    Year +
    MRP_LevelLow +
    MRP_LevelMedium +
    MRP_LevelVery_High,
  stepmax = 1e6,
  data = m_train,
  lifesign = "full",
  linear.output = T,
  act.fct = softplus,
  algorithm = "rprop+",
  hidden = c(5, 3, 2)
)

# exclude intercept and label
result <- compute(net, m_test[,2:17])
predictedValue <- result$net.result * (max(train_o) - min(train_o)) + min(train_o)

predictedValue <- as.data.frame(predictedValue)

predictedValue["Item_Identifier"] <- test_item_identifier
predictedValue["Outlet_Identifier"] <- test_outlet_identifier
colnames(predictedValue)[colnames(predictedValue)=="V1"] <- "Item_Outlet_Sales"

write.csv(predictedValue, file="Result.csv")

# release memory
gc(verbose = TRUE)
rm(list = ls(all = TRUE))
gc(verbose = TRUE)
