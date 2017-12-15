# import libraries
library(psych)
library(neuralnet)
library(corrplot)
library(caTools)
library(ggplot2)
library(caret)
set.seed(100)

# import local files
setwd("./")
data = read.csv("ProcessedData.csv")

# convert categorical data to numerical data
dummy_fat <- dummy.code(data$Item_Fat_Content)
dummy_item_type <- dummy.code(data$Item_Type)
dummy_outlet_size <- dummy.code(data$Outlet_Size)
dummy_outlet_type <- dummy.code(data$Outlet_Type)
dummy_mrp_lvel <- dummy.code(data$MRP_Level)
dummy_location_type <- dummy.code(data$Outlet_Location_Type)

# append dummy coded data to data
data <- data.frame(dummy_fat, data)
data <- data.frame(dummy_item_type, data)
data <- data.frame(dummy_outlet_size, data)
data <- data.frame(dummy_outlet_type, data)
data <- data.frame(dummy_mrp_lvel, data)
data <- data.frame(dummy_location_type, data)

# remove some not meaningful column
data$Outlet_Identifier <- NULL
data$X <- NULL
data$Item_Identifier <- NULL
data$Item_Fat_Content <- NULL
data$Item_Type <- NULL
data$Outlet_Size <- NULL
data$Outlet_Type <- NULL
data$MRP_Level <- NULL
data$Outlet_Location_Type <- NULL

# normalize data method
normalizeData <- function(x) {(x - min(x))/(max(x)-min(x))}

# normalize data with mix-max
data$Year <- normalizeData(data$Year)
data$Item_MRP <- normalizeData(data$Item_MRP)
data$Item_Weight <- normalizeData(data$Item_Weight)
data$Item_Outlet_Sales <- normalizeData(data$Item_Outlet_Sales)

summary(data)

# split data set into training and testing
#train <- data[1:8524,]
#test <- data[8524:14205,]

train <- data[1:100,]
test <- data[8524:9524,]

# softplus activation function (a close approximation to ReLU)
softplus <- function(x) log(1+exp(x))

# train a neural model
n <- names(train[1:37])
f <- as.formula(paste("Item_Outlet_Sales ~", paste(n[!n %in% "Item_Outlet_Sales"], collapse = " + ")))
net <- neuralnet(formula = f, 
                 data = as.matrix(train),
                 act.fct = softplus,
                 algorithm = "rprop+",
                 stepmax = 1e6,
                 lifesign = "full",
                 rep = 2,
                 hidden = c(10, 8, 6),
                 linear.output = T)

# plot the nn model
par(mar = numeric(4), family = 'serif')
plot(net, 
     information = TRUE, 
     arrow.length = 0.1, 
     fontsize = 8,
     rep = "best",
     alpha = 0.8,
     col.out = "orange",
     col.entry = "red")

# see result
output <- compute(net, test[, 1:37])
