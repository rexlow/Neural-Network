library(ISLR)
library(caTools)
library(neuralnet)

# head(College, 3)
College
# Omit uni name, label and find max and min data
maxs <- apply(College[,2:18], 2, max)
mins <- apply(College[,2:18], 2, min)

## Normalize data
# Use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(College[,2:18],center = mins, scale = maxs - mins))

## Splitting data to train and test sets
# Convert Private column from Yes/No to 1/0
Private = as.numeric(College$Private)-1
data = cbind(Private,scaled.data)

set.seed(101)

# Create Split (any column is fine)
split = sample.split(data$Private, SplitRatio = 0.70)

train = subset(data, split == TRUE)
test = subset(data, split == FALSE)

feats <- names(scaled.data)

# Concatenate strings
f <- paste(feats,collapse=' + ')
f <- paste('Private ~',f)

# Convert to formula
f <- as.formula(f)

# Create a neural net
nn <- neuralnet(f,train,hidden=c(10,10,10),linear.output=FALSE)

# Apply test set
predicted.nn.values <- compute(nn,test[2:18])

# Check out net.result (probability)
predicted.nn.values$net.result

# round off all results to 0 and 1
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)

table(test$Private,predicted.nn.values$net.result)

# plot
plot(nn)