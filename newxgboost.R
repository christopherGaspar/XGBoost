# setup working directory
setwd("C:/Users/AMULGASPAR/Desktop/VijayAnna")

# Load the Data
Data <- read.csv("data.csv",
                 header=TRUE,
                 stringsAsFactors = T)

# install the packages
require(xgboost)
require(Matrix)
require(data.table)


# Creatrion of new features based on the Data
Data$Time_Remaining <- Data$minutes_remaining*60 + Data$seconds_remaining

# Modify existing features
Data$shot_distance <- ifelse(Data$shot_distance<40,1,2)


# Test and Train Datasets
TrainData <- na.omit(Data)
TestData <- subset(Data,is.na(Data$shot_made_flag))

# Feature selection in Boruta Algorithm
install.packages("Boruta")
library(Boruta)

# Reproducible  
set.seed(123)

# Feature selection
data.train <- Boruta(shot_made_flag~.,data=TrainData,doTrace=2)
print(data.train)
plot(data.train,xlab="",xaxt="n")

lz<-lapply(1:ncol(data.train$ImpHistory),function(i)
  data.train$ImpHistory[is.finite(data.train$ImpHistory[,i]),i])

names(lz) <- colnames(data.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(data.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(data.train)
print(final.boruta)
getSelectedAttributes(final.boruta,withTentative = F)

# Important attributes
kobe.df <- attStats(final.boruta)

# Drop exisiting features
drops <- c("game_event_id",
           "game_id",
           "lat",
           "loc_x",
           "loc_y",
           "lon",
           "minutes_remaining",
           "seconds_remaining",
           "team_id",
           "team_name",
           "match_up"
           )

Data<-Data[,!(names(Data) %in% drops)]


# labels for the prediction
train.label <- train$shot_made_flag
test.label <- test$shot_made_flag
shotlabel <- test$shot_id

train$shot_made_flag <- NULL
test$shot_made_flag <- NULL
train$shot_id <- NULL
test$shot_id <- NULL

pred <- rep(0,nrow(test))

train.matrix <- data.matrix(train,rownames.force = NA)
train.D <- xgb.DMatrix(data=train.matrix,label=train.label,missing = NaN)

watchlist <- list(train.matrix=train.D)

# Define the parameters for xgb boost
parameters <- list ( objective        = "binary:logistic",
                  booser              = "gbtree",
                  eval_metric         = "logloss",
                  eta                 = 0.040,
                  max_depth           = 5,
                  subsample           = 0.45,
                  colsample_bytree    = 0.45
                )


# cross validation results
crossvalid <- xgb.cv( params             = parameters,
                     data                = train.D,
                     nrounds             = 1500,
                     verbose             = 1,
                     watchlist           = watchlist,
                     maximize            = FALSE,
                     nfold               = 4,
                     early.stop.round    = 10,
                     print.every.n       = 1
                  )

# Find best round in the algorithm
best <- which.min(as.matrix(crossvalid[,3]))

# xgb.train method
train.model <- xgb.train( params              = parameters,
                          data                = train.D,
                          nrounds             = best, 
                          verbose             = 1,
                          watchlist           = watchlist,
                          maximize            = FALSE
                          )
# Test the model
testmodel <- data.matrix(test,rownames.force = NA)
result <- predict (train.model,testmodel)

# Result as Data frame 
final.result <- data.frame(shot.id = shotlabel, shot.made = result)

# Write the result
write.csv(final.result, "finalresult.csv", row.names = F)

# optional Steps 
prediction <- as.numeric(result > 0.5) # For binary classification 
prediction <- data.frame(shot.id = shotlabel, shot.made = prediction)
write.csv(prediction, "binaryresults.csv", row.names = F)

# important features
importance_features <- xgb.importance(model = train.model)
print(importance_features)
xgb.plot.importance(importance_matrix = importance_features)

# View the trees
install.packages("DiagrammeR")
library(DiagrammeR)

xgb.dump(train.model,with.stats = T)
xgb.plot.tree(model = train.model)

write.csv(train, "train.csv", row.names = F)
