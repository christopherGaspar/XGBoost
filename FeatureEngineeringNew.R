# setup working directory
setwd("C:/Users/AMULGASPAR/Desktop/VijayAnna")

Data <- read.csv("data.csv",header=TRUE,stringsAsFactors = F)#load the file

# Creatrion of new features based on the Data
Data$Time_Remaining <- Data$minutes_remaining*60 + Data$seconds_remaining

# Modify existing features
Data$shot_distance <- ifelse(Data$shot_distance<40,1,2)


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

# Test and Train Data
test <- subset(Data,is.na(Data$shot_made_flag))
train <- subset(Data,!is.na(Data$shot_made_flag))

# Feature selection in Boruta Algorithm
install.packages("Boruta")
library(Boruta)

# Reproducible  
set.seed(123)

# Feature selection
data.train <- Boruta(shot_made_flag~.,data=train,doTrace=2)
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

write.csv(kobe.df, "Boruta_Importance.csv", row.names = F)
