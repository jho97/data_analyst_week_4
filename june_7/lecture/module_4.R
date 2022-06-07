

nasdaqFile <- 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/NASDAQ.csv'
spFile <- 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/SP_500.csv'
googFile <- 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/goog.csv'
oilFile <- 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/USO.csv'
xomFile <- 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/xom.csv'


# Example 2
# Preprocessing the data
goog <- read.table(googFile,header = TRUE, sep =",")[,c("Date","Adj.Close")]
names(goog)[2]<-"goog.price"
nasdaq <- read.table(nasdaqFile,header = TRUE, sep =",")[,c("Date","Adj.Close")]
names(nasdaq)[2]<-"nasdaq.price"

goog <- merge(goog, nasdaq, by = "Date")
goog[,c("Date")] <- as.Date(goog[,c("Date")])
goog <- goog[order(goog$Date, decreasing = TRUE),]

plot(goog$nasdaq.price)
plot(goog$goog.price)



googMisSpecified <- lm(goog$goog.price~goog$nasdaq.price)
plot(goog$goog.price,goog$nasdaq.price)
plot(googMisSpecified$resid,goog$nasdaq.price)
acf(googMisSpecified$resid)
summary(googMisSpecified)




goog[-nrow(goog),-1] <- goog[-nrow(goog),-1]/goog[-1,-1]-1
goog <- goog[-nrow(goog),]  
names(goog)[2:3] <- c("goog.returns","nasdaq.returns")
 

# Building a linear model
googGoodModel <- lm(goog$goog.returns~goog$nasdaq.returns)
summary(googGoodModel )


plot(goog$goog.returns,goog$nasdaq.returns)
plot(googGoodModel $resid,goog$nasdaq.returns)
acf(googGoodModel $resid)
summary(googGoodModel)



goog <- data.frame(nasdaq.returns = seq(-0.1,0.1,0.01))
predict(googM,new)

# Put all of the preprocessing in a function
preProcess <- function(xFile,yFile){
  x <-read.table(xFile,header = TRUE, sep =",")[,c("Date","Adj.Close")]
  names(x)[2]<-"x.price"
  y <- read.table(yFile,header = TRUE, sep =",")[,c("Date","Adj.Close")]
  names(y)[2]<-"y.price"
  xy <- merge(x,y, by = "Date")
  xy[,c("Date")] <- as.Date(xy[,c("Date")])
  xy <- xy[order(xy$Date, decreasing = TRUE),]
  xy[-nrow(xy),-1] <- xy[-nrow(xy),-1]/xy[-1,-1]-1
  xy <- xy[-nrow(xy),]  
  names(xy)[2:3] <- c("x.returns","y.returns")
  # Deal with missing values in the preprocessing step itself
  xy[,"y.returns"][is.na(xy[,"y.returns"])] <- mean(xy[,"y.returns"])
  return(xy)}



goog <- preProcess(nasdaqFile,googFile)
googModel <- lm(goog$y.returns~goog$x.returns, na.action = na.omit)
summary(googModel)

# use the function for preprocessing
xomGoog <- preProcess(xomFile,googFile)
xomGoogModel <- lm(xomGoog$y.returns~xomGoog$x.returns, na.action = na.omit)
summary(xomGoogModel)

