

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

# use the function for preprocessing
xomNasdaq <- preProcess(nasdaqFile,xomFile)
xomNasdaqModel <- lm(xomNasdaq$y.returns~xomNasdaq$x.returns, na.action = na.omit)
summary(xomNasdaqModel)


xom <- merge(xomNasdaq, xomSP, by = "Date",all=TRUE)
names(xom)[2:5] <- c("Nasdaq.returns","xom.returns2","SP500.returns","xom.returns" )
xomMultipleModel <- lm(xom$xom.returns ~ xom$Nasdaq.returns + xom$SP500.returns, na.action = na.omit)
summary(xomMultipleModel)



xom2 <- merge(xomOil, xomSP, by = "Date",all=TRUE)
names(xom2)[2:5] <- c("USO.returns","xom.returns2","SP500.returns","xom.returns" )
xomMultipleModelOil <- lm(xom2$xom.returns ~ xom2$USO.returns + xom2$SP500.returns, na.action = na.omit)
summary(xomMultipleModelOil)


xomKitchenSinkOil <- lm(xom2$xom.returns ~ xom2$USO.returns + xom2$SP500.returns + xom$Nasdaq.returns , na.action = na.omit)
summary(xomKitchenSinkOil )





xom <- merge(xomOil, xomSP, by = "Date")
xom <- merge(xomOil, xomSP, by = "Date",all=TRUE)
names(xom)[2:5] <- c("USO.returns","xom.returns2","SP500.returns","xom.returns" )
xomMultipleModel <- lm(xom$xom.returns ~ xom$USO.returns + xom$SP500.returns, na.action = na.omit)
summary(xomMultipleModel)



goog$Month = format(goog$Date,"%m")
googMonthModel <- lm(goog$y.returns~goog$x.returns+goog$Month+0)
summary(googMonthModel)


dummyVars <- model.matrix(~Month,goog)








# Example 3 : Multiple linear regression 
xomFile <- '/Users/swethakolalapudi/Desktop/Regression/xom.csv'
snpFile <-'/Users/swethakolalapudi/Desktop/Regression/snp.csv'
xom <- preProcess(xomFile,snpFile)
names(xom)[2:3] <- c("xom.returns","snp.returns" )
xom_SM <- lm(xom$xom.returns~xom$snp.returns)
summary(xom_SM)

uso <- read.table('/Users/swethakolalapudi/Desktop/Regression/uso.csv',header = TRUE, sep =",")[,c("Date","Adj.Close")]
names(uso)[2]<-"uso.returns"
uso[,c("Date")] <- as.Date(uso[,c("Date")])
uso <- uso[order(uso$Date, decreasing = TRUE),]
uso[-nrow(uso),-1] <- uso[-nrow(uso),-1]/uso[-1,-1]-1
xom <- merge(xom, uso, by = "Date")

xom_MLR <- lm(xom$xom.returns~xom$snp.returns + xom$uso.returns)
summary(xom_MLR)

# Example 4: Including a categorical variable
goog$Month = format(goog$Date,"%m")
dummyVars <- model.matrix(~Month,goog)
goog_MLR <- lm(goog$goog.returns~goog$nasdaq.returns+goog$Month)
summary(goog_MLR)

# Example 5 : Robust linear regression 
plot(goog$nasdaq.returns,goog$goog.returns)
abline(googM)
require(MASS)
googRLM <- rlm(goog$goog.returns~goog$nasdaq.returns)
abline(googRLM, lty ='twodash')

# Example 6 : Diagnostic plots 
plot(googM) 
