
import pandas as pd 
import numpy as np


nasdaqFile = 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/NASDAQ.csv'
spFile = 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/SP_500.csv'
googFile = 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/goog.csv'
oilFile = 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/USO.csv'
xomFile = 'C:/Users/admin/Downloads/Regression_Material/Regression/Regression_Data/xom.csv'



data=pd.read_csv(googFile,sep=",",usecols=[0,6],names=['Date','Price'],header=0)
data.head()

returns=np.array(data["Price"][:-1],np.float)/np.array(data["Price"][1:],np.float)-1
data["Returns"]=np.append(returns,np.nan)

data.head()

data.index=data["Date"]
data.head()


# In[6]:

data = data["Returns"][0:-1]
data



def readFile(filename):
    data=pd.read_csv(filename,sep=",",usecols=[0,6],names=['Date','Price'],header=0)
    returns=np.array(data["Price"][:-1],np.float)/np.array(data["Price"][1:],np.float)-1
    data["Returns"]=np.append(returns,np.nan)
    data.index=data["Date"]
    return data 



# In[9]:

googData = readFile(googFile)
nasdaqData = readFile(nasdaqFile)
xomData = readFile(xomFile)


googData.head()
nasdaqData.head()


from sklearn.linear_model import SGDRegressor,LinearRegression

regressor = SGDRegressor(eta0=0.1,n_iter=100000,fit_intercept=False)

xData = nasdaqData["Returns"][0:-1].reshape(-1,1)
yData = googData["Returns"][0:-1]
goodGoogModel = regressor.fit(xData,yData)

    

goodGoogModel = regressor.fit(xData,yData)

goodGoogModel.score(xData,yData)


regressor.coef_

goodGoogModel.predict(np.array([-0.1,0.05]).reshape(-1,1))

import matplotlib.pyplot as plt
import numpy as np
from sklearn import datasets, linear_model

goodGoogModel = linear_model.LinearRegression()
goodGoogModel .fit(xData,yData)
goodGoogModel .score(xData,yData)

goodGoogModel .predict(np.array([-0.1,0.05]).reshape(-1,1))
goodGoogModel .coef_

plt.scatter(xData, yData,  color='black')
plt.plot(xData, goodGoogModel.predict(xData), color='blue',
         linewidth=3)

plt.xticks(())
plt.yticks(())

plt.show()


oilData = readFile(oilFile)
oilDataUnfilled = oilData["Returns"][0:-1]

nasdaqDataFilled = nasdaqData["Returns"][0:-1]

nasdaqDataFilled = nasdaqDataFilled[0:len(oilDataUnfilled)]


combined = np.vstack((nasdaqDataFilled , oilDataUnfilled )).T

xomDataFilled = xomData["Returns"][0:-1][0:len(oilDataUnfilled)]

xomNasdaqOilModel= linear_model.LinearRegression()
xomNasdaqOilModel.fit(combined,xomDataFilled)
xomNasdaqOilModel.score(combined,xomDataFilled)


import statsmodels.api as sm

X = xomDataFilled.reshape(1,-1)[0]
y = nasdaqDataFilled.reshape(1,-1)[0]


model = sm.OLS(xomDataFilled, X)
results = model.fit()
print(results.summary())


googData = readFile(googFile)
nasdaqData = readFile(nasdaqFile)

googData["Months"] = [int(x[5:7]) for x in googData["Date"]]

dummy = sm.categorical(googData["Months"].reshape(1,-1) , drop=True)

#dummy = dummy[:,1:]

xData = np.hstack((dummy, nasdaqData["Returns"].reshape(len(nasdaqData["Returns"]),-1)))[:-1]
yData = googData["Returns"][:-1].reshape(-1,1)

model = sm.OLS(yData, xData)
results = model.fit()
print(results.summary())



