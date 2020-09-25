#!/usr/bin/env python
# coding: utf-8

# # Meaningful predictive modeling
# 
# By Anahita Bilimoria
# 
# ### 1.0.1 1.Dataset for predicting admissions for master’s programs.(Regression problem, regularisation and MSE, )
# 
# ### 1.1 Regression
# 
# ### 1.1.1 Dataset for predicting admissions for master’s programs.
# 
# The dataset contains several parameters which are considered important during the application for
# Masters Programs The parameters included are:
# 1. GRE Scores ( out of 340 )
# 2. TOEFL Scores ( out of 120 )
# 3. University Rating ( out of 5 )
# 4. Statement of Purpose and Letter of Recommendation Strength ( out of 5 )
# 5. Undergraduate GPA ( out of 10 )
# 6. Research Experience ( either 0 or 1 )
# 7. Chance of Admit ( ranging from 0 to 1 )
# 
# ### 1.1.2 Length of dataset : 400
# 
# ### 1.1.3 You can find the dataset here :
# 
# https://www.kaggle.com/adityadeshpande23/admissionpredictioncsv#Admission_Predict.csv
# 
# ### 1.1.4 Dataset Cleaning :
# 
# 1.The feature ”Research Experience” had values 1 or 0,so it needs to be converted into True or
# False.
# 2.The rest of the features namely ”GRE Scores”,”TOEFL Scores”,”University Rating”,”Statement
# of Purpose and Letter of Recommendation Strength”,”Undergraduate GPA” need to be converted
# into float datatype.
# 
# 

# In[1]:


# Importing python libraries
import csv
import numpy as np
from sklearn import linear_model
from sklearn.linear_model import LinearRegression,LogisticRegression
from sklearn.metrics import mean_squared_error
import random


# In[2]:


#  Reading the csv file

path = "/Users/anahitabilimoria/Desktop/Admission_Predict.csv"
csv_file = open(path, mode='r')
reader = csv.reader(csv_file,delimiter=",")
headers = next(reader)


# In[3]:


#  Dataset Cleaning

X = []
Y = []
data = []

for row in reader:
    x_values = row[1:]
    values = [float(value) for value in x_values]
    
    if values[6] == float(1):
        values[6] = True
    else:
        values[6] = False
        
    data.append(values)


# In[4]:


#  Training Features and Training labels,Testing Features and Testing labels

random.shuffle(data)
length = len(data)

X = [x[:7] for x in data]
Y = [x[7] for x in data]

trainX = X[:length//2]
validX = X[length//2:3*length//4]
testX = X[3*length//4:]

trainY = Y[:length//2]
validY = Y[length//2:3*length//4]
testY = Y[3*length//4:]

trainX = np.array(trainX)
trainY = np.array(trainY)
testX = np.array(testX)
testY = np.array(testY)
validX = np.array(validX)
validY = np.array(validY)


# In[5]:


#  Regression model using Ridge
# Regularising with a value of 1, 0.5, 1.5, 2.0. the values of the MSE increase below 1 and decrease above 1.5 

reg1 = linear_model.Ridge(1.0, fit_intercept = False)
reg = reg1.fit(trainX, trainY)


# In[6]:


# Validation data against the regression model

validX1 = validX[0]
validY1 = validY[0]

predictedValidY1 = reg.predict([validX1])


# In[7]:


#  Testing data against the regression model

testX1 = testX
testY1 = testY
predictedY1 = reg.predict(testX1)


# In[8]:


# Calculating the MSE and FVU on the test set 

difference = [(x-y)**2 for (x,y) in zip(predictedY1, testY1)]

MSE = sum(difference) / len(difference)

print("MSE : ", MSE)
FVU = MSE / np.var(testX1)
R2 = 1 - FVU

print("R2 : ", R2)


# In[9]:


# Printing key outputs

print()
print()
print("Features : ",headers[1:7])
print()
print()
print("Data sample : ",data[0])
print()
print()
print("length of dataset : ",length)
print()
print()
print("One of the Feature vectors for training : ",trainX[0])
print()
print()
print("Output for the above feature vector : ",trainY[0])
print()
print()
print("Regression parameters of the Regression model : ",reg.coef_)
print()
print()
print("One of the Feature vectors for testing : ",testX1[0])
print()
print()
print("Expected Output of the above feature vector : ",testY1[0])
print()
print()
print("Actual Output of the above feature vector using Regression model ( validation data ): ",predictedValidY1[0])
print()
print()
print("Actual Output of the above feature vector using Regression model ( testing data ): ",predictedY1[0])
print()
print()

