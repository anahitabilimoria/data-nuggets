#!/usr/bin/env python
# coding: utf-8

# # Meaningful predictive modeling
# 
# By Anahita Bilimoria
# 
# ### 1.0.2 2.Dataset for predicting presence of heart disease in the patient.(Classification problem, regularisation, Accuracy (TN, TP, FP, FN) and error (Balanced error rate))
# 

# In[1]:


# Importing python libraries
import csv
import numpy as np
from sklearn import linear_model
from sklearn.linear_model import LinearRegression,LogisticRegression
from sklearn.metrics import mean_squared_error
import random


# ### 1.2 2.Classification
# 
# ### 1.2.1 Dataset for predicting presence of heart disease in the patient.
# 
# The dataset contains several parameters which are considered important for the prediction of heart
# disease.They are as follows:
#     1. age
#     2. sex
#     3. chest pain type (cp) (4 values)
#     4. resting blood pressure (trestbps)
#     5. serum cholestoral in mg/dl (chol)
#     6. fasting blood sugar > 120 mg/dl (fbs)
#     7. resting electrocardiographic results (values 0,1,2) (restecg)
#     8. maximum heart rate achieved (thalach)
#     9. exercise induced angina (exang)
#     10. oldpeak = ST depression induced by exercise relative to rest (oldpeak)
#     11. the slope of the peak exercise ST segment (slope)
#     12. number of major vessels (0-3) colored by flourosopy (ca)
#     13. thal: 3 = normal; 6 = fixed defect; 7 = reversable defect (thal)
#     14. target(1 or 0) (target)
# 
# ### 1.2.2 Length of dataset : 303
# 
# ### 1.2.3 You can find the dataset here :
# 
# https://www.kaggle.com/ronitf/heart-disease-uci
# 
# ### 1.2.4 Dataset Cleaning :
# 
# 1.The feature ”sex”,”fbs”,”restecg”,”exang” had values 1 or 0,so it needs to be converted into True
# or False.
# 2.The rest of the features namely ”age”,”cp”,”trestbps”,”chol”,”thalach”,”oldpeak”,”slope”,”ca”,
# ”thal” need to be converted into float datatype.
# 

# In[10]:


#  Reading the csv file

path = "heart.csv"
csv_file = open(path, mode='r',encoding="utf-8-sig")
reader = csv.reader(csv_file,delimiter=",")
headers = next(reader)


# In[11]:


# Dataset Cleaning

X = []
Y = []
data = []

for row in reader:
    values = [float(value) for value in row]
    if values[1] == float(1):
        values[1] = True
    else:
        values[1] = False
    
    if values[5] == float(1):
        values[5] = True
    else:
        values[5] = False
    if values[6] == float(1):
        values[6] = True
    else:
        values[6] = False
    if values[8] == float(1):
        values[8] = True
    else:
        values[8] = False
    data.append(values)   


# In[12]:


#  Training Features and Training labels,Testing Features and Testing labels, Validation Features and Validation labels
# We split the dataset into training, validation and test sets using the ratio 50:25:25

random.shuffle(data)
length = len(data)
X = [x[:13] for x in data]
Y = [x[13] for x in data]

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


# In[13]:


# Classification model with ridge

classi1 = linear_model.Ridge(1.0, fit_intercept = False)
classi = classi1.fit(trainX, trainY)


# In[14]:


# Validation data against the model

validX1 = validX[0]
validY1 = validY[0]
predictedValidY1 = classi.predict([validX1])


# In[15]:


# Testing data against the classification model

testX1 = testX[0]
testY1 = testY[0]
predictedY1 = classi.predict([testX1])


# In[16]:


#  Calculating the list of ages and all ages > 35 as True
x_class = [x for x in data]
y_class = [(x[0] > 35) for x in data]

model1 = linear_model.LogisticRegression(max_iter = 200)
model = model1.fit(x_class, y_class)

predictions = model.predict(x_class)
correct = predictions == y_class

accuracy = sum(correct) / len(correct)
print("Accuracy : ",accuracy)


# In[17]:


# Now calculating the True positives, True negatives, False positives and False negatives

TP = sum([(p and l) for (p,l) in zip(predictions, y_class)])
FP = sum([(p and not l) for (p,l) in zip(predictions, y_class)])
TN = sum([(not p and not l) for (p,l) in zip(predictions, y_class)])
FN = sum([(not p and l) for (p,l) in zip(predictions, y_class)])

print("TP = ", TP)
print("FP = ", FP)
print("TN = ", TN)
print("FN = ", FN)


# In[18]:


# Calculating the accuracy based on above counts

total_accuracy = (TP + TN) / (TP + FP + TN + FN)
print("Total accuracy : ", total_accuracy)


# In[19]:


# Now calculating the True positive rate and True negative rate

TPR = TP / (TP+ FN)
TNR = TN / (TN + FP)

print("True positive rate : ", TPR)
print("True negative rate : ", TNR)


# In[20]:


# Calculating the balanced error rate

BER = 1 - 1/2 * (TPR + TNR)
print("Balanced error rate : ", BER)


# In[21]:


# Printing key outputs

print()
print()
print("Features : ",headers[:13])
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
print("One of the Feature vectors for testing : ",testX1)
print()
print()
print("Expected Output of the above feature vector : ",testY1)
print()
print()
print("Actual Output of the above feature vector using Classification model for Testing data: ",predictedY1[0])
print()
print()

