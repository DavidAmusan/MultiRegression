# *************************
# R Script For lab 2
# *************************
# This is where R starts execution
# *************************
# clears all objects in "global environment"
rm(list=ls())

# clears the console area
cat("\014")

# Starts random numbers at the same sequence
set.seed(123)

# prints the current working directory
print(paste("WORKING DIRECTORY: ",getwd()))

# Make sure you have loaded the files from SurreyLearn
# into the current working directory

# Define and then load the libraries used in this project

# For the 3d graphs
myLibraries<-c("scatterplot3d","caret")
library(pacman)
pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("lab2functions.R")

#read in csv file
Boston<-read.csv("boston.csv")

#randomize the data set
Boston<-Boston[sample(1:nrow(Boston)),]

# use ALL fields (columns)
training_records<-round(nrow(Boston)*(70/100))
training_data<-Boston[1:training_records,]
test_data<-Boston[-(1:training_records),]
test<-Boston[,"medv"]
#pairs plots every variable against every variable
pairs(training_data)

# ************************************************
# NscatterPlotError()
#
# Create a linear model for the named input field
# Visualise the performance error as error-bars
# and calculate R2, RMSE, MAE
#
# INPUT: data frame - datasetTrain - create model
# data frame - datasetTest - test model
# String - outputName - name of field to predict
# String - predictorName - name of predictor field
#
# OUTPUT : List - Metrics of $MAE, $RMSE and $R2

NscatterPlotError<-function(datasetTrain,datasetTest,outputName,predictorName){
  # Creates a "formula" and the trains model on TRAIN dataset
  formular<-paste(outputName,"~",predictorName)
  linearModel<-lm(formula=formular,data = datasetTrain)
  
  #extract predictor from test data set into data frame
  predicorInput<-subset(datasetTest,select=predictorName)
  
  #get predictions from the model using the data set
  y_predicted<- predict(linearModel,predicorInput)
  
  #extract the actual results from the test data frame
  y_actual<-datasetTest[,outputName]

  
  #calculate the metrics using the functions in lab2functions.r
  RMSE<-round(Nrmse(actual=y_actual, predicted=y_predicted),digits=2)
  mae<-round(Nmae(actual=y_actual, predicted=y_predicted), digits=2)
  r2<-round(Nr2(linearModel), digits=2)
  
  error<-y_actual-y_predicted
  
  # Create a data frame, so that we can sort these
  # the input predictor (x)
  # the expected value (actual_y)
  # and the residuals in the model
  results<-data.frame(predicorInput,y_actual,error)
  
  #order the values
  results<-results[order(y_actual),]
  
  #plots each point of the data set as an x
  plot(results[,predictorName],
       results$y_actual,
       pch=4,
       xlab=predictorName,
       ylab=outputName,
       main="Linear Regression Errors",
       sub=paste("Mae=",mae,"RMSE=",RMSE,"R2=",r2)
  )
  
  abline(linearModel,col="blue",lwd=3)
  
  # Plot verticle lines from the actual points to the predicted value,
  # highlighting the error magnitude
  suppressWarnings(arrows(results[,predictorName],
                          results$y_actual,
                          results[,predictorName],
                          results$y_actual-results$error,
                          length=0.05,angle=90,code=3,col="red"))
  return(
    list(
      RMSE=RMSE,
      MAE=mae,
      R2=r2
    )
  )
  }#endscatterPlotError Function

# ************************************************
# NplotAllErrors()
#
# Create & plot a linear model for all predictors
#
# INPUT: data frame - datasetTrain - TRAIN dataset
# data frame - datasetTest - TEST dataset
# String - outputName - name of field to predict
#
# OUTPUT : None
# ************************************************
NplotallErrors<-function(datasetTrain, datasetTest, outputName){
  print("RESULTS")
  
  for(i in 1:ncol(datasetTest)){
    
    xname<-names(datasetTest)[i]
    
    if(xname!="medv"){
      results<-NscatterPlotError(datasetTrain=training_data,
                                 datasetTest=test_data,
                                 outputName=outputName,
                                 predictorName=xname)  
      print(paste("Field=",xname,
                  "r2",results$R2,
                  "RMSE",results$RMSE,
                  "MAE",results$MAE))
      }
    
  }
}

TotalErrors<-NplotallErrors(training_data,test_data,outputName = "medv")

results<-NscatterPlotError(datasetTrain=training_data,
                           datasetTest=test_data,
                           outputName="medv",
                           predictorName="lstat") 

#multiple linear regression 
linearModel2Inputs<-lm(medv~log(lstat)+age,data=training_data)
formular<-paste("medv","~","lstat","+","rm")
y_actual<-test_data[,"medv"]

r2<-round(Nr2(linearModel2Inputs),digits=2)

x<-Boston[,"lstat"]
y<-Boston[,"medv"]
z<-Boston[,"rm"]
library(scatterplot3d)
scatterplot3d(x,y,z,pch=16,highlight.3d = TRUE)

#non linear regression 7 order polynomial 
nonLinearModel<- lm(medv~poly(lstat,age,rm,degree=10), data= training_data)
r2<-round(Nr2(nonLinearModel), digits = 2)

resolut<-NscatterPlotNonLinearRegression(datasetTrain=training_data,
                                datasetTest=test_data,
                                outputName="medv",
                                predictorName="lstat",
                                polyOrder=10)

threshold<-mean(training_data$medv)

NscatterPlotLinearDecisions(dataset=training_data,
                            outputName="medv",
                            predictorName="lstat",
                            threshold=threshold)

