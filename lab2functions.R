# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  MACHINE LEARNING & VISULISATIONS
#
# Prof. Nick F Ryman-Tubb
# Dept. of Computer Science
# University of Surrey
# GUILDFORD
# Surrey GU2 7XH
#
# UPDATE
# 1.00      1/2/2017    Initial Version
# 1.01      18/2/2018   Pass datafile as variable
# 1.02      10/2/2019   Updated for 2019 lab / simplify
# 1.03      20/2/2019   nrmse removed unneeded code
# 1.04      10/10/2019  Added Nmae(), updated mewtrics in NscatterPlotMultiLinearRegression()
# 1.05      13/10/2019  NscatterPlotNonLinearRegression() evaluate on test
# 1.06      15/10/2019  Tidy syntax of functions
# 1.07      17/2/2020   Added NPredict() function and updated NscatterPlotLogisticRegression()
# `.08      6/10/2020   NscatterPlotMultiLinearRegression() added draw_polygon to show surface
#                       NscatterPlotLogisticRegression() added y axis name
#
# ************************************************
# Support functions prefixed by "N"

# ************************************************
# Nrmse()
#
# Calculate Root Mean Squared Error RMSE metric
#
# INPUT:      vector double - actual    - values for expected values
#             vector double - predicted - values of predicted values
#
# OUTPUT :    float         - calculated RMSE
# ************************************************
Nrmse<-function(actual,predicted){

  return(sqrt(mean((actual-predicted)^2)))
}
#             data frame - datasetTest   - test model
# ************************************************
# Nrmse()
#
# Calculate Mean Absolute Error MAE metric
#
# INPUT:      vector double - actual    - values for expected values
#             vector double - predicted - values of predicted values
#
# OUTPUT :    float         - calculated RMSE
# ************************************************
Nmae<-function(actual,predicted){

  return((mean(abs(actual-predicted))))
}

# ************************************************
# Nr2()
#
# Calculate the r2 metric (adjusted to allow for multiple inputs)
# REMEMBER this is a measure of the fit of the training data to the model
# it is NOT a measure of how good a predictor the model is on unseen data
#
# INPUT:      object - linearModel - trained linear model
#
# OUTPUT :    double -  R2 extracted from the model object
# ************************************************
Nr2<-function(linearModel){

  rsq<-summary(linearModel)$adj.r.squared

  return(rsq)
}

# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){

  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}

# ************************************************
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){
  for(field in 1:(ncol(dataset))){
    dataset[,field]<-Nrescale(dataset[,field])
  }
  return(dataset)
}

# ************************************************
# NplotDecisions()
#
# Visulise the decisions made by the linear model
#
# INPUT:
#               String - outputName   - name of the field to predict
#               Vector - y_predicted  - values of the output field
#               Frame  -  topredict   - values of the predictor field
#               Double - threshold    - value of the threshold over which is "YES"
#               String - title        - title of the graph
# OUTPUT :      None
# ************************************************
NplotDecisions<-function(outputName,
                         y_predicted,
                         topredict,
                         threshold,
                         title){

  predictorName1<-names(topredict)
  y_decisions<-ifelse(y_predicted>threshold,1.0,0.0)

  #Create a frame with all values, then two frames with the CLASS 0/CLASS 1
  scatter<-data.frame(topredict,y_decisions)
  class1<-scatter[which(scatter$y_decisions==1),]
  class0<-scatter[which(scatter$y_decisions==0),]

  plot(class1,pch=4,ylab=outputName,xlab=predictorName1,main=paste(title,outputName),col="green",ylim=c(0, 1),sub="2 Classes. Threshold shown.",xlim=c(min(topredict),max(topredict)))
  points(class0,pch=6,ylab=outputName,xlab=predictorName1,main=paste(title,outputName),col="red",ylim=c(0, 1))

}

# ************************************************
# NscatterPlotMultiLinearRegression()
#
# Creates a multiple linear regression model on the two named fields
# Visulises on 3d graph
#
# INPUT:        data frame - datasetTrain   - Training dataset
#               data frame - datasetTest    - Test dataset
#               String     - outputName     -  name of the field to predict
#               String     - predictorName1 - predictor field 1
#               String     - predictorName2 - predictor field 2
#
# OUTPUT :      None
# ************************************************
# Uses library(scatterplot3d)
NscatterPlotMultiLinearRegression<-function(datasetTrain,
                                            datasetTest,
                                            outputName,
                                            predictorName1,
                                            predictorName2){

  formular<-paste(outputName,"~",predictorName1,"+",predictorName2)
  linearModel<-lm(formular,data=datasetTrain)
  #linearModel<-lm(medv~.-lstat+log(lstat),data=datasetTrain) # this selects ALL fields and log

  x<-datasetTrain[,predictorName1]
  y<-datasetTrain[,outputName]
  z<-datasetTrain[,predictorName2]

  # Get the predicted values from the model in TEST dataset
  dframe<-datasetTest[,c(outputName,predictorName1,predictorName2)]
  #dframe<-datasetTest
  y_actual<-dframe[,predictorName2]
  y_predicted<-as.vector(predict(linearModel,dframe))

  # Calculate metrics
  RMSE<-round(Nrmse(y_actual,y_predicted),digits=2)
  mae<-round(Nmae(y_actual,y_predicted),digits=2)
  r2<-round(Nr2(linearModel),digits=2)

  s3d<-scatterplot3d(x=x,y=y,z=z,
                     main="2 Predictor Multi-linear Regression Model",
                     xlab=predictorName1,
                     ylab=outputName,
                     zlab=predictorName2,
                     pch=16,
                     highlight.3d = TRUE,
                     sub=paste("MAE=",mae,"RMSE=",RMSE," R2=",r2))

  s3d$plane3d(linearModel,draw_polygon=TRUE)
}
# ************************************************
# NscatterPlotLinearDecisions()
#
# Using simple linear classifier
# Plot a "decision" boundary based on threshold
#
# INPUT:        data frame - datasetTrain   - Training dataset
#               data frame - datasetTest    - Test dataset
#               String     - outputName     -  name of the field to predict
#               String     - predictorNam   - predictor field
#               double     - threshold      - cutoff value [0,1]
#
# OUTPUT :      None
# ************************************************
NscatterPlotLinearDecisions<-function(dataset,
                                      outputName,
                                      predictorName,
                                      threshold){

  #Creates a frame with a single column of the predictor input field
  topredict<-data.frame(dataset[,predictorName])
  names(topredict)<-predictorName

  #These are the real values, that we scale between 0-1
  # x-min / max-min
  v<-dataset[,outputName]
  minv<-min(v)
  maxv<-max(v)

  known_values<-(v-minv)/(maxv-minv)

  scaleThreshold<-(threshold-minv)/(maxv-minv)

  #Actual classes, split by the threshold
  NplotDecisions(outputName, known_values, topredict, scaleThreshold,"Linear Classifier")

  #Show the threshold, over this dotted line should classify into class 1
  abline(h=scaleThreshold,col="blue",lty=2)

  #Create small data frame with just the y and x values
  dframe<-data.frame(known_values,topredict)
  names(dframe)[1]=outputName

  #Build the linear model
  formular<-paste(outputName,"~",predictorName)
  linearModel<-lm(formular,data=dframe)

  #Show model line on graph - we need to rescale to the decision scale
  topredict<-data.frame(seq(min(dataset[,predictorName]),max(dataset[,predictorName]),by=.1))
  names(topredict)<-predictorName
  y_predicted<-predict(linearModel, topredict)

  #y_predicted_01<-rescale(y_predicted,range(0,1))

  #This shows the line that the model will use to seperate the classes
  lines(topredict[,predictorName],y_predicted,col="blue",lwd=4)
  lines(topredict[,predictorName],y_predicted>scaleThreshold,pch=4,col="black",lty=2,lwd=2)
}
# ************************************************
# NscatterPlotNonLinearRegression()
#
# Creates a non-linear regression model on the  named field
# Visulises on graph
#
# INPUT:        data frame - datasetTrain   - Training dataset
#               data frame - datasetTest    - Test dataset
#               String     - outputName     -  name of the field to predict
#               String     - predictorName1 - predictor field 1
#               String     - predictorName2 - predictor field 2
#               int        - polyOrder      - order of polynominal
#
# OUTPUT :      None
# ************************************************
NscatterPlotNonLinearRegression<-function(datasetTrain,
                                          datasetTest,
                                          outputName,
                                          predictorName,
                                          polyOrder){

  formular<-paste(outputName,"~","poly(",predictorName,",",polyOrder,")")
  linearModel<-lm(formular,data=datasetTrain)

  predictorInput<-data.frame(datasetTest[,predictorName])
  names(predictorInput)<-predictorName

  y_actual<-datasetTest[,outputName]
  y_predicted<-predict(linearModel, predictorInput)
  error<-y_actual-y_predicted

  RMSE<-round(Nrmse(y_actual,y_predicted),digits=2)
  mae<-round(Nmae(y_actual,y_predicted),digits=2)
  r2<-round(Nr2(linearModel),digits=2)

  scatter<-data.frame(predictorInput,y_actual)
  scatter<-scatter[order(predictorInput),]

  par(mar=c(5.1,4.1,4.1,2.1))
  plot(scatter[,1],scatter[,2],pch=4,
       ylab=outputName,xlab=predictorName,
       main=paste("Non-Linear Regression:",outputName,
                  sub=paste("Polynominal order:",polyOrder,"MAE=",mae,"RMSE=",RMSE," R2=",r2)))

  # Plot the model prediction line on the chart
  topredict<-data.frame(seq(min(scatter[,1]),max(scatter[,1]),by=.1))
  names(topredict)<-predictorName
  y_predicted<-predict(linearModel, topredict)
  lines(topredict[,predictorName],y_predicted,col="red",lwd=4)
}



# ************************************************
# NscatterPlotLogisticRegression()
#
# Logistic classifier - single predictor
#
# INPUT:        data frame - datasetTrain   - Training dataset
#               String     - outputName     -  name of the field to predict
#               String     - predictorName  - predictor field
#
# OUTPUT :      object     - trained linear model object
# ************************************************
NscatterPlotLogisticRegression<-function(datasetTrain,
                                         outputName,
                                         predictorName,
                                         threshold){

  v<-datasetTrain[,outputName]
  minv<-min(v)
  maxv<-max(v)
  y<-(v-minv)/(maxv-minv)

  scaleThreshold<-(threshold-minv)/(maxv-minv)
  x<-datasetTrain[,predictorName]
  topredict<-data.frame(x)


  #These show the classification of either HIGH or LOW house prices
  NplotDecisions(paste("p(",outputName,")",sep=""), y, topredict, 0.5, "Logistic Classifier")
  abline(h=scaleThreshold,col="blue",lty=2)

  #Build a logistic regression classifier
  dframe<-data.frame(x,y)
  formular<-paste("y~x")
  logisticModel<-glm(formular,data=dframe,family=quasibinomial)

  #creates prediction for an entire x range
  topredict<-data.frame(seq(min(x),max(x),.01))
  names(topredict)<-"x"

  #Now generate predictions on  range and plot to visulise the fitted model
  y_predicted<-predict(logisticModel, topredict,type="response")
  scatter<-data.frame(topredict,y_predicted)
  lines(scatter[,"x",],scatter[,"y_predicted"],pch=4,col="blue",lwd=4 )
  lines(scatter[,"x",],scatter[,"y_predicted"]>scaleThreshold,pch=4,col="black",lty=2,lwd=2)

  return(logisticModel)
}

# ************************************************
# Npredict()
#
# Predict class using trained Logistic classifier - single predictor
#
# INPUT:        data frame - dataset        - Input values
#               String     - predictorName  - predictor field
#
# OUTPUT :      vector    - predicted class {0,1}
# ************************************************
Npredict<-function(dataset,outputName,predictorName) {

  #Scale output between 0-1
  y<-Nrescale(dataset[,outputName])
  actualClass<-y>0.5

  predictedProbs<-as.vector(predict(logisticModel, data.frame(x=dataset[,predictorName]),type="response"))
  predictedClass<-predictedProbs>0.5

  confusion<-table(predictedClass,actualClass)

  return(confusion)
}

# ***************************************************
# Various metrics : Calculate various confusion matrix metrics
#
# INPUT: TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
#
# OUTPUT : float - calculated results
# ***************************************************

NcalcAccuracy<-function(TP,FP,TN,FN){return(100.0*((TP+TN)/(TP+FP+FN+TN)))}
NcalcPgood<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FP)))}
NcalcPbad<-function(TP,FP,TN,FN){return(100.0*(TN/(FN+TN)))}
NcalcFPR<-function(TP,FP,TN,FN){return(100.0*(FP/(FP+TN)))}
NcalcTPR<-function(TP,FP,TN,FN){return(100.0*(TP/(TP+FN)))}
NcalcMCC<-function(TP,FP,TN,FN){return( ((TP*TN)-(FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN)))}

# ***************************************************
# NcalcConfusion() :
# Calculate a confusion matrix for 2-class classifier
#
# INPUT: vector - confusion - output from table()
#
# OUTPUT: A list with the following entries:
#        TP - int - True Positive records
#        FP - int - False Positive records
#        TN - int - True Negative records
#        FN - int - False Negative records
#        accuracy - float - accuracy metric
#        pgood - float - precision for "good" (values are 1) metric
#        pbad - float - precision for "bad" (values are 1) metric
#        FPR - float - FPR metric
#        TPR - float - FPR metric
# ***************************************************
NcalcConfusion<-function(confusion){

  TP<-confusion[2,2]
  FN<-confusion[1,2]
  FP<-confusion[2,1]
  TN<-confusion[1,1]

  retList<-list(  "TP"=TP,
                  "FN"=FN,
                  "TN"=TN,
                  "FP"=FP,
                  "accuracy"=NcalcAccuracy(TP,FP,TN,FN),
                  "pgood"=NcalcPgood(TP,FP,TN,FN),
                  "pbad"=NcalcPbad(TP,FP,TN,FN),
                  "FPR"=NcalcFPR(TP,FP,TN,FN),
                  "TPR"=NcalcTPR(TP,FP,TN,FN),
                  "mcc"=NcalcMCC(TP,FP,TN,FN)
  )
  return(retList)
}
