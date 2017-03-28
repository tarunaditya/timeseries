library(ggplot2)
library(plotly)
library(reshape2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(party)
library(forecast)
library(tseries)
gt<-read.csv("/home/tarun/Desktop/ipredict_models/GlobalLandTemperatures/GlobalTemperatures.csv") 
str(gt) 
gt<-gt[,-c(3,5,7,9)]        # removing uncertainity temperature columns 3,5 ,7 & 9
str(gt)
# Getting an aggregate by the maximum temperatures for "LandMaxTemperature"
gt.max   <- aggregate(gt[,3], by=list(substr((as.Date(gt$dt)),1,4)), max)
colnames(gt.max)<-c("Year","LandMaxTemperature")

# Getting an aggregate by the minimum temperatures for "LandMinTemperature"
gt.min   <- aggregate(gt[,c(4)], by=list(substr((as.Date(gt$dt)),1,4)), min)
colnames(gt.min)<-c("Year","LandMinTemperature")

# Getting aggregates by mean temperatures for "LandAverageTemperature"&" LandAndOceanAverageTemperature"
gt.mean   <- aggregate(gt[,c(2,5)], by=list(substr((as.Date(gt$dt)),1,4)), mean)
colnames(gt.mean)<-c("Year","LandAverageTemperature", "LandAndOceanAverageTemperature")

#merging all aggregates columns to create a new data.frame "m"
m1<-merge(gt.max,gt.min,by = "Year")
m<-merge(m1,gt.mean,by = "Year")
summary(m)                              # gives the result summary of the merged file 'm2'
m<-na.omit(m)


ggplot(m, aes(x= Year, y=LandAverageTemperature))+geom_point(shape= 1)+
  geom_smooth(method=lm, color = "red", se=TRUE)+
  ggtitle("Scatter plot with confidence interval around the trend\n Land Average Temperature 1850-2015")


x<- c("LandMinTemperature", "LandAverageTemperature","LandAndOceanAverageTemperature",  "LandMaxTemperature")
y<- as.formula(paste(paste(x, collapse="+"), "~Year"))
lattice::xyplot(y, data = m, type = "l")

#RF model
fitLAT<-randomForest(LandAverageTemperature~Year+LandMinTemperature+LandMaxTemperature+LandAndOceanAverageTemperature,
                     data = m,
                     importance = TRUE,
                     tree = 2000)

#RF prediction
PredictionrfLAT<- predict(fitLAT,m)
Result.rfLAT<- data.frame(Year = m$Year, LandAverageTemperature = PredictionrfLAT)
# Plotting the Predicted test data
ggplot(Result.rfLAT,aes(x=Result.rfLAT$Year,y=Result.rfLAT$LandAverageTemperature,color="red"))+geom_point()

#Evaluation
RMSE.PredictionrfLAT<-sqrt(mean((PredictionrfLAT-test$LandAverageTemperature)^2))
RMSE.PredictionrfLAT
MAE.PredictionrfLAT <- mean(abs(PredictionrfLAT-test$LandAverageTemperature))
MAE.PredictionrfLAT

# H2o implementation
library(h2o)
## Create an H2O cloud 
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "2G")    ## specify the memory size for the H2O cloud
h2o.removeAll() # Clean slate - just in case the cluster was already running

df <- h2o.importFile(path = normalizePath("/home/tarun/Desktop/ipredict_models/GlobalLandTemperatures/GlobalTemperatures.csv"))
df<-df[,-c(3,5,7,9)] 
splits <- h2o.splitFrame(
  df,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  ##  H2O will create one more split of 1-(sum of these parameters)
  ##  so we will get 0.6 / 0.2 / 1 - (0.6+0.2) = 0.6/0.2/0.2
  seed=1234)    ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex")   
## assign the first result the R variable train
## and the H2O name train.hex
valid <- h2o.assign(splits[[2]], "valid.hex")   ## R valid, H2O valid.hex
test <- h2o.assign(splits[[3]], "test.hex")     ## R test, H2O test.hex

## take a look at the first few rows of the data set
train[1:5,]   ## rows 1-5, all columns

## run our first predictive model
rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=3:4,                        ## the predictor columns, by column index
  y=2,                          ## the target index (what we are predicting)
  model_id = "rf_covType_v1",    ## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,                  ## use a maximum of 200 trees to create the
  ##  random forest model. The default is 50.
  ##  I have increased it because I will let 
  ##  the early stopping criteria decide when
  ##  the random forest is sufficiently accurate
  stopping_rounds = 2,           ## Stop fitting new trees when the 2-tree average is within 0.001 (default) of the prior two 2-tree averages.  
  #Can be thought of as a convergence setting
  score_each_iteration = T,      ## Predict against training and validation for
  ##  each tree. Default will skip several.
  seed = 1000000)                ## Set the random seed so that this can be  reproduced.

##############################################################################
summary(rf1)                     ## View information about the model.
## Keys to look for are validation performance
##  and variable importance

rf1@model$validation_metrics     ## A more direct way to access the validation 
##  metrics. Performance metrics depend on 
##  the type of model being built. With a
##  multinomial classification, we will primarily
##  look at the confusion matrix, and overall
##  accuracy via hit_ratio @ k=1.
h2o.hit_ratio_table(rf1,valid = T)[1,2]
## Even more directly, the hit_ratio @ k=1
###############################################################################

## Now we will try GBM. 
## First we will use all default settings, and then make some changes,
##  where the parameters and defaults are described.

gbm1 <- h2o.gbm(
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=3:4,                        ## the predictor columns, by column index
  y=2,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)                ## Set the random seed for reproducability

###############################################################################
summary(gbm1)                   ## View information about the model.

h2o.hit_ratio_table(gbm1,valid = T)[1,2]
## Overall accuracy.

## This default GBM is much worse than our original random forest.
## The GBM is far from converging, so there are three primary knobs to adjust
##  to get our performance up if we want to keep a similar run time.
## 1: Adding trees will help. The default is 50.
## 2: Increasing the learning rate will also help. The contribution of each
##  tree will be stronger, so the model will move further away from the
##  overall mean.
## 3: Increasing the depth will help. This is the parameter that is the least
##  straightforward. Tuning trees and learning rate both have direct impact
##  that is easy to understand. Changing the depth means you are adjusting
##  the "weakness" of each learner. Adding depth makes each tree fit the data
##  closer. 
##
## The first configuration will attack depth the most, since we've seen the
##  random forest focus on a continuous variable (elevation) and 40-class factor
##  (soil type) the most.
##
## Also we will take a look at how to review a model while it is running.

###############################################################################

