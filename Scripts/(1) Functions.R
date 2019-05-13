
#### Training/Testing Set Function ####

TrainAndTestSets <- function(label,p,data){
  
    inTrain <- createDataPartition(y= label, p = p , list = FALSE)
                                training <- data[inTrain]
                                testing <- data[-inTrain]
                                
                  list(trainingSet=training ,testingSet=testing )              
  
  
}

#### Training Model Functions #### 

TrainModel <- function(formula,data,method,tunelength = 0,tunegrid = 0)
{
  fitControl <- trainControl(method = "repeatedcv", repeats = 10)
  if (method == "rf")
  {
    model <- train(formula , 
                   data = data ,
                   method = method ,
                   trcontrol =fitControl,
                   tunegrid = tunegrid  )
  }
     else 
    {
      model <-train(formula ,
                    data = data ,
                    method = method,
                    trcontrol= fitControl,
                    tunelength = tunelength)
    }
    
  model
  }



#### Predicting Function and postResample ####


PredictionsFunction <- function(model,newdata)
{
  Brandpredictions <- predict(model,newdata =newdata)
  
  Brandpredictions

}


####PreProcessing####


pProcessing <- function(data)
{
  
  
  
  
  data$brand <- factor(data$brand,
                                      levels = c(0 ,1 ) ,
                                      label = c("Acer","Sony"))
  
  data$elevel <- factor(data$elevel,
                                       levels = c(0,1,2,3,4),
                                       labels = c("No high school", "High School", "Some College", "College Degree", "Masters PHD"))
  #CarBrand
  data$car <- factor(data$car,
                                    levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                    labels = c("BMW","Buick", "Cadillac", "Chevrolet","Chrysler","Dodge","Ford","Honda","Hyundai","Jeep","Kia","Lincoln","Mazda","Mercedes","Mitsubishi","Nissan","Ram","Subaru","Toyota","None"))
  #Region
  data$zipcode <- factor(data$zipcode,
                                        levels = c(0,1,2,3,4,5,6,7,8),
                                        labels = c("New England","Mid Atlantic","East North Central","West North Central","South Atlantic","East South Central","West South Central","Mountain","Pacific"))
  
  
 
  
}

TestData <-  read.csv(file = "/home/zordo/Downloads/CompleteResponses.csv" , header = TRUE , sep =",")











