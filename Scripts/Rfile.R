library(randomForest)
library(readr)
library(caret)
CompleteResponsesOG <- read.csv(file = "/home/zordo/Downloads/CompleteResponses.csv" , header = TRUE , sep =",")

CompleteResponsesOG$brand <- factor(CompleteResponsesOG$brand,
                                    levels = c(0 ,1 ) ,
                                    label = c("Acer","Sony")
)

CompleteResponsesOG$elevel <- factor(CompleteResponsesOG$elevel,
                                     levels = c(0,1,2,3,4),
                                     labels = c("No high school", "High School", "Some College", "College Degree", "Masters PHD"))
#CarBrand
CompleteResponsesOG$car <- factor(CompleteResponsesOG$car,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                  labels = c("BMW","Buick", "Cadillac", "Chevrolet","Chrysler","Dodge","Ford","Honda","Hyundai","Jeep","Kia","Lincoln","Mazda","Mercedes","Mitsubishi","Nissan","Ram","Subaru","Toyota","None"))
#Region
CompleteResponsesOG$zipcode <- factor(CompleteResponsesOG$zipcode,
                                      levels = c(0,1,2,3,4,5,6,7,8),
                                      labels = c("New England","Mid Atlantic","East North Central","West North Central","South Atlantic","East South Central","West South Central","Mountain","Pacific"))



inTrain <- createDataPartition(y = CompleteResponsesOG$brand , p = 0.75, list = FALSE )
training <- CompleteResponsesOG[inTrain,]
testing <- CompleteResponsesOG[-inTrain,]
fitControl <- trainControl(method = "repeatedcv", repeats = 10)
TreeComplete <- train(brand ~.,
                      data = training, 
                      method = "C5.0",
                      trcontrol = fitControl,
                      tuneLength = 2 
)

varImp(TreeComplete)

save(TreeComplete, file = "/home/zordo/Documents/Ubiqum/R/RTask2/Project/Task2/Task2AllOver/CompleTree.rsa")




fitControl <- trainControl(method = "repeatedcv", repeats = 10)


TreeComplete <- train(brand ~.,
                      data = training, 
                      method = "C5.0",
                      trcontrol = fitControl,
                      tuneLength = 2 
)

RandomForest <- train(brand ~ salary + age  ,
                            data = training,
                            method="rf",
                            trcontrol = fitControl,
                            tuneLength = 5 ) 

varImp(RandomForest)

save(RandomForest, file = "/home/zordo/Documents/Ubiqum/R/RTask2/Project/Task2/Task2AllOver/RandomForest.rsa")

summary(RandomForest)               

confusionMatrix(data = CompleteResponsesOG, testing$Brand)

RandomForest

BrandPrediction <- predict(RandomForest, newdata = testing)

str(BrandPrediction)

BrandPredictionPros <- predict(RandomForest,newdata = testing , type = "prob")

head(BrandPredictionPros)

CompleteResponsesOGwPredictions <- cbind(testing , BrandPrediction)

BrandPredictionsTree <- predict(TreeComplete, newdata = testing)

View(BrandPredictionsTree)

BrandPredictionTreeProb <- predict(TreeComplete,newdata = testing , type = "prob")

CompleteResponsesOGPrediction <- cbind(testing, BrandPredictionsTree)


postResample(BrandPrediction , testing$brand)
postResample(BrandPredictionsTree , testing$brand)

RandomForestwCar <- train(brand ~ salary + age + car,
                      data = training,
                      method="rf",
                      trcontrol = fitControl,
                      tunegrid = expand.grid(mtry= c(1,2,3,4,5)) ) 

save(RandomForestwCar, file = "/home/zordo/Documents/Ubiqum/R/RTask2/Project/Task2/Task2AllOver/RandomForestwCar.rsa")
varImp(RandomForestwCar)


BrandPredictionRandomForestwCar <- predict(RandomForest,newdata = testing)
postResample(BrandPredictionRandomForestwCar,testing$brand)


TreeASC <- train(brand ~ salary + age + car ,
                  data = training,
                  method = "C5.0",
                  trcontrol = fitControl,
                  tuneLenght = 2
                 )


TreeASC <- train(brand ~
                      data = training, 
                      method = "C5.0",
                      trcontrol = fitControl,
                      tuneLength = 2 
)


TreeASCPredictions <- predict(TreeASC,newdata = testing )
postResample(TreeASCPredictions,testing$brand)

## REAL TEST SET 

IncompleteSurvery <- read.csv(file ="/home/zordo/Downloads/SurveyIncomplete.csv",header = TRUE , sep = ",")


IncompleteSurvery$brand <- factor(IncompleteSurvery$brand,
                                    levels = c(0 ,1 ) ,
                                    label = c("Acer","Sony")
)

IncompleteSurvery$elevel <- factor(IncompleteSurvery$elevel,
                                     levels = c(0,1,2,3,4),
                                     labels = c("No high school", "High School", "Some College", "College Degree", "Masters PHD"))
#CarBrand
IncompleteSurvery$car <- factor(IncompleteSurvery$car,
                                  levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                  labels = c("BMW","Buick", "Cadillac", "Chevrolet","Chrysler","Dodge","Ford","Honda","Hyundai","Jeep","Kia","Lincoln","Mazda","Mercedes","Mitsubishi","Nissan","Ram","Subaru","Toyota","None"))
#Region
IncompleteSurvery$zipcode <- factor(IncompleteSurvery$zipcode,
                                      levels = c(0,1,2,3,4,5,6,7,8),
                                      labels = c("New England","Mid Atlantic","East North Central","West North Central","South Atlantic","East South Central","West South Central","Mountain","Pacific"))




predictionTreeALLincResp <- predict(TreeComplete , newdata = IncompleteSurvery)

summary(predictionTreeALLincResp)

IncompleteSurvery <- cbind(IncompleteSurvery, predictionTreeALLincResp)


IncompleteSurvery[8] <- NULL

postResample(predictionTreeALLincResp,IncompleteSurvery$brand )


varImp(RandomForestwCar)
summary(CompleteResponsesOG$brand)

predictionRandomForesASInc <- predict(RandomForest, newdata = IncompleteSurvery)
summary(predictionRandomForesAS)

RandomForestwCar <- predict(RandomForestwCar, newdata = IncompleteSurvery)
summary(RandomForestwCar)

predictionTreeASCInc <- predict(TreeASC, newdata = IncompleteSurvery)
summary(predictionTreeASC)


RandomForestwALL <- train(brand ~ .,
                          data = training,
                          method="rf",
                          trcontrol = fitControl,
                          tuneLength = 5 ) 

predictionRandomForestALLInc <- predict(RandomForestwALL , newdata = IncompleteSurvery)
summary(predictionRandomForestALL)
varImp(RandomForestwALL)
summary(RandomForestwALL)
RandomForestwALL

varImpPlot(RandomForest)


confusionMatrix(TreeASCPredictions,CompleteResponsesOG$brand) 


BinnedAge <-bin(CompleteResponsesOG, nbins = 4, labels = NULL, method = c("length", "content",
                                                           "clusters"), na.omit = TRUE
)

indxTrain <- createDataPartition(y = CompleteResponsesOG$brand,p = 0.75,list = FALSE)
training <- CompleteResponsesOG[indxTrain,]
testing <- CompleteResponsesOG[-indxTrain,]

trainX <- training[,names(training) != "brand"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- train(brand ~., data= training, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)





Predictions <- predict(TreeASC, testing)

Testing2 <- cbind(testing, Predictions)
Error <- Predictions != testing$brand
Testing3 <- cbind(Testing2, Error)
ggplot(data = Testing3, aes(x = salary, y = age, colour = Error)) + geom_point()


Error <- ggplot(Testing3, aes(x=salary, y=age)) + 
  geom_point(aes(col=Error), size=2) +  # Set color to vary based on state categories.
  
  labs(title="Model Error Points", subtitle="", y="Age", x="Salary")
plot(Error)

confusionMatrix(Predictions,testing$brand) 
