ggCompleteSurvey <- ggplot(CompleteResponsesOG, aes(x=salary, y=age)) + 
  geom_point(aes(col=brand), size=3) +   
  labs(title="Brand by Salary and Age", y="Age", x="Salary")
plot(ggCompleteSurvey)


IncompleteResponsesWPredictions <- cbind(IncompleteSurvery,predictionRandomForestALL)


ggIncompleteSurvey <- ggplot(IncompleteResponsesWPredictions, aes(x=salary, y=age)) + 
  geom_point(aes(col=predictionRandomForestALL), size=3) +   
  labs(title="Brand by Salary and Age", y="Age", x="Salary")
plot(ggIncompleteSurvey)


library(ggplot2)
gg <- ggplot(CompleteResponsesOG, aes(x=salary, y=age)) + 
  geom_point(aes(col=brand), size=3) +  # Set color to vary based on state categories.
 
  labs(title="Brand by Age and Salary", subtitle="", y="Age", x="Salary")
plot(gg)


plot(CompleteResponsesOG$age,CompleteResponsesOG$salary)


myplot <- ggplot(CompleteResponsesOG, aes(brand)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

myplot