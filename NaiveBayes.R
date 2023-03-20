library(tm)
library(stringr)
library(wordcloud)
#library(Snowball)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext)
library(plyr) ## for adply
library(ggplot2)
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering

library(naivebayes)
library(tidyverse)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(Amelia)
library(mice)
library(GGally)
library(e1071)

df <- read.csv("netflix_new.csv")
df

df <- subset(df, select = -c(X))
head(df,10)

str(df)

set.seed(321)
r<-df[df$type, ]
trainIndex <- sample(nrow(r), 0.7 * nrow(r))
training <- df[trainIndex,]
testing <- df[-trainIndex,]
training$type <- factor(training$type)
testing$type <- factor(testing$type)
library(e1071)
model <- naiveBayes(type ~ ., data = training)
(model)
# laplace smoothing
#model <- naiveBayes(home_team_result ~ ., data = training,laplace=1)
# Making predictions on testing set
predictions <- predict(model, newdata = testing)
(predictions)
confusionMatrix(predictions, testing$type)

nb_cm<- confusionMatrix(predictions, testing$type)

plot(nb_cm$table, col = nb_cm$byClass, 
     main = "Naive Bayes Confusion Matrix", 
     xlab = "Predicted Species", ylab = "Actual Species")

library(ggplot2)
ggplot(data = as.data.frame(nb_cm$table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Freq), size = 15, color = "black") +
  labs(title = "Naive Bayes Confusion Matrix",
       x = "Actual Species", y = "Predicted Species")