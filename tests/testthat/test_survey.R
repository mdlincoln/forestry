library(randomForest)

iris_rf <- randomForest(Species ~ ., data = iris)
survey_forest(iris_rf, data = iris)
