library(randomForest)

iris_rf <- randomForest(Species ~ ., data = iris)
partialPlot(iris_rf, pred.data = iris[,-5], x.var = "Sepal.Length", which.class = "setosa")
survey_forest(iris_rf, data = iris)

