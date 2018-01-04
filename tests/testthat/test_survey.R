library(randomForest)

iris_rf <- randomForest(Species ~ ., data = iris)
partialPlot(iris_rf, pred.data = iris[,-5], x.var = "Sepal.Length", which.class = "setosa")
simd <- simulate_data(iris_rf, d = iris, class = "setosa", var1 = "Sepal.Length")
ggplot(simd, aes(x = Sepal.Length, y = preds)) +
  geom_line()
survey_forest(iris_rf, data = iris)

