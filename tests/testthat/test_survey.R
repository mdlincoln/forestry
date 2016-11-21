library(randomForest)

iris_rf <- randomForest(Species ~ ., data = iris)
survey_forest(iris_rf)

class <- "versicolor"

iris_rf[["votes"]] %>%
  as.data.frame() %>%
  bind_cols(iris) %>%
  mutate(
    actual = iris_rf$y,
    predicted = iris_rf$predicted,
    accurate_prediction = actual == predicted
  ) %>%
  rename_(.dots = list("class" = class)) %>%
  View()

