library(devtools)
devtools::install_github('Abdouragit/GaussianNaiveBayes')

library(rsample)
library(GaussianNaiveBayes)


# loading a dataset :
data <- data.frame(
  Number = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  Category = c("A", "B", "A", "C", "B", "C", "A", "B", "C", "A"),
  Length = c(1.5, 5.6, 9.3, 1.5, 5.6, 8.9, 17, 5.6, 4.2, 3)
  )


# train-test split :
set.seed(1)
split <- initial_split(data, prop = 0.7, strata = "Category")
data_train <- training(split)
data_test <- testing(split)


# split predictors (X) and target (y) :
X_train <- subset(data_train, select = -Category)
y_train <- data_train[, "Category"]

X_test <- subset(data_test, select = -Category)
y_test <- data_test[, "Category"]


nb = Gaussian_Naive_Bayes$new()

# create object
NB <- Gaussian_Naive_Bayes$new()

# fit
NB$fit(X_train, y_train)

# predict
y_pred = NB$predict(X_test,threshold = 0.8,eps = 0)

# Accuray
length(y_test[y_pred == y_test])/length(y_test)

# predict_proba
y_proba = NB$predict_proba(X_test)
print(y_proba)

# print
print(NB)

#summary
NB$summary()

#conf_mat
conf_mat = NB$confusion_matrix(y_test,y_pred)
print(conf_mat)


