
<h1 style="color:lightgreen;">Gaussian Naïve Bayes Package - R</h1>

<h1 style="color:#008000;">Overview</h1>

The objective of this project was to create an R package that offers a `GaussianNaiveBayes` method for supervised classification, coded in the R6 class. This package incorporates a `parallelization` method to reduce computation time and accommodate large volumes of data. It can be directly installed from `GitHub`. The core implementation of the Naive Bayes algorithm did not require existing packages. This package has been designed to be as `user-friendly` as possible for Gaussian Naïve Bayes classification tasks. 

<h1 style="color:#008000;">Installation</h1>

Just like many other `R` packages, `GaussianNaiveBayes` can be installed from
the `Github` repository `Abdouragit/GaussianNaïveBayes` by simply executing in the console the following
line:

``` r
install.packages("GaussianNaiveBayes")

# Or the the development version from GitHub:
install.packages("devtools")
library(devtools)
devtools::install_github('Abdouragit/GaussianNaiveBayes')
library(GaussianNaiveBayes)
```

<h1 style="color:#008000;">Description</h1>

Here are the different functionalities of our package that we will present in the following lines:<br>
  - `fit()`<br>
  - `predict()`<br>
  - `predict_proba()`<br>
  - `print()`<br>
  - `summary()`<br>
  - `confusion_matrix()`<br>

Documentation and help : <br>

To get some help on using our package and to see the documentation, you can type the following command in your console: <br>

````r
??GaussianNaiveBayes
````


<h1 style="color:#008000;">Demonstration</h1>


### 1. Data example

In order to test the GaussianNaiveBayes package, we will use the Iris dataset that we have modified: it contains 150 observations, 7 explanatory variables (4 numeric variables, 1 categorical variable, 2 binary variables), and 1 target variable with 3 categories.<br>

Before using the package, the user must first split their data into a training set and a test set.<br> 
Then, the user calls the `fit()` method of our class, providing `Xtrain` and `yTrain` as parameters.


``` r
set.seed(123)

indices_train <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))

# Creating training set
Xtrain <- data[indices_train, !colnames(data) %in% "Species"]
ytrain <- data[indices_train, "Species"]

# Creating test set
Xtest <- data[-indices_train, !colnames(data) %in% "Species"] 
ytest <- data[-indices_train, "Species"] 
```

### 2. Fit function 

The fit method takes `Xtrain` and `ytrain` as input. Firstly, it binarizes the explanatory variables by calling the private function `binarize` and ensures that the data is of numeric type and converts it into a matrix by calling the private function `check_numeric`. Secondly, the `fit()` function calculates the prior probabilities for each class. Finally, it estimates the parameters `mean` and ``standard deviations` for each class and each predictor based on the training sample. As output, this function stores the results in private members of the class.

``` r
# Create object
NB <- Gaussian_Naive_Bayes$new()

# fit
NB$fit(X_train, y_train)
```
### 3. Predict function 

The predict function takes an input dataset `Xtest` and returns predictions as a factor, where each observation is assigned to the class with the maximum posterior probability. This function retrieves the parameters of the trained model, such as class levels `lev`, prior probabilities `prior`, means `μ` and standard deviations `σ` . Finally, the class with the highest log-posterior probability is chosen as the prediction.

``` r
y_pred <- NB$predict(Xtest)
print(y_pred)

[1] Iris-setosa     Iris-virginica  Iris-virginica  Iris-setosa     Iris-setosa     Iris-virginica 
[7] Iris-virginica  Iris-setosa     Iris-setosa     Iris-virginica  Iris-versicolor Iris-setosa    
[13] Iris-virginica  Iris-virginica  Iris-setosa     Iris-virginica  Iris-virginica  Iris-virginica 
[19] Iris-versicolor Iris-virginica  Iris-setosa     Iris-virginica  Iris-setosa     Iris-virginica 
[25] Iris-virginica  Iris-setosa     Iris-virginica  Iris-versicolor Iris-virginica  Iris-setosa    
Levels: Iris-setosa Iris-versicolor Iris-virginica
```
### 4. Predict_proba function 

The user can invoke the `predict_proba` function to obtain, for each observation, the probabilities of belonging to each class. This function takes the `Xtest` sample as input and returns the posterior probabilities for each class in a matrix.

```r
y_proba = NB$predict_proba(Xtest)
print(y_proba)
      Iris-setosa Iris-versicolor Iris-virginica
 [1,]  9.999603e-01    3.968120e-05   3.919214e-12
 [2,]  4.925166e-01    2.160305e-09   5.074834e-01
 [3,]  1.008067e-69    2.014338e-02   9.798566e-01
 [4,]  1.000000e+00    1.688240e-99   6.945925e-46
 [5,]  1.000000e+00    4.491471e-11   1.556627e-08
 [6,]  2.889050e-34    3.748154e-02   9.625185e-01
 [7,] 5.007100e-167    3.797730e-22   1.000000e+00
 [8,]  9.997297e-01    2.645241e-04   5.813608e-06
 [9,]  1.000000e+00    6.832561e-24   2.373988e-26
[10,]  7.687517e-28    3.810086e-06   9.999962e-01
[11,]  1.082810e-12    6.429134e-01   3.570866e-01
[12,]  9.996895e-01    3.105100e-04   2.408072e-11
[13,]  2.467981e-27    1.506218e-11   1.000000e+00
[14,] 8.554121e-103    2.163013e-04   9.997837e-01
[15,]  1.000000e+00   2.554305e-166   2.388875e-80
[16,]  3.390786e-15    2.245796e-04   9.997754e-01
[17,]  1.544074e-20    1.222663e-03   9.987773e-01
[18,] 5.247928e-110    3.438145e-11   1.000000e+00
[19,]  5.004796e-05    9.999494e-01   5.590664e-07
[20,]  1.528165e-47    2.260298e-01   7.739702e-01
[21,]  1.000000e+00    5.573217e-69   7.760186e-24
[22,]  7.169246e-19    2.309047e-02   9.769095e-01
[23,]  1.000000e+00    1.806070e-18   4.392665e-25
[24,]  2.300602e-23    1.484411e-13   1.000000e+00
[25,] 1.203516e-142    7.508508e-05   9.999249e-01
[26,]  1.000000e+00   9.786417e-166   2.169799e-84
[27,]  2.760857e-20    5.965532e-06   9.999940e-01
[28,]  8.759319e-36    9.892813e-01   1.071872e-02
[29,] 2.420955e-104    4.415892e-10   1.000000e+00
[30,]  1.000000e+00    1.046011e-14   4.194716e-17
```

### 5. Print function 

The user can inspect the model parameters by calling the `print` function, which aims to display the prior probabilities for each class and the tables of conditional probabilities associated with each variable. This function obtains the tables of conditional probabilities by calling the `get_gaussian_tables` private method.

```r
NB$print()

Prior probabilities: 
----------------------------
  Iris-setosa :  0.333333333333333 
  Iris-versicolor :  0.291666666666667 
  Iris-virginica :  0.375 

Conditional Probabilities:
----------------------------

 Variable: SepalLengthCm 
                      mu        sd
Iris-setosa     4.985000 0.3560548
Iris-versicolor 5.928571 0.4913953
Iris-virginica  6.562222 0.6505686

 Variable: SepalWidthCm 
                      mu        sd
Iris-setosa     3.405000 0.3898397
Iris-versicolor 2.768571 0.3169240
Iris-virginica  2.966667 0.3313273

 Variable: PetalLengthCm 
                      mu        sd
Iris-setosa     1.472500 0.1802602
Iris-versicolor 4.271429 0.4501247
Iris-virginica  5.531111 0.5679680

 Variable: PetalWidthCm 
                      mu        sd
Iris-setosa     0.252500 0.1139901
Iris-versicolor 1.328571 0.2008147
Iris-virginica  2.013333 0.2785678

Variable: categorie.columnA 
                       mu        sd
Iris-setosa     0.5000000 0.5000000
Iris-versicolor 0.5142857 0.4997959
Iris-virginica  0.4444444 0.4969040

 Variable: categorie.columnB 
                       mu        sd
Iris-setosa     0.5000000 0.5000000
Iris-versicolor 0.4857143 0.4997959
Iris-virginica  0.5555556 0.4969040

 Variable: couleur.columnbleu 
                       mu        sd
Iris-setosa     0.3750000 0.4841229
Iris-versicolor 0.4285714 0.4948717
Iris-virginica  0.5555556 0.4969040

 Variable: couleur.columnrouge 
                       mu        sd
Iris-setosa     0.3000000 0.4582576
Iris-versicolor 0.3714286 0.4831867
Iris-virginica  0.2666667 0.4422166

 Variable: couleur.columnvert 
                       mu        sd
Iris-setosa     0.3250000 0.4683748
Iris-versicolor 0.2000000 0.4000000
Iris-virginica  0.1777778 0.3823256

 Variable: variable_logique.columnFALSE 
                       mu        sd
Iris-setosa     0.4750000 0.4993746
Iris-versicolor 0.4857143 0.4997959
Iris-virginica  0.5777778 0.4939136

 Variable: variable_logique.columnTRUE 
                       mu        sd
Iris-setosa     0.5250000 0.4993746
Iris-versicolor 0.5142857 0.4997959
Iris-virginica  0.4222222 0.4939136
```

### 6. Summary function 

The `summary` function provides the user a summary of key information about the model, including:<br>
  - The total number of observations in the dataset.<br>
  - The number of observations for each class.<br>
  - The prior probabilities for each class.<br>
  - The number and names of the features.<br>
  - The standard deviations and means of each feature for each class.<br>

```r

NB$summary()

= = = = = = = = = = 
- Number of observations:  120 
 
  
- Number of training observation in each class y
    Iris-setosa Iris-versicolor  Iris-virginica 
             40              35              45 
 
  
- Prior_probabilities in y: 
    Iris-setosa Iris-versicolor  Iris-virginica 
      0.3333333       0.2916667       0.3750000 
 
  
- Number of Features: 11 
 
  
- Features: SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm categorie.columnA categorie.columnB couleur.columnbleu couleur.columnrouge couleur.columnvert variable_logique.columnFALSE variable_logique.columnTRUE 
 
  
- Standard deviation of each feature: 
                SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm categorie.columnA categorie.columnB
Iris-setosa         0.3560548    0.3898397     0.1802602    0.1139901         0.5000000         0.5000000
Iris-versicolor     0.4913953    0.3169240     0.4501247    0.2008147         0.4997959         0.4997959
Iris-virginica      0.6505686    0.3313273     0.5679680    0.2785678         0.4969040         0.4969040
                couleur.columnbleu couleur.columnrouge couleur.columnvert variable_logique.columnFALSE
Iris-setosa              0.4841229           0.4582576          0.4683748                    0.4993746
Iris-versicolor          0.4948717           0.4831867          0.4000000                    0.4997959
Iris-virginica           0.4969040           0.4422166          0.3823256                    0.4939136
                variable_logique.columnTRUE
Iris-setosa                       0.4993746
Iris-versicolor                   0.4997959
Iris-virginica                    0.4939136

- Mean of each feature per class: 
                SepalLengthCm SepalWidthCm PetalLengthCm PetalWidthCm categorie.columnA categorie.columnB
Iris-setosa          4.985000     3.405000      1.472500     0.252500         0.5000000         0.5000000
Iris-versicolor      5.928571     2.768571      4.271429     1.328571         0.5142857         0.4857143
Iris-virginica       6.562222     2.966667      5.531111     2.013333         0.4444444         0.5555556
                couleur.columnbleu couleur.columnrouge couleur.columnvert variable_logique.columnFALSE
Iris-setosa              0.3750000           0.3000000          0.3250000                    0.4750000
Iris-versicolor          0.4285714           0.3714286          0.2000000                    0.4857143
Iris-virginica           0.5555556           0.2666667          0.1777778                    0.5777778
                variable_logique.columnTRUE
Iris-setosa                       0.5250000
Iris-versicolor                   0.5142857
Iris-virginica                    0.4222222
= = = = = = = = = = 
```

### 7. Confusion_matrix function 

The user can now assess the model's performance by calling the `confusion_matrix` function. This function takes two vectors, `ytest` and `y_pred`, representing the actual class values and the predicted values by the model, respectively. It outputs a confusion matrix, displaying the count of correct and incorrect predictions for each class. This allows the user to compute various evaluation metrics as needed.

```r

conf_mat = NB$confusion_matrix(ytest,y_pred)
print(conf_mat)

                pred-Iris-setosa pred-Iris-versicolor pred-Iris-virginica
Iris-setosa                    5                    0                   5
Iris-versicolor                4                    2                   9
Iris-virginica                 2                    1                   2

length(ytest[y_pred == ytest])/length(ytest)
[1] 0.3

```
<h1 style="color:#008000;">Parallelization strategy</h1>

This `GaussianNaiveBayes` package uses a parallelization strategy to reduce computation time on large datasets. The parallelization was implemented using the `parallel` package and the `parLapply`function.

```r

import.packages("parallel")

```

<h1 style="color:#008000;">R-Shiny interactive web application</h1>

You can access to the R-Shiny application for interactive analysis in:<br>

[Click here to access the R-Shiny interactive app](https://gaussiannaivebayes.shinyapps.io/NaiveBayesClassifier/)
(Warning: on this version, model training is not functional)

Please go to the "App" folder on this github to find the functional Shiny App or [click on this link](https://github.com/Abdouragit/GaussianNaiveBayes/blob/main/App/GaussianNaiveBaye_shiny_app.R)

<h1 style="color:#008000;">Authors</h1>

Natacha Perez, Abdourahmane Ndiaye, Annabelle Narsama




