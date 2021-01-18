---
title: "R Notebook"
output: html_notebook
---

SVM Learning, install the package kernlab if not already installed

```{r}
install.packages("kernlab")
```

Call the package
```{r}
library(kernlab)
```

next brining in the data and inspect it. For class we're using the spam dataset
```{r}
data(spam)
str(spam)
dim(spam)
table(spam$type)
```

Create a random index because we'll need to be able to sample
```{r}
randIndex <- sample(1:dim(spam)[1])
cutPoint <- floor(2*dim(spam)[1]/3)
trainData <- spam[randIndex[1:cutPoint],]
testData <- spam[randIndex[(cutPoint+1):dim(spam)[1]],]
```


Next lets run create the model
```{r}
svmOutput <- ksvm(type~., data=trainData, kernel="rbfdot",kpar="automatic",C=5, cross=3, prob.model=TRUE)
svmPred <- predict(svmOutput, testData, type ="votes")
compTable <- data.frame(testData[,58],svmPred[1,])
table(compTable)
```

