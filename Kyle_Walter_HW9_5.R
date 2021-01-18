---
title: "Homework 9"
author: "Kyle Walter"
date: "11/30/2020"
output: html_document
---

IST687	– Support	Vector	Machines	Lab
Step	1:	Load	the	data
Let go	back	and	analyze	the	air	quality	dataset	(if	you	remember,	we	used	that	previously,	
in	the	visualization	lab).	Remember	to	think	about	how	to	deal	with	the	NAs	in	the	data.

```{r}
aq <- airquality #Load Airquality data set to a variable so as not to damage the original
aq$Ozone[is.na(aq$Ozone)] <- round(mean(aq$Ozone, na.rm = TRUE)) #Replaces Ozone NA values with the average Ozone Value - credit Monroe
aq$Solar.R[is.na(aq$Solar.R)] <- round(mean(aq$Ozone, na.rm=TRUE))#Replaces Solar.R NA Values with the average Solar.R values - Credit Monroe
```


Step	2:	Create	train	and	test	data	sets
Using	techniques	discussed	in	class,	create	two	datasets	– one	for	training	and	one	for	
testing.

```{r}
randomIndex <- sample(1:dim(aq)[1]) #creates a random index for the model to use.
cutpoint <- floor(2*dim(aq)[1]/3) #creates a cut point at 2/3rds of the record set
aqTrain <- aq[randomIndex[1:cutpoint],] #splits the record set into 2/3rd and stores it in a variable
aqTest <- aq[randomIndex[(cutpoint+1):dim(aq)[1]],] #loads the remaining into a seperate variable for testing.
```


Step	3:	Build	a	Model	using	KSVM	&	visualize	the	results
1) Build	a	model	(using	the	‘ksvm’	function,	trying	to	predict	onzone).	You	can	use	all	
the	possible	attributes,	or	select	the	attributes	that	you	think	would	be	the	most	
helpful.

```{r}
library(kernlab)
OzoneModel <- ksvm(Ozone~., data = aqTrain)
```


2) Test	the	model	on	the	testing	dataset,	and	compute	the	Root	Mean	Squared	Error

```{r}
library(Metrics)
OzonePred <- predict(OzoneModel, aqTest)
compTable <- data.frame(aqTest$Ozone, OzonePred,aqTest$Wind, aqTest$Temp)
compTable$Residual <- compTable$aqTest.Ozone - compTable$OzonePred
rmse(compTable$aqTest.Ozone,compTable$OzonePred)
```

3) Plot	the	results.	Use	a	scatter	plot.	Have	the	x-axis	represent	temperature,	the	y-axis	
represent	wind,	the	point	size	and	color	represent	the	error,	as	defined	by	the	actual	
ozone	level		minus	the	predicted	ozone	level).

```{r}
colnames(compTable) <- c("Ozone","OzonePred","Wind","Temp","Residual")
g <- ggplot(compTable, aes(x=Temp, y = Wind, size = Residual))+geom_point()
g
```

4) Compute	models	and	plot	the	results	for	‘svm’	(in	the	e1071	package)	and	‘lm’.
Generate	similar	charts for	each	model

```{r}
library(e1071)
model2 <- svm(Ozone~., data = aqTrain)
model3 <- lm(Ozone~., data = aqTest)
OzonePred2 <- predict(model2,aqTest)
OzonePred3 <- predict(model3, aqTest)
compTable2 <- data.frame(aqTest$Ozone, OzonePred2, aqTest$Wind, aqTest$Temp)
compTable3 <- data.frame(aqTest$Ozone, OzonePred3, aqTest$Wind, aqTest$Temp)
compTable2$Residuals <- compTable2$aqTest.Ozone - compTable2$OzonePred2
compTable3$Residuals <- compTable3$aqTest.Ozone - compTable3$OzonePred3
colnames(compTable2) <- c("Ozone","OzonePred","Wind","Temp","Residual")
colnames(compTable3) <- c("Ozone","OzonePred","Wind","Temp","Residual")
g2 <- ggplot(compTable2, aes(x=Temp, y = Wind, size = Residual))+geom_point()
g3 <- ggplot(compTable3, aes(x=Temp, y = Wind, size = Residual))+geom_point()
```

5) Show	all	three	results	(charts)	in	one	window,	using	the	grid.arrange	function

```{r}
library(gridExtra)
grid.arrange(g, g2, g3, ncol =2)
```

Step	4:	Create	a	‘goodOzone’	variable
This	variable	should	be	either	0	or	1.	It	should	be	0	if	the	ozone	is	below	the	average	for	all	
the	data	observations,	and 1	if	it	is	equal	to	or	above	the	average	ozone	observed.

```{r}
aq$goodOzone <- ifelse(aq$Ozone<mean(aq$Ozone),0,1)#Creats a column in the data set that tests if the Ozone is good or bad. Good = 0 bad = 1

```

Step	5:	See	if	we	can	do	a	better	job	predicting	‘good’	and	‘bad’	days
1) Build	a	model	(using	the	‘ksvm’	function,	trying	to	predict	‘goodOzone’).	You	can	use	
all	the	possible	attributes,	or	select	the	attributes	that	you	think	would	be	the	most	
helpful.

```{r}
cutpoint <- floor(2*dim(aq)[1]/3) #creates a cut point at 2/3rds of the record set
aqTrain <- aq[randomIndex[1:cutpoint],] #splits the record set into 2/3rd and stores it in a variable
aqTest <- aq[randomIndex[(cutpoint+1):dim(aq)[1]],] #loads the remaining into a seperate variable for testing.
goodModel <- ksvm(goodOzone~., data = aqTrain) #creates a model to see if we can predict good or bad ozone
```

2) Test	the	model	on	the	testing	dataset,	and	compute	the	percent	of	‘goodOzone’	that	
was	correctly	predicted.

```{r}
goodOzonePred <- predict(goodModel, aqTest)#use the test data
goodOzonePredRound <- round(goodOzonePred) # establish the 0 or 1 return
goodOCompTable <- data.frame(aqTest$goodOzone, goodOzonePred, goodOzonePredRound, aqTest$Wind, aqTest$Temp)
goodOCompTable$Residual <- goodOCompTable$aqTest.goodOzone - goodOCompTable$goodOzonePred #creates the residule between what the value was vs what was predicted
ModelReview <- table(goodOCompTable$aqTest.goodOzone, goodOCompTable$goodOzonePredRound) #creates a DF with the correct vs incorrect results
ModelTest <- (ModelReview[1,1] + ModelReview[2,2])/length(aqTest$Ozone) #pulls the correct results and divides by the number of samples in the test data
ModelTest*100 #Percent of the model accurately predicted
```

3) Plot	the	results.	Use	a	scatter	plot.	Have	the	x-axis	represent	temperature,	the	y-axis	
represent	wind,	the	shape	representing	what	was	predicted	(good	or	bad	day),	the	
color	representing	the	actual	value	of	‘goodOzone’	(i.e.	if	the	actual	ozone	level		was	
good)	and	the	size	represent	if	the	prediction	was	correct	(larger	symbols	should	be	
the	observations	the	model	got	wrong).

```{r}
colnames(goodOCompTable) <- c("goodOzone","goodOzonPred","goodOzonePredRound","Wind","Temp","Residual")
goodOCompTable$Discrete <- as.factor(if_else(goodOCompTable$goodOzonePredRound==1,"Bad","Good"))
g4 <- ggplot(goodOCompTable, aes(x=Temp, y=Wind, size=Residual, shape = Discrete, color = goodOzone))+geom_point()
g4
```

4) Compute	models	and	plot	the	results	for	‘svm’	(in	the	e1071	package)	and	‘nb’	
(Naive	Bayes,	also	in	the	e1071	package).

```{r}
goodModel2 <- svm(goodOzone~., data = aqTrain)
goodModel3 <- naiveBayes(goodOzone~., data = aqTrain)
goodOzonePred2 <- predict(goodModel2, aqTest)
goodOzonePred3 <- predict(goodModel3, aqTest)
goodOCompTable2 <- data.frame(aqTest$goodOzone, goodOzonePred2, goodOzonePredRound, aqTest$Wind, aqTest$Temp)
goodOCompTable3 <- data.frame(aqTest$goodOzone, goodOzonePred3, goodOzonePredRound, aqTest$Wind, aqTest$Temp)
```

5) Show	all	three	results	(charts)	in	one	window,	using	the	grid.arrange	function (have	
two	charts	in	one	row).
Step	6:	Which	are	the	best	Models	for	this	data?	
Review	what	you	have	done	and	state	which	is	the	best	and	why