#library(plyr)
library(randomForest)

set.seed(100)

myData <- read.csv(file="letter-recognition.csv",head=TRUE,sep=",")

#summary(myData)

k = 5	#k-fold cross validation.

getError <- function(trainingSet, testSet, numTrees, m, sampSize){
	if(m>0){
		fit <- randomForest(trainingSet$lettr ~ ., data=trainingSet, mtry=m, importance=TRUE, ntree=numTrees)
	} else {
		if(sampSize>0){
			fit <- randomForest(trainingSet$lettr ~ ., data=trainingSet, importance=TRUE, ntree=numTrees, sampsize=(nrow(trainingSet))*sampSize)
		} else {
			fit <- randomForest(trainingSet$lettr ~ ., data=trainingSet, importance=TRUE, ntree=numTrees)
		}
	}		
	temp <- predict(fit, testSet[,-1], type='response')
	#err <- (length(testSet$lettr!=temp))/nrow(testSet)
	err <- list(count=sum(testSet$lettr!=temp) / nrow(testSet))
	#print(paste("m - ", m, ", Error - ", err, sep=" "))
	return(err)
}

crossValidationError <- function(data, numTrees, m, sampSize){
	cerr <- 0	
	print(paste("trees - ", numTrees))
	if(m>0) print(paste("Using mtry - ", m))
	for(i in 1:k){	
		x <- sample(1:nrow(data), 16000, replace=FALSE)
		trainingSet <- data[x,]
		testSet <- data[-x,]
		err <- getError(trainingSet, testSet, numTrees, m, sampSize)
		cerr <- cerr + as.numeric(err)
	}
	cerr <- (cerr/k)
	print(paste("cerr - ", cerr))
	return(cerr)
}

binarySearch <- function(data, low, high, epsilon, sampSize){
	if(low+10>=high) return(as.integer((low+high)/2))
	cerr_l <- crossValidationError(data, low, 0, sampSize)
	cerr_m <- crossValidationError(data, as.integer((low+high)/2), 0, sampSize)
	cerr_h <- crossValidationError(data, high, 0, sampSize)
	print(paste("low - ", low, "cerr_l - ", cerr_l, "cerr_m - ", cerr_m, ", cerr_h - ", cerr_h))	
	if(abs(cerr_h - cerr_m) <= epsilon){
		high <- as.integer((low+high)/2)
		if(abs(cerr_m - cerr_l) >= atleast){
			low <- as.integer((low+high)/2)
		}		
	}
	else{
		low <- as.integer((low+high)/2)
	}
	return(binarySearch(data, low, high, epsilon, sampSize))
}

linearSearch <- function(data, low, high, sampSize){
	if(high<=low) return(high)
	else {
		cerr_l <- crossValidationError(data, low, 0, sampSize)
		cerr_h <- crossValidationError(data, low+50, 0, sampSize)
		if(abs(cerr_h-cerr_l)<epsilon){
			binarySearch(data, low-50, low+50, 0.005, sampSize)
		} else return(linearSearch(data, low+50, high, sampSize))
	}
}

#part a(i)
epsilon <- 0.01
atleast <- 0.02
ans1 <- binarySearch(myData, 2, 400, epsilon, 0)
print(paste("BinarySearch - ", ans1))

#part a(ii)
diff <- 25
i <- 25
x_cod <- 2
y_cod <- crossValidationError(myData, 2, 0, 0)
while(i<=400){
	x_cod <- append(x_cod, i)
	y_cod <- append(y_cod, crossValidationError(myData, i, 0, 0)) 
	i <- i + diff
}
plot(x_cod, y_cod, main="Level-off", xlab="No. of trees", ylab="5-fold cross validation error", type="n")
lines(x_cod, y_cod)

#part b
fit <- randomForest(myData$lettr ~ ., data=myData, importance=TRUE, ntree=ans1)
print(fit)

#part c
max_m <- 16
i <- 1
x_cod <- 1
numTrees <- as.integer(1.25*ans1)
y_cod <- crossValidationError(myData, numTrees, i, 0)
i <- 2
while(i<=max_m){
	x_cod <- append(x_cod, i)
	cerr <- crossValidationError(myData, numTrees, i, 0)
	y_cod <- append(y_cod, cerr)
	print(paste("mtry - ", i, ", error - ", cerr))
	i <- i*2
}
plot(x_cod, y_cod, main="Level-off", xlab="m", ylab="5-fold cross validation error", type="n")
lines(x_cod, y_cod)

#part d
sampSize <- 0.1
ans <- linearSearch(myData, 2, 400, 0.1)
x_cod <- 10
err <- crossValidationError(myData, ans, 0, 0.1)
y_cod <- err
print(paste("sampsize - ", sampSize, ", T - ", ans, ", err - ", err))
for(i in 2:8){
	sampSize <- 0.1*i
	ans <- linearSearch(myData, 2, 400, sampSize)
	x_cod <- append(x_cod, sampSize*100)
	err <- crossValidationError(myData, ans, 0, sampSize)
	y_cod <- append(y_cod, err)
	print(paste("sampsize - ", sampSize, ", T - ", ans, ", err - ", err))
}
plot(x_cod, y_cod, main="T vs error", xlab="Sample size", ylab="5-fold cross validation error", type="n")
lines(x_cod, y_cod)

