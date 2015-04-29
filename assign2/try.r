#library(plyr)
library(randomForest)

set.seed(100)

myData <- read.csv(file="letter-recognition.csv",head=TRUE,sep=",")

form = as.formula(lettr ~ xbox+ybox+width+high+onpix+xbar+ybar+x2bar+y2bar+xybar+x2ybr+xy2br+xege+xegvy+yege+yegvx)

getError <- function(trainingSet, testSet, numTrees){
	
		fit <- randomForest(trainingSet$lettr ~ ., data=trainingSet, importance=TRUE, ntree=numTrees)
		temp <- predict(fit, testSet[,-1], type='response')
		#err <- (length(testSet$lettr!=temp))/nrow(testSet)
		err <- list(count=sum(testSet$lettr!=temp) / nrow(testSet))
		#print(paste("Error - ", err, sep=" "))
		return(err)
}

crossValidationError <- function(data, numTrees){
	print(paste("trees - ", numTrees))
	cerr <- 0

	for(i in 1:k){	
		x <- sample(1:nrow(data), 16000, replace=FALSE)
		trainingSet <- data[x,]
		testSet <- data[-x,]
		err <- getError(trainingSet, testSet, numTrees)
		cerr <- cerr + as.numeric(err)
	}
	
	return(cerr/k)

}

i <- crossValidationError(myData, 400)

print(paste(i))

