library(e1071)

spambase <- read.csv(file="spambase.csv",head=TRUE,sep=",")
index <- 1:nrow(spambase)

cerr <- 0

for(i in 1:5){
	testindex <- sample(index, trunc(length(index)/5))
	testset <- spambase[testindex,]
	trainset <- spambase[-testindex,]

	svm.model <- svm(class ~ ., data=trainset,kernel='radial',type='C-classification',cost=100,gamma=1)
	svm.pred <- predict(svm.model,testset[,-58])
	err <- list(count=sum(testset$class!=svm.pred) / nrow(testset))
	#print(1-as.numeric(err))
	cerr <- cerr + 1 - as.numeric(err)
}
print(cerr/5)
#print(svm.pred)
#table(pred=svm.pred, true=testset[,58])
