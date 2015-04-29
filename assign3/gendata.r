require(MASS)

"h1" <- function(i){
	return(max(6-abs(i-11),0))
}

"h2" <- function(i){
	return(h1(i-4))
}

"h3" <- function(i){
	return(h1(i+4))
}

"vectorData" <- function(c,i){
	u = runif(1)
	if(c==1) return(u*h1(i) + (1-u)*h2(i) + runif(1,0,1))
	else if(c==2) return(u*h1(i) + (1-u)*h3(i) + runif(1,0,1))
	else return(u*h2(i) + (1-u)*h3(i) + runif(1,0,1))
}

"generateData" <- function(size){
	v <- vector()
	c <- sample(1:3,size,replace=T)
	for(k in 1:size){
		row <- vector()
		for(i in 1:21){
			row <- cbind(row, vectorData(c[k],i))
		}
		v <- rbind(v, row)		
	}
	return(data.frame(x = v[,1:21],y = c))
}

train <- generateData(1000)
val <- generateData(500)
test <- generateData(500)
write.table(train,file="training",quote=F,sep=",",row.names=F)
write.table(val,file="validn",quote=F,sep=",",row.names=F)
write.table(test,file="testset",quote=F,sep=",",row.names=F)

fit <- lda(y~., data = train)
testing <- predict(fit, newdata = test)
ct <- table(test$y, testing$class)
diag(prop.table(ct, 1))
sum(diag(prop.table(ct)))
fit <- qda(y~., data = train)

#plot(fit)
