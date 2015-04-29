import numpy
from sklearn import mixture

training=numpy.loadtxt('training',delimiter=',',skiprows=1)
validn=numpy.loadtxt('validn',delimiter=',',skiprows=1)
testset=numpy.loadtxt('testset',delimiter=',',skiprows=1)
training_d = training[:,0:20]
training_c = training[:,21]
testset_d = testset[:,0:20]
testset_c = testset[:,21]
validn_d = validn[:,0:20]
validn_c = validn[:,21]

max = 0
count = 0
while(count<10):
	
	numpy.random.seed(count)
	gmm = mixture.GMM(n_components=3, covariance_type='tied')
	gmm.means_ = numpy.random.random((3,21))
	gmm.covars_ = numpy.random.random((21,21))
	gmm.fit(training_d)
	cor = validn_c - gmm.predict(validn_d)
	
	total = 0	
	for s in cor:
		if(s==1):
			total = total+1
	#print total
	
	if (total > max) :
		max = total
		mean = gmm.means_
		cov = gmm.covars_
	count = count+1

gmm = mixture.GMM(n_components=3, covariance_type='tied')
gmm.means_ = mean
gmm.covars_ = cov
gmm.fit(training_d)
cor = testset_c - gmm.predict(testset_d)

total = 0
for s in cor:
	#print s
	if(s==1):
		total = total+1
print float(total)/500

