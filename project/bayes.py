#panda package to read_csv
import pandas as pd
#Beautiful Soup to remove html tags, markups
from bs4 import BeautifulSoup
from collections import Counter
import re
from stemming.porter2 import stem
import nltk
from nltk.corpus import stopwords

def review_to_words(raw_review):

    bs = BeautifulSoup(raw_review).get_text() 
    letters = re.sub("[^a-zA-Z]", " ", bs) 
    words = letters.lower().split()
    stops = set(stopwords.words("english"))                  
    meaningful_words = [stem(w) for w in words if not w in stops]  
    return(" ".join( meaningful_words))

def get_text(reviews, sentiments, sentiment):

	text = ""
	i = 0
	for r in reviews:
		if(sentiments[i]==sentiment):
			text += " " + review_to_words(reviews[i])
		i += 1
	return text

def get_review_count(reviews, sentiments, sentiment):

	count = 0
	i = 0
	for r in reviews:
		if(sentiments[i]==sentiment):
			count += 1
		i+=1
	return count

def count_words(text):

	words = re.split("\s+",text)
	return Counter(words)


def  probability(review, word_counts, class_prob, count):

	prob = 1.000000
	review_counts = Counter(re.split("\s+", review))
	for word in review_counts:
		prob *= review_counts.get(word) * ((word_counts.get(word, 0) + 1)*1000/float(sum(word_counts.values()) + count))
	return float(prob * class_prob)

def predict(review, negative_words, negative_probability, negative_reviews_count, positive_words, positive_probability, positive_reviews_count):
	neg_prob = float(probability(review, negative_words, negative_probability, negative_reviews_count))
	pos_prob = float(probability(review, positive_words, positive_probability, positive_reviews_count))
	#print str(neg_prob) + " " + str(pos_prob)
	if(neg_prob > pos_prob):
		return 0
	return 1


####
print "Reading data.." 
training_data = pd.read_csv("labeledTrainData.tsv", header=0, delimiter="\t", quoting=3)
test_data = pd.read_csv("testData.tsv", header=0, delimiter="\t", quoting=3)

print "Getting text from all negative reviews.."
negative_reviews = get_text(training_data["review"],training_data["sentiment"],0)
print "Getting text from all positive reviews.."
positive_reviews = get_text(training_data["review"],training_data["sentiment"],1)

print "Counting words in negative reviews.."
negative_words = count_words(negative_reviews)
print "Counting words in positive reviews.."
positive_words = count_words(positive_reviews)

print "Counting total number of negative reviews.."
negative_reviews_count = get_review_count(training_data["review"],training_data["sentiment"],0)
print "Counting total number of positive reviews.."
positive_reviews_count = get_review_count(training_data["review"],training_data["sentiment"],1)


total_data = len(training_data["review"])
negative_probability = negative_reviews_count/float(total_data)
positive_probability = positive_reviews_count/float(total_data)

print "Predicting test data:\n"
done = 0
correct = 0
incorrect = 0
print "id","sentiment"

for r in test_data["review"]:

	prediction = predict(r, negative_words, negative_probability, negative_reviews_count, positive_words, positive_probability, positive_reviews_count)
	print "\"" + test_data["id"][done] + "\"" + "," + str(prediction)
	# if(prediction == test_data["sentiment"][done]):
	# 	correct += 1
	# else:
	# 	incorrect += 1
	done += 1

#####
# print "Stats:"
# print "Total - " + str(done)
# print "Correctly classified - " + str(correct)
# print "Incorrectly classified - " + str(incorrect)
# print "Accuracy - " + str((correct/float(done))*100)

#####