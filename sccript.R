install.packages("twitteR")
install.packages("ROAuth")
install.packages("httpuv")
install.packages("httr")
install.packages("base64enc")

library("qdap")
library("stringr")
library("twitteR")
library("ROAuth")
library("plyr")
library("dplyr")


setup_twitter_oauth(consumerKey, consumerSecret, access_token = NULL,access_secret = NULL)


tweet1000<-ldply(searchTwitter("#Apple", n=10000, lang="en"), statusText)


#Exporting Dataset


write.csv(t_2, "C:/Users/new/Desktop/minor/Final_Dataset.csv")


# 1. RT_or_not

RT_or_not<-matrix(0, nrow=nrow(tweet1000),ncol=1)


for( i in 1:nrow(tweet1000)){
	if(grepl("RT @",tweet1000$V1[i])==TRUE){
	RT_or_not[i]<-1
	}
	else if(grepl("RT @",tweet1000$V1[i])==FALSE){
	RT_or_not[i]<-0
	}
}

RT_or_not<-as.numeric(RT_or_not)

# 2. No. of Hashtags

t_hashwords<-matrix(0,nrow=nrow(tweet1000),ncol=1)

for(i in 1:nrow(tweet1000)){
 t_hashwords[i]<-str_extract_all(tweet1000$V1[i],"#\\w+")
 }


t_hash<-matrix(0,nrow=nrow(tweet1000),ncol=1)

for(i in 1:nrow(tweet1000))
t_hash[i,1]<-length(t_hashwords[[i]])

t_hash<-as.numeric(t_hash)


# 3. No. of Mentions

t_mention_words<-matrix(0,nrow=nrow(tweet1000),ncol=1)

for(i in 1:nrow(tweet1000)){
 t_mention_words[i]<-str_extract_all(tweet1000$V1[i],"@\\w+")
 }


t_mention<-matrix(0,nrow=nrow(tweet1000),ncol=1)

for(i in 1:nrow(tweet1000))
t_mention[i,1]<-length(t_mention_words[[i]])

t_mention<-as.numeric(t_mention)

# 4. Sentiment

t_sentiment<-matrix(0,nrow(tweet1000),ncol=1)

for( i in 1:nrow(tweet1000)){
pol<-function(i){
print(i);
polar<-polarity(tweet1000$V1[i], grouping.var = NULL,
  polarity.frame = qdapDictionaries::key.pol,
  negators = qdapDictionaries::negation.words,
  amplifiers = qdapDictionaries::amplification.words,
  deamplifiers = qdapDictionaries::deamplification.words,
  question.weight = 0, amplifier.weight = 0.8, n.before = 4,
  n.after = 2, rm.incomplete = FALSE, digits = 3)



if(polar$all$polarity >0){
sentiment_1 = 3
}
else if (polar$all$polarity<0){
sentiment_1 = 1
}
else if (polar$all$polarity==0){
sentiment_1 = 2
}

t_sentiment[i]<-sentiment_1
}
}

for(i in 1:nrow(tweet1000))t_sentiment[i]<-try(pol(i),FALSE)

# 4.1 Convert to zero one

for(i in 1:nrow(tweet1000)){

	if( grepl("Error",t_sentiment[i])==TRUE){
	t_sentiment[i]<-0
	}
	else if( grepl("Error",t_sentiment[i])==FALSE){
	t_sentiment[i]<-t_sentiment[i]
	}

}



t_sentiment<-as.numeric(t_sentiment)




# 5: NER


install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")


library(NLP)
library(openNLP)

# Annotating people and places 

bio<-tweet1000$V1
bio1<-bio
bio1<-gsub("[^[:alnum:]]"," ",bio1)

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")


pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann
		 )
bio_annotations1 <- annotate(bio1, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio1, bio_annotations1)

entities <- function(doc, kind){
 s <- doc$content
  a <- annotations(doc)[[1]]
   if(hasArg(kind)) {
     k <- sapply(a$features, `[[`, "kind")
     s[a[k == kind]]
     } else {
     s[a[a$type == "entity"]]
   }
 }




#1: No_of_Person

t_no_of_persons<-matrix(0,nrow=nrow(tweet1000),ncol=1)


   pers<-function(i){
	print(i);
             bio_annotations1 <- annotate(bio1[i], pipeline);
             bio_doc <- AnnotatedPlainTextDocument(bio1[i], bio_annotations1);

             t_no_of_persons[i]<-length(entities(bio_doc, kind = "person"));
	}

for(i in 1:nrow(tweet1000)) t_no_of_persons[i] <- try(pers(i), FALSE)

#1.1: convert data into 1 and 0



for(i in 1:nrow(tweet1000)){

	if( grepl("Error",t_no_of_persons[i])==TRUE){
	t_no_of_persons[i]<-0
	}
	else if( grepl("Error",t_no_of_persons[i])==FALSE){
	t_no_of_persons[i]<-t_no_of_persons[i]
	}

}

t_no_of_persons<-t_no_of_persons$V1

t_no_of_persons<-as.numeric(t_no_of_persons)



#2: No_of_locations

t_nol<-matrix(0,nrow=nrow(tweet1000),ncol=1)


 locs<-function(i){
	print(i);
             bio_annotations1 <- annotate(bio1[i], pipeline);
             bio_doc <- AnnotatedPlainTextDocument(bio1[i], bio_annotations1);

             t_nol[i]<-length(entities(bio_doc, kind = "location"));
	}

for(i in 1:nrow(tweet1000)) t_nol[i] <- try(locs(i), FALSE)


#2.1: convert data into 1 and 0


for(i in 1:nrow(tweet1000)){

	if( grepl("Error",t_nol[i])==TRUE){
	t_nol[i]<-0
	}
	else if( grepl("Error",t_nol[i])==FALSE){
	t_nol[i]<-t_nol[i]
	}

}

t_nol<-t_nol$V1

t_nol<-as.numeric(t_nol)

#3: No_of_organizations

t_no_of_org<-matrix(0,nrow=nrow(tweet1000),ncol=1)


 orgs<-function(i){
	print(i);
             bio_annotations1 <- annotate(bio1[i], pipeline);
             bio_doc <- AnnotatedPlainTextDocument(bio1[i], bio_annotations1);

             t_no_of_org[i]<-length(entities(bio_doc, kind = "organization"));
	}

for(i in 1:nrow(tweet1000)) t_no_of_org[i] <- try(orgs(i), FALSE)


#3.1: convert data into 1 and 0



for(i in 1:nrow(tweet1000)){

	if( grepl("Error",t_no_of_org[i])==TRUE){
	t_no_of_org[i]<-0
	}
	else if( grepl("Error",t_no_of_org[i])==FALSE){
	t_no_of_org[i]<-t_no_of_org[i]
	}

}

t_no_of_org<-t_no_of_org$V1

t_no_of_org<-as.numeric(t_no_of_org)



# 6. No. of Links


t_link_words<-matrix(0,nrow=nrow(tweet1000),ncol=1)

for(i in 1:nrow(tweet1000)){
 t_link_words[i]<-str_extract_all(tweet1000$V1[i],"http|https")
print(i)
 }


t_link<-matrix(0,nrow=nrow(tweet1000),ncol=1)

for(i in 1:nrow(tweet1000))
t_link[i]<-length(t_link_words[[i]])

t_link<-as.numeric(t_link)



#7 Combining Dataset


t_2<-cbind(RT_or_not,t_hash,t_mention,t_sentiment,t_no_of_persons6,t_nol6,t_no_of_org6,t_link)


#8 Removing the zero Sentiment

#t_1000_complete1<-as.data.frame(t_1000_complete1)

t_10<-t_2[t_2$t_sentiment!=2,]


#8.1 Convert 1 to 0 and 3 to 1

for(i in 1:nrow(t_10)){

	if( t_10$t_sentiment[i]==1){
	t_10$t_sentiment[i]<-0
	}
	if( t_10$t_sentiment[i]==3){
	t_10$t_sentiment[i]<-1
	}

}

t_10<-t_10[t_10$t_sentiment!=2,]


############################## For Linear Regression(Failed) ##############3

# 9 Prediction 

h1<-lm(t_sentiment~.,data=t_2)

or

#h2<-glm(t_sentiment~.,data=t_1000_complete1,family=binomial)

summary(h)

# 9 Dividing dataset into Training Set and Test Set

# Training Set

t_3<-as.matrix(t_3)

batman1<-t_3[1:7000,]

batman_1<-as.data.frame(batman1)

# Test Set

robin1<-t_3[7001:10000,]

robin1<-as.data.frame(robin1)



##########################################################

New Code for 10,000 tweets batman1 and robin1


rbh1<-lm(t_sentiment~.,data=batman1)

t_pred<-predict(rbh1,robin1)





t_rpred<-round(t_pred)

xtab<-table(t_rpred,t_sent100)

install.packages("caret")
library("caret")

install.packages("e1071")
library("e1071")

confusionMatrix(xtab)

############################# Logistic Regression ############################

hypo1<-glm(batman2$t_sentiment~.,data=batman2,family=binomial)

# 9 Dividing dataset into Training Set and Test Set

# Training Set

t_10<-as.matrix(t_10)

batman2<-t_10[1:2334,]

batman2<-as.data.frame(batman2)

# Test Set

robin2<-t_10[2335:3335,]

robin2<-as.data.frame(robin2)

t_pred1<-predict(hypo1,robin2,type="response")





t_rpred1<-round(t_pred1)

xtab1<-table(t_rpred1,robin2$t_sentiment)
confusionMatrix(xtab1)


