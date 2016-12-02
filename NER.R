library(NLP)
library(openNLP)



bio<-tweet1000$V1[8001:10000]
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

t_no_of_persons5<-matrix(0,nrow=2000,ncol=1)


   pers<-function(i){
	print(i);
             bio_annotations1 <- annotate(bio1[i], pipeline);
             bio_doc <- AnnotatedPlainTextDocument(bio1[i], bio_annotations1);

             t_no_of_persons5[i]<-length(entities(bio_doc, kind = "person"));
	}

for(i in 1:2000) t_no_of_persons5[i] <- try(pers(i), FALSE)

#1.1: convert data into 1 and 0



for(i in 1:2000){

	if( grepl("Error",t_no_of_persons5[i])==TRUE){
	t_no_of_persons5[i]<-0
	}
	else if( grepl("Error",t_no_of_persons5[i])==FALSE){
	t_no_of_persons5[i]<-t_no_of_persons5[i]
	}

}

t_no_of_persons5<-t_no_of_persons5$V1

t_no_of_persons5<-as.numeric(t_no_of_persons5)



#2: No_of_locations

t_nol5<-matrix(0,nrow=2000,ncol=1)


 locs<-function(i){
	print(i);
             bio_annotations1 <- annotate(bio1[i], pipeline);
             bio_doc <- AnnotatedPlainTextDocument(bio1[i], bio_annotations1);

             t_nol5[i]<-length(entities(bio_doc, kind = "location"));
	}

for(i in 1:2000) t_nol5[i] <- try(locs(i), FALSE)


#2.1: convert data into 1 and 0


for(i in 1:2000){

	if( grepl("Error",t_nol5[i])==TRUE){
	t_nol5[i]<-0
	}
	else if( grepl("Error",t_nol5[i])==FALSE){
	t_nol5[i]<-t_nol5[i]
	}

}

t_nol5<-t_nol5$V1

t_nol5<-as.numeric(t_nol5)

#3: No_of_organizations

t_no_of_org5<-matrix(0,nrow=2000,ncol=1)


 orgs<-function(i){
	print(i);
             bio_annotations1 <- annotate(bio1[i], pipeline);
             bio_doc <- AnnotatedPlainTextDocument(bio1[i], bio_annotations1);

             t_no_of_org5[i]<-length(entities(bio_doc, kind = "organization"));
	}

for(i in 1:2000) t_no_of_org5[i] <- try(orgs(i), FALSE)


#3.1: convert data into 1 and 0



for(i in 1:2000){

	if( grepl("Error",t_no_of_org5[i])==TRUE){
	t_no_of_org5[i]<-0
	}
	else if( grepl("Error",t_no_of_org5[i])==FALSE){
	t_no_of_org5[i]<-t_no_of_org5[i]
	}

}

t_no_of_org5<-t_no_of_org5$V1

t_no_of_org5<-as.numeric(t_no_of_org5)

