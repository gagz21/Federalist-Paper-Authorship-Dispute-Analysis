## !! https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
## Use different methods depending on the nature of your data

## The data corpus I am using can be found HERE
## NOTE that the disputed/unknown are files 1 - 11
## Hamiltons are 12 - 62, Jays are 63 - 67 (which is not enough)
## and Madisons are 68 - 82

## First, we must read in the documents and convert them to 
## a format that we can evaluate.

library(network)
library(wordcloud)
library(tm)
library(slam)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(stringr)
library(textmineR)
library(igraph)
library(lsa)

setwd("C:/Users/gagd2/Desktop/Syracuse/IST_707/Assignments/Assignment_4")
## Next, load in the documents (the corpus)
FedCorpus <- Corpus(DirSource("Corpus2"))
(FedCorpus)

## Here, I am keeping a list of the filenames so I can associate them with the DTM
FedFilesPath <- "C:/Users/gagd2/Desktop/Syracuse/IST_707/Assignments/Assignment_4/Corpus2"
FilesList <- list.files(FedFilesPath, pattern=NULL)
(FilesList)
##Next, there are several steps needed to prepare the texts
## You will need to remove punctuation, make everything lowercase
## normalize, remove common and useless words like "and", "the", "or"
## Useless words are called "Stop Words"
## Don't forget to remove numbers as well. 
## The tm_map function allows you to perform the same 
## transformations on all of your texts at once
CleanFedCorpus <- tm_map(FedCorpus, removePunctuation)
##Make everything lowercase
CleanFedCorpus <- tm_map(CleanFedCorpus, content_transformer(tolower))
## Remove all Stop Words
CleanFedCorpus <- tm_map(CleanFedCorpus, removeWords, stopwords("english"))
## Remove all numbers
CleanFedCorpus <- tm_map(CleanFedCorpus, removeNumbers)
## Remove whitespace
CleanFedCorpus <- tm_map(CleanFedCorpus, stripWhitespace)
## You can also remove words that you do not want
MyStopWords <- c("like", "very", "can", "I", "also", "lot", "publius", "federalist", "nbsp")
CleanFedCorpus <- tm_map(CleanFedCorpus, removeWords, MyStopWords)

inspect(CleanFedCorpus[1:5])

## Next, I will write all cleaned docs  - the entire cleaned and prepped corpus
## to a file - in case I want to use it for something else.

(Feddataframe <- data.frame(text=sapply(CleanFedCorpus, identity), stringsAsFactors=F))
write.csv(Feddataframe, "Corpus2FEDoutput.csv")

## ------------------------------------------------------------------
## Now, we are ready to move forward.....
##-------------------------------------------------------------------

## View corpus as a document matrix
## TMD stands for Term Document Matrix
(Fed_TermDM <- TermDocumentMatrix(CleanFedCorpus))
inspect(Fed_TermDM)

## Before we normalize, we can look at the overall frequencies of words 
## This will find words that occur more than 100 times in the entire corpus
findFreqTerms(Fed_TermDM, 100)
## Find assocations with a selected conf
findAssocs(Fed_TermDM, 'legislative', 0.60)

## VISUALIZE
FedCleanDF <- as.data.frame(inspect(Fed_TermDM))
(FedCleanDF[1:10])
CleanDFScale <- scale(FedCleanDF)
d <- dist(CleanDFScale,method="euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit)
## NOw I have a good matrix that allows me to see all the key words of interest 
## and their frequency in each document
## HOWEVER - I still need to normalize!
## Even though this example is very small and all docs in this example are about the
## same size, this will not always be the case. If a document has 10,000 words, it
## will easily have a greater frequency of words than a doc with 1000 words.

(Fed_Doc_TM <- DocumentTermMatrix(CleanFedCorpus))
inspect(Fed_Doc_TM)
View(as.matrix(Fed_Doc_TM[1:20, 1:20]))

## NOrmalize the Term Doc Matrix from above and then visualize it again
## Warning!! It is easy to mix up the DTM and the TDM- be carefull

NormalizedTermDM <- TermDocumentMatrix(CleanFedCorpus, control = list(weighting = weightTfIdf, stopwords = TRUE))
inspect(NormalizedTermDM)


NormalizedDocTermM <- DocumentTermMatrix(CleanFedCorpus, control = list(weighting = weightTfIdf, stopwords = TRUE))
inspect(NormalizedDocTermM)

## Visualize normalized DTM
## The dendrogram:
## Terms higher in the plot appear more frequently within the corpus
## Terms grouped near to each other are more frequently found together
CleanDF_N <- as.data.frame(inspect(NormalizedTermDM))

CleanDFScale_N <- scale(CleanDF_N )
norm_d <- dist(CleanDFScale_N,method="euclidean")
fit <- hclust(norm_d, method="ward.D2")
rect.hclust(fit, k = 4) # cut tree into 4 clusters 
plot(fit)

CleanDF_NDoc_Term <- as.data.frame(inspect(NormalizedDocTermM))
CleanDFScale_N_Doc_Term <- scale(CleanDF_NDoc_Term )
norm_d2 <- dist(CleanDFScale_N_Doc_Term,method="euclidean")
fit2 <- hclust(norm_d2, method="ward.D2")
rect.hclust(fit2, k = 4) # cut tree into 4 clusters 
plot(fit2)

## Wordclouds
inspect(Fed_TermDM)

m <- as.matrix(Fed_TermDM)   ## You can use this or the next for m
(m)
#m <- as.matrix(CleanDF_N)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
word.freq
wordcloud(words = names(word.freq), freq = word.freq*2, min.freq = 100,
          random.order = F)

## Use kmeans to cluster the documents

ClusterM <- t(m) # transpose the matrix to cluster documents 
(ClusterM)
set.seed(100) # set a fixed random seed
k <- 2 # number of clusters
kmeans_FED_Result <- kmeans(ClusterM, k)
#round(kmeansResult$centers, digits = 3) # cluster centers
kmeans_FED_Result
## See the clusters  - this shows the similar documents
## This does not always work well and can also depend on the
## starting centroids
(kmeans_FED_Result$cluster)

## Let's try to find similarity using cosine similarity
## Let's look at our matrix

m_norm_term_DM<-NormalizedTermDM
inspect(m_norm_term_DM)

FEDcosine_dist_mat <- 
  1 - crossprod_simple_triplet_matrix(m_norm_term_DM)/
  (sqrt(col_sums(m_norm_term_DM^2) %*% t(col_sums(m_norm_term_DM^2))))

(FEDcosine_dist_mat)

#
## Some people will use 1 - cosine sim to get the nearness - in that case - 1 is nearest.

(FEDcos_sim_matrix <- (1 - FEDcosine_dist_mat))
## Now, larger means closer or more similar

## Compare this to the lsa cosine sim method
LSA_Cos_Method <- lsa::cosine(as.matrix(m_norm_term_DM))
(LSA_Cos_Method)  ## This is the SAME as the FEDcos_sim_matrix above

##----------------------------------------------------------
## k means clustering for the normalized data
ClusterMN <- t(m_norm_term_DM) # transpose the matrix to cluster documents 
#set.seed(100) # set a fixed random seed
k <- 3 # number of clusters
kmeans_FED_Result2 <- kmeans(ClusterMN, k)
#round(kmeansResult$centers, digits = 3) # cluster centers

## See the clusters  - this shows the similar documents
## This does not always work well and can also depend on the
## starting centroids
(kmeans_FED_Result2$cluster)

########Network of the Data......................
## For each Document, which document is it MOST
## similar to?
ClosenessMatrix <- FEDcos_sim_matrix
ClosenessMatrix[ClosenessMatrix < 0.15] <- 0
ClosenessMatrix[ClosenessMatrix >= 0.15] <- 1
View(ClosenessMatrix)
as.network(ClosenessMatrix, directed=FALSE)
#coerce the matrix into network objects
g<-as.network.matrix(ClosenessMatrix,matrix.type="adjacency", directed=FALSE)
plot(g)

My_igraph <- graph_from_adjacency_matrix(ClosenessMatrix, mode ="undirected",diag = TRUE,
                            add.colnames = TRUE)

My_igraph <- simplify(My_igraph, remove.multiple = TRUE, remove.loops = TRUE)

plot(My_igraph, vertex.size=10)
tkplot(My_igraph, vertex.size=8, vertex.color="yellow")

## This is a small example of cosine similarity so you can see how it works
## I will comment it out...
######  m3 <- matrix(1:9, nrow = 3, ncol = 3)
######   (m3)
######   ((crossprod(m3))/(  sqrt(col_sums(m3^2) %*% t(col_sums(m3^2))   )))

#heatmap https://www.rdocumentation.org/packages/stats/versions/3.5.0/topics/heatmap
heatmap(FEDcos_sim_matrix) 

