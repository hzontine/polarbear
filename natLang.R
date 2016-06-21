# Ok, here's what I'd do. First, assume docs is a vector of strings (one string per document). 
# I also assume the class labels are in the variable classes, one per document.

# First I'd remove all punctuation (DocumentTermMatrix will do that for you, but I don't like the way it does it).

docs <- gsub("[^a-z]"," ",tolower(docs))    # you can muck about with this, particularly if you want digits, or whatever.

# It is completely unnecessary, but I like to get rid of extra spaces:
docs <- gsub("\\s+"," ",docs)               # turn more than one space in a row into a single space
docs <- gsub("(^\\s)|(\\s$)","",docs)      # remove initial and trailing spaces


library(tm)
dtm0 <- DocumentTermMatrix(Corpus(VectorSource(docs)),
   control=list(weighting=weightTfIdf,      # Elizabeth and I like TFIDF. You could use weightTf if you just want term frequency.
                removePunctuation=FALSE, # already done
                removeDigits=FALSE,          # already done
                stemming=FALSE,               # you can stem if you want, but I usually don't bother
                wordLengths=c(3,Inf)))          # keep all the words that have 3 or more characters. You can muck with this.

# You can also tell it to remove words that appear in fewer than N documents and stuff if you want. Look at ?termFreq
# and ?DocumentTermMatrix.

# You might want to add stopwords=TRUE to the above to remove stopwords (or stopwords=stops if you have a list of stopwords called stops).
# stopwords are words like "the", "a", "and" and such that are essentially content free (sometimes). 
# stopwords=TRUE doesn't quite work, since you have removed punctuation, and the default stopwords list in tm has things like "don't", so
# you can do something like:

stops <- stopwords()
# then muck with stops to make them work on your documents, such as removing punctuation and strsplit() to split the resulting "don t" into "don" and "t", etc.
# or you can make up your own list of stopwords (for example, if you are dealing with the abstracts of papers, and many of them start with "abstract:" you might add
# "abstract" to your stopwords list.
# This isn't terribly important if you use TFIDF and do the variable importance thing below.

# you might also muck about with:
dtm <- removeSparseTerms(dtm0,.999)    # adjust the .999 -- closer to 1 means more words are kept.
dim(dtm)
colnames(dtm)
# The above is optional, but if you do it, keep about 1000 ish words or so, whatever seems to make sense.

# If you didn't muck about with removing sparse terms, rename dtm0 to dtm so the below will work.

# Another thing you might want to do:

library(ranger)    # Alternatively, use randomForest

rf <- ranger(Class ~ .,
   data=cbind(as.data.frame(as.matrix(dtm)),Class=as.factor(classes)),
   importance='impurity')

# look at the most important words:
vi <- rf$variable.importance
plot(vi)     

# look for a cut-off and keep the words above the cut-off. This might be better than the sparseTerms thing, but will take infinite time
# if you have a lot of words and don't sparsify first, so there's a trade-off. ranger tends to be faster than randomForest since it uses
# multiple cores.

# You can the re-run ranger (random forest) on the reduced matrix, and/or your favorite classifier and/or
# you can do what is called Latent Semantic Indexing, LSI, which is a fancy name for: embed using singular value decomposition.


library(Matrix)
library(methods)
sdtm <-  sparseMatrix(i=dtm$i,j=dtm$j,x=dtm$v,dims=dim(dtm))

# The above is changing from simple triplet format to sparse matrix format that rARPACK eats.

library(rARPACK)
sd <-  svds(sdtm,k=100)     # k is the number of singular vectors kept.

# Now apply your favorite classifier to sd$u. Note that sd$v corresponds to the words, sd$u corresponds to the documents. It is fun to do:

plot(sd$u,pch=20)
points(sd$v,pch=20,col=2)

# and using identify() you can figure out which words are next to which documents, and (hopefully) it will be cool. Or not.

