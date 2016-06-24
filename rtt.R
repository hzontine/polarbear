
# Ok, here's what I'd do. First, assume docs is a vector of strings (one string per document). 
# I also assume the class labels are in the variable classes, one per document.

# First I'd remove all punctuation (DocumentTermMatrix will do that for you, but I don't like the way it does it).

red.docs <- c(
    "I love Donald Trump",
    "Donald Trump for President",
    "Oh I love that big bad guy Donald Trump",
    "Go Donald!",
    "Here's to the Donald!",
    "GOP -- Destination: White House",
    "I'm in a red state",
    "Conservatism all the way, bruh!",
    "I love Trump because he's conservative",
    "Build the wall!",
    "Benghazi Benghazi Benghazi",
    "Vote for Trump! We want the wall!")

blue.docs <- c(
    "Hillary for President",
    "Here's to another Clinton in the White House!",
    "Liberal 4ever",
    "Go Hillary!",
    "It's about time we had a woman in the White House!",
    "Trump is a crook!",
    "I love Hillary Clinton",
    "Hillary is my homey",
    "We want Clinton!",
    "Clinton for President",
    "C'mon Hillary, pick Liz Warren!",
    "I'm dodger blue in this election, baby")

unknown.docs <- c(
    "Donald Trump is da man!",
    "Hicks for Hillary!",
    "How about a woman for a change?",
    "elephant donkey"
)

docs <- c(red.docs, blue.docs, unknown.docs)

classes <- c(
    rep("red",length(red.docs)),
    rep("blue",length(blue.docs)),
    rep(NA,length(unknown.docs)))

    
docs <- gsub("[^a-z]"," ",tolower(docs))    # you can muck about with this, particularly if you want digits, or whatever.

# It is completely unnecessary, but I like to get rid of extra spaces:
docs <- gsub("\\s+"," ",docs)               # turn more than one space in a row into a single space
docs <- gsub("(^\\s)|(\\s$)","",docs)      # remove initial and trailing spaces


dtm <- create_matrix(docs)
training.size <- length(red.docs) + length(blue.docs)
container <- create_container(dtm, classes, trainSize=1:training.size,
    testSize=(training.size+1):length(docs), virgin=FALSE)

rf.model <- train_model(container,"MAXENT")
rf.classify <- classify_model(container, rf.model)

results <- cbind(rf.classify, doc=unknown.docs)
print(results)
