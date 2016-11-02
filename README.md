# text-mining
#install.packages("wordcloud")
#install.packages("RTextTools")
library(wordcloud)
#install.packages("e1071")
#install.packages("factoextra")
library(e1071)
library(xgboost)
library(Matrix)
library(ROCR)
library(RTextTools)
library(factoextra)
# Read in the data

tweetS = read.csv("C:/Users/aman/Desktop/tweets.csv", stringsAsFactors=FALSE)

str(tweetS)


# Create dependent variable ie no neggative tweets

tweetS$Negative = as.factor(tweetS$Avg <= -1)

table(tweetS$Negative)

# FALSE  TRUE 
# 999   182 


confusionMatrixXG


# Install new packages

#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)


# Create corpus
 
corpus = Corpus(VectorSource(tweetS$Tweet))

# Look at corpus
corpus

corpus[[1]]


# Convert to lower-case

corpus = tm_map(corpus, tolower)

corpus[[1]]

#  
corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation

corpus = tm_map(corpus, removePunctuation)


corpus[[1]]



# Remove stopwords and apple

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]

myStopword <- c('can', 'say',"one","way","use","also","howev","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                  "get","well","make","ask","come","end"
                  )
corpus<- tm_map(corpus, removeWords, myStopword)
# removing numbers

corpus = tm_map(corpus, removeNumbers) 

#removing white  space

corpus <- tm_map(corpus, stripWhitespace)

# Stem document 

corpus = tm_map(corpus, stemDocument)

writeLines(as.character(corpus[[30]]))

wordcloud(corpus, max.words = 200, random.order = FALSE)




# Create matrix

frequenci = DocumentTermMatrix(corpus)

frequenci

# DocumentTermMatrix (documents: 1181, terms: 3191)>>
#   Non-/sparse entries: 8431/3760140
# Sparsity           : 100%
# Maximal term length: 99
# Weighting          : term frequency (tf)

# Look at matrix 

inspect(frequenci[1000:1005,505:515])

colsm <- colSums(as.matrix(frequenci))

ord <- order(colsm,decreasing=TRUE)

#inspect the least freq occuring words
colsm[tail(ord)]

#inspect the most freq ocuring words
colsm[head(ord)]

# Check for sparsity(to see the words that appear more than frequencies)

findFreqTerms(frequenci, lowfreq=20)

# [1] "android"              "anyon"                "app"                  "appl"                
# [5] "back"                 "batteri"              "better"               "buy"                 
# [9] "cant"                 "dont"                 "fingerprint"          "freak"               
# [13] "googl"                "ipad"                 "iphon"                "iphonec"             
# [17] "ipod"                 "ipodplayerpromo"      "itun"                 "just"                
# [21] "lol"                  "look"                 "love"                 "make"                
# [25] "market"               "microsoft"            "new"                  "now"                 
# [29] "phone"                "pleas"                "promo"                "promoipodplayerpromo"
# [33] "realli"               "releas"               "samsung"              "store"               
# [37] "thank"                "think"                "time"                 "twitter"             
# [41] "updat"                "use"                  "via"                  "want"                
# [45] "work"   

#visualization of most frequency occuring words

w=data.frame(term=names(colsm),occurrences=colsm)
library(ggplot2)
q <- ggplot(subset(w, colsm>75), aes(term, occurrences))
q<- q + geom_bar(stat = "identity")
q

# Remove sparse terms (to keep words that appear more than 5 % of tweets)

sparse = removeSparseTerms(frequenci, 0.995)
sparse
# <<DocumentTermMatrix (documents: 1181, terms: 297)>>
#   Non-/sparse entries: 4224/346533
# Sparsity           : 99%
# Maximal term length: 20
# Weighting          : term frequency (tf)


# Convert to a data frame

twtsSparse = as.data.frame(as.matrix(sparse))





# Make all variable names R-friendly

colnames(twtsSparse) = make.names(colnames(twtsSparse))

########################
#Hierarchical clustering
########################




tweetshr <- scale(twtsSparse)
d <- dist(tweetshr, method = "euclidean") # distance matrix
fits <- hclust(d, method="ward.D2")

groups <- cutree(fits, k=2) # cut tree into 5 clusters

table(groups)
# groups
# 1    2 
# 1173    8 

#name of columns if we cut the hierarcy cluster in if we want just 1 column parts

colnames(tweetshr)[groups == 1]

# 1] "actual"               "add"                  "alreadi"              "alway"               
# [5] "amaz"                 "amazon"               "android"              "announc"             
# [9] "anyon"                "app"                  "appl"                 "appstor"             
# [13] "arent"                "ask"                  "avail"                "away"                
# [17] "awesom"               "back"                 "batteri"              "best"                
# [21] "better"               "big"                  "bit"                  "black"               
# [25] "blackberri"           "break."               "bring"                "burberri"            
# [29] "busi"                 "buy"                  "call"                 "cant"                
# [33] "carbon"               "card"                 "care"                 "case"                
# [37] "cdp"                  "chang"                "charg"                "charger"             
# [41] "cheap"                "china"                "color"                "colour"              
# [45] "come"                 "compani"              "condescens"           "condom"              
# [49] "copi"                 "crack"                "creat"                "custom"              
# [53] "darn"                 "data"                 "date"                 "day"                 
# [57] "dear"                 "design"               "develop"              "devic"               
# [61] "didnt"                "die"                  "differ"               "disappoint"          
# [65] "discontinu"           "divulg"               "doesnt"               "done"                
# [69] "dont"                 "download"             "drop"                 "email"               
# [73] "emiss"                "emoji"                "evenstarz"            "event"               
# [77] "ever"                 "everi"                "everyth"              "facebook"            
# [81] "fail"                 "featur"               "feel"                 "femal"               
# [85] "figur"                "final"                "finger"               "fingerprint"         
# [89] "fire"                 "first"                "fix"                  "follow"              
# [93] "freak"                "free"                 "fun"                  "generat"             
# [97] "genius"               "get"                  "give"                 "gold"                
# [101] "gonna"                "good"                 "googl"                "got"                 
# [105] "great"                "guess"                "guy"                  "happen"              
# [109] "happi"                "hate"                 "help"                 "hey"                 
# [113] "hope"                 "hour"                 "httpbitlyxcdk"        "ibrooklynb"          
# [117] "idea"                 "ill"                  "imessag"              "impress"             
# [121] "improv"               "innov"                "instead"              "internet"            
# [125] "ipad"                 "iphon"                "iphonec"              "iphoto"              
# [129] "ipod"                 "ipodplayerpromo"      "isnt"                 "itun"                
# [133] "ive"                  "job"                  "just"                 "keynot"              
# [137] "know"                 "last"                 "launch"               "let"                 
# [141] "life"                 "like"                 "line"                 "lmao"                
# [145] "lock"                 "lol"                  "look"                 "los"                 
# [149] "lost"                 "love"                 "mac"                  "macbook"             
# [153] "made"                 "make"                 "man"                  "mani"                
# [157] "market"               "mayb"                 "mean"                 "microsoft"           
# [161] "mishiza"              "miss"                 "mobil"                "money"               
# [165] "motorola"             "move"                 "music"                "natz"                
# [169] "need"                 "never"                "new"                  "news"                
# [173] "next."                "nfc"                  "nokia"                "noth"                
# [177] "now"                  "nsa"                  "nuevo"                "offer"               
# [181] "old"                  "page"                 "para"                 "peopl"               
# [185] "perfect"              "person"               "phone"                "photog"              
# [189] "photographi"          "pictur"               "plastic"              "play"                
# [193] "pleas"                "ppl"                  "preorder"             "price"               
# [197] "print"                "pro"                  "problem"              "product"             
# [201] "promo"                "promoipodplayerpromo" "put"                  "que"                 
# [205] "quiet"                "read"                 "realli"               "recommend"           
# [209] "refus"                "releas"               "right"                "samsung"             
# [213] "samsungsa"            "say"                  "scanner"              "screen"              
# [217] "secur"                "see"                  "seem"                 "sell"                
# [221] "send"                 "servic"               "shame"                "share"               
# [225] "short"                "show"                 "simpl"                "sinc"                
# [229] "siri"                 "smart"                "smartphon"            "someth"              
# [233] "soon"                 "stand"                "start"                "steve"               
# [237] "still"                "stop"                 "store"                "stuff"               
# [241] "stupid"               "suck"                 "support"              "sure"                
# [245] "switch"               "take"                 "talk"                 "team"                
# [249] "tech"                 "technolog"            "text"                 "thank"               
# [253] "that"                 "theyr"                "thing"                "think"               
# [257] "tho"                  "thought"              "time"                 "today"               
# [261] "togeth"               "touch"                "touchid"              "tri"                 
# [265] "true"                 "tsuyoponzu"           "turn"                 "twitter"             
# [269] "two"                  "updat"                "upgrad"               "use"                 
# [273] "user"                 "via"                  "video"                "wait"                
# [277] "want"                 "watch"                "week"                 "what"                
# [281] "white"                "windowsphon"          "wish"                 "without"             
# [285] "wonder"               "wont"                 "work"                 "world"               
# [289] "worst"                "wow"                  "wtf"                  "yall"                
# [293] "year"                 "yes"                  "yet"                  "yooo"  



# Add dependent variable

twtsSparse$Negative = tweetS$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(twtsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(twtsSparse, split==TRUE)
testSparse = subset(twtsSparse, split==FALSE)



# ########################
# DecisionTree Modeling
########################

library(rpart)
library(rpart.plot)

tweettree = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweettree)

# Evaluate the performance of the model
predicttree = predict(tweettree, newdata=testSparse, type="class")

table(testSparse$Negative, predicttree)

# predicttree
# FALSE TRUE
# FALSE   294    6
# TRUE     37   18

# Compute accuracy

(294+18)/(294+6+37+18)

########################
# Bseline Modeling
########################

# made on the basis of most of negatives are falsw so we can predict 
#baseline as false/false+true

table(testSparse$Negative)
# FALSE  TRUE 
# 300    55 

300/(300+55)


########################
# RandomForest Modeling
########################

library(randomForest)
set.seed(123)

tweetRForest = randomForest(Negative ~ ., data=trainSparse)

# Make predictions:
predictRForest = predict(tweetRForest, newdata=testSparse)

table(testSparse$Negative, predictRForest)

# predictRForest
# FALSE TRUE
# FALSE   289   11
# TRUE     31   24

# Accuracy:
(293+21)/(293+7+34+21)


########################
# NaiveBayes Modeling
########################

tweetnv=naiveBayes(Negative~.,data = trainSparse)

#make prediction

predictnv = predict(tweetnv, newdata=testSparse,type=c("class"))

table(testSparse$Negative, predictnv)

# predictnv
# FALSE TRUE
# FALSE     0  300
# TRUE      0   55

#Accuracy
55/(300+55)



########################
# SVM Model
########################

SVM_tweet = svm(Negative~.,data=trainSparse)

# Predicting the validation set based on the model

predict_svm<- predict(SVM_tweet,newdata=testSparse)

# Creating the Confustion Matrix(caret package) for evaluating the metrics
table(testSparse$Negative,predict_svm)

# predict_svm
# FALSE TRUE
# FALSE   300    0
# TRUE     48    7

# Accuracy:
(300+7)/(300+48+7)



########################
# XGBoost Modeling
########################

# creating the matrix for training the model 
trainxg <- xgb.DMatrix(Matrix(data.matrix(trainSparse[,!colnames(trainSparse) %in% c('Negative')])), label = as.numeric(trainSparse$Negative)-1)

#advanced data set preparation 
testxg <- xgb.DMatrix(Matrix(data.matrix(testSparse[,!colnames(testSparse) %in% c('Negative')]))) 

# setting the watch list with train and validation dMatrixes for the modeling
watchlst <- list(train = trainxg, test = testxg)

# use the xgBoost Modeling training method
xgmodel <- xgboost(data = trainxg, max.depth = 25, eta = 0.3, nround = 200, objective = "multi:softmax", num_class = 20, verbose = 1, watchlist = watchlst)

# predict
predictxg <- predict(xgmodel, newdata = data.matrix(testSparse[, !colnames(testSparse) %in% c('Negative')])) 

# see the results of the prediction.

table(testSparse$Negative,predictxg)

# predictxg
# 0   1
# FALSE 285  15
# TRUE   30  25

#accuracy of Xg Boost

(289+24)/(289+24+11+31)

#####################################################################
# Using the RTextTools package
# Use the create container concept for different modeling techniques 
#####################################################################


container = create_container(frequenci, as.numeric(as.factor(twtsSparse$Negative)),
                           trainSize=1:700, testSize=701:1181,virgin=FALSE)


models = train_models(container, algorithms=c("TREE"))
results = classify_models(container, models)
analytics = create_analytics(container, results)
summary(analytics)

# ENSEMBLE SUMMARY
# 
# n-ENSEMBLE COVERAGE n-ENSEMBLE RECALL
# n >= 1                   1              0.62
# 
# 
# ALGORITHM PERFORMANCE
# 
# TREE_PRECISION    TREE_RECALL    TREE_FSCORE 
# 0.310          0.500          0.385 

N=4
set.seed(2016)
cross_validate(container,N,"TREE")

# Fold 1 Out of Sample Accuracy = 0.875
# Fold 2 Out of Sample Accuracy = 0.8829787
# Fold 3 Out of Sample Accuracy = 0.8971061
# Fold 4 Out of Sample Accuracy = 0.87
# [[1]]
# [1] 0.8750000 0.8829787 0.8971061 0.8700000
# 
# $meanAccuracy
# [1] 0.8812712






