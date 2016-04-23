require(data.table)

# Preprocess data
preprocess <- function(data){
  
  # remove non-ASCII characters
  data[, text := iconv(text, from = "UTF-8", to = "latin1")]
  
  # clean characters
  data[, text := cleanCharacters(text)]
  
  # extract the sentences 
  ifelse(nrow(data) > 10000, 
         sentences <- extractSentences(data), 
         sentences <- data[, splitBySentence(text)])
  
  # replace all digits with `###`
  sentences  <- gsub("\\d+.\\d+|\\d+|\\d+/\\d+|¼|½|¾", "###", sentences)
  
  # make all words lower case  
  sentences <- tolower(sentences)
  
  # restore contractions of verbs
  sentences <- restoreContractions(sentences)
  
  return(sentences)
}

# It is a wrapper for splitBySentence() to treat the input by chunks. 
extractSentences <- function(data){
  
  # set step and maximum number of chunks
  step <- 10000
  max_i <- floor(nrow(data)/step)
  
  # initilize the output
  s <- list()
  
  # treat the whole data in chunks via loop
  for(i in 0:max_i){
    
    r1 <- i*step+1
        
    ifelse(i == max_i, r2 <- nrow(data), r2 <- (i+1)*step)
    
    cat("Observation split:", r1, "-", r2 , "\n")
    
    s[[i+1]] <- data[r1:r2, splitBySentence(text)]
    }
  
  # output the vector with sentences
  unlist(s)
  
}

# Clean non-ASCII characters
# Binary to ASCII character table was taken from 
# http://www.binaryhexconverter.com/binary-ascii-characters-table
# cleanNonAscii <- function(x) {
#   
#   nonascii <- c("â€|\u009d|Â»|Â«|â€œ|Â|â€²", "â€™|â€˜", "â€“|â€”", "â€¦", "Ã´")
#   ascii <- c("", "'", " - ", "... ", "o")
#   
#   for (i in 1:length(nonascii)){
#     x <- gsub(nonascii[i], ascii[i], x)
#   }
#   return(x)
# }

# Restores contractions of verbs
restoreContractions  <- function(x){
  
  rules <- c("i'm" = "i am", "it's" = "it is", "he's" = "he is",
             "that's" = "that is", "here's" = "here is", 
             "they're" = "they are", "i've" = "i have",
             "i'd" = "i would", "you're" = "you are",
             "i'll" = "i will",
             "life's" = "life is", "school's" = "school is",
             "there's" = "there is", "aren't" = "are not",
             "can't" = "can not", "couldn't" = "could not", 
             "didn't" = "did not", "doesn't" = "does not",
             "don't" = "do not", "haven't" = "have not",
             "isn't" = "is not", "wasn't" = "was not",
             "weren't" = "were not", "won't" = "will not",
             "wouldn't" = "would not", "let's" = "let us")
  
  stringr::str_replace_all(x, rules)
}

# Clean characters
cleanCharacters <- function(x){
  
#    x <- gsub("\\s+\\/\\s+", " ", x)
   x <- gsub("&", "and", x)
   x <- gsub('\\"|\\*|#+|\\$|£|%|\\+|\\^', "", x)
   x <- gsub("à|á|å", "a", x)
   x <- gsub("è", "e", x)
   x <- gsub("~|¶|©|¬|¨", " ", x)
   
  
  return(x)
}

# Find the last word in phrase

lastWord <- function(txt){
  
  # split the phrase into words
  words <- splitByWord (txt)
  
  # return last word
  words [length (words)]
}

# Split text into words

splitByWord <- function(txt){
  
  words <- unlist(strsplit(txt, split = "\\s+"))
  words[nchar(words) > 0]
}

# Return all words in phrase except the last one

excludeLastWord <- function(txt){
  
  # split the phrase into words
  words <- splitByWord(txt)
  
  # exclude the last word 
  words <- words[1:length(words)-1]
  
  # paste and return the rest words together
  paste(words, collapse = " ")
}

# Splits text by sentences. The boundaries of sentence are determined by
# `Maxent_Sent_Token_Annotator()` from openNLP package.

splitBySentence <- function(txt){
    
  # set annatator
  annotator <- openNLP::Maxent_Sent_Token_Annotator()
  
  # convert input to class `String`
  txt  <- NLP::as.String(txt)
  
  # determine where the sentences are
  boundaries <- NLP::annotate(txt, annotator)
  
  # extract sentences
  txt  <- txt[boundaries]
  
  # annotate() added some Line Feed symbol `\n` 
  # remove them
  
  txt <- gsub("\\n", " ", txt)

  return(txt)
  
}


getNgrams <- function(data, N = 1:4){
    
  # create a tokenizer 
  ctrl <- RWeka::Weka_control(min = min(N), max = max(N))
  tok <- function(x) RWeka::NGramTokenizer(x, control = ctrl)
  
  # split sentences into n-grams
  ngrams <- data.table(phrase = unlist(lapply(data, tok)))
  
  ngrams
}

splitNgrams <- function(ngrams){
  
  # determine context, last word, and N for each ngram 
  ngrams[, context := excludeLastWord(phrase), by = phrase]
  ngrams[, word := lastWord(phrase), by = phrase]
  ngrams[, n := length(splitByWord(phrase)), by = phrase]
  
  # change order of columns just for the convinience
  setcolorder(ngrams, c("n", "phrase", "freq", "context", "word"))
  
  ngrams
}

# Calculates the maximum estimate liklihood (MLE) for the probability of ngrams.
# Fequency of a particular sequence (phrase) is divided by the frequency of a prefix (context).
# Column `prob` with MLE is aded to the data.table containing all contexts

calculateMLE <- function(ngrams){
  
  # count unique phrases (ngrams) and put frequencies in new coloumn
  ngrams <- ngrams[, freq := .N, by = phrase]
  
  # leave only unique phrases
  ngrams  <- unique(ngrams, by = "phrase")
  
  # split each phrase into context and last word, add order of ngram (n) 
  ngrams <- splitNgrams(ngrams)
  
  # count each context and set a seperate data set `context` with frequencies
  context <- ngrams[, sum(freq), by = context]
  names(context)[2] <- "context_freq"
  
  # set key for input by reference 
  setkeyv(context, "context")
  setkeyv(ngrams, "context")

  # calculate MLE 
  ngrams[context, prob := freq/context_freq]
  
} 


# Predicts the next word applying N-Gram Language Model

predict <- function(txt, model = ngrams){
  
  # ngram: N-Gram Language Model trained by `fit()` function
  # txt: input character string. It can be words, sentences.  
  
  # convert imput to data.table
  dt <- data.table(text = txt)
  
  # clean input and split by sentences
  txt <- preprocess(dt)
  
  # take the last sentence, if there were several
  txt <- tail(txt, 1)
    
  # get ngrams with order from 1 to N-1
  context <- getNgrams(txt, 1:3)
  
  # refine the context, leave only phrases ending by last word of the input
  query <- paste0("\\b", lastWord(txt), "$")
  context <- context[grepl(query, context$phrase)]
  
  # set key 
  setkeyv(context, "phrase")

  # merge context and ngrams and order the result by the order of ngrams and 
  # the value of probability
  match <- model[context, nomatch = 0][order(-n, -prob)]
  
  # remove dublicated words
  match <- match[!duplicated(match$word)]
  
  # return words and probablities
  match[,.(word, prob)]
    
}


# Define the location and length of vector with equial probabilities.
# Randomly selects the elements and returns the indexes of these elements.

sampleRunners <- function(vec, k) {
  i <- k + 1
  while(vec[i] == vec[k] & i <= length(vec)) i <- i + 1
  i <- i - 1 
  
  j <- k - 1
  while(vec[j] == vec[k] & j != 1) j <- j - 1
  
  if(vec[1] == vec[k]) j <- 1
  else j <- j + 1
  
  s <- sample(seq(j, i), k-j+1) 
  if (j != 1) return(c(1:(j-1), s)) 
  return(s)
}


# match: contains columnns `word` and `prob`

narrowOutput <- function(match, k = 5){
  # Select probabilities
  prob <- match[, prob]
  
  # Are there elements that are equal to k-th and higher it?
  if (length(prob) > k) {
    if (prob[k+1] == prob[k]) result <- match[sampleRunners(prob, k), .(word)]#apply random choice 
    else result  <- match[1:k, .(word)] 
  } else result <- match[, .(word)] 
  
  result$word
}


