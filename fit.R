# fit() function loads raw data set, cleans, samples, and tokenizes data.  
# Then makes n-grams (n = 1, 2, 3, 4), taking into account smoothing methods. 
# The function returns the fitted language model as *.RData file
# for the future use with Shiny application. It is saved in `./app/`. 
# The function gets a file path to the raw data set as an argument. 

fit <- function(path = "./data/en_US.blogs.txt"){
  
  require(data.table)
  source("./app/functions.R")
  
  # load data set 
  data <- fread(path, header = FALSE, col.names = "text", encoding = "UTF-8", sep = "\n")
  
  # select randomly 10% of observations
  set.seed(1234)
  nrows <- nrow(data)
  index <- sample(nrows, ceiling(nrows*0.1))
  data <- data[index, ]
  
  # preprocess data
  data <- preprocess(data)
    
  # get ngrams (default N = 1:4) 
  ngrams <- getNgrams(data)
  
  # calculate MLE 
  ngrams <- calculateMLE(ngrams)
  
  #save the model as *.rds file into the folder with application
  saveRDS(ngrams[,.(n, context, word, prob)], "./app/ngrams.rds")

}

# train model
fit()

ngrams <- readRDS("./app/ngrams.rds")



