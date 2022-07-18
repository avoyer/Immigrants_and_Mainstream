
## This do file includes 2 robustness tests: cosign similarity and topic models
setwd("C:/Users/zdk15001/Desktop/Academia/University_of_Connecticut/Emily_Post/posted/2022_7_11_repo/")
wd <- "C:/Users/zdk15001/Desktop/Academia/University_of_Connecticut/Emily_Post/posted/2022_7_11_repo/"



# cosign similarity
# w2vec context

library(devtools)
install_github("bmschmidt/wordVectors")
library('wordVectors')


####################
##Full Corpus  W2V##
####################

#create single, updated file of full corpus


baseFile <- "txt_full"
w2vInput <- paste("txt_full/",baseFile,".txt",sep = "")
w2vCleaned <- paste("txt_full/",baseFile,"_cleaned.txt",sep="")
w2vBin <- paste("txt_full/",baseFile,".bin",sep = "")
THREADS <- 3


# Prep the corpus
prep_word2vec(origin=w2vInput,destination=w2vCleaned,lowercase=T,bundle_ngrams=1)

# Train the model
if (!file.exists(w2vBin)) {
  w2vModel <- train_word2vec(
    w2vCleaned,
    output_file=w2vBin,
    vectors=100,
    threads=THREADS,
    window=12, iter=100, negative_samples=0
  )
} else {
  w2vModel <- read.vectors(w2vBin)
}

#Closest to
close_catholic <- w2vModel%>% closest_to('catholic', 100)
close_chinese <- w2vModel%>% closest_to('chinese', 100)
close_irish <- w2vModel%>% closest_to('irish', 100)
close_italian <- w2vModel%>% closest_to('italian', 100) 
close_jewish <- w2vModel%>% closest_to('jewish', 100) 
close_mexican <- w2vModel%>% closest_to('mexican', 100) 
close_muslim <- w2vModel%>% closest_to('muslim', 100) 


#rename
close_catholic   <- transmute(close_catholic,   word_catholic    = word)
close_chinese  <- transmute(close_chinese,  word_chinese   = word)
close_irish <- transmute(close_irish, word_irish  = word)
close_italian   <- transmute(close_italian,   word_italian    = word)
close_jewish    <- transmute(close_jewish,    word_jewish     = word)
close_mexican    <- transmute(close_mexican,    word_mexican     = word)
close_muslim    <- transmute(close_muslim,    word_muslim     = word)

# identifier

# function
add_row_numbers <- function(df, name = "row_number", zero_based = FALSE) {
  
  # Drop variable if exists
  if (name %in% names(df)) df[, name] <- NULL
  
  # Create sequence of integers
  sequence <- seq.int(1, to = nrow(df))
  
  if (zero_based) sequence <- sequence - 1L
  
  # Add sequence to data frame
  df[, name] <- sequence
  
  # Move variable to the first column position
  df <- select(df, !!name, everything())
  
  return(df)
  
}

#row number

close_catholic   <- add_row_numbers(close_catholic,   name = "rank", zero_based = FALSE)
close_chinese  <- add_row_numbers(close_chinese,  name = "rank", zero_based = FALSE)
close_irish <- add_row_numbers(close_irish, name = "rank", zero_based = FALSE)
close_italian   <- add_row_numbers(close_italian,   name = "rank", zero_based = FALSE)
close_jewish    <- add_row_numbers(close_jewish,    name = "rank", zero_based = FALSE)
close_mexican    <- add_row_numbers(close_mexican,    name = "rank", zero_based = FALSE)
close_muslim    <- add_row_numbers(close_muslim,    name = "rank", zero_based = FALSE)


# merge

close <- inner_join(close_catholic, close_chinese)
close <- inner_join(close          , close_irish)
close <- inner_join(close          , close_italian)
close <- inner_join(close          , close_jewish)
close <- inner_join(close          , close_mexican)
close <- inner_join(close          , close_muslim)

# write to table
write.table(close, "close_img.csv", row.names = FALSE)








### Revise and Resubmit 2


### concept specification: Most frequent words in each pseudocorpus



#create wordcount from pseudocorpera
wordcount_catholic <- tidy_EP_chunks_catholic  %>% count(word, sort = TRUE)
wordcount_chinese <- tidy_EP_chunks_chinese    %>% count(word, sort = TRUE)
wordcount_irish <- tidy_EP_chunks_irish  %>% count(word, sort = TRUE)
wordcount_italian <- tidy_EP_chunks_italian  %>% count(word, sort = TRUE)
wordcount_jewish <- tidy_EP_chunks_jewish   %>% count(word, sort = TRUE)
wordcount_mexican <- tidy_EP_chunks_mexican  %>% count(word, sort = TRUE)
wordcount_muslim <- tidy_EP_chunks_muslim  %>% count(word, sort = TRUE)



# add row "rank"
wordcount_catholic   <- add_row_numbers(wordcount_catholic,   name = "rank", zero_based = FALSE)
wordcount_chinese  <- add_row_numbers(wordcount_chinese,  name = "rank", zero_based = FALSE)
wordcount_irish <- add_row_numbers(wordcount_irish, name = "rank", zero_based = FALSE)
wordcount_italian   <- add_row_numbers(wordcount_italian,   name = "rank", zero_based = FALSE)
wordcount_jewish    <- add_row_numbers(wordcount_jewish,    name = "rank", zero_based = FALSE)
wordcount_mexican    <- add_row_numbers(wordcount_mexican,    name = "rank", zero_based = FALSE)
wordcount_muslim    <- add_row_numbers(wordcount_muslim,    name = "rank", zero_based = FALSE)

# rename 


wordcount_catholic   <- rename(wordcount_catholic, Catholic = word)
wordcount_chinese    <- rename(wordcount_chinese, Chinese = word)
wordcount_irish      <- rename(wordcount_irish, Irish = word)
wordcount_italian    <- rename(wordcount_italian, Italian = word)
wordcount_jewish     <- rename(wordcount_jewish, Jewish = word)
wordcount_mexican    <- rename(wordcount_mexican, Mexican = word)
wordcount_muslim     <- rename(wordcount_muslim, Muslim = word)



# drop count
wordcount_catholic   <- subset(wordcount_catholic, select = -n)
wordcount_chinese    <- subset(wordcount_chinese, select = -n)
wordcount_irish      <- subset(wordcount_irish, select = -n)
wordcount_italian    <- subset(wordcount_italian, select = -n)
wordcount_jewish     <- subset(wordcount_jewish, select = -n)
wordcount_mexican    <- subset(wordcount_mexican, select = -n)
wordcount_muslim     <- subset(wordcount_muslim, select = -n)



# combine
wordcount_combined <- inner_join(wordcount_catholic          , wordcount_chinese)
wordcount_combined <- inner_join(wordcount_combined          , wordcount_cuban)
wordcount_combined <- inner_join(wordcount_combined          , wordcount_irish)
wordcount_combined <- inner_join(wordcount_combined          , wordcount_italian)
wordcount_combined <- inner_join(wordcount_combined          , wordcount_jewish)
wordcount_combined <- inner_join(wordcount_combined          , wordcount_mexican)
wordcount_combined <- inner_join(wordcount_combined          , wordcount_muslim)

# write to table
write.table(wordcount_combined, "wordcount_combined.csv", row.names = FALSE)



### Repeat process for each group for each period




### italian
### p1: 1920-1945
tidy_EP_chunks_italian_p1 <- full_join(tidy_EP_italian_1922_select, tidy_EP_italian_1927_select)
tidy_EP_chunks_italian_p1 <- full_join(tidy_EP_chunks_italian_p1, tidy_EP_italian_1931_select)
tidy_EP_chunks_italian_p1 <- full_join(tidy_EP_chunks_italian_p1, tidy_EP_italian_1934_select)
tidy_EP_chunks_italian_p1 <- full_join(tidy_EP_chunks_italian_p1, tidy_EP_italian_1937_select)
tidy_EP_chunks_italian_p1 <- full_join(tidy_EP_chunks_italian_p1, tidy_EP_italian_1940_select)
tidy_EP_chunks_italian_p1 <- full_join(tidy_EP_chunks_italian_p1, tidy_EP_italian_1942_select)


### p2: 1945-1970


tidy_EP_chunks_italian_p2 <- full_join(tidy_EP_italian_1945_select, tidy_EP_italian_1950_select)
tidy_EP_chunks_italian_p2 <- full_join(tidy_EP_chunks_italian_p2, tidy_EP_italian_1956_select)
tidy_EP_chunks_italian_p2 <- full_join(tidy_EP_chunks_italian_p2, tidy_EP_italian_1960_select)
tidy_EP_chunks_italian_p2 <- full_join(tidy_EP_chunks_italian_p2, tidy_EP_italian_1965_select)
tidy_EP_chunks_italian_p2 <- full_join(tidy_EP_chunks_italian_p2, tidy_EP_italian_1969_select)


### p3: 1970-1995
tidy_EP_chunks_italian_p3 <- full_join(tidy_EP_italian_1975_select, tidy_EP_italian_1984_select)
tidy_EP_chunks_italian_p3 <- full_join(tidy_EP_chunks_italian_p3, tidy_EP_italian_1992_select)


### p4: 1995-2020
tidy_EP_chunks_italian_p4 <- full_join(tidy_EP_italian_1997_select, tidy_EP_italian_2004_select)
tidy_EP_chunks_italian_p4 <- full_join(tidy_EP_chunks_italian_p4, tidy_EP_italian_2011_select)
tidy_EP_chunks_italian_p4 <- full_join(tidy_EP_chunks_italian_p4, tidy_EP_italian_2017_select)





wordcount_italian_p1 <- tidy_EP_chunks_italian_p1  %>% count(word, sort = TRUE)
wordcount_italian_p2 <- tidy_EP_chunks_italian_p2  %>% count(word, sort = TRUE)
wordcount_italian_p3 <- tidy_EP_chunks_italian_p3  %>% count(word, sort = TRUE)
wordcount_italian_p4 <- tidy_EP_chunks_italian_p4  %>% count(word, sort = TRUE)


wordcount_italian_p1   <- add_row_numbers(wordcount_italian_p1,   name = "rank", zero_based = FALSE)
wordcount_italian_p2   <- add_row_numbers(wordcount_italian_p2,   name = "rank", zero_based = FALSE)
wordcount_italian_p3   <- add_row_numbers(wordcount_italian_p3,   name = "rank", zero_based = FALSE)
wordcount_italian_p4   <- add_row_numbers(wordcount_italian_p4,   name = "rank", zero_based = FALSE)


wordcount_italian_p1   <- rename(wordcount_italian_p1, italian_p1 = word)
wordcount_italian_p2   <- rename(wordcount_italian_p2, italian_p2 = word)
wordcount_italian_p3   <- rename(wordcount_italian_p3, italian_p3 = word)
wordcount_italian_p4   <- rename(wordcount_italian_p4, italian_p4 = word)


wordcount_italian_p1   <- subset(wordcount_italian_p1, select = -n)
wordcount_italian_p2   <- subset(wordcount_italian_p2, select = -n)
wordcount_italian_p3   <- subset(wordcount_italian_p3, select = -n)
wordcount_italian_p4   <- subset(wordcount_italian_p4, select = -n)


wordcount_italian_p1234 <- inner_join(wordcount_italian_p1           , wordcount_italian_p2)
wordcount_italian_p1234 <- inner_join(wordcount_italian_p1234        , wordcount_italian_p3)
wordcount_italian_p1234 <- inner_join(wordcount_italian_p1234        , wordcount_italian_p4)


write.table(wordcount_italian_p1234, "wordcount_italian_p1234.csv", row.names = FALSE)




### jewish
### p1: 1920-1945
tidy_EP_chunks_jewish_p1 <- full_join(tidy_EP_jewish_1922_select, tidy_EP_jewish_1927_select)
tidy_EP_chunks_jewish_p1 <- full_join(tidy_EP_chunks_jewish_p1, tidy_EP_jewish_1931_select)
tidy_EP_chunks_jewish_p1 <- full_join(tidy_EP_chunks_jewish_p1, tidy_EP_jewish_1934_select)
tidy_EP_chunks_jewish_p1 <- full_join(tidy_EP_chunks_jewish_p1, tidy_EP_jewish_1937_select)
tidy_EP_chunks_jewish_p1 <- full_join(tidy_EP_chunks_jewish_p1, tidy_EP_jewish_1940_select)
tidy_EP_chunks_jewish_p1 <- full_join(tidy_EP_chunks_jewish_p1, tidy_EP_jewish_1942_select)


### p2: 1945-1970


tidy_EP_chunks_jewish_p2 <- full_join(tidy_EP_jewish_1945_select, tidy_EP_jewish_1950_select)
tidy_EP_chunks_jewish_p2 <- full_join(tidy_EP_chunks_jewish_p2, tidy_EP_jewish_1956_select)
tidy_EP_chunks_jewish_p2 <- full_join(tidy_EP_chunks_jewish_p2, tidy_EP_jewish_1960_select)
tidy_EP_chunks_jewish_p2 <- full_join(tidy_EP_chunks_jewish_p2, tidy_EP_jewish_1965_select)
tidy_EP_chunks_jewish_p2 <- full_join(tidy_EP_chunks_jewish_p2, tidy_EP_jewish_1969_select)


### p3: 1970-1995
tidy_EP_chunks_jewish_p3 <- full_join(tidy_EP_jewish_1975_select, tidy_EP_jewish_1984_select)
tidy_EP_chunks_jewish_p3 <- full_join(tidy_EP_chunks_jewish_p3, tidy_EP_jewish_1992_select)


### p4: 1995-2020
tidy_EP_chunks_jewish_p4 <- full_join(tidy_EP_jewish_1997_select, tidy_EP_jewish_2004_select)
tidy_EP_chunks_jewish_p4 <- full_join(tidy_EP_chunks_jewish_p4, tidy_EP_jewish_2011_select)
tidy_EP_chunks_jewish_p4 <- full_join(tidy_EP_chunks_jewish_p4, tidy_EP_jewish_2017_select)





wordcount_jewish_p1 <- tidy_EP_chunks_jewish_p1  %>% count(word, sort = TRUE)
wordcount_jewish_p2 <- tidy_EP_chunks_jewish_p2  %>% count(word, sort = TRUE)
wordcount_jewish_p3 <- tidy_EP_chunks_jewish_p3  %>% count(word, sort = TRUE)
wordcount_jewish_p4 <- tidy_EP_chunks_jewish_p4  %>% count(word, sort = TRUE)


wordcount_jewish_p1   <- add_row_numbers(wordcount_jewish_p1,   name = "rank", zero_based = FALSE)
wordcount_jewish_p2   <- add_row_numbers(wordcount_jewish_p2,   name = "rank", zero_based = FALSE)
wordcount_jewish_p3   <- add_row_numbers(wordcount_jewish_p3,   name = "rank", zero_based = FALSE)
wordcount_jewish_p4   <- add_row_numbers(wordcount_jewish_p4,   name = "rank", zero_based = FALSE)


wordcount_jewish_p1   <- rename(wordcount_jewish_p1, jewish_p1 = word)
wordcount_jewish_p2   <- rename(wordcount_jewish_p2, jewish_p2 = word)
wordcount_jewish_p3   <- rename(wordcount_jewish_p3, jewish_p3 = word)
wordcount_jewish_p4   <- rename(wordcount_jewish_p4, jewish_p4 = word)


wordcount_jewish_p1   <- subset(wordcount_jewish_p1, select = -n)
wordcount_jewish_p2   <- subset(wordcount_jewish_p2, select = -n)
wordcount_jewish_p3   <- subset(wordcount_jewish_p3, select = -n)
wordcount_jewish_p4   <- subset(wordcount_jewish_p4, select = -n)


wordcount_jewish_p1234 <- inner_join(wordcount_jewish_p1           , wordcount_jewish_p2)
wordcount_jewish_p1234 <- inner_join(wordcount_jewish_p1234        , wordcount_jewish_p3)
wordcount_jewish_p1234 <- inner_join(wordcount_jewish_p1234        , wordcount_jewish_p4)


write.table(wordcount_jewish_p1234, "wordcount_jewish_p1234.csv", row.names = FALSE)




### catholic
### p1: 1920-1945
tidy_EP_chunks_catholic_p1 <- full_join(tidy_EP_catholic_1922_select, tidy_EP_catholic_1927_select)
tidy_EP_chunks_catholic_p1 <- full_join(tidy_EP_chunks_catholic_p1, tidy_EP_catholic_1931_select)
tidy_EP_chunks_catholic_p1 <- full_join(tidy_EP_chunks_catholic_p1, tidy_EP_catholic_1934_select)
tidy_EP_chunks_catholic_p1 <- full_join(tidy_EP_chunks_catholic_p1, tidy_EP_catholic_1937_select)
tidy_EP_chunks_catholic_p1 <- full_join(tidy_EP_chunks_catholic_p1, tidy_EP_catholic_1940_select)
tidy_EP_chunks_catholic_p1 <- full_join(tidy_EP_chunks_catholic_p1, tidy_EP_catholic_1942_select)


### p2: 1945-1970


tidy_EP_chunks_catholic_p2 <- full_join(tidy_EP_catholic_1945_select, tidy_EP_catholic_1950_select)
tidy_EP_chunks_catholic_p2 <- full_join(tidy_EP_chunks_catholic_p2, tidy_EP_catholic_1956_select)
tidy_EP_chunks_catholic_p2 <- full_join(tidy_EP_chunks_catholic_p2, tidy_EP_catholic_1960_select)
tidy_EP_chunks_catholic_p2 <- full_join(tidy_EP_chunks_catholic_p2, tidy_EP_catholic_1965_select)
tidy_EP_chunks_catholic_p2 <- full_join(tidy_EP_chunks_catholic_p2, tidy_EP_catholic_1969_select)


### p3: 1970-1995
tidy_EP_chunks_catholic_p3 <- full_join(tidy_EP_catholic_1975_select, tidy_EP_catholic_1984_select)
tidy_EP_chunks_catholic_p3 <- full_join(tidy_EP_chunks_catholic_p3, tidy_EP_catholic_1992_select)


### p4: 1995-2020
tidy_EP_chunks_catholic_p4 <- full_join(tidy_EP_catholic_1997_select, tidy_EP_catholic_2004_select)
tidy_EP_chunks_catholic_p4 <- full_join(tidy_EP_chunks_catholic_p4, tidy_EP_catholic_2011_select)
tidy_EP_chunks_catholic_p4 <- full_join(tidy_EP_chunks_catholic_p4, tidy_EP_catholic_2017_select)





wordcount_catholic_p1 <- tidy_EP_chunks_catholic_p1  %>% count(word, sort = TRUE)
wordcount_catholic_p2 <- tidy_EP_chunks_catholic_p2  %>% count(word, sort = TRUE)
wordcount_catholic_p3 <- tidy_EP_chunks_catholic_p3  %>% count(word, sort = TRUE)
wordcount_catholic_p4 <- tidy_EP_chunks_catholic_p4  %>% count(word, sort = TRUE)


wordcount_catholic_p1   <- add_row_numbers(wordcount_catholic_p1,   name = "rank", zero_based = FALSE)
wordcount_catholic_p2   <- add_row_numbers(wordcount_catholic_p2,   name = "rank", zero_based = FALSE)
wordcount_catholic_p3   <- add_row_numbers(wordcount_catholic_p3,   name = "rank", zero_based = FALSE)
wordcount_catholic_p4   <- add_row_numbers(wordcount_catholic_p4,   name = "rank", zero_based = FALSE)


wordcount_catholic_p1   <- rename(wordcount_catholic_p1, catholic_p1 = word)
wordcount_catholic_p2   <- rename(wordcount_catholic_p2, catholic_p2 = word)
wordcount_catholic_p3   <- rename(wordcount_catholic_p3, catholic_p3 = word)
wordcount_catholic_p4   <- rename(wordcount_catholic_p4, catholic_p4 = word)


wordcount_catholic_p1   <- subset(wordcount_catholic_p1, select = -n)
wordcount_catholic_p2   <- subset(wordcount_catholic_p2, select = -n)
wordcount_catholic_p3   <- subset(wordcount_catholic_p3, select = -n)
wordcount_catholic_p4   <- subset(wordcount_catholic_p4, select = -n)


wordcount_catholic_p1234 <- inner_join(wordcount_catholic_p1           , wordcount_catholic_p2)
wordcount_catholic_p1234 <- inner_join(wordcount_catholic_p1234        , wordcount_catholic_p3)
wordcount_catholic_p1234 <- inner_join(wordcount_catholic_p1234        , wordcount_catholic_p4)


write.table(wordcount_catholic_p1234, "wordcount_catholic_p1234.csv", row.names = FALSE)




### irish
### p1: 1920-1945
tidy_EP_chunks_irish_p1 <- full_join(tidy_EP_irish_1922_select, tidy_EP_irish_1927_select)
tidy_EP_chunks_irish_p1 <- full_join(tidy_EP_chunks_irish_p1, tidy_EP_irish_1931_select)
tidy_EP_chunks_irish_p1 <- full_join(tidy_EP_chunks_irish_p1, tidy_EP_irish_1934_select)
tidy_EP_chunks_irish_p1 <- full_join(tidy_EP_chunks_irish_p1, tidy_EP_irish_1937_select)
tidy_EP_chunks_irish_p1 <- full_join(tidy_EP_chunks_irish_p1, tidy_EP_irish_1940_select)
tidy_EP_chunks_irish_p1 <- full_join(tidy_EP_chunks_irish_p1, tidy_EP_irish_1942_select)


### p2: 1945-1970


tidy_EP_chunks_irish_p2 <- full_join(tidy_EP_irish_1945_select, tidy_EP_irish_1950_select)
tidy_EP_chunks_irish_p2 <- full_join(tidy_EP_chunks_irish_p2, tidy_EP_irish_1956_select)
tidy_EP_chunks_irish_p2 <- full_join(tidy_EP_chunks_irish_p2, tidy_EP_irish_1960_select)
tidy_EP_chunks_irish_p2 <- full_join(tidy_EP_chunks_irish_p2, tidy_EP_irish_1965_select)
tidy_EP_chunks_irish_p2 <- full_join(tidy_EP_chunks_irish_p2, tidy_EP_irish_1969_select)


### p3: 1970-1995
tidy_EP_chunks_irish_p3 <- full_join(tidy_EP_irish_1975_select, tidy_EP_irish_1984_select)
tidy_EP_chunks_irish_p3 <- full_join(tidy_EP_chunks_irish_p3, tidy_EP_irish_1992_select)


### p4: 1995-2020
tidy_EP_chunks_irish_p4 <- full_join(tidy_EP_irish_1997_select, tidy_EP_irish_2004_select)
tidy_EP_chunks_irish_p4 <- full_join(tidy_EP_chunks_irish_p4, tidy_EP_irish_2011_select)
tidy_EP_chunks_irish_p4 <- full_join(tidy_EP_chunks_irish_p4, tidy_EP_irish_2017_select)





wordcount_irish_p1 <- tidy_EP_chunks_irish_p1  %>% count(word, sort = TRUE)
wordcount_irish_p2 <- tidy_EP_chunks_irish_p2  %>% count(word, sort = TRUE)
wordcount_irish_p3 <- tidy_EP_chunks_irish_p3  %>% count(word, sort = TRUE)
wordcount_irish_p4 <- tidy_EP_chunks_irish_p4  %>% count(word, sort = TRUE)


wordcount_irish_p1   <- add_row_numbers(wordcount_irish_p1,   name = "rank", zero_based = FALSE)
wordcount_irish_p2   <- add_row_numbers(wordcount_irish_p2,   name = "rank", zero_based = FALSE)
wordcount_irish_p3   <- add_row_numbers(wordcount_irish_p3,   name = "rank", zero_based = FALSE)
wordcount_irish_p4   <- add_row_numbers(wordcount_irish_p4,   name = "rank", zero_based = FALSE)


wordcount_irish_p1   <- rename(wordcount_irish_p1, irish_p1 = word)
wordcount_irish_p2   <- rename(wordcount_irish_p2, irish_p2 = word)
wordcount_irish_p3   <- rename(wordcount_irish_p3, irish_p3 = word)
wordcount_irish_p4   <- rename(wordcount_irish_p4, irish_p4 = word)


wordcount_irish_p1   <- subset(wordcount_irish_p1, select = -n)
wordcount_irish_p2   <- subset(wordcount_irish_p2, select = -n)
wordcount_irish_p3   <- subset(wordcount_irish_p3, select = -n)
wordcount_irish_p4   <- subset(wordcount_irish_p4, select = -n)


wordcount_irish_p1234 <- inner_join(wordcount_irish_p1           , wordcount_irish_p2)
wordcount_irish_p1234 <- inner_join(wordcount_irish_p1234        , wordcount_irish_p3)
wordcount_irish_p1234 <- inner_join(wordcount_irish_p1234        , wordcount_irish_p4)


write.table(wordcount_irish_p1234, "wordcount_irish_p1234.csv", row.names = FALSE)






### cuban
### p1: 1920-1945
tidy_EP_chunks_cuban_p1 <- full_join(tidy_EP_cuban_1922_select, tidy_EP_cuban_1927_select)
tidy_EP_chunks_cuban_p1 <- full_join(tidy_EP_chunks_cuban_p1, tidy_EP_cuban_1931_select)
tidy_EP_chunks_cuban_p1 <- full_join(tidy_EP_chunks_cuban_p1, tidy_EP_cuban_1934_select)
tidy_EP_chunks_cuban_p1 <- full_join(tidy_EP_chunks_cuban_p1, tidy_EP_cuban_1937_select)
tidy_EP_chunks_cuban_p1 <- full_join(tidy_EP_chunks_cuban_p1, tidy_EP_cuban_1940_select)
tidy_EP_chunks_cuban_p1 <- full_join(tidy_EP_chunks_cuban_p1, tidy_EP_cuban_1942_select)


### p2: 1945-1970


tidy_EP_chunks_cuban_p2 <- full_join(tidy_EP_cuban_1945_select, tidy_EP_cuban_1950_select)
tidy_EP_chunks_cuban_p2 <- full_join(tidy_EP_chunks_cuban_p2, tidy_EP_cuban_1956_select)
tidy_EP_chunks_cuban_p2 <- full_join(tidy_EP_chunks_cuban_p2, tidy_EP_cuban_1960_select)
tidy_EP_chunks_cuban_p2 <- full_join(tidy_EP_chunks_cuban_p2, tidy_EP_cuban_1965_select)
tidy_EP_chunks_cuban_p2 <- full_join(tidy_EP_chunks_cuban_p2, tidy_EP_cuban_1969_select)


### p3: 1970-1995
tidy_EP_chunks_cuban_p3 <- full_join(tidy_EP_cuban_1975_select, tidy_EP_cuban_1984_select)
tidy_EP_chunks_cuban_p3 <- full_join(tidy_EP_chunks_cuban_p3, tidy_EP_cuban_1992_select)


### p4: 1995-2020
tidy_EP_chunks_cuban_p4 <- full_join(tidy_EP_cuban_1997_select, tidy_EP_cuban_2004_select)
tidy_EP_chunks_cuban_p4 <- full_join(tidy_EP_chunks_cuban_p4, tidy_EP_cuban_2011_select)
tidy_EP_chunks_cuban_p4 <- full_join(tidy_EP_chunks_cuban_p4, tidy_EP_cuban_2017_select)





wordcount_cuban_p1 <- tidy_EP_chunks_cuban_p1  %>% count(word, sort = TRUE)
wordcount_cuban_p2 <- tidy_EP_chunks_cuban_p2  %>% count(word, sort = TRUE)
wordcount_cuban_p3 <- tidy_EP_chunks_cuban_p3  %>% count(word, sort = TRUE)
wordcount_cuban_p4 <- tidy_EP_chunks_cuban_p4  %>% count(word, sort = TRUE)


wordcount_cuban_p1   <- add_row_numbers(wordcount_cuban_p1,   name = "rank", zero_based = FALSE)
wordcount_cuban_p2   <- add_row_numbers(wordcount_cuban_p2,   name = "rank", zero_based = FALSE)
wordcount_cuban_p3   <- add_row_numbers(wordcount_cuban_p3,   name = "rank", zero_based = FALSE)
wordcount_cuban_p4   <- add_row_numbers(wordcount_cuban_p4,   name = "rank", zero_based = FALSE)


wordcount_cuban_p1   <- rename(wordcount_cuban_p1, cuban_p1 = word)
wordcount_cuban_p2   <- rename(wordcount_cuban_p2, cuban_p2 = word)
wordcount_cuban_p3   <- rename(wordcount_cuban_p3, cuban_p3 = word)
wordcount_cuban_p4   <- rename(wordcount_cuban_p4, cuban_p4 = word)


wordcount_cuban_p1   <- subset(wordcount_cuban_p1, select = -n)
wordcount_cuban_p2   <- subset(wordcount_cuban_p2, select = -n)
wordcount_cuban_p3   <- subset(wordcount_cuban_p3, select = -n)
wordcount_cuban_p4   <- subset(wordcount_cuban_p4, select = -n)


wordcount_cuban_p1234 <- inner_join(wordcount_cuban_p1           , wordcount_cuban_p2)
wordcount_cuban_p1234 <- inner_join(wordcount_cuban_p1234        , wordcount_cuban_p3)
wordcount_cuban_p1234 <- inner_join(wordcount_cuban_p1234        , wordcount_cuban_p4)


write.table(wordcount_cuban_p1234, "wordcount_cuban_p1234.csv", row.names = FALSE)







### muslim
### p1: 1920-1945
tidy_EP_chunks_muslim_p1 <- full_join(tidy_EP_muslim_1922_select, tidy_EP_muslim_1927_select)
tidy_EP_chunks_muslim_p1 <- full_join(tidy_EP_chunks_muslim_p1, tidy_EP_muslim_1931_select)
tidy_EP_chunks_muslim_p1 <- full_join(tidy_EP_chunks_muslim_p1, tidy_EP_muslim_1934_select)
tidy_EP_chunks_muslim_p1 <- full_join(tidy_EP_chunks_muslim_p1, tidy_EP_muslim_1937_select)
tidy_EP_chunks_muslim_p1 <- full_join(tidy_EP_chunks_muslim_p1, tidy_EP_muslim_1940_select)
tidy_EP_chunks_muslim_p1 <- full_join(tidy_EP_chunks_muslim_p1, tidy_EP_muslim_1942_select)


### p2: 1945-1970


tidy_EP_chunks_muslim_p2 <- full_join(tidy_EP_muslim_1945_select, tidy_EP_muslim_1950_select)
tidy_EP_chunks_muslim_p2 <- full_join(tidy_EP_chunks_muslim_p2, tidy_EP_muslim_1956_select)
tidy_EP_chunks_muslim_p2 <- full_join(tidy_EP_chunks_muslim_p2, tidy_EP_muslim_1960_select)
tidy_EP_chunks_muslim_p2 <- full_join(tidy_EP_chunks_muslim_p2, tidy_EP_muslim_1965_select)
tidy_EP_chunks_muslim_p2 <- full_join(tidy_EP_chunks_muslim_p2, tidy_EP_muslim_1969_select)


### p3: 1970-1995
tidy_EP_chunks_muslim_p3 <- full_join(tidy_EP_muslim_1975_select, tidy_EP_muslim_1984_select)
tidy_EP_chunks_muslim_p3 <- full_join(tidy_EP_chunks_muslim_p3, tidy_EP_muslim_1992_select)


### p4: 1995-2020
tidy_EP_chunks_muslim_p4 <- full_join(tidy_EP_muslim_1997_select, tidy_EP_muslim_2004_select)
tidy_EP_chunks_muslim_p4 <- full_join(tidy_EP_chunks_muslim_p4, tidy_EP_muslim_2011_select)
tidy_EP_chunks_muslim_p4 <- full_join(tidy_EP_chunks_muslim_p4, tidy_EP_muslim_2017_select)





wordcount_muslim_p1 <- tidy_EP_chunks_muslim_p1  %>% count(word, sort = TRUE)
wordcount_muslim_p2 <- tidy_EP_chunks_muslim_p2  %>% count(word, sort = TRUE)
wordcount_muslim_p3 <- tidy_EP_chunks_muslim_p3  %>% count(word, sort = TRUE)
wordcount_muslim_p4 <- tidy_EP_chunks_muslim_p4  %>% count(word, sort = TRUE)


wordcount_muslim_p1   <- add_row_numbers(wordcount_muslim_p1,   name = "rank", zero_based = FALSE)
wordcount_muslim_p2   <- add_row_numbers(wordcount_muslim_p2,   name = "rank", zero_based = FALSE)
wordcount_muslim_p3   <- add_row_numbers(wordcount_muslim_p3,   name = "rank", zero_based = FALSE)
wordcount_muslim_p4   <- add_row_numbers(wordcount_muslim_p4,   name = "rank", zero_based = FALSE)


wordcount_muslim_p1   <- rename(wordcount_muslim_p1, muslim_p1 = word)
wordcount_muslim_p2   <- rename(wordcount_muslim_p2, muslim_p2 = word)
wordcount_muslim_p3   <- rename(wordcount_muslim_p3, muslim_p3 = word)
wordcount_muslim_p4   <- rename(wordcount_muslim_p4, muslim_p4 = word)


wordcount_muslim_p1   <- subset(wordcount_muslim_p1, select = -n)
wordcount_muslim_p2   <- subset(wordcount_muslim_p2, select = -n)
wordcount_muslim_p3   <- subset(wordcount_muslim_p3, select = -n)
wordcount_muslim_p4   <- subset(wordcount_muslim_p4, select = -n)


wordcount_muslim_p1234 <- inner_join(wordcount_muslim_p1           , wordcount_muslim_p2)
wordcount_muslim_p1234 <- inner_join(wordcount_muslim_p1234        , wordcount_muslim_p3)
wordcount_muslim_p1234 <- inner_join(wordcount_muslim_p1234        , wordcount_muslim_p4)


write.table(wordcount_muslim_p1234, "wordcount_muslim_p1234.csv", row.names = FALSE)






### chinese
### p1: 1920-1945
tidy_EP_chunks_chinese_p1 <- full_join(tidy_EP_chinese_1922_select, tidy_EP_chinese_1927_select)
tidy_EP_chunks_chinese_p1 <- full_join(tidy_EP_chunks_chinese_p1, tidy_EP_chinese_1931_select)
tidy_EP_chunks_chinese_p1 <- full_join(tidy_EP_chunks_chinese_p1, tidy_EP_chinese_1934_select)
tidy_EP_chunks_chinese_p1 <- full_join(tidy_EP_chunks_chinese_p1, tidy_EP_chinese_1937_select)
tidy_EP_chunks_chinese_p1 <- full_join(tidy_EP_chunks_chinese_p1, tidy_EP_chinese_1940_select)
tidy_EP_chunks_chinese_p1 <- full_join(tidy_EP_chunks_chinese_p1, tidy_EP_chinese_1942_select)


### p2: 1945-1970


tidy_EP_chunks_chinese_p2 <- full_join(tidy_EP_chinese_1945_select, tidy_EP_chinese_1950_select)
tidy_EP_chunks_chinese_p2 <- full_join(tidy_EP_chunks_chinese_p2, tidy_EP_chinese_1956_select)
tidy_EP_chunks_chinese_p2 <- full_join(tidy_EP_chunks_chinese_p2, tidy_EP_chinese_1960_select)
tidy_EP_chunks_chinese_p2 <- full_join(tidy_EP_chunks_chinese_p2, tidy_EP_chinese_1965_select)
tidy_EP_chunks_chinese_p2 <- full_join(tidy_EP_chunks_chinese_p2, tidy_EP_chinese_1969_select)


### p3: 1970-1995
tidy_EP_chunks_chinese_p3 <- full_join(tidy_EP_chinese_1975_select, tidy_EP_chinese_1984_select)
tidy_EP_chunks_chinese_p3 <- full_join(tidy_EP_chunks_chinese_p3, tidy_EP_chinese_1992_select)


### p4: 1995-2020
tidy_EP_chunks_chinese_p4 <- full_join(tidy_EP_chinese_1997_select, tidy_EP_chinese_2004_select)
tidy_EP_chunks_chinese_p4 <- full_join(tidy_EP_chunks_chinese_p4, tidy_EP_chinese_2011_select)
tidy_EP_chunks_chinese_p4 <- full_join(tidy_EP_chunks_chinese_p4, tidy_EP_chinese_2017_select)





wordcount_chinese_p1 <- tidy_EP_chunks_chinese_p1  %>% count(word, sort = TRUE)
wordcount_chinese_p2 <- tidy_EP_chunks_chinese_p2  %>% count(word, sort = TRUE)
wordcount_chinese_p3 <- tidy_EP_chunks_chinese_p3  %>% count(word, sort = TRUE)
wordcount_chinese_p4 <- tidy_EP_chunks_chinese_p4  %>% count(word, sort = TRUE)


wordcount_chinese_p1   <- add_row_numbers(wordcount_chinese_p1,   name = "rank", zero_based = FALSE)
wordcount_chinese_p2   <- add_row_numbers(wordcount_chinese_p2,   name = "rank", zero_based = FALSE)
wordcount_chinese_p3   <- add_row_numbers(wordcount_chinese_p3,   name = "rank", zero_based = FALSE)
wordcount_chinese_p4   <- add_row_numbers(wordcount_chinese_p4,   name = "rank", zero_based = FALSE)


wordcount_chinese_p1   <- rename(wordcount_chinese_p1, chinese_p1 = word)
wordcount_chinese_p2   <- rename(wordcount_chinese_p2, chinese_p2 = word)
wordcount_chinese_p3   <- rename(wordcount_chinese_p3, chinese_p3 = word)
wordcount_chinese_p4   <- rename(wordcount_chinese_p4, chinese_p4 = word)


wordcount_chinese_p1   <- subset(wordcount_chinese_p1, select = -n)
wordcount_chinese_p2   <- subset(wordcount_chinese_p2, select = -n)
wordcount_chinese_p3   <- subset(wordcount_chinese_p3, select = -n)
wordcount_chinese_p4   <- subset(wordcount_chinese_p4, select = -n)


wordcount_chinese_p1234 <- inner_join(wordcount_chinese_p1           , wordcount_chinese_p2)
wordcount_chinese_p1234 <- inner_join(wordcount_chinese_p1234        , wordcount_chinese_p3)
wordcount_chinese_p1234 <- inner_join(wordcount_chinese_p1234        , wordcount_chinese_p4)


write.table(wordcount_chinese_p1234, "wordcount_chinese_p1234.csv", row.names = FALSE)






### mexican
### p1: 1920-1945
tidy_EP_chunks_mexican_p1 <- full_join(tidy_EP_mexican_1922_select, tidy_EP_mexican_1927_select)
tidy_EP_chunks_mexican_p1 <- full_join(tidy_EP_chunks_mexican_p1, tidy_EP_mexican_1931_select)
tidy_EP_chunks_mexican_p1 <- full_join(tidy_EP_chunks_mexican_p1, tidy_EP_mexican_1934_select)
tidy_EP_chunks_mexican_p1 <- full_join(tidy_EP_chunks_mexican_p1, tidy_EP_mexican_1937_select)
tidy_EP_chunks_mexican_p1 <- full_join(tidy_EP_chunks_mexican_p1, tidy_EP_mexican_1940_select)
tidy_EP_chunks_mexican_p1 <- full_join(tidy_EP_chunks_mexican_p1, tidy_EP_mexican_1942_select)


### p2: 1945-1970


tidy_EP_chunks_mexican_p2 <- full_join(tidy_EP_mexican_1945_select, tidy_EP_mexican_1950_select)
tidy_EP_chunks_mexican_p2 <- full_join(tidy_EP_chunks_mexican_p2, tidy_EP_mexican_1956_select)
tidy_EP_chunks_mexican_p2 <- full_join(tidy_EP_chunks_mexican_p2, tidy_EP_mexican_1960_select)
tidy_EP_chunks_mexican_p2 <- full_join(tidy_EP_chunks_mexican_p2, tidy_EP_mexican_1965_select)
tidy_EP_chunks_mexican_p2 <- full_join(tidy_EP_chunks_mexican_p2, tidy_EP_mexican_1969_select)


### p3: 1970-1995
tidy_EP_chunks_mexican_p3 <- full_join(tidy_EP_mexican_1975_select, tidy_EP_mexican_1984_select)
tidy_EP_chunks_mexican_p3 <- full_join(tidy_EP_chunks_mexican_p3, tidy_EP_mexican_1992_select)


### p4: 1995-2020
tidy_EP_chunks_mexican_p4 <- full_join(tidy_EP_mexican_1997_select, tidy_EP_mexican_2004_select)
tidy_EP_chunks_mexican_p4 <- full_join(tidy_EP_chunks_mexican_p4, tidy_EP_mexican_2011_select)
tidy_EP_chunks_mexican_p4 <- full_join(tidy_EP_chunks_mexican_p4, tidy_EP_mexican_2017_select)





wordcount_mexican_p1 <- tidy_EP_chunks_mexican_p1  %>% count(word, sort = TRUE)
wordcount_mexican_p2 <- tidy_EP_chunks_mexican_p2  %>% count(word, sort = TRUE)
wordcount_mexican_p3 <- tidy_EP_chunks_mexican_p3  %>% count(word, sort = TRUE)
wordcount_mexican_p4 <- tidy_EP_chunks_mexican_p4  %>% count(word, sort = TRUE)


wordcount_mexican_p1   <- add_row_numbers(wordcount_mexican_p1,   name = "rank", zero_based = FALSE)
wordcount_mexican_p2   <- add_row_numbers(wordcount_mexican_p2,   name = "rank", zero_based = FALSE)
wordcount_mexican_p3   <- add_row_numbers(wordcount_mexican_p3,   name = "rank", zero_based = FALSE)
wordcount_mexican_p4   <- add_row_numbers(wordcount_mexican_p4,   name = "rank", zero_based = FALSE)


wordcount_mexican_p1   <- rename(wordcount_mexican_p1, mexican_p1 = word)
wordcount_mexican_p2   <- rename(wordcount_mexican_p2, mexican_p2 = word)
wordcount_mexican_p3   <- rename(wordcount_mexican_p3, mexican_p3 = word)
wordcount_mexican_p4   <- rename(wordcount_mexican_p4, mexican_p4 = word)


wordcount_mexican_p1   <- subset(wordcount_mexican_p1, select = -n)
wordcount_mexican_p2   <- subset(wordcount_mexican_p2, select = -n)
wordcount_mexican_p3   <- subset(wordcount_mexican_p3, select = -n)
wordcount_mexican_p4   <- subset(wordcount_mexican_p4, select = -n)


wordcount_mexican_p1234 <- inner_join(wordcount_mexican_p1           , wordcount_mexican_p2)
wordcount_mexican_p1234 <- inner_join(wordcount_mexican_p1234        , wordcount_mexican_p3)
wordcount_mexican_p1234 <- inner_join(wordcount_mexican_p1234        , wordcount_mexican_p4)


write.table(wordcount_mexican_p1234, "wordcount_mexican_p1234.csv", row.names = FALSE)





















############ Count of how many times each chunk is selected for a group by year


# 1922
chunk_1922 <- c(italian_chunk_1922, jewish_chunk_1922, 
                catholic_chunk_1922, irish_chunk_1922, 
                cuban_chunk_1922, muslim_chunk_1922, 
                chinese_chunk_1922, mexican_chunk_1922)

chunk_1922_count <- as.data.frame(table(chunk_1922))
chunk_1922_count   <- subset(chunk_1922_count, select = Freq)
chunk_1922_count <- mutate(chunk_1922_count, year = 1922)


#1927
chunk_1927 <- c(italian_chunk_1927, jewish_chunk_1927, 
                catholic_chunk_1927, irish_chunk_1927, 
                cuban_chunk_1927, muslim_chunk_1927, 
                chinese_chunk_1927, mexican_chunk_1927)

chunk_1927_count <- as.data.frame(table(chunk_1927))
chunk_1927_count   <- subset(chunk_1927_count, select = Freq)
chunk_1927_count <- mutate(chunk_1927_count, year = 1927)

#1931
chunk_1931 <- c(italian_chunk_1931, jewish_chunk_1931, 
                catholic_chunk_1931, irish_chunk_1931, 
                cuban_chunk_1931, muslim_chunk_1931, 
                chinese_chunk_1931, mexican_chunk_1931)

chunk_1931_count <- as.data.frame(table(chunk_1931))
chunk_1931_count   <- subset(chunk_1931_count, select = Freq)
chunk_1931_count <- mutate(chunk_1931_count, year = 1931)


#1934
chunk_1934 <- c(italian_chunk_1934, jewish_chunk_1934, 
                catholic_chunk_1934, irish_chunk_1934, 
                cuban_chunk_1934, muslim_chunk_1934, 
                chinese_chunk_1934, mexican_chunk_1934)

chunk_1934_count <- as.data.frame(table(chunk_1934))
chunk_1934_count   <- subset(chunk_1934_count, select = Freq)
chunk_1934_count <- mutate(chunk_1934_count, year = 1934)

#1937
chunk_1937 <- c(italian_chunk_1937, jewish_chunk_1937, 
                catholic_chunk_1937, irish_chunk_1937, 
                cuban_chunk_1937, muslim_chunk_1937, 
                chinese_chunk_1937, mexican_chunk_1937)

chunk_1937_count <- as.data.frame(table(chunk_1937))
chunk_1937_count   <- subset(chunk_1937_count, select = Freq)
chunk_1937_count <- mutate(chunk_1937_count, year = 1937)


#1940
chunk_1940 <- c(italian_chunk_1940, jewish_chunk_1940, 
                catholic_chunk_1940, irish_chunk_1940, 
                cuban_chunk_1940, muslim_chunk_1940, 
                chinese_chunk_1940, mexican_chunk_1940)

chunk_1940_count <- as.data.frame(table(chunk_1940))
chunk_1940_count   <- subset(chunk_1940_count, select = Freq)
chunk_1940_count <- mutate(chunk_1940_count, year = 1940)



#1942
chunk_1942 <- c(italian_chunk_1942, jewish_chunk_1942, 
                catholic_chunk_1942, irish_chunk_1942, 
                cuban_chunk_1942, muslim_chunk_1942, 
                chinese_chunk_1942, mexican_chunk_1942)

chunk_1942_count <- as.data.frame(table(chunk_1942))
chunk_1942_count   <- subset(chunk_1942_count, select = Freq)
chunk_1942_count <- mutate(chunk_1942_count, year = 1942)





#1945
chunk_1945 <- c(italian_chunk_1945, jewish_chunk_1945, 
                catholic_chunk_1945, irish_chunk_1945, 
                cuban_chunk_1945, muslim_chunk_1945, 
                chinese_chunk_1945, mexican_chunk_1945)

chunk_1945_count <- as.data.frame(table(chunk_1945))
chunk_1945_count   <- subset(chunk_1945_count, select = Freq)
chunk_1945_count <- mutate(chunk_1945_count, year = 1945)




#1950
chunk_1950 <- c(italian_chunk_1950, jewish_chunk_1950, 
                catholic_chunk_1950, irish_chunk_1950, 
                cuban_chunk_1950, muslim_chunk_1950, 
                chinese_chunk_1950, mexican_chunk_1950)

chunk_1950_count <- as.data.frame(table(chunk_1950))
chunk_1950_count   <- subset(chunk_1950_count, select = Freq)
chunk_1950_count <- mutate(chunk_1950_count, year = 1950)




#1956
chunk_1956 <- c(italian_chunk_1956, jewish_chunk_1956, 
                catholic_chunk_1956, irish_chunk_1956, 
                cuban_chunk_1956, muslim_chunk_1956, 
                chinese_chunk_1956, mexican_chunk_1956)

chunk_1956_count <- as.data.frame(table(chunk_1956))
chunk_1956_count   <- subset(chunk_1956_count, select = Freq)
chunk_1956_count <- mutate(chunk_1956_count, year = 1956)

#1960
chunk_1960 <- c(italian_chunk_1960, jewish_chunk_1960, 
                catholic_chunk_1960, irish_chunk_1960, 
                cuban_chunk_1960, muslim_chunk_1960, 
                chinese_chunk_1960, mexican_chunk_1960)

chunk_1960_count <- as.data.frame(table(chunk_1960))
chunk_1960_count   <- subset(chunk_1960_count, select = Freq)
chunk_1960_count <- mutate(chunk_1960_count, year = 1960)


#1965
chunk_1965 <- c(italian_chunk_1965, jewish_chunk_1965, 
                catholic_chunk_1965, irish_chunk_1965, 
                cuban_chunk_1965, muslim_chunk_1965, 
                chinese_chunk_1965, mexican_chunk_1965)

chunk_1965_count <- as.data.frame(table(chunk_1965))
chunk_1965_count   <- subset(chunk_1965_count, select = Freq)
chunk_1965_count <- mutate(chunk_1965_count, year = 1965)




#1969
chunk_1969 <- c(italian_chunk_1969, jewish_chunk_1969, 
                catholic_chunk_1969, irish_chunk_1969, 
                cuban_chunk_1969, muslim_chunk_1969, 
                chinese_chunk_1969, mexican_chunk_1969)

chunk_1969_count <- as.data.frame(table(chunk_1969))
chunk_1969_count   <- subset(chunk_1969_count, select = Freq)
chunk_1969_count <- mutate(chunk_1969_count, year = 1969)



#1975
chunk_1975 <- c(italian_chunk_1975, jewish_chunk_1975, 
                catholic_chunk_1975, irish_chunk_1975, 
                cuban_chunk_1975, muslim_chunk_1975, 
                chinese_chunk_1975, mexican_chunk_1975)

chunk_1975_count <- as.data.frame(table(chunk_1975))
chunk_1975_count   <- subset(chunk_1975_count, select = Freq)
chunk_1975_count <- mutate(chunk_1975_count, year = 1975)

#1984
chunk_1984 <- c(italian_chunk_1984, jewish_chunk_1984, 
                catholic_chunk_1984, irish_chunk_1984, 
                cuban_chunk_1984, muslim_chunk_1984, 
                chinese_chunk_1984, mexican_chunk_1984)

chunk_1984_count <- as.data.frame(table(chunk_1984))
chunk_1984_count   <- subset(chunk_1984_count, select = Freq)
chunk_1984_count <- mutate(chunk_1984_count, year = 1984)


#1992
chunk_1992 <- c(italian_chunk_1992, jewish_chunk_1992, 
                catholic_chunk_1992, irish_chunk_1992, 
                cuban_chunk_1992, muslim_chunk_1992, 
                chinese_chunk_1992, mexican_chunk_1992)

chunk_1992_count <- as.data.frame(table(chunk_1992))
chunk_1992_count   <- subset(chunk_1992_count, select = Freq)
chunk_1992_count <- mutate(chunk_1992_count, year = 1992)



#1997
chunk_1997 <- c(italian_chunk_1997, jewish_chunk_1997, 
                catholic_chunk_1997, irish_chunk_1997, 
                cuban_chunk_1997, muslim_chunk_1997, 
                chinese_chunk_1997, mexican_chunk_1997)

chunk_1997_count <- as.data.frame(table(chunk_1997))
chunk_1997_count   <- subset(chunk_1997_count, select = Freq)
chunk_1997_count <- mutate(chunk_1997_count, year = 1997)




#2004
chunk_2004 <- c(italian_chunk_2004, jewish_chunk_2004, 
                catholic_chunk_2004, irish_chunk_2004, 
                cuban_chunk_2004, muslim_chunk_2004, 
                chinese_chunk_2004, mexican_chunk_2004)

chunk_2004_count <- as.data.frame(table(chunk_2004))
chunk_2004_count   <- subset(chunk_2004_count, select = Freq)
chunk_2004_count <- mutate(chunk_2004_count, year = 2004)




#2011
chunk_2011 <- c(italian_chunk_2011, jewish_chunk_2011, 
                catholic_chunk_2011, irish_chunk_2011, 
                cuban_chunk_2011, muslim_chunk_2011, 
                chinese_chunk_2011, mexican_chunk_2011)

chunk_2011_count <- as.data.frame(table(chunk_2011))
chunk_2011_count   <- subset(chunk_2011_count, select = Freq)
chunk_2011_count <- mutate(chunk_2011_count, year = 2011)


#2017
chunk_2017 <- c(italian_chunk_2017, jewish_chunk_2017, 
                catholic_chunk_2017, irish_chunk_2017, 
                cuban_chunk_2017, muslim_chunk_2017, 
                chinese_chunk_2017, mexican_chunk_2017)

chunk_2017_count <- as.data.frame(table(chunk_2017))
chunk_2017_count   <- subset(chunk_2017_count, select = Freq)
chunk_2017_count <- mutate(chunk_2017_count, year = 2017)



# combine
chunk_count <- full_join(chunk_1922_count, chunk_1927_count)
chunk_count <- full_join(chunk_count, chunk_1931_count)
chunk_count <- full_join(chunk_count, chunk_1934_count)
chunk_count <- full_join(chunk_count, chunk_1937_count)
chunk_count <- full_join(chunk_count, chunk_1940_count)
chunk_count <- full_join(chunk_count, chunk_1942_count)
chunk_count <- full_join(chunk_count, chunk_1945_count)
chunk_count <- full_join(chunk_count, chunk_1950_count)
chunk_count <- full_join(chunk_count, chunk_1956_count)
chunk_count <- full_join(chunk_count, chunk_1960_count)
chunk_count <- full_join(chunk_count, chunk_1965_count)
chunk_count <- full_join(chunk_count, chunk_1969_count)
chunk_count <- full_join(chunk_count, chunk_1975_count)
chunk_count <- full_join(chunk_count, chunk_1984_count)
chunk_count <- full_join(chunk_count, chunk_1992_count)
chunk_count <- full_join(chunk_count, chunk_1997_count)
chunk_count <- full_join(chunk_count, chunk_2004_count)
chunk_count <- full_join(chunk_count, chunk_2011_count)
chunk_count <- full_join(chunk_count, chunk_2017_count)

# period 1: 1920-1945
chunk_count_p1 <- full_join(chunk_1922_count, chunk_1927_count)
chunk_count_p1 <- full_join(chunk_count_p1, chunk_1931_count)
chunk_count_p1 <- full_join(chunk_count_p1, chunk_1934_count)
chunk_count_p1 <- full_join(chunk_count_p1, chunk_1937_count)
chunk_count_p1 <- full_join(chunk_count_p1, chunk_1940_count)
chunk_count_p1 <- full_join(chunk_count_p1, chunk_1942_count)


# period 2: 1945-1970

chunk_count_p2 <- full_join(chunk_1942_count, chunk_1945_count)
chunk_count_p2 <- full_join(chunk_count_p2, chunk_1950_count)
chunk_count_p2 <- full_join(chunk_count_p2, chunk_1956_count)
chunk_count_p2 <- full_join(chunk_count_p2, chunk_1960_count)
chunk_count_p2 <- full_join(chunk_count_p2, chunk_1965_count)
chunk_count_p2 <- full_join(chunk_count_p2, chunk_1969_count)


# period 3: 1970-1995

chunk_count_p3 <- full_join(chunk_1975_count, chunk_1984_count)
chunk_count_p3 <- full_join(chunk_count_p3, chunk_1992_count)


# period 4: 1995-2020
chunk_count_p4 <- full_join(chunk_1997_count, chunk_2004_count)
chunk_count_p4 <- full_join(chunk_count_p4, chunk_2011_count)
chunk_count_p4 <- full_join(chunk_count_p4, chunk_2017_count)







# add row value
chunk_count   <- add_row_numbers(chunk_count,   name = "n", zero_based = FALSE)
chunk_count   <- mutate(chunk_count, freq = as.integer(Freq))
chunk_count   <- subset(chunk_count, select = -Freq)


chunk_count_p1   <- add_row_numbers(chunk_count_p1,   name = "n", zero_based = FALSE)
chunk_count_p1   <- mutate(chunk_count_p1, freq = as.integer(Freq))
chunk_count_p1   <- subset(chunk_count_p1, select = -Freq)


chunk_count_p2   <- add_row_numbers(chunk_count_p2,   name = "n", zero_based = FALSE)
chunk_count_p2   <- mutate(chunk_count_p2, freq = as.integer(Freq))
chunk_count_p2   <- subset(chunk_count_p2, select = -Freq)


chunk_count_p3   <- add_row_numbers(chunk_count_p3,   name = "n", zero_based = FALSE)
chunk_count_p3   <- mutate(chunk_count_p3, freq = as.integer(Freq))
chunk_count_p3   <- subset(chunk_count_p3, select = -Freq)


chunk_count_p4   <- add_row_numbers(chunk_count_p4,   name = "n", zero_based = FALSE)
chunk_count_p4   <- mutate(chunk_count_p4, freq = as.integer(Freq))
chunk_count_p4   <- subset(chunk_count_p4, select = -Freq)





chunk_count_freq <- ggplot(chunk_count, aes(freq)) 
chunk_count_freq <- chunk_count_freq + geom_bar()
chunk_count_freq <- chunk_count_freq + labs(title = "All Periods", x = "Frequency in Any Pseudocorpus", y = "Count")
print(chunk_count_freq)


chunk_count_freq_p1 <- ggplot(chunk_count_p1, aes(freq)) 
chunk_count_freq_p1 <- chunk_count_freq_p1 + geom_bar()
chunk_count_freq_p1 <- chunk_count_freq_p1 + labs(title = "Period 1", x = "Frequency in Any Pseudocorpus", y = "Count")
print(chunk_count_freq_p1)

chunk_count_freq_p2 <- ggplot(chunk_count_p2, aes(freq)) 
chunk_count_freq_p2 <- chunk_count_freq_p2 + geom_bar()
chunk_count_freq_p2 <- chunk_count_freq_p2 + labs(title = "Period 2", x = "Frequency in Any Pseudocorpus", y = "Count")
print(chunk_count_freq_p2)

chunk_count_freq_p3 <- ggplot(chunk_count_p3, aes(freq)) 
chunk_count_freq_p3 <- chunk_count_freq_p3 + geom_bar()
chunk_count_freq_p3 <- chunk_count_freq_p3 + labs(title = "Period 3", x = "Frequency in Any Pseudocorpus", y = "Count")
print(chunk_count_freq_p3)

chunk_count_freq_p4 <- ggplot(chunk_count_p4, aes(freq)) 
chunk_count_freq_p4 <- chunk_count_freq_p4 + geom_bar()
chunk_count_freq_p4 <- chunk_count_freq_p4 + labs(title = "Period 4", x = "Frequency in Any Pseudocorpus", y = "Count")
print(chunk_count_freq_p4)







sum_chunk_count <- summary(chunk_count$freq)

sum_chunk_count_p1 <- summary(chunk_count_p1$freq)
sum_chunk_count_p2 <- summary(chunk_count_p2$freq)
sum_chunk_count_p3 <- summary(chunk_count_p3$freq)
sum_chunk_count_p4 <- summary(chunk_count_p4$freq)

print(sum_chunk_count) 

print(sum_chunk_count_p1)
print(sum_chunk_count_p2)
print(sum_chunk_count_p3)
print(sum_chunk_count_p4)


chunk_count_freq_p1234 <- ggarrange(chunk_count_freq_p1, 
                                    chunk_count_freq_p2,  
                                    chunk_count_freq_p3,  
                                    chunk_count_freq_p4, 
                                    nrow = 2, ncol = 2,
                                    common.legend = TRUE,
                                    legend = "bottom")


print(chunk_count_freq_p1234)




pdf("Kline(WORKING)IMG-chunk_count_freq.pdf")
print(sum_chunk_count) 

print(sum_chunk_count_p1)
print(sum_chunk_count_p2)
print(sum_chunk_count_p3)
print(sum_chunk_count_p4)

print(chunk_count_freq)
print(chunk_count_freq_p1)
print(chunk_count_freq_p2)
print(chunk_count_freq_p3)
print(chunk_count_freq_p4)
print(chunk_count_freq_p1234)
dev.off()



############ Topic model by pseudo corpus ##################################
############ Cast as DTM
# italian

tidy_EP_chunks_italian_dtm <- tidy_EP_chunks_italian %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_italian_dtm_p1 <- tidy_EP_chunks_italian_p1 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_italian_dtm_p2 <- tidy_EP_chunks_italian_p2 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_italian_dtm_p3 <- tidy_EP_chunks_italian_p3 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_italian_dtm_p4 <- tidy_EP_chunks_italian_p4 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

# jewish

tidy_EP_chunks_jewish_dtm <- tidy_EP_chunks_jewish %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_jewish_dtm_p1 <- tidy_EP_chunks_jewish_p1 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_jewish_dtm_p2 <- tidy_EP_chunks_jewish_p2 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_jewish_dtm_p3 <- tidy_EP_chunks_jewish_p3 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_jewish_dtm_p4 <- tidy_EP_chunks_jewish_p4 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 








# catholic

tidy_EP_chunks_catholic_dtm <- tidy_EP_chunks_catholic %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_catholic_dtm_p1 <- tidy_EP_chunks_catholic_p1 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_catholic_dtm_p2 <- tidy_EP_chunks_catholic_p2 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_catholic_dtm_p3 <- tidy_EP_chunks_catholic_p3 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_catholic_dtm_p4 <- tidy_EP_chunks_catholic_p4 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 





# irish

tidy_EP_chunks_irish_dtm <- tidy_EP_chunks_irish %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_irish_dtm_p1 <- tidy_EP_chunks_irish_p1 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_irish_dtm_p2 <- tidy_EP_chunks_irish_p2 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_irish_dtm_p3 <- tidy_EP_chunks_irish_p3 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_irish_dtm_p4 <- tidy_EP_chunks_irish_p4 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 








# cuban

tidy_EP_chunks_cuban_dtm <- tidy_EP_chunks_cuban %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_cuban_dtm_p1 <- tidy_EP_chunks_cuban_p1 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_cuban_dtm_p2 <- tidy_EP_chunks_cuban_p2 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_cuban_dtm_p3 <- tidy_EP_chunks_cuban_p3 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_cuban_dtm_p4 <- tidy_EP_chunks_cuban_p4 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 








# muslim

tidy_EP_chunks_muslim_dtm <- tidy_EP_chunks_muslim %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_muslim_dtm_p1 <- tidy_EP_chunks_muslim_p1 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_muslim_dtm_p2 <- tidy_EP_chunks_muslim_p2 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_muslim_dtm_p3 <- tidy_EP_chunks_muslim_p3 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_muslim_dtm_p4 <- tidy_EP_chunks_muslim_p4 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 








# chinese

tidy_EP_chunks_chinese_dtm <- tidy_EP_chunks_chinese %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_chinese_dtm_p1 <- tidy_EP_chunks_chinese_p1 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_chinese_dtm_p2 <- tidy_EP_chunks_chinese_p2 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_chinese_dtm_p3 <- tidy_EP_chunks_chinese_p3 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_chinese_dtm_p4 <- tidy_EP_chunks_chinese_p4 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 








# mexican

tidy_EP_chunks_mexican_dtm <- tidy_EP_chunks_mexican %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_mexican_dtm_p1 <- tidy_EP_chunks_mexican_p1 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_mexican_dtm_p2 <- tidy_EP_chunks_mexican_p2 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_mexican_dtm_p3 <- tidy_EP_chunks_mexican_p3 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 

tidy_EP_chunks_mexican_dtm_p4 <- tidy_EP_chunks_mexican_p4 %>% 
  cast_dtm(term = word, 
           document = unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) 





















############ Topic model by pseudo corpus: Italian



### k = 3

# cast topic models

italian_lda_k3 <- LDA(tidy_EP_chunks_italian_dtm, k = 3, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k3 <- tidy(italian_lda_k3)
italian_lda_td_k3

# check gamma
italian_lda_gamma_k3 <- tidy(italian_lda_k3, matrix = "gamma")


# most common terms

top_terms_italian_k3 <- italian_lda_td_k3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k3

# figure

topic_italian_k3 <- top_terms_italian_k3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

italian_lda_k3_p1 <- LDA(tidy_EP_chunks_italian_dtm_p1, k = 3, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k3_p1 <- tidy(italian_lda_k3_p1)
italian_lda_td_k3_p1

# check gamma
italian_lda_gamma_k3_p1 <- tidy(italian_lda_k3_p1, matrix = "gamma")


# most common terms

top_terms_italian_k3_p1 <- italian_lda_td_k3_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k3_p1

# figure

topic_italian_k3_p1 <- top_terms_italian_k3_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

italian_lda_k3_p2 <- LDA(tidy_EP_chunks_italian_dtm_p2, k = 3, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k3_p2 <- tidy(italian_lda_k3_p2)
italian_lda_td_k3_p2

# check gamma
italian_lda_gamma_k3_p2 <- tidy(italian_lda_k3_p2, matrix = "gamma")


# most common terms

top_terms_italian_k3_p2 <- italian_lda_td_k3_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k3_p2

# figure

topic_italian_k3_p2 <- top_terms_italian_k3_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

italian_lda_k3_p3 <- LDA(tidy_EP_chunks_italian_dtm_p3, k = 3, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k3_p3 <- tidy(italian_lda_k3_p3)
italian_lda_td_k3_p3

# check gamma
italian_lda_gamma_k3_p3 <- tidy(italian_lda_k3_p3, matrix = "gamma")


# most common terms

top_terms_italian_k3_p3 <- italian_lda_td_k3_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k3_p3

# figure

topic_italian_k3_p3 <- top_terms_italian_k3_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

italian_lda_k3_p4 <- LDA(tidy_EP_chunks_italian_dtm_p4, k = 3, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k3_p4 <- tidy(italian_lda_k3_p4)
italian_lda_td_k3_p4

# check gamma
italian_lda_gamma_k3_p4 <- tidy(italian_lda_k3_p4, matrix = "gamma")


# most common terms

top_terms_italian_k3_p4 <- italian_lda_td_k3_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k3_p4

# figure

topic_italian_k3_p4 <- top_terms_italian_k3_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_italian_k3
top_terms_italian_k3_p1
top_terms_italian_k3_p2
top_terms_italian_k3_p3
top_terms_italian_k3_p4



### Figure summary

print(topic_italian_k3)
print(topic_italian_k3_p1)
print(topic_italian_k3_p2)
print(topic_italian_k3_p3)
print(topic_italian_k3_p4)




### k = 4

# cast topic models

italian_lda_k4 <- LDA(tidy_EP_chunks_italian_dtm, k = 4, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k4 <- tidy(italian_lda_k4)
italian_lda_td_k4

# check gamma
italian_lda_gamma_k4 <- tidy(italian_lda_k4, matrix = "gamma")


# most common terms

top_terms_italian_k4 <- italian_lda_td_k4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k4

# figure

topic_italian_k4 <- top_terms_italian_k4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

italian_lda_k4_p1 <- LDA(tidy_EP_chunks_italian_dtm_p1, k = 4, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k4_p1 <- tidy(italian_lda_k4_p1)
italian_lda_td_k4_p1

# check gamma
italian_lda_gamma_k4_p1 <- tidy(italian_lda_k4_p1, matrix = "gamma")


# most common terms

top_terms_italian_k4_p1 <- italian_lda_td_k4_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k4_p1

# figure

topic_italian_k4_p1 <- top_terms_italian_k4_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

italian_lda_k4_p2 <- LDA(tidy_EP_chunks_italian_dtm_p2, k = 4, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k4_p2 <- tidy(italian_lda_k4_p2)
italian_lda_td_k4_p2

# check gamma
italian_lda_gamma_k4_p2 <- tidy(italian_lda_k4_p2, matrix = "gamma")


# most common terms

top_terms_italian_k4_p2 <- italian_lda_td_k4_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k4_p2

# figure

topic_italian_k4_p2 <- top_terms_italian_k4_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

italian_lda_k4_p3 <- LDA(tidy_EP_chunks_italian_dtm_p3, k = 4, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k4_p3 <- tidy(italian_lda_k4_p3)
italian_lda_td_k4_p3

# check gamma
italian_lda_gamma_k4_p3 <- tidy(italian_lda_k4_p3, matrix = "gamma")


# most common terms

top_terms_italian_k4_p3 <- italian_lda_td_k4_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k4_p3

# figure

topic_italian_k4_p3 <- top_terms_italian_k4_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

italian_lda_k4_p4 <- LDA(tidy_EP_chunks_italian_dtm_p4, k = 4, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k4_p4 <- tidy(italian_lda_k4_p4)
italian_lda_td_k4_p4

# check gamma
italian_lda_gamma_k4_p4 <- tidy(italian_lda_k4_p4, matrix = "gamma")


# most common terms

top_terms_italian_k4_p4 <- italian_lda_td_k4_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k4_p4

# figure

topic_italian_k4_p4 <- top_terms_italian_k4_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_italian_k4
top_terms_italian_k4_p1
top_terms_italian_k4_p2
top_terms_italian_k4_p3
top_terms_italian_k4_p4



### Figure summary

print(topic_italian_k4)
print(topic_italian_k4_p1)
print(topic_italian_k4_p2)
print(topic_italian_k4_p3)
print(topic_italian_k4_p4)



### k = 5

# cast topic models

italian_lda_k5 <- LDA(tidy_EP_chunks_italian_dtm, k = 5, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k5 <- tidy(italian_lda_k5)
italian_lda_td_k5

# check gamma
italian_lda_gamma_k5 <- tidy(italian_lda_k5, matrix = "gamma")


# most common terms

top_terms_italian_k5 <- italian_lda_td_k5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k5

# figure

topic_italian_k5 <- top_terms_italian_k5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

italian_lda_k5_p1 <- LDA(tidy_EP_chunks_italian_dtm_p1, k = 5, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k5_p1 <- tidy(italian_lda_k5_p1)
italian_lda_td_k5_p1

# check gamma
italian_lda_gamma_k5_p1 <- tidy(italian_lda_k5_p1, matrix = "gamma")


# most common terms

top_terms_italian_k5_p1 <- italian_lda_td_k5_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k5_p1

# figure

topic_italian_k5_p1 <- top_terms_italian_k5_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

italian_lda_k5_p2 <- LDA(tidy_EP_chunks_italian_dtm_p2, k = 5, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k5_p2 <- tidy(italian_lda_k5_p2)
italian_lda_td_k5_p2

# check gamma
italian_lda_gamma_k5_p2 <- tidy(italian_lda_k5_p2, matrix = "gamma")


# most common terms

top_terms_italian_k5_p2 <- italian_lda_td_k5_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k5_p2

# figure

topic_italian_k5_p2 <- top_terms_italian_k5_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

italian_lda_k5_p3 <- LDA(tidy_EP_chunks_italian_dtm_p3, k = 5, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k5_p3 <- tidy(italian_lda_k5_p3)
italian_lda_td_k5_p3

# check gamma
italian_lda_gamma_k5_p3 <- tidy(italian_lda_k5_p3, matrix = "gamma")


# most common terms

top_terms_italian_k5_p3 <- italian_lda_td_k5_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k5_p3

# figure

topic_italian_k5_p3 <- top_terms_italian_k5_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

italian_lda_k5_p4 <- LDA(tidy_EP_chunks_italian_dtm_p4, k = 5, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k5_p4 <- tidy(italian_lda_k5_p4)
italian_lda_td_k5_p4

# check gamma
italian_lda_gamma_k5_p4 <- tidy(italian_lda_k5_p4, matrix = "gamma")


# most common terms

top_terms_italian_k5_p4 <- italian_lda_td_k5_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k5_p4

# figure

topic_italian_k5_p4 <- top_terms_italian_k5_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_italian_k5
top_terms_italian_k5_p1
top_terms_italian_k5_p2
top_terms_italian_k5_p3
top_terms_italian_k5_p4



### Figure summary

print(topic_italian_k5)
print(topic_italian_k5_p1)
print(topic_italian_k5_p2)
print(topic_italian_k5_p3)
print(topic_italian_k5_p4)







### k = 6

# cast topic models

italian_lda_k6 <- LDA(tidy_EP_chunks_italian_dtm, k = 6, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k6 <- tidy(italian_lda_k6)
italian_lda_td_k6

# check gamma
italian_lda_gamma_k6 <- tidy(italian_lda_k6, matrix = "gamma")


# most common terms

top_terms_italian_k6 <- italian_lda_td_k6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k6

# figure

topic_italian_k6 <- top_terms_italian_k6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

italian_lda_k6_p1 <- LDA(tidy_EP_chunks_italian_dtm_p1, k = 6, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k6_p1 <- tidy(italian_lda_k6_p1)
italian_lda_td_k6_p1

# check gamma
italian_lda_gamma_k6_p1 <- tidy(italian_lda_k6_p1, matrix = "gamma")


# most common terms

top_terms_italian_k6_p1 <- italian_lda_td_k6_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k6_p1

# figure

topic_italian_k6_p1 <- top_terms_italian_k6_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

italian_lda_k6_p2 <- LDA(tidy_EP_chunks_italian_dtm_p2, k = 6, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k6_p2 <- tidy(italian_lda_k6_p2)
italian_lda_td_k6_p2

# check gamma
italian_lda_gamma_k6_p2 <- tidy(italian_lda_k6_p2, matrix = "gamma")


# most common terms

top_terms_italian_k6_p2 <- italian_lda_td_k6_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k6_p2

# figure

topic_italian_k6_p2 <- top_terms_italian_k6_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

italian_lda_k6_p3 <- LDA(tidy_EP_chunks_italian_dtm_p3, k = 6, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k6_p3 <- tidy(italian_lda_k6_p3)
italian_lda_td_k6_p3

# check gamma
italian_lda_gamma_k6_p3 <- tidy(italian_lda_k6_p3, matrix = "gamma")


# most common terms

top_terms_italian_k6_p3 <- italian_lda_td_k6_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k6_p3

# figure

topic_italian_k6_p3 <- top_terms_italian_k6_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

italian_lda_k6_p4 <- LDA(tidy_EP_chunks_italian_dtm_p4, k = 6, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k6_p4 <- tidy(italian_lda_k6_p4)
italian_lda_td_k6_p4

# check gamma
italian_lda_gamma_k6_p4 <- tidy(italian_lda_k6_p4, matrix = "gamma")


# most common terms

top_terms_italian_k6_p4 <- italian_lda_td_k6_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k6_p4

# figure

topic_italian_k6_p4 <- top_terms_italian_k6_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_italian_k6
top_terms_italian_k6_p1
top_terms_italian_k6_p2
top_terms_italian_k6_p3
top_terms_italian_k6_p4



### Figure summary

print(topic_italian_k6)
print(topic_italian_k6_p1)
print(topic_italian_k6_p2)
print(topic_italian_k6_p3)
print(topic_italian_k6_p4)







### k = 7

# cast topic models

italian_lda_k7 <- LDA(tidy_EP_chunks_italian_dtm, k = 7, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k7 <- tidy(italian_lda_k7)
italian_lda_td_k7

# check gamma
italian_lda_gamma_k7 <- tidy(italian_lda_k7, matrix = "gamma")


# most common terms

top_terms_italian_k7 <- italian_lda_td_k7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k7

# figure

topic_italian_k7 <- top_terms_italian_k7 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

italian_lda_k7_p1 <- LDA(tidy_EP_chunks_italian_dtm_p1, k = 7, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k7_p1 <- tidy(italian_lda_k7_p1)
italian_lda_td_k7_p1

# check gamma
italian_lda_gamma_k7_p1 <- tidy(italian_lda_k7_p1, matrix = "gamma")


# most common terms

top_terms_italian_k7_p1 <- italian_lda_td_k7_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k7_p1

# figure

topic_italian_k7_p1 <- top_terms_italian_k7_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

italian_lda_k7_p2 <- LDA(tidy_EP_chunks_italian_dtm_p2, k = 7, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k7_p2 <- tidy(italian_lda_k7_p2)
italian_lda_td_k7_p2

# check gamma
italian_lda_gamma_k7_p2 <- tidy(italian_lda_k7_p2, matrix = "gamma")


# most common terms

top_terms_italian_k7_p2 <- italian_lda_td_k7_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k7_p2

# figure

topic_italian_k7_p2 <- top_terms_italian_k7_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

italian_lda_k7_p3 <- LDA(tidy_EP_chunks_italian_dtm_p3, k = 7, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k7_p3 <- tidy(italian_lda_k7_p3)
italian_lda_td_k7_p3

# check gamma
italian_lda_gamma_k7_p3 <- tidy(italian_lda_k7_p3, matrix = "gamma")


# most common terms

top_terms_italian_k7_p3 <- italian_lda_td_k7_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k7_p3

# figure

topic_italian_k7_p3 <- top_terms_italian_k7_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

italian_lda_k7_p4 <- LDA(tidy_EP_chunks_italian_dtm_p4, k = 7, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k7_p4 <- tidy(italian_lda_k7_p4)
italian_lda_td_k7_p4

# check gamma
italian_lda_gamma_k7_p4 <- tidy(italian_lda_k7_p4, matrix = "gamma")


# most common terms

top_terms_italian_k7_p4 <- italian_lda_td_k7_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k7_p4

# figure

topic_italian_k7_p4 <- top_terms_italian_k7_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_italian_k7
top_terms_italian_k7_p1
top_terms_italian_k7_p2
top_terms_italian_k7_p3
top_terms_italian_k7_p4



### Figure summary

print(topic_italian_k7)
print(topic_italian_k7_p1)
print(topic_italian_k7_p2)
print(topic_italian_k7_p3)
print(topic_italian_k7_p4)







### k = 8

# cast topic models

italian_lda_k8 <- LDA(tidy_EP_chunks_italian_dtm, k = 8, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k8 <- tidy(italian_lda_k8)
italian_lda_td_k8

# check gamma
italian_lda_gamma_k8 <- tidy(italian_lda_k8, matrix = "gamma")


# most common terms

top_terms_italian_k8 <- italian_lda_td_k8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k8

# figure

topic_italian_k8 <- top_terms_italian_k8 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

italian_lda_k8_p1 <- LDA(tidy_EP_chunks_italian_dtm_p1, k = 8, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k8_p1 <- tidy(italian_lda_k8_p1)
italian_lda_td_k8_p1

# check gamma
italian_lda_gamma_k8_p1 <- tidy(italian_lda_k8_p1, matrix = "gamma")


# most common terms

top_terms_italian_k8_p1 <- italian_lda_td_k8_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k8_p1

# figure

topic_italian_k8_p1 <- top_terms_italian_k8_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

italian_lda_k8_p2 <- LDA(tidy_EP_chunks_italian_dtm_p2, k = 8, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k8_p2 <- tidy(italian_lda_k8_p2)
italian_lda_td_k8_p2

# check gamma
italian_lda_gamma_k8_p2 <- tidy(italian_lda_k8_p2, matrix = "gamma")


# most common terms

top_terms_italian_k8_p2 <- italian_lda_td_k8_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k8_p2

# figure

topic_italian_k8_p2 <- top_terms_italian_k8_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

italian_lda_k8_p3 <- LDA(tidy_EP_chunks_italian_dtm_p3, k = 8, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k8_p3 <- tidy(italian_lda_k8_p3)
italian_lda_td_k8_p3

# check gamma
italian_lda_gamma_k8_p3 <- tidy(italian_lda_k8_p3, matrix = "gamma")


# most common terms

top_terms_italian_k8_p3 <- italian_lda_td_k8_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k8_p3

# figure

topic_italian_k8_p3 <- top_terms_italian_k8_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

italian_lda_k8_p4 <- LDA(tidy_EP_chunks_italian_dtm_p4, k = 8, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k8_p4 <- tidy(italian_lda_k8_p4)
italian_lda_td_k8_p4

# check gamma
italian_lda_gamma_k8_p4 <- tidy(italian_lda_k8_p4, matrix = "gamma")


# most common terms

top_terms_italian_k8_p4 <- italian_lda_td_k8_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k8_p4

# figure

topic_italian_k8_p4 <- top_terms_italian_k8_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_italian_k8
top_terms_italian_k8_p1
top_terms_italian_k8_p2
top_terms_italian_k8_p3
top_terms_italian_k8_p4



### Figure summary

print(topic_italian_k8)
print(topic_italian_k8_p1)
print(topic_italian_k8_p2)
print(topic_italian_k8_p3)
print(topic_italian_k8_p4)







### k = 9

# cast topic models

italian_lda_k9 <- LDA(tidy_EP_chunks_italian_dtm, k = 9, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k9 <- tidy(italian_lda_k9)
italian_lda_td_k9

# check gamma
italian_lda_gamma_k9 <- tidy(italian_lda_k9, matrix = "gamma")


# most common terms

top_terms_italian_k9 <- italian_lda_td_k9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k9

# figure

topic_italian_k9 <- top_terms_italian_k9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

italian_lda_k9_p1 <- LDA(tidy_EP_chunks_italian_dtm_p1, k = 9, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k9_p1 <- tidy(italian_lda_k9_p1)
italian_lda_td_k9_p1

# check gamma
italian_lda_gamma_k9_p1 <- tidy(italian_lda_k9_p1, matrix = "gamma")


# most common terms

top_terms_italian_k9_p1 <- italian_lda_td_k9_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k9_p1

# figure

topic_italian_k9_p1 <- top_terms_italian_k9_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

italian_lda_k9_p2 <- LDA(tidy_EP_chunks_italian_dtm_p2, k = 9, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k9_p2 <- tidy(italian_lda_k9_p2)
italian_lda_td_k9_p2

# check gamma
italian_lda_gamma_k9_p2 <- tidy(italian_lda_k9_p2, matrix = "gamma")


# most common terms

top_terms_italian_k9_p2 <- italian_lda_td_k9_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k9_p2

# figure

topic_italian_k9_p2 <- top_terms_italian_k9_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

italian_lda_k9_p3 <- LDA(tidy_EP_chunks_italian_dtm_p3, k = 9, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k9_p3 <- tidy(italian_lda_k9_p3)
italian_lda_td_k9_p3

# check gamma
italian_lda_gamma_k9_p3 <- tidy(italian_lda_k9_p3, matrix = "gamma")


# most common terms

top_terms_italian_k9_p3 <- italian_lda_td_k9_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k9_p3

# figure

topic_italian_k9_p3 <- top_terms_italian_k9_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

italian_lda_k9_p4 <- LDA(tidy_EP_chunks_italian_dtm_p4, k = 9, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k9_p4 <- tidy(italian_lda_k9_p4)
italian_lda_td_k9_p4

# check gamma
italian_lda_gamma_k9_p4 <- tidy(italian_lda_k9_p4, matrix = "gamma")


# most common terms

top_terms_italian_k9_p4 <- italian_lda_td_k9_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k9_p4

# figure

topic_italian_k9_p4 <- top_terms_italian_k9_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_italian_k9
top_terms_italian_k9_p1
top_terms_italian_k9_p2
top_terms_italian_k9_p3
top_terms_italian_k9_p4



### Figure summary

print(topic_italian_k9)
print(topic_italian_k9_p1)
print(topic_italian_k9_p2)
print(topic_italian_k9_p3)
print(topic_italian_k9_p4)







### k = 10

# cast topic models

italian_lda_k10 <- LDA(tidy_EP_chunks_italian_dtm, k = 10, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k10 <- tidy(italian_lda_k10)
italian_lda_td_k10

# check gamma
italian_lda_gamma_k10 <- tidy(italian_lda_k10, matrix = "gamma")


# most common terms

top_terms_italian_k10 <- italian_lda_td_k10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k10

# figure

topic_italian_k10 <- top_terms_italian_k10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

italian_lda_k10_p1 <- LDA(tidy_EP_chunks_italian_dtm_p1, k = 10, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k10_p1 <- tidy(italian_lda_k10_p1)
italian_lda_td_k10_p1

# check gamma
italian_lda_gamma_k10_p1 <- tidy(italian_lda_k10_p1, matrix = "gamma")


# most common terms

top_terms_italian_k10_p1 <- italian_lda_td_k10_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k10_p1

# figure

topic_italian_k10_p1 <- top_terms_italian_k10_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

italian_lda_k10_p2 <- LDA(tidy_EP_chunks_italian_dtm_p2, k = 10, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k10_p2 <- tidy(italian_lda_k10_p2)
italian_lda_td_k10_p2

# check gamma
italian_lda_gamma_k10_p2 <- tidy(italian_lda_k10_p2, matrix = "gamma")


# most common terms

top_terms_italian_k10_p2 <- italian_lda_td_k10_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k10_p2

# figure

topic_italian_k10_p2 <- top_terms_italian_k10_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

italian_lda_k10_p3 <- LDA(tidy_EP_chunks_italian_dtm_p3, k = 10, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k10_p3 <- tidy(italian_lda_k10_p3)
italian_lda_td_k10_p3

# check gamma
italian_lda_gamma_k10_p3 <- tidy(italian_lda_k10_p3, matrix = "gamma")


# most common terms

top_terms_italian_k10_p3 <- italian_lda_td_k10_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k10_p3

# figure

topic_italian_k10_p3 <- top_terms_italian_k10_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

italian_lda_k10_p4 <- LDA(tidy_EP_chunks_italian_dtm_p4, k = 10, control = list(seed = 1234))


# convert back to tidy

italian_lda_td_k10_p4 <- tidy(italian_lda_k10_p4)
italian_lda_td_k10_p4

# check gamma
italian_lda_gamma_k10_p4 <- tidy(italian_lda_k10_p4, matrix = "gamma")


# most common terms

top_terms_italian_k10_p4 <- italian_lda_td_k10_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_italian_k10_p4

# figure

topic_italian_k10_p4 <- top_terms_italian_k10_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_italian_k10
top_terms_italian_k10_p1
top_terms_italian_k10_p2
top_terms_italian_k10_p3
top_terms_italian_k10_p4



### Figure summary

print(topic_italian_k10)
print(topic_italian_k10_p1)
print(topic_italian_k10_p2)
print(topic_italian_k10_p3)
print(topic_italian_k10_p4)









############ Topic model by pseudo corpus: jewish



### k = 3

# cast topic models

jewish_lda_k3 <- LDA(tidy_EP_chunks_jewish_dtm, k = 3, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k3 <- tidy(jewish_lda_k3)
jewish_lda_td_k3

# check gamma
jewish_lda_gamma_k3 <- tidy(jewish_lda_k3, matrix = "gamma")


# most common terms

top_terms_jewish_k3 <- jewish_lda_td_k3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k3

# figure

topic_jewish_k3 <- top_terms_jewish_k3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

jewish_lda_k3_p1 <- LDA(tidy_EP_chunks_jewish_dtm_p1, k = 3, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k3_p1 <- tidy(jewish_lda_k3_p1)
jewish_lda_td_k3_p1

# check gamma
jewish_lda_gamma_k3_p1 <- tidy(jewish_lda_k3_p1, matrix = "gamma")


# most common terms

top_terms_jewish_k3_p1 <- jewish_lda_td_k3_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k3_p1

# figure

topic_jewish_k3_p1 <- top_terms_jewish_k3_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

jewish_lda_k3_p2 <- LDA(tidy_EP_chunks_jewish_dtm_p2, k = 3, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k3_p2 <- tidy(jewish_lda_k3_p2)
jewish_lda_td_k3_p2

# check gamma
jewish_lda_gamma_k3_p2 <- tidy(jewish_lda_k3_p2, matrix = "gamma")


# most common terms

top_terms_jewish_k3_p2 <- jewish_lda_td_k3_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k3_p2

# figure

topic_jewish_k3_p2 <- top_terms_jewish_k3_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

jewish_lda_k3_p3 <- LDA(tidy_EP_chunks_jewish_dtm_p3, k = 3, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k3_p3 <- tidy(jewish_lda_k3_p3)
jewish_lda_td_k3_p3

# check gamma
jewish_lda_gamma_k3_p3 <- tidy(jewish_lda_k3_p3, matrix = "gamma")


# most common terms

top_terms_jewish_k3_p3 <- jewish_lda_td_k3_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k3_p3

# figure

topic_jewish_k3_p3 <- top_terms_jewish_k3_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

jewish_lda_k3_p4 <- LDA(tidy_EP_chunks_jewish_dtm_p4, k = 3, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k3_p4 <- tidy(jewish_lda_k3_p4)
jewish_lda_td_k3_p4

# check gamma
jewish_lda_gamma_k3_p4 <- tidy(jewish_lda_k3_p4, matrix = "gamma")


# most common terms

top_terms_jewish_k3_p4 <- jewish_lda_td_k3_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k3_p4

# figure

topic_jewish_k3_p4 <- top_terms_jewish_k3_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_jewish_k3
top_terms_jewish_k3_p1
top_terms_jewish_k3_p2
top_terms_jewish_k3_p3
top_terms_jewish_k3_p4



### Figure summary

print(topic_jewish_k3)
print(topic_jewish_k3_p1)
print(topic_jewish_k3_p2)
print(topic_jewish_k3_p3)
print(topic_jewish_k3_p4)




### k = 4

# cast topic models

jewish_lda_k4 <- LDA(tidy_EP_chunks_jewish_dtm, k = 4, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k4 <- tidy(jewish_lda_k4)
jewish_lda_td_k4

# check gamma
jewish_lda_gamma_k4 <- tidy(jewish_lda_k4, matrix = "gamma")


# most common terms

top_terms_jewish_k4 <- jewish_lda_td_k4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k4

# figure

topic_jewish_k4 <- top_terms_jewish_k4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

jewish_lda_k4_p1 <- LDA(tidy_EP_chunks_jewish_dtm_p1, k = 4, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k4_p1 <- tidy(jewish_lda_k4_p1)
jewish_lda_td_k4_p1

# check gamma
jewish_lda_gamma_k4_p1 <- tidy(jewish_lda_k4_p1, matrix = "gamma")


# most common terms

top_terms_jewish_k4_p1 <- jewish_lda_td_k4_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k4_p1

# figure

topic_jewish_k4_p1 <- top_terms_jewish_k4_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

jewish_lda_k4_p2 <- LDA(tidy_EP_chunks_jewish_dtm_p2, k = 4, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k4_p2 <- tidy(jewish_lda_k4_p2)
jewish_lda_td_k4_p2

# check gamma
jewish_lda_gamma_k4_p2 <- tidy(jewish_lda_k4_p2, matrix = "gamma")


# most common terms

top_terms_jewish_k4_p2 <- jewish_lda_td_k4_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k4_p2

# figure

topic_jewish_k4_p2 <- top_terms_jewish_k4_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

jewish_lda_k4_p3 <- LDA(tidy_EP_chunks_jewish_dtm_p3, k = 4, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k4_p3 <- tidy(jewish_lda_k4_p3)
jewish_lda_td_k4_p3

# check gamma
jewish_lda_gamma_k4_p3 <- tidy(jewish_lda_k4_p3, matrix = "gamma")


# most common terms

top_terms_jewish_k4_p3 <- jewish_lda_td_k4_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k4_p3

# figure

topic_jewish_k4_p3 <- top_terms_jewish_k4_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

jewish_lda_k4_p4 <- LDA(tidy_EP_chunks_jewish_dtm_p4, k = 4, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k4_p4 <- tidy(jewish_lda_k4_p4)
jewish_lda_td_k4_p4

# check gamma
jewish_lda_gamma_k4_p4 <- tidy(jewish_lda_k4_p4, matrix = "gamma")


# most common terms

top_terms_jewish_k4_p4 <- jewish_lda_td_k4_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k4_p4

# figure

topic_jewish_k4_p4 <- top_terms_jewish_k4_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_jewish_k4
top_terms_jewish_k4_p1
top_terms_jewish_k4_p2
top_terms_jewish_k4_p3
top_terms_jewish_k4_p4



### Figure summary

print(topic_jewish_k4)
print(topic_jewish_k4_p1)
print(topic_jewish_k4_p2)
print(topic_jewish_k4_p3)
print(topic_jewish_k4_p4)



### k = 5

# cast topic models

jewish_lda_k5 <- LDA(tidy_EP_chunks_jewish_dtm, k = 5, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k5 <- tidy(jewish_lda_k5)
jewish_lda_td_k5

# check gamma
jewish_lda_gamma_k5 <- tidy(jewish_lda_k5, matrix = "gamma")


# most common terms

top_terms_jewish_k5 <- jewish_lda_td_k5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k5

# figure

topic_jewish_k5 <- top_terms_jewish_k5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

jewish_lda_k5_p1 <- LDA(tidy_EP_chunks_jewish_dtm_p1, k = 5, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k5_p1 <- tidy(jewish_lda_k5_p1)
jewish_lda_td_k5_p1

# check gamma
jewish_lda_gamma_k5_p1 <- tidy(jewish_lda_k5_p1, matrix = "gamma")


# most common terms

top_terms_jewish_k5_p1 <- jewish_lda_td_k5_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k5_p1

# figure

topic_jewish_k5_p1 <- top_terms_jewish_k5_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

jewish_lda_k5_p2 <- LDA(tidy_EP_chunks_jewish_dtm_p2, k = 5, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k5_p2 <- tidy(jewish_lda_k5_p2)
jewish_lda_td_k5_p2

# check gamma
jewish_lda_gamma_k5_p2 <- tidy(jewish_lda_k5_p2, matrix = "gamma")


# most common terms

top_terms_jewish_k5_p2 <- jewish_lda_td_k5_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k5_p2

# figure

topic_jewish_k5_p2 <- top_terms_jewish_k5_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

jewish_lda_k5_p3 <- LDA(tidy_EP_chunks_jewish_dtm_p3, k = 5, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k5_p3 <- tidy(jewish_lda_k5_p3)
jewish_lda_td_k5_p3

# check gamma
jewish_lda_gamma_k5_p3 <- tidy(jewish_lda_k5_p3, matrix = "gamma")


# most common terms

top_terms_jewish_k5_p3 <- jewish_lda_td_k5_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k5_p3

# figure

topic_jewish_k5_p3 <- top_terms_jewish_k5_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

jewish_lda_k5_p4 <- LDA(tidy_EP_chunks_jewish_dtm_p4, k = 5, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k5_p4 <- tidy(jewish_lda_k5_p4)
jewish_lda_td_k5_p4

# check gamma
jewish_lda_gamma_k5_p4 <- tidy(jewish_lda_k5_p4, matrix = "gamma")


# most common terms

top_terms_jewish_k5_p4 <- jewish_lda_td_k5_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k5_p4

# figure

topic_jewish_k5_p4 <- top_terms_jewish_k5_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_jewish_k5
top_terms_jewish_k5_p1
top_terms_jewish_k5_p2
top_terms_jewish_k5_p3
top_terms_jewish_k5_p4



### Figure summary

print(topic_jewish_k5)
print(topic_jewish_k5_p1)
print(topic_jewish_k5_p2)
print(topic_jewish_k5_p3)
print(topic_jewish_k5_p4)







### k = 6

# cast topic models

jewish_lda_k6 <- LDA(tidy_EP_chunks_jewish_dtm, k = 6, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k6 <- tidy(jewish_lda_k6)
jewish_lda_td_k6

# check gamma
jewish_lda_gamma_k6 <- tidy(jewish_lda_k6, matrix = "gamma")


# most common terms

top_terms_jewish_k6 <- jewish_lda_td_k6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k6

# figure

topic_jewish_k6 <- top_terms_jewish_k6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

jewish_lda_k6_p1 <- LDA(tidy_EP_chunks_jewish_dtm_p1, k = 6, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k6_p1 <- tidy(jewish_lda_k6_p1)
jewish_lda_td_k6_p1

# check gamma
jewish_lda_gamma_k6_p1 <- tidy(jewish_lda_k6_p1, matrix = "gamma")


# most common terms

top_terms_jewish_k6_p1 <- jewish_lda_td_k6_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k6_p1

# figure

topic_jewish_k6_p1 <- top_terms_jewish_k6_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

jewish_lda_k6_p2 <- LDA(tidy_EP_chunks_jewish_dtm_p2, k = 6, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k6_p2 <- tidy(jewish_lda_k6_p2)
jewish_lda_td_k6_p2

# check gamma
jewish_lda_gamma_k6_p2 <- tidy(jewish_lda_k6_p2, matrix = "gamma")


# most common terms

top_terms_jewish_k6_p2 <- jewish_lda_td_k6_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k6_p2

# figure

topic_jewish_k6_p2 <- top_terms_jewish_k6_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

jewish_lda_k6_p3 <- LDA(tidy_EP_chunks_jewish_dtm_p3, k = 6, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k6_p3 <- tidy(jewish_lda_k6_p3)
jewish_lda_td_k6_p3

# check gamma
jewish_lda_gamma_k6_p3 <- tidy(jewish_lda_k6_p3, matrix = "gamma")


# most common terms

top_terms_jewish_k6_p3 <- jewish_lda_td_k6_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k6_p3

# figure

topic_jewish_k6_p3 <- top_terms_jewish_k6_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

jewish_lda_k6_p4 <- LDA(tidy_EP_chunks_jewish_dtm_p4, k = 6, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k6_p4 <- tidy(jewish_lda_k6_p4)
jewish_lda_td_k6_p4

# check gamma
jewish_lda_gamma_k6_p4 <- tidy(jewish_lda_k6_p4, matrix = "gamma")


# most common terms

top_terms_jewish_k6_p4 <- jewish_lda_td_k6_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k6_p4

# figure

topic_jewish_k6_p4 <- top_terms_jewish_k6_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_jewish_k6
top_terms_jewish_k6_p1
top_terms_jewish_k6_p2
top_terms_jewish_k6_p3
top_terms_jewish_k6_p4



### Figure summary

print(topic_jewish_k6)
print(topic_jewish_k6_p1)
print(topic_jewish_k6_p2)
print(topic_jewish_k6_p3)
print(topic_jewish_k6_p4)







### k = 7

# cast topic models

jewish_lda_k7 <- LDA(tidy_EP_chunks_jewish_dtm, k = 7, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k7 <- tidy(jewish_lda_k7)
jewish_lda_td_k7

# check gamma
jewish_lda_gamma_k7 <- tidy(jewish_lda_k7, matrix = "gamma")


# most common terms

top_terms_jewish_k7 <- jewish_lda_td_k7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k7

# figure

topic_jewish_k7 <- top_terms_jewish_k7 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

jewish_lda_k7_p1 <- LDA(tidy_EP_chunks_jewish_dtm_p1, k = 7, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k7_p1 <- tidy(jewish_lda_k7_p1)
jewish_lda_td_k7_p1

# check gamma
jewish_lda_gamma_k7_p1 <- tidy(jewish_lda_k7_p1, matrix = "gamma")


# most common terms

top_terms_jewish_k7_p1 <- jewish_lda_td_k7_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k7_p1

# figure

topic_jewish_k7_p1 <- top_terms_jewish_k7_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

jewish_lda_k7_p2 <- LDA(tidy_EP_chunks_jewish_dtm_p2, k = 7, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k7_p2 <- tidy(jewish_lda_k7_p2)
jewish_lda_td_k7_p2

# check gamma
jewish_lda_gamma_k7_p2 <- tidy(jewish_lda_k7_p2, matrix = "gamma")


# most common terms

top_terms_jewish_k7_p2 <- jewish_lda_td_k7_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k7_p2

# figure

topic_jewish_k7_p2 <- top_terms_jewish_k7_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

jewish_lda_k7_p3 <- LDA(tidy_EP_chunks_jewish_dtm_p3, k = 7, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k7_p3 <- tidy(jewish_lda_k7_p3)
jewish_lda_td_k7_p3

# check gamma
jewish_lda_gamma_k7_p3 <- tidy(jewish_lda_k7_p3, matrix = "gamma")


# most common terms

top_terms_jewish_k7_p3 <- jewish_lda_td_k7_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k7_p3

# figure

topic_jewish_k7_p3 <- top_terms_jewish_k7_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

jewish_lda_k7_p4 <- LDA(tidy_EP_chunks_jewish_dtm_p4, k = 7, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k7_p4 <- tidy(jewish_lda_k7_p4)
jewish_lda_td_k7_p4

# check gamma
jewish_lda_gamma_k7_p4 <- tidy(jewish_lda_k7_p4, matrix = "gamma")


# most common terms

top_terms_jewish_k7_p4 <- jewish_lda_td_k7_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k7_p4

# figure

topic_jewish_k7_p4 <- top_terms_jewish_k7_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_jewish_k7
top_terms_jewish_k7_p1
top_terms_jewish_k7_p2
top_terms_jewish_k7_p3
top_terms_jewish_k7_p4



### Figure summary

print(topic_jewish_k7)
print(topic_jewish_k7_p1)
print(topic_jewish_k7_p2)
print(topic_jewish_k7_p3)
print(topic_jewish_k7_p4)







### k = 8

# cast topic models

jewish_lda_k8 <- LDA(tidy_EP_chunks_jewish_dtm, k = 8, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k8 <- tidy(jewish_lda_k8)
jewish_lda_td_k8

# check gamma
jewish_lda_gamma_k8 <- tidy(jewish_lda_k8, matrix = "gamma")


# most common terms

top_terms_jewish_k8 <- jewish_lda_td_k8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k8

# figure

topic_jewish_k8 <- top_terms_jewish_k8 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

jewish_lda_k8_p1 <- LDA(tidy_EP_chunks_jewish_dtm_p1, k = 8, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k8_p1 <- tidy(jewish_lda_k8_p1)
jewish_lda_td_k8_p1

# check gamma
jewish_lda_gamma_k8_p1 <- tidy(jewish_lda_k8_p1, matrix = "gamma")


# most common terms

top_terms_jewish_k8_p1 <- jewish_lda_td_k8_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k8_p1

# figure

topic_jewish_k8_p1 <- top_terms_jewish_k8_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

jewish_lda_k8_p2 <- LDA(tidy_EP_chunks_jewish_dtm_p2, k = 8, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k8_p2 <- tidy(jewish_lda_k8_p2)
jewish_lda_td_k8_p2

# check gamma
jewish_lda_gamma_k8_p2 <- tidy(jewish_lda_k8_p2, matrix = "gamma")


# most common terms

top_terms_jewish_k8_p2 <- jewish_lda_td_k8_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k8_p2

# figure

topic_jewish_k8_p2 <- top_terms_jewish_k8_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

jewish_lda_k8_p3 <- LDA(tidy_EP_chunks_jewish_dtm_p3, k = 8, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k8_p3 <- tidy(jewish_lda_k8_p3)
jewish_lda_td_k8_p3

# check gamma
jewish_lda_gamma_k8_p3 <- tidy(jewish_lda_k8_p3, matrix = "gamma")


# most common terms

top_terms_jewish_k8_p3 <- jewish_lda_td_k8_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k8_p3

# figure

topic_jewish_k8_p3 <- top_terms_jewish_k8_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

jewish_lda_k8_p4 <- LDA(tidy_EP_chunks_jewish_dtm_p4, k = 8, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k8_p4 <- tidy(jewish_lda_k8_p4)
jewish_lda_td_k8_p4

# check gamma
jewish_lda_gamma_k8_p4 <- tidy(jewish_lda_k8_p4, matrix = "gamma")


# most common terms

top_terms_jewish_k8_p4 <- jewish_lda_td_k8_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k8_p4

# figure

topic_jewish_k8_p4 <- top_terms_jewish_k8_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_jewish_k8
top_terms_jewish_k8_p1
top_terms_jewish_k8_p2
top_terms_jewish_k8_p3
top_terms_jewish_k8_p4



### Figure summary

print(topic_jewish_k8)
print(topic_jewish_k8_p1)
print(topic_jewish_k8_p2)
print(topic_jewish_k8_p3)
print(topic_jewish_k8_p4)







### k = 9

# cast topic models

jewish_lda_k9 <- LDA(tidy_EP_chunks_jewish_dtm, k = 9, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k9 <- tidy(jewish_lda_k9)
jewish_lda_td_k9

# check gamma
jewish_lda_gamma_k9 <- tidy(jewish_lda_k9, matrix = "gamma")


# most common terms

top_terms_jewish_k9 <- jewish_lda_td_k9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k9

# figure

topic_jewish_k9 <- top_terms_jewish_k9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

jewish_lda_k9_p1 <- LDA(tidy_EP_chunks_jewish_dtm_p1, k = 9, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k9_p1 <- tidy(jewish_lda_k9_p1)
jewish_lda_td_k9_p1

# check gamma
jewish_lda_gamma_k9_p1 <- tidy(jewish_lda_k9_p1, matrix = "gamma")


# most common terms

top_terms_jewish_k9_p1 <- jewish_lda_td_k9_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k9_p1

# figure

topic_jewish_k9_p1 <- top_terms_jewish_k9_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

jewish_lda_k9_p2 <- LDA(tidy_EP_chunks_jewish_dtm_p2, k = 9, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k9_p2 <- tidy(jewish_lda_k9_p2)
jewish_lda_td_k9_p2

# check gamma
jewish_lda_gamma_k9_p2 <- tidy(jewish_lda_k9_p2, matrix = "gamma")


# most common terms

top_terms_jewish_k9_p2 <- jewish_lda_td_k9_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k9_p2

# figure

topic_jewish_k9_p2 <- top_terms_jewish_k9_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

jewish_lda_k9_p3 <- LDA(tidy_EP_chunks_jewish_dtm_p3, k = 9, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k9_p3 <- tidy(jewish_lda_k9_p3)
jewish_lda_td_k9_p3

# check gamma
jewish_lda_gamma_k9_p3 <- tidy(jewish_lda_k9_p3, matrix = "gamma")


# most common terms

top_terms_jewish_k9_p3 <- jewish_lda_td_k9_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k9_p3

# figure

topic_jewish_k9_p3 <- top_terms_jewish_k9_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

jewish_lda_k9_p4 <- LDA(tidy_EP_chunks_jewish_dtm_p4, k = 9, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k9_p4 <- tidy(jewish_lda_k9_p4)
jewish_lda_td_k9_p4

# check gamma
jewish_lda_gamma_k9_p4 <- tidy(jewish_lda_k9_p4, matrix = "gamma")


# most common terms

top_terms_jewish_k9_p4 <- jewish_lda_td_k9_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k9_p4

# figure

topic_jewish_k9_p4 <- top_terms_jewish_k9_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_jewish_k9
top_terms_jewish_k9_p1
top_terms_jewish_k9_p2
top_terms_jewish_k9_p3
top_terms_jewish_k9_p4



### Figure summary

print(topic_jewish_k9)
print(topic_jewish_k9_p1)
print(topic_jewish_k9_p2)
print(topic_jewish_k9_p3)
print(topic_jewish_k9_p4)







### k = 10

# cast topic models

jewish_lda_k10 <- LDA(tidy_EP_chunks_jewish_dtm, k = 10, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k10 <- tidy(jewish_lda_k10)
jewish_lda_td_k10

# check gamma
jewish_lda_gamma_k10 <- tidy(jewish_lda_k10, matrix = "gamma")


# most common terms

top_terms_jewish_k10 <- jewish_lda_td_k10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k10

# figure

topic_jewish_k10 <- top_terms_jewish_k10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

jewish_lda_k10_p1 <- LDA(tidy_EP_chunks_jewish_dtm_p1, k = 10, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k10_p1 <- tidy(jewish_lda_k10_p1)
jewish_lda_td_k10_p1

# check gamma
jewish_lda_gamma_k10_p1 <- tidy(jewish_lda_k10_p1, matrix = "gamma")


# most common terms

top_terms_jewish_k10_p1 <- jewish_lda_td_k10_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k10_p1

# figure

topic_jewish_k10_p1 <- top_terms_jewish_k10_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

jewish_lda_k10_p2 <- LDA(tidy_EP_chunks_jewish_dtm_p2, k = 10, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k10_p2 <- tidy(jewish_lda_k10_p2)
jewish_lda_td_k10_p2

# check gamma
jewish_lda_gamma_k10_p2 <- tidy(jewish_lda_k10_p2, matrix = "gamma")


# most common terms

top_terms_jewish_k10_p2 <- jewish_lda_td_k10_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k10_p2

# figure

topic_jewish_k10_p2 <- top_terms_jewish_k10_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

jewish_lda_k10_p3 <- LDA(tidy_EP_chunks_jewish_dtm_p3, k = 10, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k10_p3 <- tidy(jewish_lda_k10_p3)
jewish_lda_td_k10_p3

# check gamma
jewish_lda_gamma_k10_p3 <- tidy(jewish_lda_k10_p3, matrix = "gamma")


# most common terms

top_terms_jewish_k10_p3 <- jewish_lda_td_k10_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k10_p3

# figure

topic_jewish_k10_p3 <- top_terms_jewish_k10_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

jewish_lda_k10_p4 <- LDA(tidy_EP_chunks_jewish_dtm_p4, k = 10, control = list(seed = 1234))


# convert back to tidy

jewish_lda_td_k10_p4 <- tidy(jewish_lda_k10_p4)
jewish_lda_td_k10_p4

# check gamma
jewish_lda_gamma_k10_p4 <- tidy(jewish_lda_k10_p4, matrix = "gamma")


# most common terms

top_terms_jewish_k10_p4 <- jewish_lda_td_k10_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_jewish_k10_p4

# figure

topic_jewish_k10_p4 <- top_terms_jewish_k10_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_jewish_k10
top_terms_jewish_k10_p1
top_terms_jewish_k10_p2
top_terms_jewish_k10_p3
top_terms_jewish_k10_p4



### Figure summary

print(topic_jewish_k10)
print(topic_jewish_k10_p1)
print(topic_jewish_k10_p2)
print(topic_jewish_k10_p3)
print(topic_jewish_k10_p4)









############ Topic model by pseudo corpus: catholic



### k = 3

# cast topic models

catholic_lda_k3 <- LDA(tidy_EP_chunks_catholic_dtm, k = 3, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k3 <- tidy(catholic_lda_k3)
catholic_lda_td_k3

# check gamma
catholic_lda_gamma_k3 <- tidy(catholic_lda_k3, matrix = "gamma")


# most common terms

top_terms_catholic_k3 <- catholic_lda_td_k3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k3

# figure

topic_catholic_k3 <- top_terms_catholic_k3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

catholic_lda_k3_p1 <- LDA(tidy_EP_chunks_catholic_dtm_p1, k = 3, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k3_p1 <- tidy(catholic_lda_k3_p1)
catholic_lda_td_k3_p1

# check gamma
catholic_lda_gamma_k3_p1 <- tidy(catholic_lda_k3_p1, matrix = "gamma")


# most common terms

top_terms_catholic_k3_p1 <- catholic_lda_td_k3_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k3_p1

# figure

topic_catholic_k3_p1 <- top_terms_catholic_k3_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

catholic_lda_k3_p2 <- LDA(tidy_EP_chunks_catholic_dtm_p2, k = 3, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k3_p2 <- tidy(catholic_lda_k3_p2)
catholic_lda_td_k3_p2

# check gamma
catholic_lda_gamma_k3_p2 <- tidy(catholic_lda_k3_p2, matrix = "gamma")


# most common terms

top_terms_catholic_k3_p2 <- catholic_lda_td_k3_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k3_p2

# figure

topic_catholic_k3_p2 <- top_terms_catholic_k3_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

catholic_lda_k3_p3 <- LDA(tidy_EP_chunks_catholic_dtm_p3, k = 3, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k3_p3 <- tidy(catholic_lda_k3_p3)
catholic_lda_td_k3_p3

# check gamma
catholic_lda_gamma_k3_p3 <- tidy(catholic_lda_k3_p3, matrix = "gamma")


# most common terms

top_terms_catholic_k3_p3 <- catholic_lda_td_k3_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k3_p3

# figure

topic_catholic_k3_p3 <- top_terms_catholic_k3_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

catholic_lda_k3_p4 <- LDA(tidy_EP_chunks_catholic_dtm_p4, k = 3, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k3_p4 <- tidy(catholic_lda_k3_p4)
catholic_lda_td_k3_p4

# check gamma
catholic_lda_gamma_k3_p4 <- tidy(catholic_lda_k3_p4, matrix = "gamma")


# most common terms

top_terms_catholic_k3_p4 <- catholic_lda_td_k3_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k3_p4

# figure

topic_catholic_k3_p4 <- top_terms_catholic_k3_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_catholic_k3
top_terms_catholic_k3_p1
top_terms_catholic_k3_p2
top_terms_catholic_k3_p3
top_terms_catholic_k3_p4



### Figure summary

print(topic_catholic_k3)
print(topic_catholic_k3_p1)
print(topic_catholic_k3_p2)
print(topic_catholic_k3_p3)
print(topic_catholic_k3_p4)




### k = 4

# cast topic models

catholic_lda_k4 <- LDA(tidy_EP_chunks_catholic_dtm, k = 4, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k4 <- tidy(catholic_lda_k4)
catholic_lda_td_k4

# check gamma
catholic_lda_gamma_k4 <- tidy(catholic_lda_k4, matrix = "gamma")


# most common terms

top_terms_catholic_k4 <- catholic_lda_td_k4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k4

# figure

topic_catholic_k4 <- top_terms_catholic_k4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

catholic_lda_k4_p1 <- LDA(tidy_EP_chunks_catholic_dtm_p1, k = 4, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k4_p1 <- tidy(catholic_lda_k4_p1)
catholic_lda_td_k4_p1

# check gamma
catholic_lda_gamma_k4_p1 <- tidy(catholic_lda_k4_p1, matrix = "gamma")


# most common terms

top_terms_catholic_k4_p1 <- catholic_lda_td_k4_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k4_p1

# figure

topic_catholic_k4_p1 <- top_terms_catholic_k4_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

catholic_lda_k4_p2 <- LDA(tidy_EP_chunks_catholic_dtm_p2, k = 4, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k4_p2 <- tidy(catholic_lda_k4_p2)
catholic_lda_td_k4_p2

# check gamma
catholic_lda_gamma_k4_p2 <- tidy(catholic_lda_k4_p2, matrix = "gamma")


# most common terms

top_terms_catholic_k4_p2 <- catholic_lda_td_k4_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k4_p2

# figure

topic_catholic_k4_p2 <- top_terms_catholic_k4_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

catholic_lda_k4_p3 <- LDA(tidy_EP_chunks_catholic_dtm_p3, k = 4, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k4_p3 <- tidy(catholic_lda_k4_p3)
catholic_lda_td_k4_p3

# check gamma
catholic_lda_gamma_k4_p3 <- tidy(catholic_lda_k4_p3, matrix = "gamma")


# most common terms

top_terms_catholic_k4_p3 <- catholic_lda_td_k4_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k4_p3

# figure

topic_catholic_k4_p3 <- top_terms_catholic_k4_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

catholic_lda_k4_p4 <- LDA(tidy_EP_chunks_catholic_dtm_p4, k = 4, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k4_p4 <- tidy(catholic_lda_k4_p4)
catholic_lda_td_k4_p4

# check gamma
catholic_lda_gamma_k4_p4 <- tidy(catholic_lda_k4_p4, matrix = "gamma")


# most common terms

top_terms_catholic_k4_p4 <- catholic_lda_td_k4_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k4_p4

# figure

topic_catholic_k4_p4 <- top_terms_catholic_k4_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_catholic_k4
top_terms_catholic_k4_p1
top_terms_catholic_k4_p2
top_terms_catholic_k4_p3
top_terms_catholic_k4_p4



### Figure summary

print(topic_catholic_k4)
print(topic_catholic_k4_p1)
print(topic_catholic_k4_p2)
print(topic_catholic_k4_p3)
print(topic_catholic_k4_p4)



### k = 5

# cast topic models

catholic_lda_k5 <- LDA(tidy_EP_chunks_catholic_dtm, k = 5, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k5 <- tidy(catholic_lda_k5)
catholic_lda_td_k5

# check gamma
catholic_lda_gamma_k5 <- tidy(catholic_lda_k5, matrix = "gamma")


# most common terms

top_terms_catholic_k5 <- catholic_lda_td_k5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k5

# figure

topic_catholic_k5 <- top_terms_catholic_k5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

catholic_lda_k5_p1 <- LDA(tidy_EP_chunks_catholic_dtm_p1, k = 5, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k5_p1 <- tidy(catholic_lda_k5_p1)
catholic_lda_td_k5_p1

# check gamma
catholic_lda_gamma_k5_p1 <- tidy(catholic_lda_k5_p1, matrix = "gamma")


# most common terms

top_terms_catholic_k5_p1 <- catholic_lda_td_k5_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k5_p1

# figure

topic_catholic_k5_p1 <- top_terms_catholic_k5_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

catholic_lda_k5_p2 <- LDA(tidy_EP_chunks_catholic_dtm_p2, k = 5, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k5_p2 <- tidy(catholic_lda_k5_p2)
catholic_lda_td_k5_p2

# check gamma
catholic_lda_gamma_k5_p2 <- tidy(catholic_lda_k5_p2, matrix = "gamma")


# most common terms

top_terms_catholic_k5_p2 <- catholic_lda_td_k5_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k5_p2

# figure

topic_catholic_k5_p2 <- top_terms_catholic_k5_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

catholic_lda_k5_p3 <- LDA(tidy_EP_chunks_catholic_dtm_p3, k = 5, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k5_p3 <- tidy(catholic_lda_k5_p3)
catholic_lda_td_k5_p3

# check gamma
catholic_lda_gamma_k5_p3 <- tidy(catholic_lda_k5_p3, matrix = "gamma")


# most common terms

top_terms_catholic_k5_p3 <- catholic_lda_td_k5_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k5_p3

# figure

topic_catholic_k5_p3 <- top_terms_catholic_k5_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

catholic_lda_k5_p4 <- LDA(tidy_EP_chunks_catholic_dtm_p4, k = 5, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k5_p4 <- tidy(catholic_lda_k5_p4)
catholic_lda_td_k5_p4

# check gamma
catholic_lda_gamma_k5_p4 <- tidy(catholic_lda_k5_p4, matrix = "gamma")


# most common terms

top_terms_catholic_k5_p4 <- catholic_lda_td_k5_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k5_p4

# figure

topic_catholic_k5_p4 <- top_terms_catholic_k5_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_catholic_k5
top_terms_catholic_k5_p1
top_terms_catholic_k5_p2
top_terms_catholic_k5_p3
top_terms_catholic_k5_p4



### Figure summary

print(topic_catholic_k5)
print(topic_catholic_k5_p1)
print(topic_catholic_k5_p2)
print(topic_catholic_k5_p3)
print(topic_catholic_k5_p4)







### k = 6

# cast topic models

catholic_lda_k6 <- LDA(tidy_EP_chunks_catholic_dtm, k = 6, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k6 <- tidy(catholic_lda_k6)
catholic_lda_td_k6

# check gamma
catholic_lda_gamma_k6 <- tidy(catholic_lda_k6, matrix = "gamma")


# most common terms

top_terms_catholic_k6 <- catholic_lda_td_k6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k6

# figure

topic_catholic_k6 <- top_terms_catholic_k6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

catholic_lda_k6_p1 <- LDA(tidy_EP_chunks_catholic_dtm_p1, k = 6, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k6_p1 <- tidy(catholic_lda_k6_p1)
catholic_lda_td_k6_p1

# check gamma
catholic_lda_gamma_k6_p1 <- tidy(catholic_lda_k6_p1, matrix = "gamma")


# most common terms

top_terms_catholic_k6_p1 <- catholic_lda_td_k6_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k6_p1

# figure

topic_catholic_k6_p1 <- top_terms_catholic_k6_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

catholic_lda_k6_p2 <- LDA(tidy_EP_chunks_catholic_dtm_p2, k = 6, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k6_p2 <- tidy(catholic_lda_k6_p2)
catholic_lda_td_k6_p2

# check gamma
catholic_lda_gamma_k6_p2 <- tidy(catholic_lda_k6_p2, matrix = "gamma")


# most common terms

top_terms_catholic_k6_p2 <- catholic_lda_td_k6_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k6_p2

# figure

topic_catholic_k6_p2 <- top_terms_catholic_k6_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

catholic_lda_k6_p3 <- LDA(tidy_EP_chunks_catholic_dtm_p3, k = 6, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k6_p3 <- tidy(catholic_lda_k6_p3)
catholic_lda_td_k6_p3

# check gamma
catholic_lda_gamma_k6_p3 <- tidy(catholic_lda_k6_p3, matrix = "gamma")


# most common terms

top_terms_catholic_k6_p3 <- catholic_lda_td_k6_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k6_p3

# figure

topic_catholic_k6_p3 <- top_terms_catholic_k6_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

catholic_lda_k6_p4 <- LDA(tidy_EP_chunks_catholic_dtm_p4, k = 6, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k6_p4 <- tidy(catholic_lda_k6_p4)
catholic_lda_td_k6_p4

# check gamma
catholic_lda_gamma_k6_p4 <- tidy(catholic_lda_k6_p4, matrix = "gamma")


# most common terms

top_terms_catholic_k6_p4 <- catholic_lda_td_k6_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k6_p4

# figure

topic_catholic_k6_p4 <- top_terms_catholic_k6_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_catholic_k6
top_terms_catholic_k6_p1
top_terms_catholic_k6_p2
top_terms_catholic_k6_p3
top_terms_catholic_k6_p4



### Figure summary

print(topic_catholic_k6)
print(topic_catholic_k6_p1)
print(topic_catholic_k6_p2)
print(topic_catholic_k6_p3)
print(topic_catholic_k6_p4)







### k = 7

# cast topic models

catholic_lda_k7 <- LDA(tidy_EP_chunks_catholic_dtm, k = 7, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k7 <- tidy(catholic_lda_k7)
catholic_lda_td_k7

# check gamma
catholic_lda_gamma_k7 <- tidy(catholic_lda_k7, matrix = "gamma")


# most common terms

top_terms_catholic_k7 <- catholic_lda_td_k7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k7

# figure

topic_catholic_k7 <- top_terms_catholic_k7 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

catholic_lda_k7_p1 <- LDA(tidy_EP_chunks_catholic_dtm_p1, k = 7, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k7_p1 <- tidy(catholic_lda_k7_p1)
catholic_lda_td_k7_p1

# check gamma
catholic_lda_gamma_k7_p1 <- tidy(catholic_lda_k7_p1, matrix = "gamma")


# most common terms

top_terms_catholic_k7_p1 <- catholic_lda_td_k7_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k7_p1

# figure

topic_catholic_k7_p1 <- top_terms_catholic_k7_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

catholic_lda_k7_p2 <- LDA(tidy_EP_chunks_catholic_dtm_p2, k = 7, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k7_p2 <- tidy(catholic_lda_k7_p2)
catholic_lda_td_k7_p2

# check gamma
catholic_lda_gamma_k7_p2 <- tidy(catholic_lda_k7_p2, matrix = "gamma")


# most common terms

top_terms_catholic_k7_p2 <- catholic_lda_td_k7_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k7_p2

# figure

topic_catholic_k7_p2 <- top_terms_catholic_k7_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

catholic_lda_k7_p3 <- LDA(tidy_EP_chunks_catholic_dtm_p3, k = 7, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k7_p3 <- tidy(catholic_lda_k7_p3)
catholic_lda_td_k7_p3

# check gamma
catholic_lda_gamma_k7_p3 <- tidy(catholic_lda_k7_p3, matrix = "gamma")


# most common terms

top_terms_catholic_k7_p3 <- catholic_lda_td_k7_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k7_p3

# figure

topic_catholic_k7_p3 <- top_terms_catholic_k7_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

catholic_lda_k7_p4 <- LDA(tidy_EP_chunks_catholic_dtm_p4, k = 7, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k7_p4 <- tidy(catholic_lda_k7_p4)
catholic_lda_td_k7_p4

# check gamma
catholic_lda_gamma_k7_p4 <- tidy(catholic_lda_k7_p4, matrix = "gamma")


# most common terms

top_terms_catholic_k7_p4 <- catholic_lda_td_k7_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k7_p4

# figure

topic_catholic_k7_p4 <- top_terms_catholic_k7_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_catholic_k7
top_terms_catholic_k7_p1
top_terms_catholic_k7_p2
top_terms_catholic_k7_p3
top_terms_catholic_k7_p4



### Figure summary

print(topic_catholic_k7)
print(topic_catholic_k7_p1)
print(topic_catholic_k7_p2)
print(topic_catholic_k7_p3)
print(topic_catholic_k7_p4)







### k = 8

# cast topic models

catholic_lda_k8 <- LDA(tidy_EP_chunks_catholic_dtm, k = 8, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k8 <- tidy(catholic_lda_k8)
catholic_lda_td_k8

# check gamma
catholic_lda_gamma_k8 <- tidy(catholic_lda_k8, matrix = "gamma")


# most common terms

top_terms_catholic_k8 <- catholic_lda_td_k8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k8

# figure

topic_catholic_k8 <- top_terms_catholic_k8 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

catholic_lda_k8_p1 <- LDA(tidy_EP_chunks_catholic_dtm_p1, k = 8, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k8_p1 <- tidy(catholic_lda_k8_p1)
catholic_lda_td_k8_p1

# check gamma
catholic_lda_gamma_k8_p1 <- tidy(catholic_lda_k8_p1, matrix = "gamma")


# most common terms

top_terms_catholic_k8_p1 <- catholic_lda_td_k8_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k8_p1

# figure

topic_catholic_k8_p1 <- top_terms_catholic_k8_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

catholic_lda_k8_p2 <- LDA(tidy_EP_chunks_catholic_dtm_p2, k = 8, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k8_p2 <- tidy(catholic_lda_k8_p2)
catholic_lda_td_k8_p2

# check gamma
catholic_lda_gamma_k8_p2 <- tidy(catholic_lda_k8_p2, matrix = "gamma")


# most common terms

top_terms_catholic_k8_p2 <- catholic_lda_td_k8_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k8_p2

# figure

topic_catholic_k8_p2 <- top_terms_catholic_k8_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

catholic_lda_k8_p3 <- LDA(tidy_EP_chunks_catholic_dtm_p3, k = 8, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k8_p3 <- tidy(catholic_lda_k8_p3)
catholic_lda_td_k8_p3

# check gamma
catholic_lda_gamma_k8_p3 <- tidy(catholic_lda_k8_p3, matrix = "gamma")


# most common terms

top_terms_catholic_k8_p3 <- catholic_lda_td_k8_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k8_p3

# figure

topic_catholic_k8_p3 <- top_terms_catholic_k8_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

catholic_lda_k8_p4 <- LDA(tidy_EP_chunks_catholic_dtm_p4, k = 8, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k8_p4 <- tidy(catholic_lda_k8_p4)
catholic_lda_td_k8_p4

# check gamma
catholic_lda_gamma_k8_p4 <- tidy(catholic_lda_k8_p4, matrix = "gamma")


# most common terms

top_terms_catholic_k8_p4 <- catholic_lda_td_k8_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k8_p4

# figure

topic_catholic_k8_p4 <- top_terms_catholic_k8_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_catholic_k8
top_terms_catholic_k8_p1
top_terms_catholic_k8_p2
top_terms_catholic_k8_p3
top_terms_catholic_k8_p4



### Figure summary

print(topic_catholic_k8)
print(topic_catholic_k8_p1)
print(topic_catholic_k8_p2)
print(topic_catholic_k8_p3)
print(topic_catholic_k8_p4)







### k = 9

# cast topic models

catholic_lda_k9 <- LDA(tidy_EP_chunks_catholic_dtm, k = 9, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k9 <- tidy(catholic_lda_k9)
catholic_lda_td_k9

# check gamma
catholic_lda_gamma_k9 <- tidy(catholic_lda_k9, matrix = "gamma")


# most common terms

top_terms_catholic_k9 <- catholic_lda_td_k9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k9

# figure

topic_catholic_k9 <- top_terms_catholic_k9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

catholic_lda_k9_p1 <- LDA(tidy_EP_chunks_catholic_dtm_p1, k = 9, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k9_p1 <- tidy(catholic_lda_k9_p1)
catholic_lda_td_k9_p1

# check gamma
catholic_lda_gamma_k9_p1 <- tidy(catholic_lda_k9_p1, matrix = "gamma")


# most common terms

top_terms_catholic_k9_p1 <- catholic_lda_td_k9_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k9_p1

# figure

topic_catholic_k9_p1 <- top_terms_catholic_k9_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

catholic_lda_k9_p2 <- LDA(tidy_EP_chunks_catholic_dtm_p2, k = 9, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k9_p2 <- tidy(catholic_lda_k9_p2)
catholic_lda_td_k9_p2

# check gamma
catholic_lda_gamma_k9_p2 <- tidy(catholic_lda_k9_p2, matrix = "gamma")


# most common terms

top_terms_catholic_k9_p2 <- catholic_lda_td_k9_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k9_p2

# figure

topic_catholic_k9_p2 <- top_terms_catholic_k9_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

catholic_lda_k9_p3 <- LDA(tidy_EP_chunks_catholic_dtm_p3, k = 9, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k9_p3 <- tidy(catholic_lda_k9_p3)
catholic_lda_td_k9_p3

# check gamma
catholic_lda_gamma_k9_p3 <- tidy(catholic_lda_k9_p3, matrix = "gamma")


# most common terms

top_terms_catholic_k9_p3 <- catholic_lda_td_k9_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k9_p3

# figure

topic_catholic_k9_p3 <- top_terms_catholic_k9_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

catholic_lda_k9_p4 <- LDA(tidy_EP_chunks_catholic_dtm_p4, k = 9, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k9_p4 <- tidy(catholic_lda_k9_p4)
catholic_lda_td_k9_p4

# check gamma
catholic_lda_gamma_k9_p4 <- tidy(catholic_lda_k9_p4, matrix = "gamma")


# most common terms

top_terms_catholic_k9_p4 <- catholic_lda_td_k9_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k9_p4

# figure

topic_catholic_k9_p4 <- top_terms_catholic_k9_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_catholic_k9
top_terms_catholic_k9_p1
top_terms_catholic_k9_p2
top_terms_catholic_k9_p3
top_terms_catholic_k9_p4



### Figure summary

print(topic_catholic_k9)
print(topic_catholic_k9_p1)
print(topic_catholic_k9_p2)
print(topic_catholic_k9_p3)
print(topic_catholic_k9_p4)







### k = 10

# cast topic models

catholic_lda_k10 <- LDA(tidy_EP_chunks_catholic_dtm, k = 10, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k10 <- tidy(catholic_lda_k10)
catholic_lda_td_k10

# check gamma
catholic_lda_gamma_k10 <- tidy(catholic_lda_k10, matrix = "gamma")


# most common terms

top_terms_catholic_k10 <- catholic_lda_td_k10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k10

# figure

topic_catholic_k10 <- top_terms_catholic_k10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

catholic_lda_k10_p1 <- LDA(tidy_EP_chunks_catholic_dtm_p1, k = 10, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k10_p1 <- tidy(catholic_lda_k10_p1)
catholic_lda_td_k10_p1

# check gamma
catholic_lda_gamma_k10_p1 <- tidy(catholic_lda_k10_p1, matrix = "gamma")


# most common terms

top_terms_catholic_k10_p1 <- catholic_lda_td_k10_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k10_p1

# figure

topic_catholic_k10_p1 <- top_terms_catholic_k10_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

catholic_lda_k10_p2 <- LDA(tidy_EP_chunks_catholic_dtm_p2, k = 10, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k10_p2 <- tidy(catholic_lda_k10_p2)
catholic_lda_td_k10_p2

# check gamma
catholic_lda_gamma_k10_p2 <- tidy(catholic_lda_k10_p2, matrix = "gamma")


# most common terms

top_terms_catholic_k10_p2 <- catholic_lda_td_k10_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k10_p2

# figure

topic_catholic_k10_p2 <- top_terms_catholic_k10_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

catholic_lda_k10_p3 <- LDA(tidy_EP_chunks_catholic_dtm_p3, k = 10, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k10_p3 <- tidy(catholic_lda_k10_p3)
catholic_lda_td_k10_p3

# check gamma
catholic_lda_gamma_k10_p3 <- tidy(catholic_lda_k10_p3, matrix = "gamma")


# most common terms

top_terms_catholic_k10_p3 <- catholic_lda_td_k10_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k10_p3

# figure

topic_catholic_k10_p3 <- top_terms_catholic_k10_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

catholic_lda_k10_p4 <- LDA(tidy_EP_chunks_catholic_dtm_p4, k = 10, control = list(seed = 1234))


# convert back to tidy

catholic_lda_td_k10_p4 <- tidy(catholic_lda_k10_p4)
catholic_lda_td_k10_p4

# check gamma
catholic_lda_gamma_k10_p4 <- tidy(catholic_lda_k10_p4, matrix = "gamma")


# most common terms

top_terms_catholic_k10_p4 <- catholic_lda_td_k10_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_catholic_k10_p4

# figure

topic_catholic_k10_p4 <- top_terms_catholic_k10_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_catholic_k10
top_terms_catholic_k10_p1
top_terms_catholic_k10_p2
top_terms_catholic_k10_p3
top_terms_catholic_k10_p4



### Figure summary

print(topic_catholic_k10)
print(topic_catholic_k10_p1)
print(topic_catholic_k10_p2)
print(topic_catholic_k10_p3)
print(topic_catholic_k10_p4)









############ Topic model by pseudo corpus: irish



### k = 3

# cast topic models

irish_lda_k3 <- LDA(tidy_EP_chunks_irish_dtm, k = 3, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k3 <- tidy(irish_lda_k3)
irish_lda_td_k3

# check gamma
irish_lda_gamma_k3 <- tidy(irish_lda_k3, matrix = "gamma")


# most common terms

top_terms_irish_k3 <- irish_lda_td_k3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k3

# figure

topic_irish_k3 <- top_terms_irish_k3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

irish_lda_k3_p1 <- LDA(tidy_EP_chunks_irish_dtm_p1, k = 3, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k3_p1 <- tidy(irish_lda_k3_p1)
irish_lda_td_k3_p1

# check gamma
irish_lda_gamma_k3_p1 <- tidy(irish_lda_k3_p1, matrix = "gamma")


# most common terms

top_terms_irish_k3_p1 <- irish_lda_td_k3_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k3_p1

# figure

topic_irish_k3_p1 <- top_terms_irish_k3_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

irish_lda_k3_p2 <- LDA(tidy_EP_chunks_irish_dtm_p2, k = 3, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k3_p2 <- tidy(irish_lda_k3_p2)
irish_lda_td_k3_p2

# check gamma
irish_lda_gamma_k3_p2 <- tidy(irish_lda_k3_p2, matrix = "gamma")


# most common terms

top_terms_irish_k3_p2 <- irish_lda_td_k3_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k3_p2

# figure

topic_irish_k3_p2 <- top_terms_irish_k3_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

irish_lda_k3_p3 <- LDA(tidy_EP_chunks_irish_dtm_p3, k = 3, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k3_p3 <- tidy(irish_lda_k3_p3)
irish_lda_td_k3_p3

# check gamma
irish_lda_gamma_k3_p3 <- tidy(irish_lda_k3_p3, matrix = "gamma")


# most common terms

top_terms_irish_k3_p3 <- irish_lda_td_k3_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k3_p3

# figure

topic_irish_k3_p3 <- top_terms_irish_k3_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

irish_lda_k3_p4 <- LDA(tidy_EP_chunks_irish_dtm_p4, k = 3, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k3_p4 <- tidy(irish_lda_k3_p4)
irish_lda_td_k3_p4

# check gamma
irish_lda_gamma_k3_p4 <- tidy(irish_lda_k3_p4, matrix = "gamma")


# most common terms

top_terms_irish_k3_p4 <- irish_lda_td_k3_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k3_p4

# figure

topic_irish_k3_p4 <- top_terms_irish_k3_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_irish_k3
top_terms_irish_k3_p1
top_terms_irish_k3_p2
top_terms_irish_k3_p3
top_terms_irish_k3_p4



### Figure summary

print(topic_irish_k3)
print(topic_irish_k3_p1)
print(topic_irish_k3_p2)
print(topic_irish_k3_p3)
print(topic_irish_k3_p4)




### k = 4

# cast topic models

irish_lda_k4 <- LDA(tidy_EP_chunks_irish_dtm, k = 4, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k4 <- tidy(irish_lda_k4)
irish_lda_td_k4

# check gamma
irish_lda_gamma_k4 <- tidy(irish_lda_k4, matrix = "gamma")


# most common terms

top_terms_irish_k4 <- irish_lda_td_k4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k4

# figure

topic_irish_k4 <- top_terms_irish_k4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

irish_lda_k4_p1 <- LDA(tidy_EP_chunks_irish_dtm_p1, k = 4, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k4_p1 <- tidy(irish_lda_k4_p1)
irish_lda_td_k4_p1

# check gamma
irish_lda_gamma_k4_p1 <- tidy(irish_lda_k4_p1, matrix = "gamma")


# most common terms

top_terms_irish_k4_p1 <- irish_lda_td_k4_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k4_p1

# figure

topic_irish_k4_p1 <- top_terms_irish_k4_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

irish_lda_k4_p2 <- LDA(tidy_EP_chunks_irish_dtm_p2, k = 4, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k4_p2 <- tidy(irish_lda_k4_p2)
irish_lda_td_k4_p2

# check gamma
irish_lda_gamma_k4_p2 <- tidy(irish_lda_k4_p2, matrix = "gamma")


# most common terms

top_terms_irish_k4_p2 <- irish_lda_td_k4_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k4_p2

# figure

topic_irish_k4_p2 <- top_terms_irish_k4_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

irish_lda_k4_p3 <- LDA(tidy_EP_chunks_irish_dtm_p3, k = 4, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k4_p3 <- tidy(irish_lda_k4_p3)
irish_lda_td_k4_p3

# check gamma
irish_lda_gamma_k4_p3 <- tidy(irish_lda_k4_p3, matrix = "gamma")


# most common terms

top_terms_irish_k4_p3 <- irish_lda_td_k4_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k4_p3

# figure

topic_irish_k4_p3 <- top_terms_irish_k4_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

irish_lda_k4_p4 <- LDA(tidy_EP_chunks_irish_dtm_p4, k = 4, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k4_p4 <- tidy(irish_lda_k4_p4)
irish_lda_td_k4_p4

# check gamma
irish_lda_gamma_k4_p4 <- tidy(irish_lda_k4_p4, matrix = "gamma")


# most common terms

top_terms_irish_k4_p4 <- irish_lda_td_k4_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k4_p4

# figure

topic_irish_k4_p4 <- top_terms_irish_k4_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_irish_k4
top_terms_irish_k4_p1
top_terms_irish_k4_p2
top_terms_irish_k4_p3
top_terms_irish_k4_p4



### Figure summary

print(topic_irish_k4)
print(topic_irish_k4_p1)
print(topic_irish_k4_p2)
print(topic_irish_k4_p3)
print(topic_irish_k4_p4)



### k = 5

# cast topic models

irish_lda_k5 <- LDA(tidy_EP_chunks_irish_dtm, k = 5, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k5 <- tidy(irish_lda_k5)
irish_lda_td_k5

# check gamma
irish_lda_gamma_k5 <- tidy(irish_lda_k5, matrix = "gamma")


# most common terms

top_terms_irish_k5 <- irish_lda_td_k5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k5

# figure

topic_irish_k5 <- top_terms_irish_k5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

irish_lda_k5_p1 <- LDA(tidy_EP_chunks_irish_dtm_p1, k = 5, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k5_p1 <- tidy(irish_lda_k5_p1)
irish_lda_td_k5_p1

# check gamma
irish_lda_gamma_k5_p1 <- tidy(irish_lda_k5_p1, matrix = "gamma")


# most common terms

top_terms_irish_k5_p1 <- irish_lda_td_k5_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k5_p1

# figure

topic_irish_k5_p1 <- top_terms_irish_k5_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

irish_lda_k5_p2 <- LDA(tidy_EP_chunks_irish_dtm_p2, k = 5, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k5_p2 <- tidy(irish_lda_k5_p2)
irish_lda_td_k5_p2

# check gamma
irish_lda_gamma_k5_p2 <- tidy(irish_lda_k5_p2, matrix = "gamma")


# most common terms

top_terms_irish_k5_p2 <- irish_lda_td_k5_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k5_p2

# figure

topic_irish_k5_p2 <- top_terms_irish_k5_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

irish_lda_k5_p3 <- LDA(tidy_EP_chunks_irish_dtm_p3, k = 5, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k5_p3 <- tidy(irish_lda_k5_p3)
irish_lda_td_k5_p3

# check gamma
irish_lda_gamma_k5_p3 <- tidy(irish_lda_k5_p3, matrix = "gamma")


# most common terms

top_terms_irish_k5_p3 <- irish_lda_td_k5_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k5_p3

# figure

topic_irish_k5_p3 <- top_terms_irish_k5_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

irish_lda_k5_p4 <- LDA(tidy_EP_chunks_irish_dtm_p4, k = 5, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k5_p4 <- tidy(irish_lda_k5_p4)
irish_lda_td_k5_p4

# check gamma
irish_lda_gamma_k5_p4 <- tidy(irish_lda_k5_p4, matrix = "gamma")


# most common terms

top_terms_irish_k5_p4 <- irish_lda_td_k5_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k5_p4

# figure

topic_irish_k5_p4 <- top_terms_irish_k5_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_irish_k5
top_terms_irish_k5_p1
top_terms_irish_k5_p2
top_terms_irish_k5_p3
top_terms_irish_k5_p4



### Figure summary

print(topic_irish_k5)
print(topic_irish_k5_p1)
print(topic_irish_k5_p2)
print(topic_irish_k5_p3)
print(topic_irish_k5_p4)







### k = 6

# cast topic models

irish_lda_k6 <- LDA(tidy_EP_chunks_irish_dtm, k = 6, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k6 <- tidy(irish_lda_k6)
irish_lda_td_k6

# check gamma
irish_lda_gamma_k6 <- tidy(irish_lda_k6, matrix = "gamma")


# most common terms

top_terms_irish_k6 <- irish_lda_td_k6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k6

# figure

topic_irish_k6 <- top_terms_irish_k6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

irish_lda_k6_p1 <- LDA(tidy_EP_chunks_irish_dtm_p1, k = 6, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k6_p1 <- tidy(irish_lda_k6_p1)
irish_lda_td_k6_p1

# check gamma
irish_lda_gamma_k6_p1 <- tidy(irish_lda_k6_p1, matrix = "gamma")


# most common terms

top_terms_irish_k6_p1 <- irish_lda_td_k6_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k6_p1

# figure

topic_irish_k6_p1 <- top_terms_irish_k6_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

irish_lda_k6_p2 <- LDA(tidy_EP_chunks_irish_dtm_p2, k = 6, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k6_p2 <- tidy(irish_lda_k6_p2)
irish_lda_td_k6_p2

# check gamma
irish_lda_gamma_k6_p2 <- tidy(irish_lda_k6_p2, matrix = "gamma")


# most common terms

top_terms_irish_k6_p2 <- irish_lda_td_k6_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k6_p2

# figure

topic_irish_k6_p2 <- top_terms_irish_k6_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

irish_lda_k6_p3 <- LDA(tidy_EP_chunks_irish_dtm_p3, k = 6, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k6_p3 <- tidy(irish_lda_k6_p3)
irish_lda_td_k6_p3

# check gamma
irish_lda_gamma_k6_p3 <- tidy(irish_lda_k6_p3, matrix = "gamma")


# most common terms

top_terms_irish_k6_p3 <- irish_lda_td_k6_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k6_p3

# figure

topic_irish_k6_p3 <- top_terms_irish_k6_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

irish_lda_k6_p4 <- LDA(tidy_EP_chunks_irish_dtm_p4, k = 6, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k6_p4 <- tidy(irish_lda_k6_p4)
irish_lda_td_k6_p4

# check gamma
irish_lda_gamma_k6_p4 <- tidy(irish_lda_k6_p4, matrix = "gamma")


# most common terms

top_terms_irish_k6_p4 <- irish_lda_td_k6_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k6_p4

# figure

topic_irish_k6_p4 <- top_terms_irish_k6_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_irish_k6
top_terms_irish_k6_p1
top_terms_irish_k6_p2
top_terms_irish_k6_p3
top_terms_irish_k6_p4



### Figure summary

print(topic_irish_k6)
print(topic_irish_k6_p1)
print(topic_irish_k6_p2)
print(topic_irish_k6_p3)
print(topic_irish_k6_p4)







### k = 7

# cast topic models

irish_lda_k7 <- LDA(tidy_EP_chunks_irish_dtm, k = 7, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k7 <- tidy(irish_lda_k7)
irish_lda_td_k7

# check gamma
irish_lda_gamma_k7 <- tidy(irish_lda_k7, matrix = "gamma")


# most common terms

top_terms_irish_k7 <- irish_lda_td_k7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k7

# figure

topic_irish_k7 <- top_terms_irish_k7 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

irish_lda_k7_p1 <- LDA(tidy_EP_chunks_irish_dtm_p1, k = 7, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k7_p1 <- tidy(irish_lda_k7_p1)
irish_lda_td_k7_p1

# check gamma
irish_lda_gamma_k7_p1 <- tidy(irish_lda_k7_p1, matrix = "gamma")


# most common terms

top_terms_irish_k7_p1 <- irish_lda_td_k7_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k7_p1

# figure

topic_irish_k7_p1 <- top_terms_irish_k7_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

irish_lda_k7_p2 <- LDA(tidy_EP_chunks_irish_dtm_p2, k = 7, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k7_p2 <- tidy(irish_lda_k7_p2)
irish_lda_td_k7_p2

# check gamma
irish_lda_gamma_k7_p2 <- tidy(irish_lda_k7_p2, matrix = "gamma")


# most common terms

top_terms_irish_k7_p2 <- irish_lda_td_k7_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k7_p2

# figure

topic_irish_k7_p2 <- top_terms_irish_k7_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

irish_lda_k7_p3 <- LDA(tidy_EP_chunks_irish_dtm_p3, k = 7, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k7_p3 <- tidy(irish_lda_k7_p3)
irish_lda_td_k7_p3

# check gamma
irish_lda_gamma_k7_p3 <- tidy(irish_lda_k7_p3, matrix = "gamma")


# most common terms

top_terms_irish_k7_p3 <- irish_lda_td_k7_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k7_p3

# figure

topic_irish_k7_p3 <- top_terms_irish_k7_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

irish_lda_k7_p4 <- LDA(tidy_EP_chunks_irish_dtm_p4, k = 7, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k7_p4 <- tidy(irish_lda_k7_p4)
irish_lda_td_k7_p4

# check gamma
irish_lda_gamma_k7_p4 <- tidy(irish_lda_k7_p4, matrix = "gamma")


# most common terms

top_terms_irish_k7_p4 <- irish_lda_td_k7_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k7_p4

# figure

topic_irish_k7_p4 <- top_terms_irish_k7_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_irish_k7
top_terms_irish_k7_p1
top_terms_irish_k7_p2
top_terms_irish_k7_p3
top_terms_irish_k7_p4



### Figure summary

print(topic_irish_k7)
print(topic_irish_k7_p1)
print(topic_irish_k7_p2)
print(topic_irish_k7_p3)
print(topic_irish_k7_p4)







### k = 8

# cast topic models

irish_lda_k8 <- LDA(tidy_EP_chunks_irish_dtm, k = 8, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k8 <- tidy(irish_lda_k8)
irish_lda_td_k8

# check gamma
irish_lda_gamma_k8 <- tidy(irish_lda_k8, matrix = "gamma")


# most common terms

top_terms_irish_k8 <- irish_lda_td_k8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k8

# figure

topic_irish_k8 <- top_terms_irish_k8 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

irish_lda_k8_p1 <- LDA(tidy_EP_chunks_irish_dtm_p1, k = 8, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k8_p1 <- tidy(irish_lda_k8_p1)
irish_lda_td_k8_p1

# check gamma
irish_lda_gamma_k8_p1 <- tidy(irish_lda_k8_p1, matrix = "gamma")


# most common terms

top_terms_irish_k8_p1 <- irish_lda_td_k8_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k8_p1

# figure

topic_irish_k8_p1 <- top_terms_irish_k8_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

irish_lda_k8_p2 <- LDA(tidy_EP_chunks_irish_dtm_p2, k = 8, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k8_p2 <- tidy(irish_lda_k8_p2)
irish_lda_td_k8_p2

# check gamma
irish_lda_gamma_k8_p2 <- tidy(irish_lda_k8_p2, matrix = "gamma")


# most common terms

top_terms_irish_k8_p2 <- irish_lda_td_k8_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k8_p2

# figure

topic_irish_k8_p2 <- top_terms_irish_k8_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

irish_lda_k8_p3 <- LDA(tidy_EP_chunks_irish_dtm_p3, k = 8, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k8_p3 <- tidy(irish_lda_k8_p3)
irish_lda_td_k8_p3

# check gamma
irish_lda_gamma_k8_p3 <- tidy(irish_lda_k8_p3, matrix = "gamma")


# most common terms

top_terms_irish_k8_p3 <- irish_lda_td_k8_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k8_p3

# figure

topic_irish_k8_p3 <- top_terms_irish_k8_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

irish_lda_k8_p4 <- LDA(tidy_EP_chunks_irish_dtm_p4, k = 8, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k8_p4 <- tidy(irish_lda_k8_p4)
irish_lda_td_k8_p4

# check gamma
irish_lda_gamma_k8_p4 <- tidy(irish_lda_k8_p4, matrix = "gamma")


# most common terms

top_terms_irish_k8_p4 <- irish_lda_td_k8_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k8_p4

# figure

topic_irish_k8_p4 <- top_terms_irish_k8_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_irish_k8
top_terms_irish_k8_p1
top_terms_irish_k8_p2
top_terms_irish_k8_p3
top_terms_irish_k8_p4



### Figure summary

print(topic_irish_k8)
print(topic_irish_k8_p1)
print(topic_irish_k8_p2)
print(topic_irish_k8_p3)
print(topic_irish_k8_p4)







### k = 9

# cast topic models

irish_lda_k9 <- LDA(tidy_EP_chunks_irish_dtm, k = 9, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k9 <- tidy(irish_lda_k9)
irish_lda_td_k9

# check gamma
irish_lda_gamma_k9 <- tidy(irish_lda_k9, matrix = "gamma")


# most common terms

top_terms_irish_k9 <- irish_lda_td_k9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k9

# figure

topic_irish_k9 <- top_terms_irish_k9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

irish_lda_k9_p1 <- LDA(tidy_EP_chunks_irish_dtm_p1, k = 9, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k9_p1 <- tidy(irish_lda_k9_p1)
irish_lda_td_k9_p1

# check gamma
irish_lda_gamma_k9_p1 <- tidy(irish_lda_k9_p1, matrix = "gamma")


# most common terms

top_terms_irish_k9_p1 <- irish_lda_td_k9_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k9_p1

# figure

topic_irish_k9_p1 <- top_terms_irish_k9_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

irish_lda_k9_p2 <- LDA(tidy_EP_chunks_irish_dtm_p2, k = 9, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k9_p2 <- tidy(irish_lda_k9_p2)
irish_lda_td_k9_p2

# check gamma
irish_lda_gamma_k9_p2 <- tidy(irish_lda_k9_p2, matrix = "gamma")


# most common terms

top_terms_irish_k9_p2 <- irish_lda_td_k9_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k9_p2

# figure

topic_irish_k9_p2 <- top_terms_irish_k9_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

irish_lda_k9_p3 <- LDA(tidy_EP_chunks_irish_dtm_p3, k = 9, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k9_p3 <- tidy(irish_lda_k9_p3)
irish_lda_td_k9_p3

# check gamma
irish_lda_gamma_k9_p3 <- tidy(irish_lda_k9_p3, matrix = "gamma")


# most common terms

top_terms_irish_k9_p3 <- irish_lda_td_k9_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k9_p3

# figure

topic_irish_k9_p3 <- top_terms_irish_k9_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

irish_lda_k9_p4 <- LDA(tidy_EP_chunks_irish_dtm_p4, k = 9, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k9_p4 <- tidy(irish_lda_k9_p4)
irish_lda_td_k9_p4

# check gamma
irish_lda_gamma_k9_p4 <- tidy(irish_lda_k9_p4, matrix = "gamma")


# most common terms

top_terms_irish_k9_p4 <- irish_lda_td_k9_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k9_p4

# figure

topic_irish_k9_p4 <- top_terms_irish_k9_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_irish_k9
top_terms_irish_k9_p1
top_terms_irish_k9_p2
top_terms_irish_k9_p3
top_terms_irish_k9_p4



### Figure summary

print(topic_irish_k9)
print(topic_irish_k9_p1)
print(topic_irish_k9_p2)
print(topic_irish_k9_p3)
print(topic_irish_k9_p4)







### k = 10

# cast topic models

irish_lda_k10 <- LDA(tidy_EP_chunks_irish_dtm, k = 10, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k10 <- tidy(irish_lda_k10)
irish_lda_td_k10

# check gamma
irish_lda_gamma_k10 <- tidy(irish_lda_k10, matrix = "gamma")


# most common terms

top_terms_irish_k10 <- irish_lda_td_k10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k10

# figure

topic_irish_k10 <- top_terms_irish_k10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

irish_lda_k10_p1 <- LDA(tidy_EP_chunks_irish_dtm_p1, k = 10, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k10_p1 <- tidy(irish_lda_k10_p1)
irish_lda_td_k10_p1

# check gamma
irish_lda_gamma_k10_p1 <- tidy(irish_lda_k10_p1, matrix = "gamma")


# most common terms

top_terms_irish_k10_p1 <- irish_lda_td_k10_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k10_p1

# figure

topic_irish_k10_p1 <- top_terms_irish_k10_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

irish_lda_k10_p2 <- LDA(tidy_EP_chunks_irish_dtm_p2, k = 10, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k10_p2 <- tidy(irish_lda_k10_p2)
irish_lda_td_k10_p2

# check gamma
irish_lda_gamma_k10_p2 <- tidy(irish_lda_k10_p2, matrix = "gamma")


# most common terms

top_terms_irish_k10_p2 <- irish_lda_td_k10_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k10_p2

# figure

topic_irish_k10_p2 <- top_terms_irish_k10_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

irish_lda_k10_p3 <- LDA(tidy_EP_chunks_irish_dtm_p3, k = 10, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k10_p3 <- tidy(irish_lda_k10_p3)
irish_lda_td_k10_p3

# check gamma
irish_lda_gamma_k10_p3 <- tidy(irish_lda_k10_p3, matrix = "gamma")


# most common terms

top_terms_irish_k10_p3 <- irish_lda_td_k10_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k10_p3

# figure

topic_irish_k10_p3 <- top_terms_irish_k10_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

irish_lda_k10_p4 <- LDA(tidy_EP_chunks_irish_dtm_p4, k = 10, control = list(seed = 1234))


# convert back to tidy

irish_lda_td_k10_p4 <- tidy(irish_lda_k10_p4)
irish_lda_td_k10_p4

# check gamma
irish_lda_gamma_k10_p4 <- tidy(irish_lda_k10_p4, matrix = "gamma")


# most common terms

top_terms_irish_k10_p4 <- irish_lda_td_k10_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_irish_k10_p4

# figure

topic_irish_k10_p4 <- top_terms_irish_k10_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_irish_k10
top_terms_irish_k10_p1
top_terms_irish_k10_p2
top_terms_irish_k10_p3
top_terms_irish_k10_p4



### Figure summary

print(topic_irish_k10)
print(topic_irish_k10_p1)
print(topic_irish_k10_p2)
print(topic_irish_k10_p3)
print(topic_irish_k10_p4)









############ Topic model by pseudo corpus: cuban



### k = 3

# cast topic models

cuban_lda_k3 <- LDA(tidy_EP_chunks_cuban_dtm, k = 3, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k3 <- tidy(cuban_lda_k3)
cuban_lda_td_k3

# check gamma
cuban_lda_gamma_k3 <- tidy(cuban_lda_k3, matrix = "gamma")


# most common terms

top_terms_cuban_k3 <- cuban_lda_td_k3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k3

# figure

topic_cuban_k3 <- top_terms_cuban_k3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

cuban_lda_k3_p1 <- LDA(tidy_EP_chunks_cuban_dtm_p1, k = 3, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k3_p1 <- tidy(cuban_lda_k3_p1)
cuban_lda_td_k3_p1

# check gamma
cuban_lda_gamma_k3_p1 <- tidy(cuban_lda_k3_p1, matrix = "gamma")


# most common terms

top_terms_cuban_k3_p1 <- cuban_lda_td_k3_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k3_p1

# figure

topic_cuban_k3_p1 <- top_terms_cuban_k3_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

cuban_lda_k3_p2 <- LDA(tidy_EP_chunks_cuban_dtm_p2, k = 3, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k3_p2 <- tidy(cuban_lda_k3_p2)
cuban_lda_td_k3_p2

# check gamma
cuban_lda_gamma_k3_p2 <- tidy(cuban_lda_k3_p2, matrix = "gamma")


# most common terms

top_terms_cuban_k3_p2 <- cuban_lda_td_k3_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k3_p2

# figure

topic_cuban_k3_p2 <- top_terms_cuban_k3_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

cuban_lda_k3_p3 <- LDA(tidy_EP_chunks_cuban_dtm_p3, k = 3, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k3_p3 <- tidy(cuban_lda_k3_p3)
cuban_lda_td_k3_p3

# check gamma
cuban_lda_gamma_k3_p3 <- tidy(cuban_lda_k3_p3, matrix = "gamma")


# most common terms

top_terms_cuban_k3_p3 <- cuban_lda_td_k3_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k3_p3

# figure

topic_cuban_k3_p3 <- top_terms_cuban_k3_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

cuban_lda_k3_p4 <- LDA(tidy_EP_chunks_cuban_dtm_p4, k = 3, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k3_p4 <- tidy(cuban_lda_k3_p4)
cuban_lda_td_k3_p4

# check gamma
cuban_lda_gamma_k3_p4 <- tidy(cuban_lda_k3_p4, matrix = "gamma")


# most common terms

top_terms_cuban_k3_p4 <- cuban_lda_td_k3_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k3_p4

# figure

topic_cuban_k3_p4 <- top_terms_cuban_k3_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_cuban_k3
top_terms_cuban_k3_p1
top_terms_cuban_k3_p2
top_terms_cuban_k3_p3
top_terms_cuban_k3_p4



### Figure summary

print(topic_cuban_k3)
print(topic_cuban_k3_p1)
print(topic_cuban_k3_p2)
print(topic_cuban_k3_p3)
print(topic_cuban_k3_p4)




### k = 4

# cast topic models

cuban_lda_k4 <- LDA(tidy_EP_chunks_cuban_dtm, k = 4, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k4 <- tidy(cuban_lda_k4)
cuban_lda_td_k4

# check gamma
cuban_lda_gamma_k4 <- tidy(cuban_lda_k4, matrix = "gamma")


# most common terms

top_terms_cuban_k4 <- cuban_lda_td_k4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k4

# figure

topic_cuban_k4 <- top_terms_cuban_k4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

cuban_lda_k4_p1 <- LDA(tidy_EP_chunks_cuban_dtm_p1, k = 4, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k4_p1 <- tidy(cuban_lda_k4_p1)
cuban_lda_td_k4_p1

# check gamma
cuban_lda_gamma_k4_p1 <- tidy(cuban_lda_k4_p1, matrix = "gamma")


# most common terms

top_terms_cuban_k4_p1 <- cuban_lda_td_k4_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k4_p1

# figure

topic_cuban_k4_p1 <- top_terms_cuban_k4_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

cuban_lda_k4_p2 <- LDA(tidy_EP_chunks_cuban_dtm_p2, k = 4, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k4_p2 <- tidy(cuban_lda_k4_p2)
cuban_lda_td_k4_p2

# check gamma
cuban_lda_gamma_k4_p2 <- tidy(cuban_lda_k4_p2, matrix = "gamma")


# most common terms

top_terms_cuban_k4_p2 <- cuban_lda_td_k4_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k4_p2

# figure

topic_cuban_k4_p2 <- top_terms_cuban_k4_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

cuban_lda_k4_p3 <- LDA(tidy_EP_chunks_cuban_dtm_p3, k = 4, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k4_p3 <- tidy(cuban_lda_k4_p3)
cuban_lda_td_k4_p3

# check gamma
cuban_lda_gamma_k4_p3 <- tidy(cuban_lda_k4_p3, matrix = "gamma")


# most common terms

top_terms_cuban_k4_p3 <- cuban_lda_td_k4_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k4_p3

# figure

topic_cuban_k4_p3 <- top_terms_cuban_k4_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

cuban_lda_k4_p4 <- LDA(tidy_EP_chunks_cuban_dtm_p4, k = 4, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k4_p4 <- tidy(cuban_lda_k4_p4)
cuban_lda_td_k4_p4

# check gamma
cuban_lda_gamma_k4_p4 <- tidy(cuban_lda_k4_p4, matrix = "gamma")


# most common terms

top_terms_cuban_k4_p4 <- cuban_lda_td_k4_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k4_p4

# figure

topic_cuban_k4_p4 <- top_terms_cuban_k4_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_cuban_k4
top_terms_cuban_k4_p1
top_terms_cuban_k4_p2
top_terms_cuban_k4_p3
top_terms_cuban_k4_p4



### Figure summary

print(topic_cuban_k4)
print(topic_cuban_k4_p1)
print(topic_cuban_k4_p2)
print(topic_cuban_k4_p3)
print(topic_cuban_k4_p4)



### k = 5

# cast topic models

cuban_lda_k5 <- LDA(tidy_EP_chunks_cuban_dtm, k = 5, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k5 <- tidy(cuban_lda_k5)
cuban_lda_td_k5

# check gamma
cuban_lda_gamma_k5 <- tidy(cuban_lda_k5, matrix = "gamma")


# most common terms

top_terms_cuban_k5 <- cuban_lda_td_k5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k5

# figure

topic_cuban_k5 <- top_terms_cuban_k5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

cuban_lda_k5_p1 <- LDA(tidy_EP_chunks_cuban_dtm_p1, k = 5, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k5_p1 <- tidy(cuban_lda_k5_p1)
cuban_lda_td_k5_p1

# check gamma
cuban_lda_gamma_k5_p1 <- tidy(cuban_lda_k5_p1, matrix = "gamma")


# most common terms

top_terms_cuban_k5_p1 <- cuban_lda_td_k5_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k5_p1

# figure

topic_cuban_k5_p1 <- top_terms_cuban_k5_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

cuban_lda_k5_p2 <- LDA(tidy_EP_chunks_cuban_dtm_p2, k = 5, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k5_p2 <- tidy(cuban_lda_k5_p2)
cuban_lda_td_k5_p2

# check gamma
cuban_lda_gamma_k5_p2 <- tidy(cuban_lda_k5_p2, matrix = "gamma")


# most common terms

top_terms_cuban_k5_p2 <- cuban_lda_td_k5_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k5_p2

# figure

topic_cuban_k5_p2 <- top_terms_cuban_k5_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

cuban_lda_k5_p3 <- LDA(tidy_EP_chunks_cuban_dtm_p3, k = 5, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k5_p3 <- tidy(cuban_lda_k5_p3)
cuban_lda_td_k5_p3

# check gamma
cuban_lda_gamma_k5_p3 <- tidy(cuban_lda_k5_p3, matrix = "gamma")


# most common terms

top_terms_cuban_k5_p3 <- cuban_lda_td_k5_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k5_p3

# figure

topic_cuban_k5_p3 <- top_terms_cuban_k5_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

cuban_lda_k5_p4 <- LDA(tidy_EP_chunks_cuban_dtm_p4, k = 5, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k5_p4 <- tidy(cuban_lda_k5_p4)
cuban_lda_td_k5_p4

# check gamma
cuban_lda_gamma_k5_p4 <- tidy(cuban_lda_k5_p4, matrix = "gamma")


# most common terms

top_terms_cuban_k5_p4 <- cuban_lda_td_k5_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k5_p4

# figure

topic_cuban_k5_p4 <- top_terms_cuban_k5_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_cuban_k5
top_terms_cuban_k5_p1
top_terms_cuban_k5_p2
top_terms_cuban_k5_p3
top_terms_cuban_k5_p4



### Figure summary

print(topic_cuban_k5)
print(topic_cuban_k5_p1)
print(topic_cuban_k5_p2)
print(topic_cuban_k5_p3)
print(topic_cuban_k5_p4)







### k = 6

# cast topic models

cuban_lda_k6 <- LDA(tidy_EP_chunks_cuban_dtm, k = 6, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k6 <- tidy(cuban_lda_k6)
cuban_lda_td_k6

# check gamma
cuban_lda_gamma_k6 <- tidy(cuban_lda_k6, matrix = "gamma")


# most common terms

top_terms_cuban_k6 <- cuban_lda_td_k6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k6

# figure

topic_cuban_k6 <- top_terms_cuban_k6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

cuban_lda_k6_p1 <- LDA(tidy_EP_chunks_cuban_dtm_p1, k = 6, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k6_p1 <- tidy(cuban_lda_k6_p1)
cuban_lda_td_k6_p1

# check gamma
cuban_lda_gamma_k6_p1 <- tidy(cuban_lda_k6_p1, matrix = "gamma")


# most common terms

top_terms_cuban_k6_p1 <- cuban_lda_td_k6_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k6_p1

# figure

topic_cuban_k6_p1 <- top_terms_cuban_k6_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

cuban_lda_k6_p2 <- LDA(tidy_EP_chunks_cuban_dtm_p2, k = 6, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k6_p2 <- tidy(cuban_lda_k6_p2)
cuban_lda_td_k6_p2

# check gamma
cuban_lda_gamma_k6_p2 <- tidy(cuban_lda_k6_p2, matrix = "gamma")


# most common terms

top_terms_cuban_k6_p2 <- cuban_lda_td_k6_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k6_p2

# figure

topic_cuban_k6_p2 <- top_terms_cuban_k6_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

cuban_lda_k6_p3 <- LDA(tidy_EP_chunks_cuban_dtm_p3, k = 6, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k6_p3 <- tidy(cuban_lda_k6_p3)
cuban_lda_td_k6_p3

# check gamma
cuban_lda_gamma_k6_p3 <- tidy(cuban_lda_k6_p3, matrix = "gamma")


# most common terms

top_terms_cuban_k6_p3 <- cuban_lda_td_k6_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k6_p3

# figure

topic_cuban_k6_p3 <- top_terms_cuban_k6_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

cuban_lda_k6_p4 <- LDA(tidy_EP_chunks_cuban_dtm_p4, k = 6, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k6_p4 <- tidy(cuban_lda_k6_p4)
cuban_lda_td_k6_p4

# check gamma
cuban_lda_gamma_k6_p4 <- tidy(cuban_lda_k6_p4, matrix = "gamma")


# most common terms

top_terms_cuban_k6_p4 <- cuban_lda_td_k6_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k6_p4

# figure

topic_cuban_k6_p4 <- top_terms_cuban_k6_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_cuban_k6
top_terms_cuban_k6_p1
top_terms_cuban_k6_p2
top_terms_cuban_k6_p3
top_terms_cuban_k6_p4



### Figure summary

print(topic_cuban_k6)
print(topic_cuban_k6_p1)
print(topic_cuban_k6_p2)
print(topic_cuban_k6_p3)
print(topic_cuban_k6_p4)







### k = 7

# cast topic models

cuban_lda_k7 <- LDA(tidy_EP_chunks_cuban_dtm, k = 7, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k7 <- tidy(cuban_lda_k7)
cuban_lda_td_k7

# check gamma
cuban_lda_gamma_k7 <- tidy(cuban_lda_k7, matrix = "gamma")


# most common terms

top_terms_cuban_k7 <- cuban_lda_td_k7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k7

# figure

topic_cuban_k7 <- top_terms_cuban_k7 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

cuban_lda_k7_p1 <- LDA(tidy_EP_chunks_cuban_dtm_p1, k = 7, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k7_p1 <- tidy(cuban_lda_k7_p1)
cuban_lda_td_k7_p1

# check gamma
cuban_lda_gamma_k7_p1 <- tidy(cuban_lda_k7_p1, matrix = "gamma")


# most common terms

top_terms_cuban_k7_p1 <- cuban_lda_td_k7_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k7_p1

# figure

topic_cuban_k7_p1 <- top_terms_cuban_k7_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

cuban_lda_k7_p2 <- LDA(tidy_EP_chunks_cuban_dtm_p2, k = 7, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k7_p2 <- tidy(cuban_lda_k7_p2)
cuban_lda_td_k7_p2

# check gamma
cuban_lda_gamma_k7_p2 <- tidy(cuban_lda_k7_p2, matrix = "gamma")


# most common terms

top_terms_cuban_k7_p2 <- cuban_lda_td_k7_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k7_p2

# figure

topic_cuban_k7_p2 <- top_terms_cuban_k7_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

cuban_lda_k7_p3 <- LDA(tidy_EP_chunks_cuban_dtm_p3, k = 7, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k7_p3 <- tidy(cuban_lda_k7_p3)
cuban_lda_td_k7_p3

# check gamma
cuban_lda_gamma_k7_p3 <- tidy(cuban_lda_k7_p3, matrix = "gamma")


# most common terms

top_terms_cuban_k7_p3 <- cuban_lda_td_k7_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k7_p3

# figure

topic_cuban_k7_p3 <- top_terms_cuban_k7_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

cuban_lda_k7_p4 <- LDA(tidy_EP_chunks_cuban_dtm_p4, k = 7, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k7_p4 <- tidy(cuban_lda_k7_p4)
cuban_lda_td_k7_p4

# check gamma
cuban_lda_gamma_k7_p4 <- tidy(cuban_lda_k7_p4, matrix = "gamma")


# most common terms

top_terms_cuban_k7_p4 <- cuban_lda_td_k7_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k7_p4

# figure

topic_cuban_k7_p4 <- top_terms_cuban_k7_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_cuban_k7
top_terms_cuban_k7_p1
top_terms_cuban_k7_p2
top_terms_cuban_k7_p3
top_terms_cuban_k7_p4



### Figure summary

print(topic_cuban_k7)
print(topic_cuban_k7_p1)
print(topic_cuban_k7_p2)
print(topic_cuban_k7_p3)
print(topic_cuban_k7_p4)







### k = 8

# cast topic models

cuban_lda_k8 <- LDA(tidy_EP_chunks_cuban_dtm, k = 8, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k8 <- tidy(cuban_lda_k8)
cuban_lda_td_k8

# check gamma
cuban_lda_gamma_k8 <- tidy(cuban_lda_k8, matrix = "gamma")


# most common terms

top_terms_cuban_k8 <- cuban_lda_td_k8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k8

# figure

topic_cuban_k8 <- top_terms_cuban_k8 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

cuban_lda_k8_p1 <- LDA(tidy_EP_chunks_cuban_dtm_p1, k = 8, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k8_p1 <- tidy(cuban_lda_k8_p1)
cuban_lda_td_k8_p1

# check gamma
cuban_lda_gamma_k8_p1 <- tidy(cuban_lda_k8_p1, matrix = "gamma")


# most common terms

top_terms_cuban_k8_p1 <- cuban_lda_td_k8_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k8_p1

# figure

topic_cuban_k8_p1 <- top_terms_cuban_k8_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

cuban_lda_k8_p2 <- LDA(tidy_EP_chunks_cuban_dtm_p2, k = 8, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k8_p2 <- tidy(cuban_lda_k8_p2)
cuban_lda_td_k8_p2

# check gamma
cuban_lda_gamma_k8_p2 <- tidy(cuban_lda_k8_p2, matrix = "gamma")


# most common terms

top_terms_cuban_k8_p2 <- cuban_lda_td_k8_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k8_p2

# figure

topic_cuban_k8_p2 <- top_terms_cuban_k8_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

cuban_lda_k8_p3 <- LDA(tidy_EP_chunks_cuban_dtm_p3, k = 8, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k8_p3 <- tidy(cuban_lda_k8_p3)
cuban_lda_td_k8_p3

# check gamma
cuban_lda_gamma_k8_p3 <- tidy(cuban_lda_k8_p3, matrix = "gamma")


# most common terms

top_terms_cuban_k8_p3 <- cuban_lda_td_k8_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k8_p3

# figure

topic_cuban_k8_p3 <- top_terms_cuban_k8_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

cuban_lda_k8_p4 <- LDA(tidy_EP_chunks_cuban_dtm_p4, k = 8, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k8_p4 <- tidy(cuban_lda_k8_p4)
cuban_lda_td_k8_p4

# check gamma
cuban_lda_gamma_k8_p4 <- tidy(cuban_lda_k8_p4, matrix = "gamma")


# most common terms

top_terms_cuban_k8_p4 <- cuban_lda_td_k8_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k8_p4

# figure

topic_cuban_k8_p4 <- top_terms_cuban_k8_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_cuban_k8
top_terms_cuban_k8_p1
top_terms_cuban_k8_p2
top_terms_cuban_k8_p3
top_terms_cuban_k8_p4



### Figure summary

print(topic_cuban_k8)
print(topic_cuban_k8_p1)
print(topic_cuban_k8_p2)
print(topic_cuban_k8_p3)
print(topic_cuban_k8_p4)







### k = 9

# cast topic models

cuban_lda_k9 <- LDA(tidy_EP_chunks_cuban_dtm, k = 9, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k9 <- tidy(cuban_lda_k9)
cuban_lda_td_k9

# check gamma
cuban_lda_gamma_k9 <- tidy(cuban_lda_k9, matrix = "gamma")


# most common terms

top_terms_cuban_k9 <- cuban_lda_td_k9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k9

# figure

topic_cuban_k9 <- top_terms_cuban_k9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

cuban_lda_k9_p1 <- LDA(tidy_EP_chunks_cuban_dtm_p1, k = 9, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k9_p1 <- tidy(cuban_lda_k9_p1)
cuban_lda_td_k9_p1

# check gamma
cuban_lda_gamma_k9_p1 <- tidy(cuban_lda_k9_p1, matrix = "gamma")


# most common terms

top_terms_cuban_k9_p1 <- cuban_lda_td_k9_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k9_p1

# figure

topic_cuban_k9_p1 <- top_terms_cuban_k9_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

cuban_lda_k9_p2 <- LDA(tidy_EP_chunks_cuban_dtm_p2, k = 9, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k9_p2 <- tidy(cuban_lda_k9_p2)
cuban_lda_td_k9_p2

# check gamma
cuban_lda_gamma_k9_p2 <- tidy(cuban_lda_k9_p2, matrix = "gamma")


# most common terms

top_terms_cuban_k9_p2 <- cuban_lda_td_k9_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k9_p2

# figure

topic_cuban_k9_p2 <- top_terms_cuban_k9_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

cuban_lda_k9_p3 <- LDA(tidy_EP_chunks_cuban_dtm_p3, k = 9, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k9_p3 <- tidy(cuban_lda_k9_p3)
cuban_lda_td_k9_p3

# check gamma
cuban_lda_gamma_k9_p3 <- tidy(cuban_lda_k9_p3, matrix = "gamma")


# most common terms

top_terms_cuban_k9_p3 <- cuban_lda_td_k9_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k9_p3

# figure

topic_cuban_k9_p3 <- top_terms_cuban_k9_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

cuban_lda_k9_p4 <- LDA(tidy_EP_chunks_cuban_dtm_p4, k = 9, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k9_p4 <- tidy(cuban_lda_k9_p4)
cuban_lda_td_k9_p4

# check gamma
cuban_lda_gamma_k9_p4 <- tidy(cuban_lda_k9_p4, matrix = "gamma")


# most common terms

top_terms_cuban_k9_p4 <- cuban_lda_td_k9_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k9_p4

# figure

topic_cuban_k9_p4 <- top_terms_cuban_k9_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_cuban_k9
top_terms_cuban_k9_p1
top_terms_cuban_k9_p2
top_terms_cuban_k9_p3
top_terms_cuban_k9_p4



### Figure summary

print(topic_cuban_k9)
print(topic_cuban_k9_p1)
print(topic_cuban_k9_p2)
print(topic_cuban_k9_p3)
print(topic_cuban_k9_p4)







### k = 10

# cast topic models

cuban_lda_k10 <- LDA(tidy_EP_chunks_cuban_dtm, k = 10, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k10 <- tidy(cuban_lda_k10)
cuban_lda_td_k10

# check gamma
cuban_lda_gamma_k10 <- tidy(cuban_lda_k10, matrix = "gamma")


# most common terms

top_terms_cuban_k10 <- cuban_lda_td_k10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k10

# figure

topic_cuban_k10 <- top_terms_cuban_k10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

cuban_lda_k10_p1 <- LDA(tidy_EP_chunks_cuban_dtm_p1, k = 10, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k10_p1 <- tidy(cuban_lda_k10_p1)
cuban_lda_td_k10_p1

# check gamma
cuban_lda_gamma_k10_p1 <- tidy(cuban_lda_k10_p1, matrix = "gamma")


# most common terms

top_terms_cuban_k10_p1 <- cuban_lda_td_k10_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k10_p1

# figure

topic_cuban_k10_p1 <- top_terms_cuban_k10_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

cuban_lda_k10_p2 <- LDA(tidy_EP_chunks_cuban_dtm_p2, k = 10, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k10_p2 <- tidy(cuban_lda_k10_p2)
cuban_lda_td_k10_p2

# check gamma
cuban_lda_gamma_k10_p2 <- tidy(cuban_lda_k10_p2, matrix = "gamma")


# most common terms

top_terms_cuban_k10_p2 <- cuban_lda_td_k10_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k10_p2

# figure

topic_cuban_k10_p2 <- top_terms_cuban_k10_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

cuban_lda_k10_p3 <- LDA(tidy_EP_chunks_cuban_dtm_p3, k = 10, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k10_p3 <- tidy(cuban_lda_k10_p3)
cuban_lda_td_k10_p3

# check gamma
cuban_lda_gamma_k10_p3 <- tidy(cuban_lda_k10_p3, matrix = "gamma")


# most common terms

top_terms_cuban_k10_p3 <- cuban_lda_td_k10_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k10_p3

# figure

topic_cuban_k10_p3 <- top_terms_cuban_k10_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

cuban_lda_k10_p4 <- LDA(tidy_EP_chunks_cuban_dtm_p4, k = 10, control = list(seed = 1234))


# convert back to tidy

cuban_lda_td_k10_p4 <- tidy(cuban_lda_k10_p4)
cuban_lda_td_k10_p4

# check gamma
cuban_lda_gamma_k10_p4 <- tidy(cuban_lda_k10_p4, matrix = "gamma")


# most common terms

top_terms_cuban_k10_p4 <- cuban_lda_td_k10_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_cuban_k10_p4

# figure

topic_cuban_k10_p4 <- top_terms_cuban_k10_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_cuban_k10
top_terms_cuban_k10_p1
top_terms_cuban_k10_p2
top_terms_cuban_k10_p3
top_terms_cuban_k10_p4



### Figure summary

print(topic_cuban_k10)
print(topic_cuban_k10_p1)
print(topic_cuban_k10_p2)
print(topic_cuban_k10_p3)
print(topic_cuban_k10_p4)









############ Topic model by pseudo corpus: muslim



### k = 3

# cast topic models

muslim_lda_k3 <- LDA(tidy_EP_chunks_muslim_dtm, k = 3, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k3 <- tidy(muslim_lda_k3)
muslim_lda_td_k3

# check gamma
muslim_lda_gamma_k3 <- tidy(muslim_lda_k3, matrix = "gamma")


# most common terms

top_terms_muslim_k3 <- muslim_lda_td_k3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k3

# figure

topic_muslim_k3 <- top_terms_muslim_k3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

muslim_lda_k3_p1 <- LDA(tidy_EP_chunks_muslim_dtm_p1, k = 3, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k3_p1 <- tidy(muslim_lda_k3_p1)
muslim_lda_td_k3_p1

# check gamma
muslim_lda_gamma_k3_p1 <- tidy(muslim_lda_k3_p1, matrix = "gamma")


# most common terms

top_terms_muslim_k3_p1 <- muslim_lda_td_k3_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k3_p1

# figure

topic_muslim_k3_p1 <- top_terms_muslim_k3_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

muslim_lda_k3_p2 <- LDA(tidy_EP_chunks_muslim_dtm_p2, k = 3, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k3_p2 <- tidy(muslim_lda_k3_p2)
muslim_lda_td_k3_p2

# check gamma
muslim_lda_gamma_k3_p2 <- tidy(muslim_lda_k3_p2, matrix = "gamma")


# most common terms

top_terms_muslim_k3_p2 <- muslim_lda_td_k3_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k3_p2

# figure

topic_muslim_k3_p2 <- top_terms_muslim_k3_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

muslim_lda_k3_p3 <- LDA(tidy_EP_chunks_muslim_dtm_p3, k = 3, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k3_p3 <- tidy(muslim_lda_k3_p3)
muslim_lda_td_k3_p3

# check gamma
muslim_lda_gamma_k3_p3 <- tidy(muslim_lda_k3_p3, matrix = "gamma")


# most common terms

top_terms_muslim_k3_p3 <- muslim_lda_td_k3_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k3_p3

# figure

topic_muslim_k3_p3 <- top_terms_muslim_k3_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

muslim_lda_k3_p4 <- LDA(tidy_EP_chunks_muslim_dtm_p4, k = 3, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k3_p4 <- tidy(muslim_lda_k3_p4)
muslim_lda_td_k3_p4

# check gamma
muslim_lda_gamma_k3_p4 <- tidy(muslim_lda_k3_p4, matrix = "gamma")


# most common terms

top_terms_muslim_k3_p4 <- muslim_lda_td_k3_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k3_p4

# figure

topic_muslim_k3_p4 <- top_terms_muslim_k3_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_muslim_k3
top_terms_muslim_k3_p1
top_terms_muslim_k3_p2
top_terms_muslim_k3_p3
top_terms_muslim_k3_p4



### Figure summary

print(topic_muslim_k3)
print(topic_muslim_k3_p1)
print(topic_muslim_k3_p2)
print(topic_muslim_k3_p3)
print(topic_muslim_k3_p4)




### k = 4

# cast topic models

muslim_lda_k4 <- LDA(tidy_EP_chunks_muslim_dtm, k = 4, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k4 <- tidy(muslim_lda_k4)
muslim_lda_td_k4

# check gamma
muslim_lda_gamma_k4 <- tidy(muslim_lda_k4, matrix = "gamma")


# most common terms

top_terms_muslim_k4 <- muslim_lda_td_k4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k4

# figure

topic_muslim_k4 <- top_terms_muslim_k4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

muslim_lda_k4_p1 <- LDA(tidy_EP_chunks_muslim_dtm_p1, k = 4, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k4_p1 <- tidy(muslim_lda_k4_p1)
muslim_lda_td_k4_p1

# check gamma
muslim_lda_gamma_k4_p1 <- tidy(muslim_lda_k4_p1, matrix = "gamma")


# most common terms

top_terms_muslim_k4_p1 <- muslim_lda_td_k4_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k4_p1

# figure

topic_muslim_k4_p1 <- top_terms_muslim_k4_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

muslim_lda_k4_p2 <- LDA(tidy_EP_chunks_muslim_dtm_p2, k = 4, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k4_p2 <- tidy(muslim_lda_k4_p2)
muslim_lda_td_k4_p2

# check gamma
muslim_lda_gamma_k4_p2 <- tidy(muslim_lda_k4_p2, matrix = "gamma")


# most common terms

top_terms_muslim_k4_p2 <- muslim_lda_td_k4_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k4_p2

# figure

topic_muslim_k4_p2 <- top_terms_muslim_k4_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

muslim_lda_k4_p3 <- LDA(tidy_EP_chunks_muslim_dtm_p3, k = 4, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k4_p3 <- tidy(muslim_lda_k4_p3)
muslim_lda_td_k4_p3

# check gamma
muslim_lda_gamma_k4_p3 <- tidy(muslim_lda_k4_p3, matrix = "gamma")


# most common terms

top_terms_muslim_k4_p3 <- muslim_lda_td_k4_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k4_p3

# figure

topic_muslim_k4_p3 <- top_terms_muslim_k4_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

muslim_lda_k4_p4 <- LDA(tidy_EP_chunks_muslim_dtm_p4, k = 4, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k4_p4 <- tidy(muslim_lda_k4_p4)
muslim_lda_td_k4_p4

# check gamma
muslim_lda_gamma_k4_p4 <- tidy(muslim_lda_k4_p4, matrix = "gamma")


# most common terms

top_terms_muslim_k4_p4 <- muslim_lda_td_k4_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k4_p4

# figure

topic_muslim_k4_p4 <- top_terms_muslim_k4_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_muslim_k4
top_terms_muslim_k4_p1
top_terms_muslim_k4_p2
top_terms_muslim_k4_p3
top_terms_muslim_k4_p4



### Figure summary

print(topic_muslim_k4)
print(topic_muslim_k4_p1)
print(topic_muslim_k4_p2)
print(topic_muslim_k4_p3)
print(topic_muslim_k4_p4)



### k = 5

# cast topic models

muslim_lda_k5 <- LDA(tidy_EP_chunks_muslim_dtm, k = 5, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k5 <- tidy(muslim_lda_k5)
muslim_lda_td_k5

# check gamma
muslim_lda_gamma_k5 <- tidy(muslim_lda_k5, matrix = "gamma")


# most common terms

top_terms_muslim_k5 <- muslim_lda_td_k5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k5

# figure

topic_muslim_k5 <- top_terms_muslim_k5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

muslim_lda_k5_p1 <- LDA(tidy_EP_chunks_muslim_dtm_p1, k = 5, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k5_p1 <- tidy(muslim_lda_k5_p1)
muslim_lda_td_k5_p1

# check gamma
muslim_lda_gamma_k5_p1 <- tidy(muslim_lda_k5_p1, matrix = "gamma")


# most common terms

top_terms_muslim_k5_p1 <- muslim_lda_td_k5_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k5_p1

# figure

topic_muslim_k5_p1 <- top_terms_muslim_k5_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

muslim_lda_k5_p2 <- LDA(tidy_EP_chunks_muslim_dtm_p2, k = 5, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k5_p2 <- tidy(muslim_lda_k5_p2)
muslim_lda_td_k5_p2

# check gamma
muslim_lda_gamma_k5_p2 <- tidy(muslim_lda_k5_p2, matrix = "gamma")


# most common terms

top_terms_muslim_k5_p2 <- muslim_lda_td_k5_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k5_p2

# figure

topic_muslim_k5_p2 <- top_terms_muslim_k5_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

muslim_lda_k5_p3 <- LDA(tidy_EP_chunks_muslim_dtm_p3, k = 5, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k5_p3 <- tidy(muslim_lda_k5_p3)
muslim_lda_td_k5_p3

# check gamma
muslim_lda_gamma_k5_p3 <- tidy(muslim_lda_k5_p3, matrix = "gamma")


# most common terms

top_terms_muslim_k5_p3 <- muslim_lda_td_k5_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k5_p3

# figure

topic_muslim_k5_p3 <- top_terms_muslim_k5_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

muslim_lda_k5_p4 <- LDA(tidy_EP_chunks_muslim_dtm_p4, k = 5, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k5_p4 <- tidy(muslim_lda_k5_p4)
muslim_lda_td_k5_p4

# check gamma
muslim_lda_gamma_k5_p4 <- tidy(muslim_lda_k5_p4, matrix = "gamma")


# most common terms

top_terms_muslim_k5_p4 <- muslim_lda_td_k5_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k5_p4

# figure

topic_muslim_k5_p4 <- top_terms_muslim_k5_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_muslim_k5
top_terms_muslim_k5_p1
top_terms_muslim_k5_p2
top_terms_muslim_k5_p3
top_terms_muslim_k5_p4



### Figure summary

print(topic_muslim_k5)
print(topic_muslim_k5_p1)
print(topic_muslim_k5_p2)
print(topic_muslim_k5_p3)
print(topic_muslim_k5_p4)







### k = 6

# cast topic models

muslim_lda_k6 <- LDA(tidy_EP_chunks_muslim_dtm, k = 6, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k6 <- tidy(muslim_lda_k6)
muslim_lda_td_k6

# check gamma
muslim_lda_gamma_k6 <- tidy(muslim_lda_k6, matrix = "gamma")


# most common terms

top_terms_muslim_k6 <- muslim_lda_td_k6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k6

# figure

topic_muslim_k6 <- top_terms_muslim_k6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

muslim_lda_k6_p1 <- LDA(tidy_EP_chunks_muslim_dtm_p1, k = 6, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k6_p1 <- tidy(muslim_lda_k6_p1)
muslim_lda_td_k6_p1

# check gamma
muslim_lda_gamma_k6_p1 <- tidy(muslim_lda_k6_p1, matrix = "gamma")


# most common terms

top_terms_muslim_k6_p1 <- muslim_lda_td_k6_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k6_p1

# figure

topic_muslim_k6_p1 <- top_terms_muslim_k6_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

muslim_lda_k6_p2 <- LDA(tidy_EP_chunks_muslim_dtm_p2, k = 6, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k6_p2 <- tidy(muslim_lda_k6_p2)
muslim_lda_td_k6_p2

# check gamma
muslim_lda_gamma_k6_p2 <- tidy(muslim_lda_k6_p2, matrix = "gamma")


# most common terms

top_terms_muslim_k6_p2 <- muslim_lda_td_k6_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k6_p2

# figure

topic_muslim_k6_p2 <- top_terms_muslim_k6_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

muslim_lda_k6_p3 <- LDA(tidy_EP_chunks_muslim_dtm_p3, k = 6, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k6_p3 <- tidy(muslim_lda_k6_p3)
muslim_lda_td_k6_p3

# check gamma
muslim_lda_gamma_k6_p3 <- tidy(muslim_lda_k6_p3, matrix = "gamma")


# most common terms

top_terms_muslim_k6_p3 <- muslim_lda_td_k6_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k6_p3

# figure

topic_muslim_k6_p3 <- top_terms_muslim_k6_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

muslim_lda_k6_p4 <- LDA(tidy_EP_chunks_muslim_dtm_p4, k = 6, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k6_p4 <- tidy(muslim_lda_k6_p4)
muslim_lda_td_k6_p4

# check gamma
muslim_lda_gamma_k6_p4 <- tidy(muslim_lda_k6_p4, matrix = "gamma")


# most common terms

top_terms_muslim_k6_p4 <- muslim_lda_td_k6_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k6_p4

# figure

topic_muslim_k6_p4 <- top_terms_muslim_k6_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_muslim_k6
top_terms_muslim_k6_p1
top_terms_muslim_k6_p2
top_terms_muslim_k6_p3
top_terms_muslim_k6_p4



### Figure summary

print(topic_muslim_k6)
print(topic_muslim_k6_p1)
print(topic_muslim_k6_p2)
print(topic_muslim_k6_p3)
print(topic_muslim_k6_p4)







### k = 7

# cast topic models

muslim_lda_k7 <- LDA(tidy_EP_chunks_muslim_dtm, k = 7, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k7 <- tidy(muslim_lda_k7)
muslim_lda_td_k7

# check gamma
muslim_lda_gamma_k7 <- tidy(muslim_lda_k7, matrix = "gamma")


# most common terms

top_terms_muslim_k7 <- muslim_lda_td_k7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k7

# figure

topic_muslim_k7 <- top_terms_muslim_k7 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

muslim_lda_k7_p1 <- LDA(tidy_EP_chunks_muslim_dtm_p1, k = 7, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k7_p1 <- tidy(muslim_lda_k7_p1)
muslim_lda_td_k7_p1

# check gamma
muslim_lda_gamma_k7_p1 <- tidy(muslim_lda_k7_p1, matrix = "gamma")


# most common terms

top_terms_muslim_k7_p1 <- muslim_lda_td_k7_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k7_p1

# figure

topic_muslim_k7_p1 <- top_terms_muslim_k7_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

muslim_lda_k7_p2 <- LDA(tidy_EP_chunks_muslim_dtm_p2, k = 7, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k7_p2 <- tidy(muslim_lda_k7_p2)
muslim_lda_td_k7_p2

# check gamma
muslim_lda_gamma_k7_p2 <- tidy(muslim_lda_k7_p2, matrix = "gamma")


# most common terms

top_terms_muslim_k7_p2 <- muslim_lda_td_k7_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k7_p2

# figure

topic_muslim_k7_p2 <- top_terms_muslim_k7_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

muslim_lda_k7_p3 <- LDA(tidy_EP_chunks_muslim_dtm_p3, k = 7, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k7_p3 <- tidy(muslim_lda_k7_p3)
muslim_lda_td_k7_p3

# check gamma
muslim_lda_gamma_k7_p3 <- tidy(muslim_lda_k7_p3, matrix = "gamma")


# most common terms

top_terms_muslim_k7_p3 <- muslim_lda_td_k7_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k7_p3

# figure

topic_muslim_k7_p3 <- top_terms_muslim_k7_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

muslim_lda_k7_p4 <- LDA(tidy_EP_chunks_muslim_dtm_p4, k = 7, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k7_p4 <- tidy(muslim_lda_k7_p4)
muslim_lda_td_k7_p4

# check gamma
muslim_lda_gamma_k7_p4 <- tidy(muslim_lda_k7_p4, matrix = "gamma")


# most common terms

top_terms_muslim_k7_p4 <- muslim_lda_td_k7_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k7_p4

# figure

topic_muslim_k7_p4 <- top_terms_muslim_k7_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_muslim_k7
top_terms_muslim_k7_p1
top_terms_muslim_k7_p2
top_terms_muslim_k7_p3
top_terms_muslim_k7_p4



### Figure summary

print(topic_muslim_k7)
print(topic_muslim_k7_p1)
print(topic_muslim_k7_p2)
print(topic_muslim_k7_p3)
print(topic_muslim_k7_p4)







### k = 8

# cast topic models

muslim_lda_k8 <- LDA(tidy_EP_chunks_muslim_dtm, k = 8, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k8 <- tidy(muslim_lda_k8)
muslim_lda_td_k8

# check gamma
muslim_lda_gamma_k8 <- tidy(muslim_lda_k8, matrix = "gamma")


# most common terms

top_terms_muslim_k8 <- muslim_lda_td_k8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k8

# figure

topic_muslim_k8 <- top_terms_muslim_k8 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

muslim_lda_k8_p1 <- LDA(tidy_EP_chunks_muslim_dtm_p1, k = 8, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k8_p1 <- tidy(muslim_lda_k8_p1)
muslim_lda_td_k8_p1

# check gamma
muslim_lda_gamma_k8_p1 <- tidy(muslim_lda_k8_p1, matrix = "gamma")


# most common terms

top_terms_muslim_k8_p1 <- muslim_lda_td_k8_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k8_p1

# figure

topic_muslim_k8_p1 <- top_terms_muslim_k8_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

muslim_lda_k8_p2 <- LDA(tidy_EP_chunks_muslim_dtm_p2, k = 8, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k8_p2 <- tidy(muslim_lda_k8_p2)
muslim_lda_td_k8_p2

# check gamma
muslim_lda_gamma_k8_p2 <- tidy(muslim_lda_k8_p2, matrix = "gamma")


# most common terms

top_terms_muslim_k8_p2 <- muslim_lda_td_k8_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k8_p2

# figure

topic_muslim_k8_p2 <- top_terms_muslim_k8_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

muslim_lda_k8_p3 <- LDA(tidy_EP_chunks_muslim_dtm_p3, k = 8, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k8_p3 <- tidy(muslim_lda_k8_p3)
muslim_lda_td_k8_p3

# check gamma
muslim_lda_gamma_k8_p3 <- tidy(muslim_lda_k8_p3, matrix = "gamma")


# most common terms

top_terms_muslim_k8_p3 <- muslim_lda_td_k8_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k8_p3

# figure

topic_muslim_k8_p3 <- top_terms_muslim_k8_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

muslim_lda_k8_p4 <- LDA(tidy_EP_chunks_muslim_dtm_p4, k = 8, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k8_p4 <- tidy(muslim_lda_k8_p4)
muslim_lda_td_k8_p4

# check gamma
muslim_lda_gamma_k8_p4 <- tidy(muslim_lda_k8_p4, matrix = "gamma")


# most common terms

top_terms_muslim_k8_p4 <- muslim_lda_td_k8_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k8_p4

# figure

topic_muslim_k8_p4 <- top_terms_muslim_k8_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_muslim_k8
top_terms_muslim_k8_p1
top_terms_muslim_k8_p2
top_terms_muslim_k8_p3
top_terms_muslim_k8_p4



### Figure summary

print(topic_muslim_k8)
print(topic_muslim_k8_p1)
print(topic_muslim_k8_p2)
print(topic_muslim_k8_p3)
print(topic_muslim_k8_p4)







### k = 9

# cast topic models

muslim_lda_k9 <- LDA(tidy_EP_chunks_muslim_dtm, k = 9, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k9 <- tidy(muslim_lda_k9)
muslim_lda_td_k9

# check gamma
muslim_lda_gamma_k9 <- tidy(muslim_lda_k9, matrix = "gamma")


# most common terms

top_terms_muslim_k9 <- muslim_lda_td_k9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k9

# figure

topic_muslim_k9 <- top_terms_muslim_k9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

muslim_lda_k9_p1 <- LDA(tidy_EP_chunks_muslim_dtm_p1, k = 9, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k9_p1 <- tidy(muslim_lda_k9_p1)
muslim_lda_td_k9_p1

# check gamma
muslim_lda_gamma_k9_p1 <- tidy(muslim_lda_k9_p1, matrix = "gamma")


# most common terms

top_terms_muslim_k9_p1 <- muslim_lda_td_k9_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k9_p1

# figure

topic_muslim_k9_p1 <- top_terms_muslim_k9_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

muslim_lda_k9_p2 <- LDA(tidy_EP_chunks_muslim_dtm_p2, k = 9, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k9_p2 <- tidy(muslim_lda_k9_p2)
muslim_lda_td_k9_p2

# check gamma
muslim_lda_gamma_k9_p2 <- tidy(muslim_lda_k9_p2, matrix = "gamma")


# most common terms

top_terms_muslim_k9_p2 <- muslim_lda_td_k9_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k9_p2

# figure

topic_muslim_k9_p2 <- top_terms_muslim_k9_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

muslim_lda_k9_p3 <- LDA(tidy_EP_chunks_muslim_dtm_p3, k = 9, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k9_p3 <- tidy(muslim_lda_k9_p3)
muslim_lda_td_k9_p3

# check gamma
muslim_lda_gamma_k9_p3 <- tidy(muslim_lda_k9_p3, matrix = "gamma")


# most common terms

top_terms_muslim_k9_p3 <- muslim_lda_td_k9_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k9_p3

# figure

topic_muslim_k9_p3 <- top_terms_muslim_k9_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

muslim_lda_k9_p4 <- LDA(tidy_EP_chunks_muslim_dtm_p4, k = 9, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k9_p4 <- tidy(muslim_lda_k9_p4)
muslim_lda_td_k9_p4

# check gamma
muslim_lda_gamma_k9_p4 <- tidy(muslim_lda_k9_p4, matrix = "gamma")


# most common terms

top_terms_muslim_k9_p4 <- muslim_lda_td_k9_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k9_p4

# figure

topic_muslim_k9_p4 <- top_terms_muslim_k9_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_muslim_k9
top_terms_muslim_k9_p1
top_terms_muslim_k9_p2
top_terms_muslim_k9_p3
top_terms_muslim_k9_p4



### Figure summary

print(topic_muslim_k9)
print(topic_muslim_k9_p1)
print(topic_muslim_k9_p2)
print(topic_muslim_k9_p3)
print(topic_muslim_k9_p4)







### k = 10

# cast topic models

muslim_lda_k10 <- LDA(tidy_EP_chunks_muslim_dtm, k = 10, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k10 <- tidy(muslim_lda_k10)
muslim_lda_td_k10

# check gamma
muslim_lda_gamma_k10 <- tidy(muslim_lda_k10, matrix = "gamma")


# most common terms

top_terms_muslim_k10 <- muslim_lda_td_k10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k10

# figure

topic_muslim_k10 <- top_terms_muslim_k10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

muslim_lda_k10_p1 <- LDA(tidy_EP_chunks_muslim_dtm_p1, k = 10, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k10_p1 <- tidy(muslim_lda_k10_p1)
muslim_lda_td_k10_p1

# check gamma
muslim_lda_gamma_k10_p1 <- tidy(muslim_lda_k10_p1, matrix = "gamma")


# most common terms

top_terms_muslim_k10_p1 <- muslim_lda_td_k10_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k10_p1

# figure

topic_muslim_k10_p1 <- top_terms_muslim_k10_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

muslim_lda_k10_p2 <- LDA(tidy_EP_chunks_muslim_dtm_p2, k = 10, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k10_p2 <- tidy(muslim_lda_k10_p2)
muslim_lda_td_k10_p2

# check gamma
muslim_lda_gamma_k10_p2 <- tidy(muslim_lda_k10_p2, matrix = "gamma")


# most common terms

top_terms_muslim_k10_p2 <- muslim_lda_td_k10_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k10_p2

# figure

topic_muslim_k10_p2 <- top_terms_muslim_k10_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

muslim_lda_k10_p3 <- LDA(tidy_EP_chunks_muslim_dtm_p3, k = 10, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k10_p3 <- tidy(muslim_lda_k10_p3)
muslim_lda_td_k10_p3

# check gamma
muslim_lda_gamma_k10_p3 <- tidy(muslim_lda_k10_p3, matrix = "gamma")


# most common terms

top_terms_muslim_k10_p3 <- muslim_lda_td_k10_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k10_p3

# figure

topic_muslim_k10_p3 <- top_terms_muslim_k10_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

muslim_lda_k10_p4 <- LDA(tidy_EP_chunks_muslim_dtm_p4, k = 10, control = list(seed = 1234))


# convert back to tidy

muslim_lda_td_k10_p4 <- tidy(muslim_lda_k10_p4)
muslim_lda_td_k10_p4

# check gamma
muslim_lda_gamma_k10_p4 <- tidy(muslim_lda_k10_p4, matrix = "gamma")


# most common terms

top_terms_muslim_k10_p4 <- muslim_lda_td_k10_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_muslim_k10_p4

# figure

topic_muslim_k10_p4 <- top_terms_muslim_k10_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_muslim_k10
top_terms_muslim_k10_p1
top_terms_muslim_k10_p2
top_terms_muslim_k10_p3
top_terms_muslim_k10_p4



### Figure summary

print(topic_muslim_k10)
print(topic_muslim_k10_p1)
print(topic_muslim_k10_p2)
print(topic_muslim_k10_p3)
print(topic_muslim_k10_p4)









############ Topic model by pseudo corpus: chinese



### k = 3

# cast topic models

chinese_lda_k3 <- LDA(tidy_EP_chunks_chinese_dtm, k = 3, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k3 <- tidy(chinese_lda_k3)
chinese_lda_td_k3

# check gamma
chinese_lda_gamma_k3 <- tidy(chinese_lda_k3, matrix = "gamma")


# most common terms

top_terms_chinese_k3 <- chinese_lda_td_k3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k3

# figure

topic_chinese_k3 <- top_terms_chinese_k3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

chinese_lda_k3_p1 <- LDA(tidy_EP_chunks_chinese_dtm_p1, k = 3, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k3_p1 <- tidy(chinese_lda_k3_p1)
chinese_lda_td_k3_p1

# check gamma
chinese_lda_gamma_k3_p1 <- tidy(chinese_lda_k3_p1, matrix = "gamma")


# most common terms

top_terms_chinese_k3_p1 <- chinese_lda_td_k3_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k3_p1

# figure

topic_chinese_k3_p1 <- top_terms_chinese_k3_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

chinese_lda_k3_p2 <- LDA(tidy_EP_chunks_chinese_dtm_p2, k = 3, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k3_p2 <- tidy(chinese_lda_k3_p2)
chinese_lda_td_k3_p2

# check gamma
chinese_lda_gamma_k3_p2 <- tidy(chinese_lda_k3_p2, matrix = "gamma")


# most common terms

top_terms_chinese_k3_p2 <- chinese_lda_td_k3_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k3_p2

# figure

topic_chinese_k3_p2 <- top_terms_chinese_k3_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

chinese_lda_k3_p3 <- LDA(tidy_EP_chunks_chinese_dtm_p3, k = 3, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k3_p3 <- tidy(chinese_lda_k3_p3)
chinese_lda_td_k3_p3

# check gamma
chinese_lda_gamma_k3_p3 <- tidy(chinese_lda_k3_p3, matrix = "gamma")


# most common terms

top_terms_chinese_k3_p3 <- chinese_lda_td_k3_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k3_p3

# figure

topic_chinese_k3_p3 <- top_terms_chinese_k3_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

chinese_lda_k3_p4 <- LDA(tidy_EP_chunks_chinese_dtm_p4, k = 3, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k3_p4 <- tidy(chinese_lda_k3_p4)
chinese_lda_td_k3_p4

# check gamma
chinese_lda_gamma_k3_p4 <- tidy(chinese_lda_k3_p4, matrix = "gamma")


# most common terms

top_terms_chinese_k3_p4 <- chinese_lda_td_k3_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k3_p4

# figure

topic_chinese_k3_p4 <- top_terms_chinese_k3_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_chinese_k3
top_terms_chinese_k3_p1
top_terms_chinese_k3_p2
top_terms_chinese_k3_p3
top_terms_chinese_k3_p4



### Figure summary

print(topic_chinese_k3)
print(topic_chinese_k3_p1)
print(topic_chinese_k3_p2)
print(topic_chinese_k3_p3)
print(topic_chinese_k3_p4)




### k = 4

# cast topic models

chinese_lda_k4 <- LDA(tidy_EP_chunks_chinese_dtm, k = 4, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k4 <- tidy(chinese_lda_k4)
chinese_lda_td_k4

# check gamma
chinese_lda_gamma_k4 <- tidy(chinese_lda_k4, matrix = "gamma")


# most common terms

top_terms_chinese_k4 <- chinese_lda_td_k4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k4

# figure

topic_chinese_k4 <- top_terms_chinese_k4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

chinese_lda_k4_p1 <- LDA(tidy_EP_chunks_chinese_dtm_p1, k = 4, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k4_p1 <- tidy(chinese_lda_k4_p1)
chinese_lda_td_k4_p1

# check gamma
chinese_lda_gamma_k4_p1 <- tidy(chinese_lda_k4_p1, matrix = "gamma")


# most common terms

top_terms_chinese_k4_p1 <- chinese_lda_td_k4_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k4_p1

# figure

topic_chinese_k4_p1 <- top_terms_chinese_k4_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

chinese_lda_k4_p2 <- LDA(tidy_EP_chunks_chinese_dtm_p2, k = 4, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k4_p2 <- tidy(chinese_lda_k4_p2)
chinese_lda_td_k4_p2

# check gamma
chinese_lda_gamma_k4_p2 <- tidy(chinese_lda_k4_p2, matrix = "gamma")


# most common terms

top_terms_chinese_k4_p2 <- chinese_lda_td_k4_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k4_p2

# figure

topic_chinese_k4_p2 <- top_terms_chinese_k4_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

chinese_lda_k4_p3 <- LDA(tidy_EP_chunks_chinese_dtm_p3, k = 4, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k4_p3 <- tidy(chinese_lda_k4_p3)
chinese_lda_td_k4_p3

# check gamma
chinese_lda_gamma_k4_p3 <- tidy(chinese_lda_k4_p3, matrix = "gamma")


# most common terms

top_terms_chinese_k4_p3 <- chinese_lda_td_k4_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k4_p3

# figure

topic_chinese_k4_p3 <- top_terms_chinese_k4_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

chinese_lda_k4_p4 <- LDA(tidy_EP_chunks_chinese_dtm_p4, k = 4, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k4_p4 <- tidy(chinese_lda_k4_p4)
chinese_lda_td_k4_p4

# check gamma
chinese_lda_gamma_k4_p4 <- tidy(chinese_lda_k4_p4, matrix = "gamma")


# most common terms

top_terms_chinese_k4_p4 <- chinese_lda_td_k4_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k4_p4

# figure

topic_chinese_k4_p4 <- top_terms_chinese_k4_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_chinese_k4
top_terms_chinese_k4_p1
top_terms_chinese_k4_p2
top_terms_chinese_k4_p3
top_terms_chinese_k4_p4



### Figure summary

print(topic_chinese_k4)
print(topic_chinese_k4_p1)
print(topic_chinese_k4_p2)
print(topic_chinese_k4_p3)
print(topic_chinese_k4_p4)



### k = 5

# cast topic models

chinese_lda_k5 <- LDA(tidy_EP_chunks_chinese_dtm, k = 5, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k5 <- tidy(chinese_lda_k5)
chinese_lda_td_k5

# check gamma
chinese_lda_gamma_k5 <- tidy(chinese_lda_k5, matrix = "gamma")


# most common terms

top_terms_chinese_k5 <- chinese_lda_td_k5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k5

# figure

topic_chinese_k5 <- top_terms_chinese_k5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

chinese_lda_k5_p1 <- LDA(tidy_EP_chunks_chinese_dtm_p1, k = 5, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k5_p1 <- tidy(chinese_lda_k5_p1)
chinese_lda_td_k5_p1

# check gamma
chinese_lda_gamma_k5_p1 <- tidy(chinese_lda_k5_p1, matrix = "gamma")


# most common terms

top_terms_chinese_k5_p1 <- chinese_lda_td_k5_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k5_p1

# figure

topic_chinese_k5_p1 <- top_terms_chinese_k5_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

chinese_lda_k5_p2 <- LDA(tidy_EP_chunks_chinese_dtm_p2, k = 5, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k5_p2 <- tidy(chinese_lda_k5_p2)
chinese_lda_td_k5_p2

# check gamma
chinese_lda_gamma_k5_p2 <- tidy(chinese_lda_k5_p2, matrix = "gamma")


# most common terms

top_terms_chinese_k5_p2 <- chinese_lda_td_k5_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k5_p2

# figure

topic_chinese_k5_p2 <- top_terms_chinese_k5_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

chinese_lda_k5_p3 <- LDA(tidy_EP_chunks_chinese_dtm_p3, k = 5, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k5_p3 <- tidy(chinese_lda_k5_p3)
chinese_lda_td_k5_p3

# check gamma
chinese_lda_gamma_k5_p3 <- tidy(chinese_lda_k5_p3, matrix = "gamma")


# most common terms

top_terms_chinese_k5_p3 <- chinese_lda_td_k5_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k5_p3

# figure

topic_chinese_k5_p3 <- top_terms_chinese_k5_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

chinese_lda_k5_p4 <- LDA(tidy_EP_chunks_chinese_dtm_p4, k = 5, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k5_p4 <- tidy(chinese_lda_k5_p4)
chinese_lda_td_k5_p4

# check gamma
chinese_lda_gamma_k5_p4 <- tidy(chinese_lda_k5_p4, matrix = "gamma")


# most common terms

top_terms_chinese_k5_p4 <- chinese_lda_td_k5_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k5_p4

# figure

topic_chinese_k5_p4 <- top_terms_chinese_k5_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_chinese_k5
top_terms_chinese_k5_p1
top_terms_chinese_k5_p2
top_terms_chinese_k5_p3
top_terms_chinese_k5_p4



### Figure summary

print(topic_chinese_k5)
print(topic_chinese_k5_p1)
print(topic_chinese_k5_p2)
print(topic_chinese_k5_p3)
print(topic_chinese_k5_p4)







### k = 6

# cast topic models

chinese_lda_k6 <- LDA(tidy_EP_chunks_chinese_dtm, k = 6, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k6 <- tidy(chinese_lda_k6)
chinese_lda_td_k6

# check gamma
chinese_lda_gamma_k6 <- tidy(chinese_lda_k6, matrix = "gamma")


# most common terms

top_terms_chinese_k6 <- chinese_lda_td_k6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k6

# figure

topic_chinese_k6 <- top_terms_chinese_k6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

chinese_lda_k6_p1 <- LDA(tidy_EP_chunks_chinese_dtm_p1, k = 6, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k6_p1 <- tidy(chinese_lda_k6_p1)
chinese_lda_td_k6_p1

# check gamma
chinese_lda_gamma_k6_p1 <- tidy(chinese_lda_k6_p1, matrix = "gamma")


# most common terms

top_terms_chinese_k6_p1 <- chinese_lda_td_k6_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k6_p1

# figure

topic_chinese_k6_p1 <- top_terms_chinese_k6_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

chinese_lda_k6_p2 <- LDA(tidy_EP_chunks_chinese_dtm_p2, k = 6, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k6_p2 <- tidy(chinese_lda_k6_p2)
chinese_lda_td_k6_p2

# check gamma
chinese_lda_gamma_k6_p2 <- tidy(chinese_lda_k6_p2, matrix = "gamma")


# most common terms

top_terms_chinese_k6_p2 <- chinese_lda_td_k6_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k6_p2

# figure

topic_chinese_k6_p2 <- top_terms_chinese_k6_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

chinese_lda_k6_p3 <- LDA(tidy_EP_chunks_chinese_dtm_p3, k = 6, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k6_p3 <- tidy(chinese_lda_k6_p3)
chinese_lda_td_k6_p3

# check gamma
chinese_lda_gamma_k6_p3 <- tidy(chinese_lda_k6_p3, matrix = "gamma")


# most common terms

top_terms_chinese_k6_p3 <- chinese_lda_td_k6_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k6_p3

# figure

topic_chinese_k6_p3 <- top_terms_chinese_k6_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

chinese_lda_k6_p4 <- LDA(tidy_EP_chunks_chinese_dtm_p4, k = 6, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k6_p4 <- tidy(chinese_lda_k6_p4)
chinese_lda_td_k6_p4

# check gamma
chinese_lda_gamma_k6_p4 <- tidy(chinese_lda_k6_p4, matrix = "gamma")


# most common terms

top_terms_chinese_k6_p4 <- chinese_lda_td_k6_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k6_p4

# figure

topic_chinese_k6_p4 <- top_terms_chinese_k6_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_chinese_k6
top_terms_chinese_k6_p1
top_terms_chinese_k6_p2
top_terms_chinese_k6_p3
top_terms_chinese_k6_p4



### Figure summary

print(topic_chinese_k6)
print(topic_chinese_k6_p1)
print(topic_chinese_k6_p2)
print(topic_chinese_k6_p3)
print(topic_chinese_k6_p4)







### k = 7

# cast topic models

chinese_lda_k7 <- LDA(tidy_EP_chunks_chinese_dtm, k = 7, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k7 <- tidy(chinese_lda_k7)
chinese_lda_td_k7

# check gamma
chinese_lda_gamma_k7 <- tidy(chinese_lda_k7, matrix = "gamma")


# most common terms

top_terms_chinese_k7 <- chinese_lda_td_k7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k7

# figure

topic_chinese_k7 <- top_terms_chinese_k7 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

chinese_lda_k7_p1 <- LDA(tidy_EP_chunks_chinese_dtm_p1, k = 7, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k7_p1 <- tidy(chinese_lda_k7_p1)
chinese_lda_td_k7_p1

# check gamma
chinese_lda_gamma_k7_p1 <- tidy(chinese_lda_k7_p1, matrix = "gamma")


# most common terms

top_terms_chinese_k7_p1 <- chinese_lda_td_k7_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k7_p1

# figure

topic_chinese_k7_p1 <- top_terms_chinese_k7_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

chinese_lda_k7_p2 <- LDA(tidy_EP_chunks_chinese_dtm_p2, k = 7, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k7_p2 <- tidy(chinese_lda_k7_p2)
chinese_lda_td_k7_p2

# check gamma
chinese_lda_gamma_k7_p2 <- tidy(chinese_lda_k7_p2, matrix = "gamma")


# most common terms

top_terms_chinese_k7_p2 <- chinese_lda_td_k7_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k7_p2

# figure

topic_chinese_k7_p2 <- top_terms_chinese_k7_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

chinese_lda_k7_p3 <- LDA(tidy_EP_chunks_chinese_dtm_p3, k = 7, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k7_p3 <- tidy(chinese_lda_k7_p3)
chinese_lda_td_k7_p3

# check gamma
chinese_lda_gamma_k7_p3 <- tidy(chinese_lda_k7_p3, matrix = "gamma")


# most common terms

top_terms_chinese_k7_p3 <- chinese_lda_td_k7_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k7_p3

# figure

topic_chinese_k7_p3 <- top_terms_chinese_k7_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

chinese_lda_k7_p4 <- LDA(tidy_EP_chunks_chinese_dtm_p4, k = 7, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k7_p4 <- tidy(chinese_lda_k7_p4)
chinese_lda_td_k7_p4

# check gamma
chinese_lda_gamma_k7_p4 <- tidy(chinese_lda_k7_p4, matrix = "gamma")


# most common terms

top_terms_chinese_k7_p4 <- chinese_lda_td_k7_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k7_p4

# figure

topic_chinese_k7_p4 <- top_terms_chinese_k7_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_chinese_k7
top_terms_chinese_k7_p1
top_terms_chinese_k7_p2
top_terms_chinese_k7_p3
top_terms_chinese_k7_p4



### Figure summary

print(topic_chinese_k7)
print(topic_chinese_k7_p1)
print(topic_chinese_k7_p2)
print(topic_chinese_k7_p3)
print(topic_chinese_k7_p4)







### k = 8

# cast topic models

chinese_lda_k8 <- LDA(tidy_EP_chunks_chinese_dtm, k = 8, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k8 <- tidy(chinese_lda_k8)
chinese_lda_td_k8

# check gamma
chinese_lda_gamma_k8 <- tidy(chinese_lda_k8, matrix = "gamma")


# most common terms

top_terms_chinese_k8 <- chinese_lda_td_k8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k8

# figure

topic_chinese_k8 <- top_terms_chinese_k8 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

chinese_lda_k8_p1 <- LDA(tidy_EP_chunks_chinese_dtm_p1, k = 8, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k8_p1 <- tidy(chinese_lda_k8_p1)
chinese_lda_td_k8_p1

# check gamma
chinese_lda_gamma_k8_p1 <- tidy(chinese_lda_k8_p1, matrix = "gamma")


# most common terms

top_terms_chinese_k8_p1 <- chinese_lda_td_k8_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k8_p1

# figure

topic_chinese_k8_p1 <- top_terms_chinese_k8_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

chinese_lda_k8_p2 <- LDA(tidy_EP_chunks_chinese_dtm_p2, k = 8, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k8_p2 <- tidy(chinese_lda_k8_p2)
chinese_lda_td_k8_p2

# check gamma
chinese_lda_gamma_k8_p2 <- tidy(chinese_lda_k8_p2, matrix = "gamma")


# most common terms

top_terms_chinese_k8_p2 <- chinese_lda_td_k8_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k8_p2

# figure

topic_chinese_k8_p2 <- top_terms_chinese_k8_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

chinese_lda_k8_p3 <- LDA(tidy_EP_chunks_chinese_dtm_p3, k = 8, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k8_p3 <- tidy(chinese_lda_k8_p3)
chinese_lda_td_k8_p3

# check gamma
chinese_lda_gamma_k8_p3 <- tidy(chinese_lda_k8_p3, matrix = "gamma")


# most common terms

top_terms_chinese_k8_p3 <- chinese_lda_td_k8_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k8_p3

# figure

topic_chinese_k8_p3 <- top_terms_chinese_k8_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

chinese_lda_k8_p4 <- LDA(tidy_EP_chunks_chinese_dtm_p4, k = 8, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k8_p4 <- tidy(chinese_lda_k8_p4)
chinese_lda_td_k8_p4

# check gamma
chinese_lda_gamma_k8_p4 <- tidy(chinese_lda_k8_p4, matrix = "gamma")


# most common terms

top_terms_chinese_k8_p4 <- chinese_lda_td_k8_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k8_p4

# figure

topic_chinese_k8_p4 <- top_terms_chinese_k8_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_chinese_k8
top_terms_chinese_k8_p1
top_terms_chinese_k8_p2
top_terms_chinese_k8_p3
top_terms_chinese_k8_p4



### Figure summary

print(topic_chinese_k8)
print(topic_chinese_k8_p1)
print(topic_chinese_k8_p2)
print(topic_chinese_k8_p3)
print(topic_chinese_k8_p4)







### k = 9

# cast topic models

chinese_lda_k9 <- LDA(tidy_EP_chunks_chinese_dtm, k = 9, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k9 <- tidy(chinese_lda_k9)
chinese_lda_td_k9

# check gamma
chinese_lda_gamma_k9 <- tidy(chinese_lda_k9, matrix = "gamma")


# most common terms

top_terms_chinese_k9 <- chinese_lda_td_k9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k9

# figure

topic_chinese_k9 <- top_terms_chinese_k9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

chinese_lda_k9_p1 <- LDA(tidy_EP_chunks_chinese_dtm_p1, k = 9, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k9_p1 <- tidy(chinese_lda_k9_p1)
chinese_lda_td_k9_p1

# check gamma
chinese_lda_gamma_k9_p1 <- tidy(chinese_lda_k9_p1, matrix = "gamma")


# most common terms

top_terms_chinese_k9_p1 <- chinese_lda_td_k9_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k9_p1

# figure

topic_chinese_k9_p1 <- top_terms_chinese_k9_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

chinese_lda_k9_p2 <- LDA(tidy_EP_chunks_chinese_dtm_p2, k = 9, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k9_p2 <- tidy(chinese_lda_k9_p2)
chinese_lda_td_k9_p2

# check gamma
chinese_lda_gamma_k9_p2 <- tidy(chinese_lda_k9_p2, matrix = "gamma")


# most common terms

top_terms_chinese_k9_p2 <- chinese_lda_td_k9_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k9_p2

# figure

topic_chinese_k9_p2 <- top_terms_chinese_k9_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

chinese_lda_k9_p3 <- LDA(tidy_EP_chunks_chinese_dtm_p3, k = 9, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k9_p3 <- tidy(chinese_lda_k9_p3)
chinese_lda_td_k9_p3

# check gamma
chinese_lda_gamma_k9_p3 <- tidy(chinese_lda_k9_p3, matrix = "gamma")


# most common terms

top_terms_chinese_k9_p3 <- chinese_lda_td_k9_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k9_p3

# figure

topic_chinese_k9_p3 <- top_terms_chinese_k9_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

chinese_lda_k9_p4 <- LDA(tidy_EP_chunks_chinese_dtm_p4, k = 9, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k9_p4 <- tidy(chinese_lda_k9_p4)
chinese_lda_td_k9_p4

# check gamma
chinese_lda_gamma_k9_p4 <- tidy(chinese_lda_k9_p4, matrix = "gamma")


# most common terms

top_terms_chinese_k9_p4 <- chinese_lda_td_k9_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k9_p4

# figure

topic_chinese_k9_p4 <- top_terms_chinese_k9_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_chinese_k9
top_terms_chinese_k9_p1
top_terms_chinese_k9_p2
top_terms_chinese_k9_p3
top_terms_chinese_k9_p4



### Figure summary

print(topic_chinese_k9)
print(topic_chinese_k9_p1)
print(topic_chinese_k9_p2)
print(topic_chinese_k9_p3)
print(topic_chinese_k9_p4)







### k = 10

# cast topic models

chinese_lda_k10 <- LDA(tidy_EP_chunks_chinese_dtm, k = 10, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k10 <- tidy(chinese_lda_k10)
chinese_lda_td_k10

# check gamma
chinese_lda_gamma_k10 <- tidy(chinese_lda_k10, matrix = "gamma")


# most common terms

top_terms_chinese_k10 <- chinese_lda_td_k10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k10

# figure

topic_chinese_k10 <- top_terms_chinese_k10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

chinese_lda_k10_p1 <- LDA(tidy_EP_chunks_chinese_dtm_p1, k = 10, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k10_p1 <- tidy(chinese_lda_k10_p1)
chinese_lda_td_k10_p1

# check gamma
chinese_lda_gamma_k10_p1 <- tidy(chinese_lda_k10_p1, matrix = "gamma")


# most common terms

top_terms_chinese_k10_p1 <- chinese_lda_td_k10_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k10_p1

# figure

topic_chinese_k10_p1 <- top_terms_chinese_k10_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

chinese_lda_k10_p2 <- LDA(tidy_EP_chunks_chinese_dtm_p2, k = 10, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k10_p2 <- tidy(chinese_lda_k10_p2)
chinese_lda_td_k10_p2

# check gamma
chinese_lda_gamma_k10_p2 <- tidy(chinese_lda_k10_p2, matrix = "gamma")


# most common terms

top_terms_chinese_k10_p2 <- chinese_lda_td_k10_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k10_p2

# figure

topic_chinese_k10_p2 <- top_terms_chinese_k10_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

chinese_lda_k10_p3 <- LDA(tidy_EP_chunks_chinese_dtm_p3, k = 10, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k10_p3 <- tidy(chinese_lda_k10_p3)
chinese_lda_td_k10_p3

# check gamma
chinese_lda_gamma_k10_p3 <- tidy(chinese_lda_k10_p3, matrix = "gamma")


# most common terms

top_terms_chinese_k10_p3 <- chinese_lda_td_k10_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k10_p3

# figure

topic_chinese_k10_p3 <- top_terms_chinese_k10_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

chinese_lda_k10_p4 <- LDA(tidy_EP_chunks_chinese_dtm_p4, k = 10, control = list(seed = 1234))


# convert back to tidy

chinese_lda_td_k10_p4 <- tidy(chinese_lda_k10_p4)
chinese_lda_td_k10_p4

# check gamma
chinese_lda_gamma_k10_p4 <- tidy(chinese_lda_k10_p4, matrix = "gamma")


# most common terms

top_terms_chinese_k10_p4 <- chinese_lda_td_k10_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_chinese_k10_p4

# figure

topic_chinese_k10_p4 <- top_terms_chinese_k10_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_chinese_k10
top_terms_chinese_k10_p1
top_terms_chinese_k10_p2
top_terms_chinese_k10_p3
top_terms_chinese_k10_p4



### Figure summary

print(topic_chinese_k10)
print(topic_chinese_k10_p1)
print(topic_chinese_k10_p2)
print(topic_chinese_k10_p3)
print(topic_chinese_k10_p4)









############ Topic model by pseudo corpus: mexican



### k = 3

# cast topic models

mexican_lda_k3 <- LDA(tidy_EP_chunks_mexican_dtm, k = 3, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k3 <- tidy(mexican_lda_k3)
mexican_lda_td_k3

# check gamma
mexican_lda_gamma_k3 <- tidy(mexican_lda_k3, matrix = "gamma")


# most common terms

top_terms_mexican_k3 <- mexican_lda_td_k3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k3

# figure

topic_mexican_k3 <- top_terms_mexican_k3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

mexican_lda_k3_p1 <- LDA(tidy_EP_chunks_mexican_dtm_p1, k = 3, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k3_p1 <- tidy(mexican_lda_k3_p1)
mexican_lda_td_k3_p1

# check gamma
mexican_lda_gamma_k3_p1 <- tidy(mexican_lda_k3_p1, matrix = "gamma")


# most common terms

top_terms_mexican_k3_p1 <- mexican_lda_td_k3_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k3_p1

# figure

topic_mexican_k3_p1 <- top_terms_mexican_k3_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

mexican_lda_k3_p2 <- LDA(tidy_EP_chunks_mexican_dtm_p2, k = 3, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k3_p2 <- tidy(mexican_lda_k3_p2)
mexican_lda_td_k3_p2

# check gamma
mexican_lda_gamma_k3_p2 <- tidy(mexican_lda_k3_p2, matrix = "gamma")


# most common terms

top_terms_mexican_k3_p2 <- mexican_lda_td_k3_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k3_p2

# figure

topic_mexican_k3_p2 <- top_terms_mexican_k3_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

mexican_lda_k3_p3 <- LDA(tidy_EP_chunks_mexican_dtm_p3, k = 3, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k3_p3 <- tidy(mexican_lda_k3_p3)
mexican_lda_td_k3_p3

# check gamma
mexican_lda_gamma_k3_p3 <- tidy(mexican_lda_k3_p3, matrix = "gamma")


# most common terms

top_terms_mexican_k3_p3 <- mexican_lda_td_k3_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k3_p3

# figure

topic_mexican_k3_p3 <- top_terms_mexican_k3_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

mexican_lda_k3_p4 <- LDA(tidy_EP_chunks_mexican_dtm_p4, k = 3, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k3_p4 <- tidy(mexican_lda_k3_p4)
mexican_lda_td_k3_p4

# check gamma
mexican_lda_gamma_k3_p4 <- tidy(mexican_lda_k3_p4, matrix = "gamma")


# most common terms

top_terms_mexican_k3_p4 <- mexican_lda_td_k3_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k3_p4

# figure

topic_mexican_k3_p4 <- top_terms_mexican_k3_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_mexican_k3
top_terms_mexican_k3_p1
top_terms_mexican_k3_p2
top_terms_mexican_k3_p3
top_terms_mexican_k3_p4



### Figure summary

print(topic_mexican_k3)
print(topic_mexican_k3_p1)
print(topic_mexican_k3_p2)
print(topic_mexican_k3_p3)
print(topic_mexican_k3_p4)




### k = 4

# cast topic models

mexican_lda_k4 <- LDA(tidy_EP_chunks_mexican_dtm, k = 4, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k4 <- tidy(mexican_lda_k4)
mexican_lda_td_k4

# check gamma
mexican_lda_gamma_k4 <- tidy(mexican_lda_k4, matrix = "gamma")


# most common terms

top_terms_mexican_k4 <- mexican_lda_td_k4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k4

# figure

topic_mexican_k4 <- top_terms_mexican_k4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

mexican_lda_k4_p1 <- LDA(tidy_EP_chunks_mexican_dtm_p1, k = 4, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k4_p1 <- tidy(mexican_lda_k4_p1)
mexican_lda_td_k4_p1

# check gamma
mexican_lda_gamma_k4_p1 <- tidy(mexican_lda_k4_p1, matrix = "gamma")


# most common terms

top_terms_mexican_k4_p1 <- mexican_lda_td_k4_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k4_p1

# figure

topic_mexican_k4_p1 <- top_terms_mexican_k4_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

mexican_lda_k4_p2 <- LDA(tidy_EP_chunks_mexican_dtm_p2, k = 4, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k4_p2 <- tidy(mexican_lda_k4_p2)
mexican_lda_td_k4_p2

# check gamma
mexican_lda_gamma_k4_p2 <- tidy(mexican_lda_k4_p2, matrix = "gamma")


# most common terms

top_terms_mexican_k4_p2 <- mexican_lda_td_k4_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k4_p2

# figure

topic_mexican_k4_p2 <- top_terms_mexican_k4_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

mexican_lda_k4_p3 <- LDA(tidy_EP_chunks_mexican_dtm_p3, k = 4, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k4_p3 <- tidy(mexican_lda_k4_p3)
mexican_lda_td_k4_p3

# check gamma
mexican_lda_gamma_k4_p3 <- tidy(mexican_lda_k4_p3, matrix = "gamma")


# most common terms

top_terms_mexican_k4_p3 <- mexican_lda_td_k4_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k4_p3

# figure

topic_mexican_k4_p3 <- top_terms_mexican_k4_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

mexican_lda_k4_p4 <- LDA(tidy_EP_chunks_mexican_dtm_p4, k = 4, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k4_p4 <- tidy(mexican_lda_k4_p4)
mexican_lda_td_k4_p4

# check gamma
mexican_lda_gamma_k4_p4 <- tidy(mexican_lda_k4_p4, matrix = "gamma")


# most common terms

top_terms_mexican_k4_p4 <- mexican_lda_td_k4_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k4_p4

# figure

topic_mexican_k4_p4 <- top_terms_mexican_k4_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_mexican_k4
top_terms_mexican_k4_p1
top_terms_mexican_k4_p2
top_terms_mexican_k4_p3
top_terms_mexican_k4_p4



### Figure summary

print(topic_mexican_k4)
print(topic_mexican_k4_p1)
print(topic_mexican_k4_p2)
print(topic_mexican_k4_p3)
print(topic_mexican_k4_p4)



### k = 5

# cast topic models

mexican_lda_k5 <- LDA(tidy_EP_chunks_mexican_dtm, k = 5, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k5 <- tidy(mexican_lda_k5)
mexican_lda_td_k5

# check gamma
mexican_lda_gamma_k5 <- tidy(mexican_lda_k5, matrix = "gamma")


# most common terms

top_terms_mexican_k5 <- mexican_lda_td_k5 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k5

# figure

topic_mexican_k5 <- top_terms_mexican_k5 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

mexican_lda_k5_p1 <- LDA(tidy_EP_chunks_mexican_dtm_p1, k = 5, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k5_p1 <- tidy(mexican_lda_k5_p1)
mexican_lda_td_k5_p1

# check gamma
mexican_lda_gamma_k5_p1 <- tidy(mexican_lda_k5_p1, matrix = "gamma")


# most common terms

top_terms_mexican_k5_p1 <- mexican_lda_td_k5_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k5_p1

# figure

topic_mexican_k5_p1 <- top_terms_mexican_k5_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

mexican_lda_k5_p2 <- LDA(tidy_EP_chunks_mexican_dtm_p2, k = 5, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k5_p2 <- tidy(mexican_lda_k5_p2)
mexican_lda_td_k5_p2

# check gamma
mexican_lda_gamma_k5_p2 <- tidy(mexican_lda_k5_p2, matrix = "gamma")


# most common terms

top_terms_mexican_k5_p2 <- mexican_lda_td_k5_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k5_p2

# figure

topic_mexican_k5_p2 <- top_terms_mexican_k5_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

mexican_lda_k5_p3 <- LDA(tidy_EP_chunks_mexican_dtm_p3, k = 5, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k5_p3 <- tidy(mexican_lda_k5_p3)
mexican_lda_td_k5_p3

# check gamma
mexican_lda_gamma_k5_p3 <- tidy(mexican_lda_k5_p3, matrix = "gamma")


# most common terms

top_terms_mexican_k5_p3 <- mexican_lda_td_k5_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k5_p3

# figure

topic_mexican_k5_p3 <- top_terms_mexican_k5_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

mexican_lda_k5_p4 <- LDA(tidy_EP_chunks_mexican_dtm_p4, k = 5, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k5_p4 <- tidy(mexican_lda_k5_p4)
mexican_lda_td_k5_p4

# check gamma
mexican_lda_gamma_k5_p4 <- tidy(mexican_lda_k5_p4, matrix = "gamma")


# most common terms

top_terms_mexican_k5_p4 <- mexican_lda_td_k5_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k5_p4

# figure

topic_mexican_k5_p4 <- top_terms_mexican_k5_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_mexican_k5
top_terms_mexican_k5_p1
top_terms_mexican_k5_p2
top_terms_mexican_k5_p3
top_terms_mexican_k5_p4



### Figure summary

print(topic_mexican_k5)
print(topic_mexican_k5_p1)
print(topic_mexican_k5_p2)
print(topic_mexican_k5_p3)
print(topic_mexican_k5_p4)







### k = 6

# cast topic models

mexican_lda_k6 <- LDA(tidy_EP_chunks_mexican_dtm, k = 6, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k6 <- tidy(mexican_lda_k6)
mexican_lda_td_k6

# check gamma
mexican_lda_gamma_k6 <- tidy(mexican_lda_k6, matrix = "gamma")


# most common terms

top_terms_mexican_k6 <- mexican_lda_td_k6 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k6

# figure

topic_mexican_k6 <- top_terms_mexican_k6 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

mexican_lda_k6_p1 <- LDA(tidy_EP_chunks_mexican_dtm_p1, k = 6, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k6_p1 <- tidy(mexican_lda_k6_p1)
mexican_lda_td_k6_p1

# check gamma
mexican_lda_gamma_k6_p1 <- tidy(mexican_lda_k6_p1, matrix = "gamma")


# most common terms

top_terms_mexican_k6_p1 <- mexican_lda_td_k6_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k6_p1

# figure

topic_mexican_k6_p1 <- top_terms_mexican_k6_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

mexican_lda_k6_p2 <- LDA(tidy_EP_chunks_mexican_dtm_p2, k = 6, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k6_p2 <- tidy(mexican_lda_k6_p2)
mexican_lda_td_k6_p2

# check gamma
mexican_lda_gamma_k6_p2 <- tidy(mexican_lda_k6_p2, matrix = "gamma")


# most common terms

top_terms_mexican_k6_p2 <- mexican_lda_td_k6_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k6_p2

# figure

topic_mexican_k6_p2 <- top_terms_mexican_k6_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

mexican_lda_k6_p3 <- LDA(tidy_EP_chunks_mexican_dtm_p3, k = 6, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k6_p3 <- tidy(mexican_lda_k6_p3)
mexican_lda_td_k6_p3

# check gamma
mexican_lda_gamma_k6_p3 <- tidy(mexican_lda_k6_p3, matrix = "gamma")


# most common terms

top_terms_mexican_k6_p3 <- mexican_lda_td_k6_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k6_p3

# figure

topic_mexican_k6_p3 <- top_terms_mexican_k6_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

mexican_lda_k6_p4 <- LDA(tidy_EP_chunks_mexican_dtm_p4, k = 6, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k6_p4 <- tidy(mexican_lda_k6_p4)
mexican_lda_td_k6_p4

# check gamma
mexican_lda_gamma_k6_p4 <- tidy(mexican_lda_k6_p4, matrix = "gamma")


# most common terms

top_terms_mexican_k6_p4 <- mexican_lda_td_k6_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k6_p4

# figure

topic_mexican_k6_p4 <- top_terms_mexican_k6_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_mexican_k6
top_terms_mexican_k6_p1
top_terms_mexican_k6_p2
top_terms_mexican_k6_p3
top_terms_mexican_k6_p4



### Figure summary

print(topic_mexican_k6)
print(topic_mexican_k6_p1)
print(topic_mexican_k6_p2)
print(topic_mexican_k6_p3)
print(topic_mexican_k6_p4)







### k = 7

# cast topic models

mexican_lda_k7 <- LDA(tidy_EP_chunks_mexican_dtm, k = 7, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k7 <- tidy(mexican_lda_k7)
mexican_lda_td_k7

# check gamma
mexican_lda_gamma_k7 <- tidy(mexican_lda_k7, matrix = "gamma")


# most common terms

top_terms_mexican_k7 <- mexican_lda_td_k7 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k7

# figure

topic_mexican_k7 <- top_terms_mexican_k7 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

mexican_lda_k7_p1 <- LDA(tidy_EP_chunks_mexican_dtm_p1, k = 7, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k7_p1 <- tidy(mexican_lda_k7_p1)
mexican_lda_td_k7_p1

# check gamma
mexican_lda_gamma_k7_p1 <- tidy(mexican_lda_k7_p1, matrix = "gamma")


# most common terms

top_terms_mexican_k7_p1 <- mexican_lda_td_k7_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k7_p1

# figure

topic_mexican_k7_p1 <- top_terms_mexican_k7_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

mexican_lda_k7_p2 <- LDA(tidy_EP_chunks_mexican_dtm_p2, k = 7, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k7_p2 <- tidy(mexican_lda_k7_p2)
mexican_lda_td_k7_p2

# check gamma
mexican_lda_gamma_k7_p2 <- tidy(mexican_lda_k7_p2, matrix = "gamma")


# most common terms

top_terms_mexican_k7_p2 <- mexican_lda_td_k7_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k7_p2

# figure

topic_mexican_k7_p2 <- top_terms_mexican_k7_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

mexican_lda_k7_p3 <- LDA(tidy_EP_chunks_mexican_dtm_p3, k = 7, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k7_p3 <- tidy(mexican_lda_k7_p3)
mexican_lda_td_k7_p3

# check gamma
mexican_lda_gamma_k7_p3 <- tidy(mexican_lda_k7_p3, matrix = "gamma")


# most common terms

top_terms_mexican_k7_p3 <- mexican_lda_td_k7_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k7_p3

# figure

topic_mexican_k7_p3 <- top_terms_mexican_k7_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

mexican_lda_k7_p4 <- LDA(tidy_EP_chunks_mexican_dtm_p4, k = 7, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k7_p4 <- tidy(mexican_lda_k7_p4)
mexican_lda_td_k7_p4

# check gamma
mexican_lda_gamma_k7_p4 <- tidy(mexican_lda_k7_p4, matrix = "gamma")


# most common terms

top_terms_mexican_k7_p4 <- mexican_lda_td_k7_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k7_p4

# figure

topic_mexican_k7_p4 <- top_terms_mexican_k7_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_mexican_k7
top_terms_mexican_k7_p1
top_terms_mexican_k7_p2
top_terms_mexican_k7_p3
top_terms_mexican_k7_p4



### Figure summary

print(topic_mexican_k7)
print(topic_mexican_k7_p1)
print(topic_mexican_k7_p2)
print(topic_mexican_k7_p3)
print(topic_mexican_k7_p4)







### k = 8

# cast topic models

mexican_lda_k8 <- LDA(tidy_EP_chunks_mexican_dtm, k = 8, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k8 <- tidy(mexican_lda_k8)
mexican_lda_td_k8

# check gamma
mexican_lda_gamma_k8 <- tidy(mexican_lda_k8, matrix = "gamma")


# most common terms

top_terms_mexican_k8 <- mexican_lda_td_k8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k8

# figure

topic_mexican_k8 <- top_terms_mexican_k8 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

mexican_lda_k8_p1 <- LDA(tidy_EP_chunks_mexican_dtm_p1, k = 8, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k8_p1 <- tidy(mexican_lda_k8_p1)
mexican_lda_td_k8_p1

# check gamma
mexican_lda_gamma_k8_p1 <- tidy(mexican_lda_k8_p1, matrix = "gamma")


# most common terms

top_terms_mexican_k8_p1 <- mexican_lda_td_k8_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k8_p1

# figure

topic_mexican_k8_p1 <- top_terms_mexican_k8_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

mexican_lda_k8_p2 <- LDA(tidy_EP_chunks_mexican_dtm_p2, k = 8, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k8_p2 <- tidy(mexican_lda_k8_p2)
mexican_lda_td_k8_p2

# check gamma
mexican_lda_gamma_k8_p2 <- tidy(mexican_lda_k8_p2, matrix = "gamma")


# most common terms

top_terms_mexican_k8_p2 <- mexican_lda_td_k8_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k8_p2

# figure

topic_mexican_k8_p2 <- top_terms_mexican_k8_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

mexican_lda_k8_p3 <- LDA(tidy_EP_chunks_mexican_dtm_p3, k = 8, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k8_p3 <- tidy(mexican_lda_k8_p3)
mexican_lda_td_k8_p3

# check gamma
mexican_lda_gamma_k8_p3 <- tidy(mexican_lda_k8_p3, matrix = "gamma")


# most common terms

top_terms_mexican_k8_p3 <- mexican_lda_td_k8_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k8_p3

# figure

topic_mexican_k8_p3 <- top_terms_mexican_k8_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

mexican_lda_k8_p4 <- LDA(tidy_EP_chunks_mexican_dtm_p4, k = 8, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k8_p4 <- tidy(mexican_lda_k8_p4)
mexican_lda_td_k8_p4

# check gamma
mexican_lda_gamma_k8_p4 <- tidy(mexican_lda_k8_p4, matrix = "gamma")


# most common terms

top_terms_mexican_k8_p4 <- mexican_lda_td_k8_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k8_p4

# figure

topic_mexican_k8_p4 <- top_terms_mexican_k8_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_mexican_k8
top_terms_mexican_k8_p1
top_terms_mexican_k8_p2
top_terms_mexican_k8_p3
top_terms_mexican_k8_p4



### Figure summary

print(topic_mexican_k8)
print(topic_mexican_k8_p1)
print(topic_mexican_k8_p2)
print(topic_mexican_k8_p3)
print(topic_mexican_k8_p4)







### k = 9

# cast topic models

mexican_lda_k9 <- LDA(tidy_EP_chunks_mexican_dtm, k = 9, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k9 <- tidy(mexican_lda_k9)
mexican_lda_td_k9

# check gamma
mexican_lda_gamma_k9 <- tidy(mexican_lda_k9, matrix = "gamma")


# most common terms

top_terms_mexican_k9 <- mexican_lda_td_k9 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k9

# figure

topic_mexican_k9 <- top_terms_mexican_k9 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

mexican_lda_k9_p1 <- LDA(tidy_EP_chunks_mexican_dtm_p1, k = 9, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k9_p1 <- tidy(mexican_lda_k9_p1)
mexican_lda_td_k9_p1

# check gamma
mexican_lda_gamma_k9_p1 <- tidy(mexican_lda_k9_p1, matrix = "gamma")


# most common terms

top_terms_mexican_k9_p1 <- mexican_lda_td_k9_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k9_p1

# figure

topic_mexican_k9_p1 <- top_terms_mexican_k9_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

mexican_lda_k9_p2 <- LDA(tidy_EP_chunks_mexican_dtm_p2, k = 9, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k9_p2 <- tidy(mexican_lda_k9_p2)
mexican_lda_td_k9_p2

# check gamma
mexican_lda_gamma_k9_p2 <- tidy(mexican_lda_k9_p2, matrix = "gamma")


# most common terms

top_terms_mexican_k9_p2 <- mexican_lda_td_k9_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k9_p2

# figure

topic_mexican_k9_p2 <- top_terms_mexican_k9_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

mexican_lda_k9_p3 <- LDA(tidy_EP_chunks_mexican_dtm_p3, k = 9, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k9_p3 <- tidy(mexican_lda_k9_p3)
mexican_lda_td_k9_p3

# check gamma
mexican_lda_gamma_k9_p3 <- tidy(mexican_lda_k9_p3, matrix = "gamma")


# most common terms

top_terms_mexican_k9_p3 <- mexican_lda_td_k9_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k9_p3

# figure

topic_mexican_k9_p3 <- top_terms_mexican_k9_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

mexican_lda_k9_p4 <- LDA(tidy_EP_chunks_mexican_dtm_p4, k = 9, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k9_p4 <- tidy(mexican_lda_k9_p4)
mexican_lda_td_k9_p4

# check gamma
mexican_lda_gamma_k9_p4 <- tidy(mexican_lda_k9_p4, matrix = "gamma")


# most common terms

top_terms_mexican_k9_p4 <- mexican_lda_td_k9_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k9_p4

# figure

topic_mexican_k9_p4 <- top_terms_mexican_k9_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_mexican_k9
top_terms_mexican_k9_p1
top_terms_mexican_k9_p2
top_terms_mexican_k9_p3
top_terms_mexican_k9_p4



### Figure summary

print(topic_mexican_k9)
print(topic_mexican_k9_p1)
print(topic_mexican_k9_p2)
print(topic_mexican_k9_p3)
print(topic_mexican_k9_p4)







### k = 10

# cast topic models

mexican_lda_k10 <- LDA(tidy_EP_chunks_mexican_dtm, k = 10, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k10 <- tidy(mexican_lda_k10)
mexican_lda_td_k10

# check gamma
mexican_lda_gamma_k10 <- tidy(mexican_lda_k10, matrix = "gamma")


# most common terms

top_terms_mexican_k10 <- mexican_lda_td_k10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k10

# figure

topic_mexican_k10 <- top_terms_mexican_k10 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#Topic model by pseudo corpus by period
#p1




# cast topic models

mexican_lda_k10_p1 <- LDA(tidy_EP_chunks_mexican_dtm_p1, k = 10, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k10_p1 <- tidy(mexican_lda_k10_p1)
mexican_lda_td_k10_p1

# check gamma
mexican_lda_gamma_k10_p1 <- tidy(mexican_lda_k10_p1, matrix = "gamma")


# most common terms

top_terms_mexican_k10_p1 <- mexican_lda_td_k10_p1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k10_p1

# figure

topic_mexican_k10_p1 <- top_terms_mexican_k10_p1 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p2




# cast topic models

mexican_lda_k10_p2 <- LDA(tidy_EP_chunks_mexican_dtm_p2, k = 10, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k10_p2 <- tidy(mexican_lda_k10_p2)
mexican_lda_td_k10_p2

# check gamma
mexican_lda_gamma_k10_p2 <- tidy(mexican_lda_k10_p2, matrix = "gamma")


# most common terms

top_terms_mexican_k10_p2 <- mexican_lda_td_k10_p2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k10_p2

# figure

topic_mexican_k10_p2 <- top_terms_mexican_k10_p2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p3




# cast topic models

mexican_lda_k10_p3 <- LDA(tidy_EP_chunks_mexican_dtm_p3, k = 10, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k10_p3 <- tidy(mexican_lda_k10_p3)
mexican_lda_td_k10_p3

# check gamma
mexican_lda_gamma_k10_p3 <- tidy(mexican_lda_k10_p3, matrix = "gamma")


# most common terms

top_terms_mexican_k10_p3 <- mexican_lda_td_k10_p3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k10_p3

# figure

topic_mexican_k10_p3 <- top_terms_mexican_k10_p3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


#p4


# cast topic models

mexican_lda_k10_p4 <- LDA(tidy_EP_chunks_mexican_dtm_p4, k = 10, control = list(seed = 1234))


# convert back to tidy

mexican_lda_td_k10_p4 <- tidy(mexican_lda_k10_p4)
mexican_lda_td_k10_p4

# check gamma
mexican_lda_gamma_k10_p4 <- tidy(mexican_lda_k10_p4, matrix = "gamma")


# most common terms

top_terms_mexican_k10_p4 <- mexican_lda_td_k10_p4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_mexican_k10_p4

# figure

topic_mexican_k10_p4 <- top_terms_mexican_k10_p4 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x")


### term summary
top_terms_mexican_k10
top_terms_mexican_k10_p1
top_terms_mexican_k10_p2
top_terms_mexican_k10_p3
top_terms_mexican_k10_p4



### Figure summary

print(topic_mexican_k10)
print(topic_mexican_k10_p1)
print(topic_mexican_k10_p2)
print(topic_mexican_k10_p3)
print(topic_mexican_k10_p4)




############ Topic model by pseudo corpus: PDF




pdf("Kline(WORKING)IMG-topic_k3.pdf")

print(topic_italian_k3)
print(topic_italian_k3_p1)
print(topic_italian_k3_p2)
print(topic_italian_k3_p3)
print(topic_italian_k3_p4)

print(topic_jewish_k3)
print(topic_jewish_k3_p1)
print(topic_jewish_k3_p2)
print(topic_jewish_k3_p3)
print(topic_jewish_k3_p4)

print(topic_catholic_k3)
print(topic_catholic_k3_p1)
print(topic_catholic_k3_p2)
print(topic_catholic_k3_p3)
print(topic_catholic_k3_p4)

print(topic_irish_k3)
print(topic_irish_k3_p1)
print(topic_irish_k3_p2)
print(topic_irish_k3_p3)
print(topic_irish_k3_p4)

print(topic_cuban_k3)
print(topic_cuban_k3_p1)
print(topic_cuban_k3_p2)
print(topic_cuban_k3_p3)
print(topic_cuban_k3_p4)

print(topic_muslim_k3)
print(topic_muslim_k3_p1)
print(topic_muslim_k3_p2)
print(topic_muslim_k3_p3)
print(topic_muslim_k3_p4)

print(topic_chinese_k3)
print(topic_chinese_k3_p1)
print(topic_chinese_k3_p2)
print(topic_chinese_k3_p3)
print(topic_chinese_k3_p4)

print(topic_mexican_k3)
print(topic_mexican_k3_p1)
print(topic_mexican_k3_p2)
print(topic_mexican_k3_p3)
print(topic_mexican_k3_p4)

dev.off()





pdf("Kline(WORKING)IMG-topic_k4.pdf")

print(topic_italian_k4)
print(topic_italian_k4_p1)
print(topic_italian_k4_p2)
print(topic_italian_k4_p3)
print(topic_italian_k4_p4)

print(topic_jewish_k4)
print(topic_jewish_k4_p1)
print(topic_jewish_k4_p2)
print(topic_jewish_k4_p3)
print(topic_jewish_k4_p4)

print(topic_catholic_k4)
print(topic_catholic_k4_p1)
print(topic_catholic_k4_p2)
print(topic_catholic_k4_p3)
print(topic_catholic_k4_p4)

print(topic_irish_k4)
print(topic_irish_k4_p1)
print(topic_irish_k4_p2)
print(topic_irish_k4_p3)
print(topic_irish_k4_p4)

print(topic_cuban_k4)
print(topic_cuban_k4_p1)
print(topic_cuban_k4_p2)
print(topic_cuban_k4_p3)
print(topic_cuban_k4_p4)

print(topic_muslim_k4)
print(topic_muslim_k4_p1)
print(topic_muslim_k4_p2)
print(topic_muslim_k4_p3)
print(topic_muslim_k4_p4)

print(topic_chinese_k4)
print(topic_chinese_k4_p1)
print(topic_chinese_k4_p2)
print(topic_chinese_k4_p3)
print(topic_chinese_k4_p4)

print(topic_mexican_k4)
print(topic_mexican_k4_p1)
print(topic_mexican_k4_p2)
print(topic_mexican_k4_p3)
print(topic_mexican_k4_p4)

dev.off()








pdf("Kline(WORKING)IMG-topic_k5.pdf")

print(topic_italian_k5)
print(topic_italian_k5_p1)
print(topic_italian_k5_p2)
print(topic_italian_k5_p3)
print(topic_italian_k5_p4)

print(topic_jewish_k5)
print(topic_jewish_k5_p1)
print(topic_jewish_k5_p2)
print(topic_jewish_k5_p3)
print(topic_jewish_k5_p4)

print(topic_catholic_k5)
print(topic_catholic_k5_p1)
print(topic_catholic_k5_p2)
print(topic_catholic_k5_p3)
print(topic_catholic_k5_p4)

print(topic_irish_k5)
print(topic_irish_k5_p1)
print(topic_irish_k5_p2)
print(topic_irish_k5_p3)
print(topic_irish_k5_p4)

print(topic_cuban_k5)
print(topic_cuban_k5_p1)
print(topic_cuban_k5_p2)
print(topic_cuban_k5_p3)
print(topic_cuban_k5_p4)

print(topic_muslim_k5)
print(topic_muslim_k5_p1)
print(topic_muslim_k5_p2)
print(topic_muslim_k5_p3)
print(topic_muslim_k5_p4)

print(topic_chinese_k5)
print(topic_chinese_k5_p1)
print(topic_chinese_k5_p2)
print(topic_chinese_k5_p3)
print(topic_chinese_k5_p4)

print(topic_mexican_k5)
print(topic_mexican_k5_p1)
print(topic_mexican_k5_p2)
print(topic_mexican_k5_p3)
print(topic_mexican_k5_p4)

dev.off()








pdf("Kline(WORKING)IMG-topic_k6.pdf")

print(topic_italian_k6)
print(topic_italian_k6_p1)
print(topic_italian_k6_p2)
print(topic_italian_k6_p3)
print(topic_italian_k6_p4)

print(topic_jewish_k6)
print(topic_jewish_k6_p1)
print(topic_jewish_k6_p2)
print(topic_jewish_k6_p3)
print(topic_jewish_k6_p4)

print(topic_catholic_k6)
print(topic_catholic_k6_p1)
print(topic_catholic_k6_p2)
print(topic_catholic_k6_p3)
print(topic_catholic_k6_p4)

print(topic_irish_k6)
print(topic_irish_k6_p1)
print(topic_irish_k6_p2)
print(topic_irish_k6_p3)
print(topic_irish_k6_p4)

print(topic_cuban_k6)
print(topic_cuban_k6_p1)
print(topic_cuban_k6_p2)
print(topic_cuban_k6_p3)
print(topic_cuban_k6_p4)

print(topic_muslim_k6)
print(topic_muslim_k6_p1)
print(topic_muslim_k6_p2)
print(topic_muslim_k6_p3)
print(topic_muslim_k6_p4)

print(topic_chinese_k6)
print(topic_chinese_k6_p1)
print(topic_chinese_k6_p2)
print(topic_chinese_k6_p3)
print(topic_chinese_k6_p4)

print(topic_mexican_k6)
print(topic_mexican_k6_p1)
print(topic_mexican_k6_p2)
print(topic_mexican_k6_p3)
print(topic_mexican_k6_p4)

dev.off()








pdf("Kline(WORKING)IMG-topic_k7.pdf")

print(topic_italian_k7)
print(topic_italian_k7_p1)
print(topic_italian_k7_p2)
print(topic_italian_k7_p3)
print(topic_italian_k7_p4)

print(topic_jewish_k7)
print(topic_jewish_k7_p1)
print(topic_jewish_k7_p2)
print(topic_jewish_k7_p3)
print(topic_jewish_k7_p4)

print(topic_catholic_k7)
print(topic_catholic_k7_p1)
print(topic_catholic_k7_p2)
print(topic_catholic_k7_p3)
print(topic_catholic_k7_p4)

print(topic_irish_k7)
print(topic_irish_k7_p1)
print(topic_irish_k7_p2)
print(topic_irish_k7_p3)
print(topic_irish_k7_p4)

print(topic_cuban_k7)
print(topic_cuban_k7_p1)
print(topic_cuban_k7_p2)
print(topic_cuban_k7_p3)
print(topic_cuban_k7_p4)

print(topic_muslim_k7)
print(topic_muslim_k7_p1)
print(topic_muslim_k7_p2)
print(topic_muslim_k7_p3)
print(topic_muslim_k7_p4)

print(topic_chinese_k7)
print(topic_chinese_k7_p1)
print(topic_chinese_k7_p2)
print(topic_chinese_k7_p3)
print(topic_chinese_k7_p4)

print(topic_mexican_k7)
print(topic_mexican_k7_p1)
print(topic_mexican_k7_p2)
print(topic_mexican_k7_p3)
print(topic_mexican_k7_p4)

dev.off()








pdf("Kline(WORKING)IMG-topic_k8.pdf")

print(topic_italian_k8)
print(topic_italian_k8_p1)
print(topic_italian_k8_p2)
print(topic_italian_k8_p3)
print(topic_italian_k8_p4)

print(topic_jewish_k8)
print(topic_jewish_k8_p1)
print(topic_jewish_k8_p2)
print(topic_jewish_k8_p3)
print(topic_jewish_k8_p4)

print(topic_catholic_k8)
print(topic_catholic_k8_p1)
print(topic_catholic_k8_p2)
print(topic_catholic_k8_p3)
print(topic_catholic_k8_p4)

print(topic_irish_k8)
print(topic_irish_k8_p1)
print(topic_irish_k8_p2)
print(topic_irish_k8_p3)
print(topic_irish_k8_p4)

print(topic_cuban_k8)
print(topic_cuban_k8_p1)
print(topic_cuban_k8_p2)
print(topic_cuban_k8_p3)
print(topic_cuban_k8_p4)

print(topic_muslim_k8)
print(topic_muslim_k8_p1)
print(topic_muslim_k8_p2)
print(topic_muslim_k8_p3)
print(topic_muslim_k8_p4)

print(topic_chinese_k8)
print(topic_chinese_k8_p1)
print(topic_chinese_k8_p2)
print(topic_chinese_k8_p3)
print(topic_chinese_k8_p4)

print(topic_mexican_k8)
print(topic_mexican_k8_p1)
print(topic_mexican_k8_p2)
print(topic_mexican_k8_p3)
print(topic_mexican_k8_p4)

dev.off()








pdf("Kline(WORKING)IMG-topic_k9.pdf")

print(topic_italian_k9)
print(topic_italian_k9_p1)
print(topic_italian_k9_p2)
print(topic_italian_k9_p3)
print(topic_italian_k9_p4)

print(topic_jewish_k9)
print(topic_jewish_k9_p1)
print(topic_jewish_k9_p2)
print(topic_jewish_k9_p3)
print(topic_jewish_k9_p4)

print(topic_catholic_k9)
print(topic_catholic_k9_p1)
print(topic_catholic_k9_p2)
print(topic_catholic_k9_p3)
print(topic_catholic_k9_p4)

print(topic_irish_k9)
print(topic_irish_k9_p1)
print(topic_irish_k9_p2)
print(topic_irish_k9_p3)
print(topic_irish_k9_p4)

print(topic_cuban_k9)
print(topic_cuban_k9_p1)
print(topic_cuban_k9_p2)
print(topic_cuban_k9_p3)
print(topic_cuban_k9_p4)

print(topic_muslim_k9)
print(topic_muslim_k9_p1)
print(topic_muslim_k9_p2)
print(topic_muslim_k9_p3)
print(topic_muslim_k9_p4)

print(topic_chinese_k9)
print(topic_chinese_k9_p1)
print(topic_chinese_k9_p2)
print(topic_chinese_k9_p3)
print(topic_chinese_k9_p4)

print(topic_mexican_k9)
print(topic_mexican_k9_p1)
print(topic_mexican_k9_p2)
print(topic_mexican_k9_p3)
print(topic_mexican_k9_p4)

dev.off()








pdf("Kline(WORKING)IMG-topic_k10.pdf")

print(topic_italian_k10)
print(topic_italian_k10_p1)
print(topic_italian_k10_p2)
print(topic_italian_k10_p3)
print(topic_italian_k10_p4)

print(topic_jewish_k10)
print(topic_jewish_k10_p1)
print(topic_jewish_k10_p2)
print(topic_jewish_k10_p3)
print(topic_jewish_k10_p4)

print(topic_catholic_k10)
print(topic_catholic_k10_p1)
print(topic_catholic_k10_p2)
print(topic_catholic_k10_p3)
print(topic_catholic_k10_p4)

print(topic_irish_k10)
print(topic_irish_k10_p1)
print(topic_irish_k10_p2)
print(topic_irish_k10_p3)
print(topic_irish_k10_p4)

print(topic_cuban_k10)
print(topic_cuban_k10_p1)
print(topic_cuban_k10_p2)
print(topic_cuban_k10_p3)
print(topic_cuban_k10_p4)

print(topic_muslim_k10)
print(topic_muslim_k10_p1)
print(topic_muslim_k10_p2)
print(topic_muslim_k10_p3)
print(topic_muslim_k10_p4)

print(topic_chinese_k10)
print(topic_chinese_k10_p1)
print(topic_chinese_k10_p2)
print(topic_chinese_k10_p3)
print(topic_chinese_k10_p4)

print(topic_mexican_k10)
print(topic_mexican_k10_p1)
print(topic_mexican_k10_p2)
print(topic_mexican_k10_p3)
print(topic_mexican_k10_p4)

dev.off()
















############ Topic model by pseudo corpus: Convert to CSV




##### k3
# italian
# column name
top_terms_italian_k3_p1 <- rename(top_terms_italian_k3_p1, term_p1 = term) 
top_terms_italian_k3_p2 <- rename(top_terms_italian_k3_p2, term_p2 = term) 
top_terms_italian_k3_p3 <- rename(top_terms_italian_k3_p3, term_p3 = term) 
top_terms_italian_k3_p4 <- rename(top_terms_italian_k3_p4, term_p4 = term) 

# remove beta

top_terms_italian_k3     <- select(top_terms_italian_k3,     -(beta))
top_terms_italian_k3_p1  <- select(top_terms_italian_k3_p1,  -(beta))
top_terms_italian_k3_p2  <- select(top_terms_italian_k3_p2,  -(beta))
top_terms_italian_k3_p3  <- select(top_terms_italian_k3_p3,  -(beta))
top_terms_italian_k3_p4  <- select(top_terms_italian_k3_p4,  -(beta))

# add row name
top_terms_italian_k3      <- add_row_numbers(top_terms_italian_k3,      name = "n", zero_based = FALSE)
top_terms_italian_k3_p1   <- add_row_numbers(top_terms_italian_k3_p1,   name = "n", zero_based = FALSE)
top_terms_italian_k3_p2   <- add_row_numbers(top_terms_italian_k3_p2,   name = "n", zero_based = FALSE)
top_terms_italian_k3_p3   <- add_row_numbers(top_terms_italian_k3_p3,   name = "n", zero_based = FALSE)
top_terms_italian_k3_p4   <- add_row_numbers(top_terms_italian_k3_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_italian_k3 <- inner_join(top_terms_italian_k3, top_terms_italian_k3_p1)
top_terms_italian_k3 <- inner_join(top_terms_italian_k3, top_terms_italian_k3_p2)
top_terms_italian_k3 <- inner_join(top_terms_italian_k3, top_terms_italian_k3_p3)
top_terms_italian_k3 <- inner_join(top_terms_italian_k3, top_terms_italian_k3_p4)

write.table(top_terms_italian_k3, "top_terms_italian_k3.csv", row.names = FALSE)





# jewish
# column name
top_terms_jewish_k3_p1 <- rename(top_terms_jewish_k3_p1, term_p1 = term) 
top_terms_jewish_k3_p2 <- rename(top_terms_jewish_k3_p2, term_p2 = term) 
top_terms_jewish_k3_p3 <- rename(top_terms_jewish_k3_p3, term_p3 = term) 
top_terms_jewish_k3_p4 <- rename(top_terms_jewish_k3_p4, term_p4 = term) 

# remove beta

top_terms_jewish_k3     <- select(top_terms_jewish_k3,     -(beta))
top_terms_jewish_k3_p1  <- select(top_terms_jewish_k3_p1,  -(beta))
top_terms_jewish_k3_p2  <- select(top_terms_jewish_k3_p2,  -(beta))
top_terms_jewish_k3_p3  <- select(top_terms_jewish_k3_p3,  -(beta))
top_terms_jewish_k3_p4  <- select(top_terms_jewish_k3_p4,  -(beta))

# add row name
top_terms_jewish_k3      <- add_row_numbers(top_terms_jewish_k3,      name = "n", zero_based = FALSE)
top_terms_jewish_k3_p1   <- add_row_numbers(top_terms_jewish_k3_p1,   name = "n", zero_based = FALSE)
top_terms_jewish_k3_p2   <- add_row_numbers(top_terms_jewish_k3_p2,   name = "n", zero_based = FALSE)
top_terms_jewish_k3_p3   <- add_row_numbers(top_terms_jewish_k3_p3,   name = "n", zero_based = FALSE)
top_terms_jewish_k3_p4   <- add_row_numbers(top_terms_jewish_k3_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_jewish_k3 <- inner_join(top_terms_jewish_k3, top_terms_jewish_k3_p1)
top_terms_jewish_k3 <- inner_join(top_terms_jewish_k3, top_terms_jewish_k3_p2)
top_terms_jewish_k3 <- inner_join(top_terms_jewish_k3, top_terms_jewish_k3_p3)
top_terms_jewish_k3 <- inner_join(top_terms_jewish_k3, top_terms_jewish_k3_p4)

write.table(top_terms_jewish_k3, "top_terms_jewish_k3.csv", row.names = FALSE)





# catholic
# column name
top_terms_catholic_k3_p1 <- rename(top_terms_catholic_k3_p1, term_p1 = term) 
top_terms_catholic_k3_p2 <- rename(top_terms_catholic_k3_p2, term_p2 = term) 
top_terms_catholic_k3_p3 <- rename(top_terms_catholic_k3_p3, term_p3 = term) 
top_terms_catholic_k3_p4 <- rename(top_terms_catholic_k3_p4, term_p4 = term) 

# remove beta

top_terms_catholic_k3     <- select(top_terms_catholic_k3,     -(beta))
top_terms_catholic_k3_p1  <- select(top_terms_catholic_k3_p1,  -(beta))
top_terms_catholic_k3_p2  <- select(top_terms_catholic_k3_p2,  -(beta))
top_terms_catholic_k3_p3  <- select(top_terms_catholic_k3_p3,  -(beta))
top_terms_catholic_k3_p4  <- select(top_terms_catholic_k3_p4,  -(beta))

# add row name
top_terms_catholic_k3      <- add_row_numbers(top_terms_catholic_k3,      name = "n", zero_based = FALSE)
top_terms_catholic_k3_p1   <- add_row_numbers(top_terms_catholic_k3_p1,   name = "n", zero_based = FALSE)
top_terms_catholic_k3_p2   <- add_row_numbers(top_terms_catholic_k3_p2,   name = "n", zero_based = FALSE)
top_terms_catholic_k3_p3   <- add_row_numbers(top_terms_catholic_k3_p3,   name = "n", zero_based = FALSE)
top_terms_catholic_k3_p4   <- add_row_numbers(top_terms_catholic_k3_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_catholic_k3 <- inner_join(top_terms_catholic_k3, top_terms_catholic_k3_p1)
top_terms_catholic_k3 <- inner_join(top_terms_catholic_k3, top_terms_catholic_k3_p2)
top_terms_catholic_k3 <- inner_join(top_terms_catholic_k3, top_terms_catholic_k3_p3)
top_terms_catholic_k3 <- inner_join(top_terms_catholic_k3, top_terms_catholic_k3_p4)

write.table(top_terms_catholic_k3, "top_terms_catholic_k3.csv", row.names = FALSE)





# irish
# column name
top_terms_irish_k3_p1 <- rename(top_terms_irish_k3_p1, term_p1 = term) 
top_terms_irish_k3_p2 <- rename(top_terms_irish_k3_p2, term_p2 = term) 
top_terms_irish_k3_p3 <- rename(top_terms_irish_k3_p3, term_p3 = term) 
top_terms_irish_k3_p4 <- rename(top_terms_irish_k3_p4, term_p4 = term) 

# remove beta

top_terms_irish_k3     <- select(top_terms_irish_k3,     -(beta))
top_terms_irish_k3_p1  <- select(top_terms_irish_k3_p1,  -(beta))
top_terms_irish_k3_p2  <- select(top_terms_irish_k3_p2,  -(beta))
top_terms_irish_k3_p3  <- select(top_terms_irish_k3_p3,  -(beta))
top_terms_irish_k3_p4  <- select(top_terms_irish_k3_p4,  -(beta))

# add row name
top_terms_irish_k3      <- add_row_numbers(top_terms_irish_k3,      name = "n", zero_based = FALSE)
top_terms_irish_k3_p1   <- add_row_numbers(top_terms_irish_k3_p1,   name = "n", zero_based = FALSE)
top_terms_irish_k3_p2   <- add_row_numbers(top_terms_irish_k3_p2,   name = "n", zero_based = FALSE)
top_terms_irish_k3_p3   <- add_row_numbers(top_terms_irish_k3_p3,   name = "n", zero_based = FALSE)
top_terms_irish_k3_p4   <- add_row_numbers(top_terms_irish_k3_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_irish_k3 <- inner_join(top_terms_irish_k3, top_terms_irish_k3_p1)
top_terms_irish_k3 <- inner_join(top_terms_irish_k3, top_terms_irish_k3_p2)
top_terms_irish_k3 <- inner_join(top_terms_irish_k3, top_terms_irish_k3_p3)
top_terms_irish_k3 <- inner_join(top_terms_irish_k3, top_terms_irish_k3_p4)

write.table(top_terms_irish_k3, "top_terms_irish_k3.csv", row.names = FALSE)





# cuban
# column name
top_terms_cuban_k3_p1 <- rename(top_terms_cuban_k3_p1, term_p1 = term) 
top_terms_cuban_k3_p2 <- rename(top_terms_cuban_k3_p2, term_p2 = term) 
top_terms_cuban_k3_p3 <- rename(top_terms_cuban_k3_p3, term_p3 = term) 
top_terms_cuban_k3_p4 <- rename(top_terms_cuban_k3_p4, term_p4 = term) 

# remove beta

top_terms_cuban_k3     <- select(top_terms_cuban_k3,     -(beta))
top_terms_cuban_k3_p1  <- select(top_terms_cuban_k3_p1,  -(beta))
top_terms_cuban_k3_p2  <- select(top_terms_cuban_k3_p2,  -(beta))
top_terms_cuban_k3_p3  <- select(top_terms_cuban_k3_p3,  -(beta))
top_terms_cuban_k3_p4  <- select(top_terms_cuban_k3_p4,  -(beta))

# add row name
top_terms_cuban_k3      <- add_row_numbers(top_terms_cuban_k3,      name = "n", zero_based = FALSE)
top_terms_cuban_k3_p1   <- add_row_numbers(top_terms_cuban_k3_p1,   name = "n", zero_based = FALSE)
top_terms_cuban_k3_p2   <- add_row_numbers(top_terms_cuban_k3_p2,   name = "n", zero_based = FALSE)
top_terms_cuban_k3_p3   <- add_row_numbers(top_terms_cuban_k3_p3,   name = "n", zero_based = FALSE)
top_terms_cuban_k3_p4   <- add_row_numbers(top_terms_cuban_k3_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_cuban_k3 <- inner_join(top_terms_cuban_k3, top_terms_cuban_k3_p1)
top_terms_cuban_k3 <- inner_join(top_terms_cuban_k3, top_terms_cuban_k3_p2)
top_terms_cuban_k3 <- inner_join(top_terms_cuban_k3, top_terms_cuban_k3_p3)
top_terms_cuban_k3 <- inner_join(top_terms_cuban_k3, top_terms_cuban_k3_p4)

write.table(top_terms_cuban_k3, "top_terms_cuban_k3.csv", row.names = FALSE)





# muslim
# column name
top_terms_muslim_k3_p1 <- rename(top_terms_muslim_k3_p1, term_p1 = term) 
top_terms_muslim_k3_p2 <- rename(top_terms_muslim_k3_p2, term_p2 = term) 
top_terms_muslim_k3_p3 <- rename(top_terms_muslim_k3_p3, term_p3 = term) 
top_terms_muslim_k3_p4 <- rename(top_terms_muslim_k3_p4, term_p4 = term) 

# remove beta

top_terms_muslim_k3     <- select(top_terms_muslim_k3,     -(beta))
top_terms_muslim_k3_p1  <- select(top_terms_muslim_k3_p1,  -(beta))
top_terms_muslim_k3_p2  <- select(top_terms_muslim_k3_p2,  -(beta))
top_terms_muslim_k3_p3  <- select(top_terms_muslim_k3_p3,  -(beta))
top_terms_muslim_k3_p4  <- select(top_terms_muslim_k3_p4,  -(beta))

# add row name
top_terms_muslim_k3      <- add_row_numbers(top_terms_muslim_k3,      name = "n", zero_based = FALSE)
top_terms_muslim_k3_p1   <- add_row_numbers(top_terms_muslim_k3_p1,   name = "n", zero_based = FALSE)
top_terms_muslim_k3_p2   <- add_row_numbers(top_terms_muslim_k3_p2,   name = "n", zero_based = FALSE)
top_terms_muslim_k3_p3   <- add_row_numbers(top_terms_muslim_k3_p3,   name = "n", zero_based = FALSE)
top_terms_muslim_k3_p4   <- add_row_numbers(top_terms_muslim_k3_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_muslim_k3 <- inner_join(top_terms_muslim_k3, top_terms_muslim_k3_p1)
top_terms_muslim_k3 <- inner_join(top_terms_muslim_k3, top_terms_muslim_k3_p2)
top_terms_muslim_k3 <- inner_join(top_terms_muslim_k3, top_terms_muslim_k3_p3)
top_terms_muslim_k3 <- inner_join(top_terms_muslim_k3, top_terms_muslim_k3_p4)

write.table(top_terms_muslim_k3, "top_terms_muslim_k3.csv", row.names = FALSE)





# chinese
# column name
top_terms_chinese_k3_p1 <- rename(top_terms_chinese_k3_p1, term_p1 = term) 
top_terms_chinese_k3_p2 <- rename(top_terms_chinese_k3_p2, term_p2 = term) 
top_terms_chinese_k3_p3 <- rename(top_terms_chinese_k3_p3, term_p3 = term) 
top_terms_chinese_k3_p4 <- rename(top_terms_chinese_k3_p4, term_p4 = term) 

# remove beta

top_terms_chinese_k3     <- select(top_terms_chinese_k3,     -(beta))
top_terms_chinese_k3_p1  <- select(top_terms_chinese_k3_p1,  -(beta))
top_terms_chinese_k3_p2  <- select(top_terms_chinese_k3_p2,  -(beta))
top_terms_chinese_k3_p3  <- select(top_terms_chinese_k3_p3,  -(beta))
top_terms_chinese_k3_p4  <- select(top_terms_chinese_k3_p4,  -(beta))

# add row name
top_terms_chinese_k3      <- add_row_numbers(top_terms_chinese_k3,      name = "n", zero_based = FALSE)
top_terms_chinese_k3_p1   <- add_row_numbers(top_terms_chinese_k3_p1,   name = "n", zero_based = FALSE)
top_terms_chinese_k3_p2   <- add_row_numbers(top_terms_chinese_k3_p2,   name = "n", zero_based = FALSE)
top_terms_chinese_k3_p3   <- add_row_numbers(top_terms_chinese_k3_p3,   name = "n", zero_based = FALSE)
top_terms_chinese_k3_p4   <- add_row_numbers(top_terms_chinese_k3_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_chinese_k3 <- inner_join(top_terms_chinese_k3, top_terms_chinese_k3_p1)
top_terms_chinese_k3 <- inner_join(top_terms_chinese_k3, top_terms_chinese_k3_p2)
top_terms_chinese_k3 <- inner_join(top_terms_chinese_k3, top_terms_chinese_k3_p3)
top_terms_chinese_k3 <- inner_join(top_terms_chinese_k3, top_terms_chinese_k3_p4)

write.table(top_terms_chinese_k3, "top_terms_chinese_k3.csv", row.names = FALSE)


# mexican
# column name
top_terms_mexican_k3_p1 <- rename(top_terms_mexican_k3_p1, term_p1 = term) 
top_terms_mexican_k3_p2 <- rename(top_terms_mexican_k3_p2, term_p2 = term) 
top_terms_mexican_k3_p3 <- rename(top_terms_mexican_k3_p3, term_p3 = term) 
top_terms_mexican_k3_p4 <- rename(top_terms_mexican_k3_p4, term_p4 = term) 

# remove beta

top_terms_mexican_k3     <- select(top_terms_mexican_k3,     -(beta))
top_terms_mexican_k3_p1  <- select(top_terms_mexican_k3_p1,  -(beta))
top_terms_mexican_k3_p2  <- select(top_terms_mexican_k3_p2,  -(beta))
top_terms_mexican_k3_p3  <- select(top_terms_mexican_k3_p3,  -(beta))
top_terms_mexican_k3_p4  <- select(top_terms_mexican_k3_p4,  -(beta))

# add row name
top_terms_mexican_k3      <- add_row_numbers(top_terms_mexican_k3,      name = "n", zero_based = FALSE)
top_terms_mexican_k3_p1   <- add_row_numbers(top_terms_mexican_k3_p1,   name = "n", zero_based = FALSE)
top_terms_mexican_k3_p2   <- add_row_numbers(top_terms_mexican_k3_p2,   name = "n", zero_based = FALSE)
top_terms_mexican_k3_p3   <- add_row_numbers(top_terms_mexican_k3_p3,   name = "n", zero_based = FALSE)
top_terms_mexican_k3_p4   <- add_row_numbers(top_terms_mexican_k3_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_mexican_k3 <- inner_join(top_terms_mexican_k3, top_terms_mexican_k3_p1)
top_terms_mexican_k3 <- inner_join(top_terms_mexican_k3, top_terms_mexican_k3_p2)
top_terms_mexican_k3 <- inner_join(top_terms_mexican_k3, top_terms_mexican_k3_p3)
top_terms_mexican_k3 <- inner_join(top_terms_mexican_k3, top_terms_mexican_k3_p4)

write.table(top_terms_mexican_k3, "top_terms_mexican_k3.csv", row.names = FALSE)






##### k4
# italian
# column name
top_terms_italian_k4_p1 <- rename(top_terms_italian_k4_p1, term_p1 = term) 
top_terms_italian_k4_p2 <- rename(top_terms_italian_k4_p2, term_p2 = term) 
top_terms_italian_k4_p3 <- rename(top_terms_italian_k4_p3, term_p3 = term) 
top_terms_italian_k4_p4 <- rename(top_terms_italian_k4_p4, term_p4 = term) 

# remove beta

top_terms_italian_k4     <- select(top_terms_italian_k4,     -(beta))
top_terms_italian_k4_p1  <- select(top_terms_italian_k4_p1,  -(beta))
top_terms_italian_k4_p2  <- select(top_terms_italian_k4_p2,  -(beta))
top_terms_italian_k4_p3  <- select(top_terms_italian_k4_p3,  -(beta))
top_terms_italian_k4_p4  <- select(top_terms_italian_k4_p4,  -(beta))

# add row name
top_terms_italian_k4      <- add_row_numbers(top_terms_italian_k4,      name = "n", zero_based = FALSE)
top_terms_italian_k4_p1   <- add_row_numbers(top_terms_italian_k4_p1,   name = "n", zero_based = FALSE)
top_terms_italian_k4_p2   <- add_row_numbers(top_terms_italian_k4_p2,   name = "n", zero_based = FALSE)
top_terms_italian_k4_p3   <- add_row_numbers(top_terms_italian_k4_p3,   name = "n", zero_based = FALSE)
top_terms_italian_k4_p4   <- add_row_numbers(top_terms_italian_k4_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_italian_k4 <- inner_join(top_terms_italian_k4, top_terms_italian_k4_p1)
top_terms_italian_k4 <- inner_join(top_terms_italian_k4, top_terms_italian_k4_p2)
top_terms_italian_k4 <- inner_join(top_terms_italian_k4, top_terms_italian_k4_p3)
top_terms_italian_k4 <- inner_join(top_terms_italian_k4, top_terms_italian_k4_p4)

write.table(top_terms_italian_k4, "top_terms_italian_k4.csv", row.names = FALSE)





# jewish
# column name
top_terms_jewish_k4_p1 <- rename(top_terms_jewish_k4_p1, term_p1 = term) 
top_terms_jewish_k4_p2 <- rename(top_terms_jewish_k4_p2, term_p2 = term) 
top_terms_jewish_k4_p3 <- rename(top_terms_jewish_k4_p3, term_p3 = term) 
top_terms_jewish_k4_p4 <- rename(top_terms_jewish_k4_p4, term_p4 = term) 

# remove beta

top_terms_jewish_k4     <- select(top_terms_jewish_k4,     -(beta))
top_terms_jewish_k4_p1  <- select(top_terms_jewish_k4_p1,  -(beta))
top_terms_jewish_k4_p2  <- select(top_terms_jewish_k4_p2,  -(beta))
top_terms_jewish_k4_p3  <- select(top_terms_jewish_k4_p3,  -(beta))
top_terms_jewish_k4_p4  <- select(top_terms_jewish_k4_p4,  -(beta))

# add row name
top_terms_jewish_k4      <- add_row_numbers(top_terms_jewish_k4,      name = "n", zero_based = FALSE)
top_terms_jewish_k4_p1   <- add_row_numbers(top_terms_jewish_k4_p1,   name = "n", zero_based = FALSE)
top_terms_jewish_k4_p2   <- add_row_numbers(top_terms_jewish_k4_p2,   name = "n", zero_based = FALSE)
top_terms_jewish_k4_p3   <- add_row_numbers(top_terms_jewish_k4_p3,   name = "n", zero_based = FALSE)
top_terms_jewish_k4_p4   <- add_row_numbers(top_terms_jewish_k4_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_jewish_k4 <- inner_join(top_terms_jewish_k4, top_terms_jewish_k4_p1)
top_terms_jewish_k4 <- inner_join(top_terms_jewish_k4, top_terms_jewish_k4_p2)
top_terms_jewish_k4 <- inner_join(top_terms_jewish_k4, top_terms_jewish_k4_p3)
top_terms_jewish_k4 <- inner_join(top_terms_jewish_k4, top_terms_jewish_k4_p4)

write.table(top_terms_jewish_k4, "top_terms_jewish_k4.csv", row.names = FALSE)





# catholic
# column name
top_terms_catholic_k4_p1 <- rename(top_terms_catholic_k4_p1, term_p1 = term) 
top_terms_catholic_k4_p2 <- rename(top_terms_catholic_k4_p2, term_p2 = term) 
top_terms_catholic_k4_p3 <- rename(top_terms_catholic_k4_p3, term_p3 = term) 
top_terms_catholic_k4_p4 <- rename(top_terms_catholic_k4_p4, term_p4 = term) 

# remove beta

top_terms_catholic_k4     <- select(top_terms_catholic_k4,     -(beta))
top_terms_catholic_k4_p1  <- select(top_terms_catholic_k4_p1,  -(beta))
top_terms_catholic_k4_p2  <- select(top_terms_catholic_k4_p2,  -(beta))
top_terms_catholic_k4_p3  <- select(top_terms_catholic_k4_p3,  -(beta))
top_terms_catholic_k4_p4  <- select(top_terms_catholic_k4_p4,  -(beta))

# add row name
top_terms_catholic_k4      <- add_row_numbers(top_terms_catholic_k4,      name = "n", zero_based = FALSE)
top_terms_catholic_k4_p1   <- add_row_numbers(top_terms_catholic_k4_p1,   name = "n", zero_based = FALSE)
top_terms_catholic_k4_p2   <- add_row_numbers(top_terms_catholic_k4_p2,   name = "n", zero_based = FALSE)
top_terms_catholic_k4_p3   <- add_row_numbers(top_terms_catholic_k4_p3,   name = "n", zero_based = FALSE)
top_terms_catholic_k4_p4   <- add_row_numbers(top_terms_catholic_k4_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_catholic_k4 <- inner_join(top_terms_catholic_k4, top_terms_catholic_k4_p1)
top_terms_catholic_k4 <- inner_join(top_terms_catholic_k4, top_terms_catholic_k4_p2)
top_terms_catholic_k4 <- inner_join(top_terms_catholic_k4, top_terms_catholic_k4_p3)
top_terms_catholic_k4 <- inner_join(top_terms_catholic_k4, top_terms_catholic_k4_p4)

write.table(top_terms_catholic_k4, "top_terms_catholic_k4.csv", row.names = FALSE)





# irish
# column name
top_terms_irish_k4_p1 <- rename(top_terms_irish_k4_p1, term_p1 = term) 
top_terms_irish_k4_p2 <- rename(top_terms_irish_k4_p2, term_p2 = term) 
top_terms_irish_k4_p3 <- rename(top_terms_irish_k4_p3, term_p3 = term) 
top_terms_irish_k4_p4 <- rename(top_terms_irish_k4_p4, term_p4 = term) 

# remove beta

top_terms_irish_k4     <- select(top_terms_irish_k4,     -(beta))
top_terms_irish_k4_p1  <- select(top_terms_irish_k4_p1,  -(beta))
top_terms_irish_k4_p2  <- select(top_terms_irish_k4_p2,  -(beta))
top_terms_irish_k4_p3  <- select(top_terms_irish_k4_p3,  -(beta))
top_terms_irish_k4_p4  <- select(top_terms_irish_k4_p4,  -(beta))

# add row name
top_terms_irish_k4      <- add_row_numbers(top_terms_irish_k4,      name = "n", zero_based = FALSE)
top_terms_irish_k4_p1   <- add_row_numbers(top_terms_irish_k4_p1,   name = "n", zero_based = FALSE)
top_terms_irish_k4_p2   <- add_row_numbers(top_terms_irish_k4_p2,   name = "n", zero_based = FALSE)
top_terms_irish_k4_p3   <- add_row_numbers(top_terms_irish_k4_p3,   name = "n", zero_based = FALSE)
top_terms_irish_k4_p4   <- add_row_numbers(top_terms_irish_k4_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_irish_k4 <- inner_join(top_terms_irish_k4, top_terms_irish_k4_p1)
top_terms_irish_k4 <- inner_join(top_terms_irish_k4, top_terms_irish_k4_p2)
top_terms_irish_k4 <- inner_join(top_terms_irish_k4, top_terms_irish_k4_p3)
top_terms_irish_k4 <- inner_join(top_terms_irish_k4, top_terms_irish_k4_p4)

write.table(top_terms_irish_k4, "top_terms_irish_k4.csv", row.names = FALSE)





# cuban
# column name
top_terms_cuban_k4_p1 <- rename(top_terms_cuban_k4_p1, term_p1 = term) 
top_terms_cuban_k4_p2 <- rename(top_terms_cuban_k4_p2, term_p2 = term) 
top_terms_cuban_k4_p3 <- rename(top_terms_cuban_k4_p3, term_p3 = term) 
top_terms_cuban_k4_p4 <- rename(top_terms_cuban_k4_p4, term_p4 = term) 

# remove beta

top_terms_cuban_k4     <- select(top_terms_cuban_k4,     -(beta))
top_terms_cuban_k4_p1  <- select(top_terms_cuban_k4_p1,  -(beta))
top_terms_cuban_k4_p2  <- select(top_terms_cuban_k4_p2,  -(beta))
top_terms_cuban_k4_p3  <- select(top_terms_cuban_k4_p3,  -(beta))
top_terms_cuban_k4_p4  <- select(top_terms_cuban_k4_p4,  -(beta))

# add row name
top_terms_cuban_k4      <- add_row_numbers(top_terms_cuban_k4,      name = "n", zero_based = FALSE)
top_terms_cuban_k4_p1   <- add_row_numbers(top_terms_cuban_k4_p1,   name = "n", zero_based = FALSE)
top_terms_cuban_k4_p2   <- add_row_numbers(top_terms_cuban_k4_p2,   name = "n", zero_based = FALSE)
top_terms_cuban_k4_p3   <- add_row_numbers(top_terms_cuban_k4_p3,   name = "n", zero_based = FALSE)
top_terms_cuban_k4_p4   <- add_row_numbers(top_terms_cuban_k4_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_cuban_k4 <- inner_join(top_terms_cuban_k4, top_terms_cuban_k4_p1)
top_terms_cuban_k4 <- inner_join(top_terms_cuban_k4, top_terms_cuban_k4_p2)
top_terms_cuban_k4 <- inner_join(top_terms_cuban_k4, top_terms_cuban_k4_p3)
top_terms_cuban_k4 <- inner_join(top_terms_cuban_k4, top_terms_cuban_k4_p4)

write.table(top_terms_cuban_k4, "top_terms_cuban_k4.csv", row.names = FALSE)





# muslim
# column name
top_terms_muslim_k4_p1 <- rename(top_terms_muslim_k4_p1, term_p1 = term) 
top_terms_muslim_k4_p2 <- rename(top_terms_muslim_k4_p2, term_p2 = term) 
top_terms_muslim_k4_p3 <- rename(top_terms_muslim_k4_p3, term_p3 = term) 
top_terms_muslim_k4_p4 <- rename(top_terms_muslim_k4_p4, term_p4 = term) 

# remove beta

top_terms_muslim_k4     <- select(top_terms_muslim_k4,     -(beta))
top_terms_muslim_k4_p1  <- select(top_terms_muslim_k4_p1,  -(beta))
top_terms_muslim_k4_p2  <- select(top_terms_muslim_k4_p2,  -(beta))
top_terms_muslim_k4_p3  <- select(top_terms_muslim_k4_p3,  -(beta))
top_terms_muslim_k4_p4  <- select(top_terms_muslim_k4_p4,  -(beta))

# add row name
top_terms_muslim_k4      <- add_row_numbers(top_terms_muslim_k4,      name = "n", zero_based = FALSE)
top_terms_muslim_k4_p1   <- add_row_numbers(top_terms_muslim_k4_p1,   name = "n", zero_based = FALSE)
top_terms_muslim_k4_p2   <- add_row_numbers(top_terms_muslim_k4_p2,   name = "n", zero_based = FALSE)
top_terms_muslim_k4_p3   <- add_row_numbers(top_terms_muslim_k4_p3,   name = "n", zero_based = FALSE)
top_terms_muslim_k4_p4   <- add_row_numbers(top_terms_muslim_k4_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_muslim_k4 <- inner_join(top_terms_muslim_k4, top_terms_muslim_k4_p1)
top_terms_muslim_k4 <- inner_join(top_terms_muslim_k4, top_terms_muslim_k4_p2)
top_terms_muslim_k4 <- inner_join(top_terms_muslim_k4, top_terms_muslim_k4_p3)
top_terms_muslim_k4 <- inner_join(top_terms_muslim_k4, top_terms_muslim_k4_p4)

write.table(top_terms_muslim_k4, "top_terms_muslim_k4.csv", row.names = FALSE)





# chinese
# column name
top_terms_chinese_k4_p1 <- rename(top_terms_chinese_k4_p1, term_p1 = term) 
top_terms_chinese_k4_p2 <- rename(top_terms_chinese_k4_p2, term_p2 = term) 
top_terms_chinese_k4_p3 <- rename(top_terms_chinese_k4_p3, term_p3 = term) 
top_terms_chinese_k4_p4 <- rename(top_terms_chinese_k4_p4, term_p4 = term) 

# remove beta

top_terms_chinese_k4     <- select(top_terms_chinese_k4,     -(beta))
top_terms_chinese_k4_p1  <- select(top_terms_chinese_k4_p1,  -(beta))
top_terms_chinese_k4_p2  <- select(top_terms_chinese_k4_p2,  -(beta))
top_terms_chinese_k4_p3  <- select(top_terms_chinese_k4_p3,  -(beta))
top_terms_chinese_k4_p4  <- select(top_terms_chinese_k4_p4,  -(beta))

# add row name
top_terms_chinese_k4      <- add_row_numbers(top_terms_chinese_k4,      name = "n", zero_based = FALSE)
top_terms_chinese_k4_p1   <- add_row_numbers(top_terms_chinese_k4_p1,   name = "n", zero_based = FALSE)
top_terms_chinese_k4_p2   <- add_row_numbers(top_terms_chinese_k4_p2,   name = "n", zero_based = FALSE)
top_terms_chinese_k4_p3   <- add_row_numbers(top_terms_chinese_k4_p3,   name = "n", zero_based = FALSE)
top_terms_chinese_k4_p4   <- add_row_numbers(top_terms_chinese_k4_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_chinese_k4 <- inner_join(top_terms_chinese_k4, top_terms_chinese_k4_p1)
top_terms_chinese_k4 <- inner_join(top_terms_chinese_k4, top_terms_chinese_k4_p2)
top_terms_chinese_k4 <- inner_join(top_terms_chinese_k4, top_terms_chinese_k4_p3)
top_terms_chinese_k4 <- inner_join(top_terms_chinese_k4, top_terms_chinese_k4_p4)

write.table(top_terms_chinese_k4, "top_terms_chinese_k4.csv", row.names = FALSE)


# mexican
# column name
top_terms_mexican_k4_p1 <- rename(top_terms_mexican_k4_p1, term_p1 = term) 
top_terms_mexican_k4_p2 <- rename(top_terms_mexican_k4_p2, term_p2 = term) 
top_terms_mexican_k4_p3 <- rename(top_terms_mexican_k4_p3, term_p3 = term) 
top_terms_mexican_k4_p4 <- rename(top_terms_mexican_k4_p4, term_p4 = term) 

# remove beta

top_terms_mexican_k4     <- select(top_terms_mexican_k4,     -(beta))
top_terms_mexican_k4_p1  <- select(top_terms_mexican_k4_p1,  -(beta))
top_terms_mexican_k4_p2  <- select(top_terms_mexican_k4_p2,  -(beta))
top_terms_mexican_k4_p3  <- select(top_terms_mexican_k4_p3,  -(beta))
top_terms_mexican_k4_p4  <- select(top_terms_mexican_k4_p4,  -(beta))

# add row name
top_terms_mexican_k4      <- add_row_numbers(top_terms_mexican_k4,      name = "n", zero_based = FALSE)
top_terms_mexican_k4_p1   <- add_row_numbers(top_terms_mexican_k4_p1,   name = "n", zero_based = FALSE)
top_terms_mexican_k4_p2   <- add_row_numbers(top_terms_mexican_k4_p2,   name = "n", zero_based = FALSE)
top_terms_mexican_k4_p3   <- add_row_numbers(top_terms_mexican_k4_p3,   name = "n", zero_based = FALSE)
top_terms_mexican_k4_p4   <- add_row_numbers(top_terms_mexican_k4_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_mexican_k4 <- inner_join(top_terms_mexican_k4, top_terms_mexican_k4_p1)
top_terms_mexican_k4 <- inner_join(top_terms_mexican_k4, top_terms_mexican_k4_p2)
top_terms_mexican_k4 <- inner_join(top_terms_mexican_k4, top_terms_mexican_k4_p3)
top_terms_mexican_k4 <- inner_join(top_terms_mexican_k4, top_terms_mexican_k4_p4)

write.table(top_terms_mexican_k4, "top_terms_mexican_k4.csv", row.names = FALSE)








##### k5
# italian
# column name
top_terms_italian_k5_p1 <- rename(top_terms_italian_k5_p1, term_p1 = term) 
top_terms_italian_k5_p2 <- rename(top_terms_italian_k5_p2, term_p2 = term) 
top_terms_italian_k5_p3 <- rename(top_terms_italian_k5_p3, term_p3 = term) 
top_terms_italian_k5_p4 <- rename(top_terms_italian_k5_p4, term_p4 = term) 

# remove beta

top_terms_italian_k5     <- select(top_terms_italian_k5,     -(beta))
top_terms_italian_k5_p1  <- select(top_terms_italian_k5_p1,  -(beta))
top_terms_italian_k5_p2  <- select(top_terms_italian_k5_p2,  -(beta))
top_terms_italian_k5_p3  <- select(top_terms_italian_k5_p3,  -(beta))
top_terms_italian_k5_p4  <- select(top_terms_italian_k5_p4,  -(beta))

# add row name
top_terms_italian_k5      <- add_row_numbers(top_terms_italian_k5,      name = "n", zero_based = FALSE)
top_terms_italian_k5_p1   <- add_row_numbers(top_terms_italian_k5_p1,   name = "n", zero_based = FALSE)
top_terms_italian_k5_p2   <- add_row_numbers(top_terms_italian_k5_p2,   name = "n", zero_based = FALSE)
top_terms_italian_k5_p3   <- add_row_numbers(top_terms_italian_k5_p3,   name = "n", zero_based = FALSE)
top_terms_italian_k5_p4   <- add_row_numbers(top_terms_italian_k5_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_italian_k5 <- inner_join(top_terms_italian_k5, top_terms_italian_k5_p1)
top_terms_italian_k5 <- inner_join(top_terms_italian_k5, top_terms_italian_k5_p2)
top_terms_italian_k5 <- inner_join(top_terms_italian_k5, top_terms_italian_k5_p3)
top_terms_italian_k5 <- inner_join(top_terms_italian_k5, top_terms_italian_k5_p4)

write.table(top_terms_italian_k5, "top_terms_italian_k5.csv", row.names = FALSE)





# jewish
# column name
top_terms_jewish_k5_p1 <- rename(top_terms_jewish_k5_p1, term_p1 = term) 
top_terms_jewish_k5_p2 <- rename(top_terms_jewish_k5_p2, term_p2 = term) 
top_terms_jewish_k5_p3 <- rename(top_terms_jewish_k5_p3, term_p3 = term) 
top_terms_jewish_k5_p4 <- rename(top_terms_jewish_k5_p4, term_p4 = term) 

# remove beta

top_terms_jewish_k5     <- select(top_terms_jewish_k5,     -(beta))
top_terms_jewish_k5_p1  <- select(top_terms_jewish_k5_p1,  -(beta))
top_terms_jewish_k5_p2  <- select(top_terms_jewish_k5_p2,  -(beta))
top_terms_jewish_k5_p3  <- select(top_terms_jewish_k5_p3,  -(beta))
top_terms_jewish_k5_p4  <- select(top_terms_jewish_k5_p4,  -(beta))

# add row name
top_terms_jewish_k5      <- add_row_numbers(top_terms_jewish_k5,      name = "n", zero_based = FALSE)
top_terms_jewish_k5_p1   <- add_row_numbers(top_terms_jewish_k5_p1,   name = "n", zero_based = FALSE)
top_terms_jewish_k5_p2   <- add_row_numbers(top_terms_jewish_k5_p2,   name = "n", zero_based = FALSE)
top_terms_jewish_k5_p3   <- add_row_numbers(top_terms_jewish_k5_p3,   name = "n", zero_based = FALSE)
top_terms_jewish_k5_p4   <- add_row_numbers(top_terms_jewish_k5_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_jewish_k5 <- inner_join(top_terms_jewish_k5, top_terms_jewish_k5_p1)
top_terms_jewish_k5 <- inner_join(top_terms_jewish_k5, top_terms_jewish_k5_p2)
top_terms_jewish_k5 <- inner_join(top_terms_jewish_k5, top_terms_jewish_k5_p3)
top_terms_jewish_k5 <- inner_join(top_terms_jewish_k5, top_terms_jewish_k5_p4)

write.table(top_terms_jewish_k5, "top_terms_jewish_k5.csv", row.names = FALSE)





# catholic
# column name
top_terms_catholic_k5_p1 <- rename(top_terms_catholic_k5_p1, term_p1 = term) 
top_terms_catholic_k5_p2 <- rename(top_terms_catholic_k5_p2, term_p2 = term) 
top_terms_catholic_k5_p3 <- rename(top_terms_catholic_k5_p3, term_p3 = term) 
top_terms_catholic_k5_p4 <- rename(top_terms_catholic_k5_p4, term_p4 = term) 

# remove beta

top_terms_catholic_k5     <- select(top_terms_catholic_k5,     -(beta))
top_terms_catholic_k5_p1  <- select(top_terms_catholic_k5_p1,  -(beta))
top_terms_catholic_k5_p2  <- select(top_terms_catholic_k5_p2,  -(beta))
top_terms_catholic_k5_p3  <- select(top_terms_catholic_k5_p3,  -(beta))
top_terms_catholic_k5_p4  <- select(top_terms_catholic_k5_p4,  -(beta))

# add row name
top_terms_catholic_k5      <- add_row_numbers(top_terms_catholic_k5,      name = "n", zero_based = FALSE)
top_terms_catholic_k5_p1   <- add_row_numbers(top_terms_catholic_k5_p1,   name = "n", zero_based = FALSE)
top_terms_catholic_k5_p2   <- add_row_numbers(top_terms_catholic_k5_p2,   name = "n", zero_based = FALSE)
top_terms_catholic_k5_p3   <- add_row_numbers(top_terms_catholic_k5_p3,   name = "n", zero_based = FALSE)
top_terms_catholic_k5_p4   <- add_row_numbers(top_terms_catholic_k5_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_catholic_k5 <- inner_join(top_terms_catholic_k5, top_terms_catholic_k5_p1)
top_terms_catholic_k5 <- inner_join(top_terms_catholic_k5, top_terms_catholic_k5_p2)
top_terms_catholic_k5 <- inner_join(top_terms_catholic_k5, top_terms_catholic_k5_p3)
top_terms_catholic_k5 <- inner_join(top_terms_catholic_k5, top_terms_catholic_k5_p4)

write.table(top_terms_catholic_k5, "top_terms_catholic_k5.csv", row.names = FALSE)





# irish
# column name
top_terms_irish_k5_p1 <- rename(top_terms_irish_k5_p1, term_p1 = term) 
top_terms_irish_k5_p2 <- rename(top_terms_irish_k5_p2, term_p2 = term) 
top_terms_irish_k5_p3 <- rename(top_terms_irish_k5_p3, term_p3 = term) 
top_terms_irish_k5_p4 <- rename(top_terms_irish_k5_p4, term_p4 = term) 

# remove beta

top_terms_irish_k5     <- select(top_terms_irish_k5,     -(beta))
top_terms_irish_k5_p1  <- select(top_terms_irish_k5_p1,  -(beta))
top_terms_irish_k5_p2  <- select(top_terms_irish_k5_p2,  -(beta))
top_terms_irish_k5_p3  <- select(top_terms_irish_k5_p3,  -(beta))
top_terms_irish_k5_p4  <- select(top_terms_irish_k5_p4,  -(beta))

# add row name
top_terms_irish_k5      <- add_row_numbers(top_terms_irish_k5,      name = "n", zero_based = FALSE)
top_terms_irish_k5_p1   <- add_row_numbers(top_terms_irish_k5_p1,   name = "n", zero_based = FALSE)
top_terms_irish_k5_p2   <- add_row_numbers(top_terms_irish_k5_p2,   name = "n", zero_based = FALSE)
top_terms_irish_k5_p3   <- add_row_numbers(top_terms_irish_k5_p3,   name = "n", zero_based = FALSE)
top_terms_irish_k5_p4   <- add_row_numbers(top_terms_irish_k5_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_irish_k5 <- inner_join(top_terms_irish_k5, top_terms_irish_k5_p1)
top_terms_irish_k5 <- inner_join(top_terms_irish_k5, top_terms_irish_k5_p2)
top_terms_irish_k5 <- inner_join(top_terms_irish_k5, top_terms_irish_k5_p3)
top_terms_irish_k5 <- inner_join(top_terms_irish_k5, top_terms_irish_k5_p4)

write.table(top_terms_irish_k5, "top_terms_irish_k5.csv", row.names = FALSE)





# cuban
# column name
top_terms_cuban_k5_p1 <- rename(top_terms_cuban_k5_p1, term_p1 = term) 
top_terms_cuban_k5_p2 <- rename(top_terms_cuban_k5_p2, term_p2 = term) 
top_terms_cuban_k5_p3 <- rename(top_terms_cuban_k5_p3, term_p3 = term) 
top_terms_cuban_k5_p4 <- rename(top_terms_cuban_k5_p4, term_p4 = term) 

# remove beta

top_terms_cuban_k5     <- select(top_terms_cuban_k5,     -(beta))
top_terms_cuban_k5_p1  <- select(top_terms_cuban_k5_p1,  -(beta))
top_terms_cuban_k5_p2  <- select(top_terms_cuban_k5_p2,  -(beta))
top_terms_cuban_k5_p3  <- select(top_terms_cuban_k5_p3,  -(beta))
top_terms_cuban_k5_p4  <- select(top_terms_cuban_k5_p4,  -(beta))

# add row name
top_terms_cuban_k5      <- add_row_numbers(top_terms_cuban_k5,      name = "n", zero_based = FALSE)
top_terms_cuban_k5_p1   <- add_row_numbers(top_terms_cuban_k5_p1,   name = "n", zero_based = FALSE)
top_terms_cuban_k5_p2   <- add_row_numbers(top_terms_cuban_k5_p2,   name = "n", zero_based = FALSE)
top_terms_cuban_k5_p3   <- add_row_numbers(top_terms_cuban_k5_p3,   name = "n", zero_based = FALSE)
top_terms_cuban_k5_p4   <- add_row_numbers(top_terms_cuban_k5_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_cuban_k5 <- inner_join(top_terms_cuban_k5, top_terms_cuban_k5_p1)
top_terms_cuban_k5 <- inner_join(top_terms_cuban_k5, top_terms_cuban_k5_p2)
top_terms_cuban_k5 <- inner_join(top_terms_cuban_k5, top_terms_cuban_k5_p3)
top_terms_cuban_k5 <- inner_join(top_terms_cuban_k5, top_terms_cuban_k5_p4)

write.table(top_terms_cuban_k5, "top_terms_cuban_k5.csv", row.names = FALSE)





# muslim
# column name
top_terms_muslim_k5_p1 <- rename(top_terms_muslim_k5_p1, term_p1 = term) 
top_terms_muslim_k5_p2 <- rename(top_terms_muslim_k5_p2, term_p2 = term) 
top_terms_muslim_k5_p3 <- rename(top_terms_muslim_k5_p3, term_p3 = term) 
top_terms_muslim_k5_p4 <- rename(top_terms_muslim_k5_p4, term_p4 = term) 

# remove beta

top_terms_muslim_k5     <- select(top_terms_muslim_k5,     -(beta))
top_terms_muslim_k5_p1  <- select(top_terms_muslim_k5_p1,  -(beta))
top_terms_muslim_k5_p2  <- select(top_terms_muslim_k5_p2,  -(beta))
top_terms_muslim_k5_p3  <- select(top_terms_muslim_k5_p3,  -(beta))
top_terms_muslim_k5_p4  <- select(top_terms_muslim_k5_p4,  -(beta))

# add row name
top_terms_muslim_k5      <- add_row_numbers(top_terms_muslim_k5,      name = "n", zero_based = FALSE)
top_terms_muslim_k5_p1   <- add_row_numbers(top_terms_muslim_k5_p1,   name = "n", zero_based = FALSE)
top_terms_muslim_k5_p2   <- add_row_numbers(top_terms_muslim_k5_p2,   name = "n", zero_based = FALSE)
top_terms_muslim_k5_p3   <- add_row_numbers(top_terms_muslim_k5_p3,   name = "n", zero_based = FALSE)
top_terms_muslim_k5_p4   <- add_row_numbers(top_terms_muslim_k5_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_muslim_k5 <- inner_join(top_terms_muslim_k5, top_terms_muslim_k5_p1)
top_terms_muslim_k5 <- inner_join(top_terms_muslim_k5, top_terms_muslim_k5_p2)
top_terms_muslim_k5 <- inner_join(top_terms_muslim_k5, top_terms_muslim_k5_p3)
top_terms_muslim_k5 <- inner_join(top_terms_muslim_k5, top_terms_muslim_k5_p4)

write.table(top_terms_muslim_k5, "top_terms_muslim_k5.csv", row.names = FALSE)





# chinese
# column name
top_terms_chinese_k5_p1 <- rename(top_terms_chinese_k5_p1, term_p1 = term) 
top_terms_chinese_k5_p2 <- rename(top_terms_chinese_k5_p2, term_p2 = term) 
top_terms_chinese_k5_p3 <- rename(top_terms_chinese_k5_p3, term_p3 = term) 
top_terms_chinese_k5_p4 <- rename(top_terms_chinese_k5_p4, term_p4 = term) 

# remove beta

top_terms_chinese_k5     <- select(top_terms_chinese_k5,     -(beta))
top_terms_chinese_k5_p1  <- select(top_terms_chinese_k5_p1,  -(beta))
top_terms_chinese_k5_p2  <- select(top_terms_chinese_k5_p2,  -(beta))
top_terms_chinese_k5_p3  <- select(top_terms_chinese_k5_p3,  -(beta))
top_terms_chinese_k5_p4  <- select(top_terms_chinese_k5_p4,  -(beta))

# add row name
top_terms_chinese_k5      <- add_row_numbers(top_terms_chinese_k5,      name = "n", zero_based = FALSE)
top_terms_chinese_k5_p1   <- add_row_numbers(top_terms_chinese_k5_p1,   name = "n", zero_based = FALSE)
top_terms_chinese_k5_p2   <- add_row_numbers(top_terms_chinese_k5_p2,   name = "n", zero_based = FALSE)
top_terms_chinese_k5_p3   <- add_row_numbers(top_terms_chinese_k5_p3,   name = "n", zero_based = FALSE)
top_terms_chinese_k5_p4   <- add_row_numbers(top_terms_chinese_k5_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_chinese_k5 <- inner_join(top_terms_chinese_k5, top_terms_chinese_k5_p1)
top_terms_chinese_k5 <- inner_join(top_terms_chinese_k5, top_terms_chinese_k5_p2)
top_terms_chinese_k5 <- inner_join(top_terms_chinese_k5, top_terms_chinese_k5_p3)
top_terms_chinese_k5 <- inner_join(top_terms_chinese_k5, top_terms_chinese_k5_p4)

write.table(top_terms_chinese_k5, "top_terms_chinese_k5.csv", row.names = FALSE)


# mexican
# column name
top_terms_mexican_k5_p1 <- rename(top_terms_mexican_k5_p1, term_p1 = term) 
top_terms_mexican_k5_p2 <- rename(top_terms_mexican_k5_p2, term_p2 = term) 
top_terms_mexican_k5_p3 <- rename(top_terms_mexican_k5_p3, term_p3 = term) 
top_terms_mexican_k5_p4 <- rename(top_terms_mexican_k5_p4, term_p4 = term) 

# remove beta

top_terms_mexican_k5     <- select(top_terms_mexican_k5,     -(beta))
top_terms_mexican_k5_p1  <- select(top_terms_mexican_k5_p1,  -(beta))
top_terms_mexican_k5_p2  <- select(top_terms_mexican_k5_p2,  -(beta))
top_terms_mexican_k5_p3  <- select(top_terms_mexican_k5_p3,  -(beta))
top_terms_mexican_k5_p4  <- select(top_terms_mexican_k5_p4,  -(beta))

# add row name
top_terms_mexican_k5      <- add_row_numbers(top_terms_mexican_k5,      name = "n", zero_based = FALSE)
top_terms_mexican_k5_p1   <- add_row_numbers(top_terms_mexican_k5_p1,   name = "n", zero_based = FALSE)
top_terms_mexican_k5_p2   <- add_row_numbers(top_terms_mexican_k5_p2,   name = "n", zero_based = FALSE)
top_terms_mexican_k5_p3   <- add_row_numbers(top_terms_mexican_k5_p3,   name = "n", zero_based = FALSE)
top_terms_mexican_k5_p4   <- add_row_numbers(top_terms_mexican_k5_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_mexican_k5 <- inner_join(top_terms_mexican_k5, top_terms_mexican_k5_p1)
top_terms_mexican_k5 <- inner_join(top_terms_mexican_k5, top_terms_mexican_k5_p2)
top_terms_mexican_k5 <- inner_join(top_terms_mexican_k5, top_terms_mexican_k5_p3)
top_terms_mexican_k5 <- inner_join(top_terms_mexican_k5, top_terms_mexican_k5_p4)

write.table(top_terms_mexican_k5, "top_terms_mexican_k5.csv", row.names = FALSE)






##### k6
# italian
# column name
top_terms_italian_k6_p1 <- rename(top_terms_italian_k6_p1, term_p1 = term) 
top_terms_italian_k6_p2 <- rename(top_terms_italian_k6_p2, term_p2 = term) 
top_terms_italian_k6_p3 <- rename(top_terms_italian_k6_p3, term_p3 = term) 
top_terms_italian_k6_p4 <- rename(top_terms_italian_k6_p4, term_p4 = term) 

# remove beta

top_terms_italian_k6     <- select(top_terms_italian_k6,     -(beta))
top_terms_italian_k6_p1  <- select(top_terms_italian_k6_p1,  -(beta))
top_terms_italian_k6_p2  <- select(top_terms_italian_k6_p2,  -(beta))
top_terms_italian_k6_p3  <- select(top_terms_italian_k6_p3,  -(beta))
top_terms_italian_k6_p4  <- select(top_terms_italian_k6_p4,  -(beta))

# add row name
top_terms_italian_k6      <- add_row_numbers(top_terms_italian_k6,      name = "n", zero_based = FALSE)
top_terms_italian_k6_p1   <- add_row_numbers(top_terms_italian_k6_p1,   name = "n", zero_based = FALSE)
top_terms_italian_k6_p2   <- add_row_numbers(top_terms_italian_k6_p2,   name = "n", zero_based = FALSE)
top_terms_italian_k6_p3   <- add_row_numbers(top_terms_italian_k6_p3,   name = "n", zero_based = FALSE)
top_terms_italian_k6_p4   <- add_row_numbers(top_terms_italian_k6_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_italian_k6 <- inner_join(top_terms_italian_k6, top_terms_italian_k6_p1)
top_terms_italian_k6 <- inner_join(top_terms_italian_k6, top_terms_italian_k6_p2)
top_terms_italian_k6 <- inner_join(top_terms_italian_k6, top_terms_italian_k6_p3)
top_terms_italian_k6 <- inner_join(top_terms_italian_k6, top_terms_italian_k6_p4)

write.table(top_terms_italian_k6, "top_terms_italian_k6.csv", row.names = FALSE)





# jewish
# column name
top_terms_jewish_k6_p1 <- rename(top_terms_jewish_k6_p1, term_p1 = term) 
top_terms_jewish_k6_p2 <- rename(top_terms_jewish_k6_p2, term_p2 = term) 
top_terms_jewish_k6_p3 <- rename(top_terms_jewish_k6_p3, term_p3 = term) 
top_terms_jewish_k6_p4 <- rename(top_terms_jewish_k6_p4, term_p4 = term) 

# remove beta

top_terms_jewish_k6     <- select(top_terms_jewish_k6,     -(beta))
top_terms_jewish_k6_p1  <- select(top_terms_jewish_k6_p1,  -(beta))
top_terms_jewish_k6_p2  <- select(top_terms_jewish_k6_p2,  -(beta))
top_terms_jewish_k6_p3  <- select(top_terms_jewish_k6_p3,  -(beta))
top_terms_jewish_k6_p4  <- select(top_terms_jewish_k6_p4,  -(beta))

# add row name
top_terms_jewish_k6      <- add_row_numbers(top_terms_jewish_k6,      name = "n", zero_based = FALSE)
top_terms_jewish_k6_p1   <- add_row_numbers(top_terms_jewish_k6_p1,   name = "n", zero_based = FALSE)
top_terms_jewish_k6_p2   <- add_row_numbers(top_terms_jewish_k6_p2,   name = "n", zero_based = FALSE)
top_terms_jewish_k6_p3   <- add_row_numbers(top_terms_jewish_k6_p3,   name = "n", zero_based = FALSE)
top_terms_jewish_k6_p4   <- add_row_numbers(top_terms_jewish_k6_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_jewish_k6 <- inner_join(top_terms_jewish_k6, top_terms_jewish_k6_p1)
top_terms_jewish_k6 <- inner_join(top_terms_jewish_k6, top_terms_jewish_k6_p2)
top_terms_jewish_k6 <- inner_join(top_terms_jewish_k6, top_terms_jewish_k6_p3)
top_terms_jewish_k6 <- inner_join(top_terms_jewish_k6, top_terms_jewish_k6_p4)

write.table(top_terms_jewish_k6, "top_terms_jewish_k6.csv", row.names = FALSE)





# catholic
# column name
top_terms_catholic_k6_p1 <- rename(top_terms_catholic_k6_p1, term_p1 = term) 
top_terms_catholic_k6_p2 <- rename(top_terms_catholic_k6_p2, term_p2 = term) 
top_terms_catholic_k6_p3 <- rename(top_terms_catholic_k6_p3, term_p3 = term) 
top_terms_catholic_k6_p4 <- rename(top_terms_catholic_k6_p4, term_p4 = term) 

# remove beta

top_terms_catholic_k6     <- select(top_terms_catholic_k6,     -(beta))
top_terms_catholic_k6_p1  <- select(top_terms_catholic_k6_p1,  -(beta))
top_terms_catholic_k6_p2  <- select(top_terms_catholic_k6_p2,  -(beta))
top_terms_catholic_k6_p3  <- select(top_terms_catholic_k6_p3,  -(beta))
top_terms_catholic_k6_p4  <- select(top_terms_catholic_k6_p4,  -(beta))

# add row name
top_terms_catholic_k6      <- add_row_numbers(top_terms_catholic_k6,      name = "n", zero_based = FALSE)
top_terms_catholic_k6_p1   <- add_row_numbers(top_terms_catholic_k6_p1,   name = "n", zero_based = FALSE)
top_terms_catholic_k6_p2   <- add_row_numbers(top_terms_catholic_k6_p2,   name = "n", zero_based = FALSE)
top_terms_catholic_k6_p3   <- add_row_numbers(top_terms_catholic_k6_p3,   name = "n", zero_based = FALSE)
top_terms_catholic_k6_p4   <- add_row_numbers(top_terms_catholic_k6_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_catholic_k6 <- inner_join(top_terms_catholic_k6, top_terms_catholic_k6_p1)
top_terms_catholic_k6 <- inner_join(top_terms_catholic_k6, top_terms_catholic_k6_p2)
top_terms_catholic_k6 <- inner_join(top_terms_catholic_k6, top_terms_catholic_k6_p3)
top_terms_catholic_k6 <- inner_join(top_terms_catholic_k6, top_terms_catholic_k6_p4)

write.table(top_terms_catholic_k6, "top_terms_catholic_k6.csv", row.names = FALSE)





# irish
# column name
top_terms_irish_k6_p1 <- rename(top_terms_irish_k6_p1, term_p1 = term) 
top_terms_irish_k6_p2 <- rename(top_terms_irish_k6_p2, term_p2 = term) 
top_terms_irish_k6_p3 <- rename(top_terms_irish_k6_p3, term_p3 = term) 
top_terms_irish_k6_p4 <- rename(top_terms_irish_k6_p4, term_p4 = term) 

# remove beta

top_terms_irish_k6     <- select(top_terms_irish_k6,     -(beta))
top_terms_irish_k6_p1  <- select(top_terms_irish_k6_p1,  -(beta))
top_terms_irish_k6_p2  <- select(top_terms_irish_k6_p2,  -(beta))
top_terms_irish_k6_p3  <- select(top_terms_irish_k6_p3,  -(beta))
top_terms_irish_k6_p4  <- select(top_terms_irish_k6_p4,  -(beta))

# add row name
top_terms_irish_k6      <- add_row_numbers(top_terms_irish_k6,      name = "n", zero_based = FALSE)
top_terms_irish_k6_p1   <- add_row_numbers(top_terms_irish_k6_p1,   name = "n", zero_based = FALSE)
top_terms_irish_k6_p2   <- add_row_numbers(top_terms_irish_k6_p2,   name = "n", zero_based = FALSE)
top_terms_irish_k6_p3   <- add_row_numbers(top_terms_irish_k6_p3,   name = "n", zero_based = FALSE)
top_terms_irish_k6_p4   <- add_row_numbers(top_terms_irish_k6_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_irish_k6 <- inner_join(top_terms_irish_k6, top_terms_irish_k6_p1)
top_terms_irish_k6 <- inner_join(top_terms_irish_k6, top_terms_irish_k6_p2)
top_terms_irish_k6 <- inner_join(top_terms_irish_k6, top_terms_irish_k6_p3)
top_terms_irish_k6 <- inner_join(top_terms_irish_k6, top_terms_irish_k6_p4)

write.table(top_terms_irish_k6, "top_terms_irish_k6.csv", row.names = FALSE)





# cuban
# column name
top_terms_cuban_k6_p1 <- rename(top_terms_cuban_k6_p1, term_p1 = term) 
top_terms_cuban_k6_p2 <- rename(top_terms_cuban_k6_p2, term_p2 = term) 
top_terms_cuban_k6_p3 <- rename(top_terms_cuban_k6_p3, term_p3 = term) 
top_terms_cuban_k6_p4 <- rename(top_terms_cuban_k6_p4, term_p4 = term) 

# remove beta

top_terms_cuban_k6     <- select(top_terms_cuban_k6,     -(beta))
top_terms_cuban_k6_p1  <- select(top_terms_cuban_k6_p1,  -(beta))
top_terms_cuban_k6_p2  <- select(top_terms_cuban_k6_p2,  -(beta))
top_terms_cuban_k6_p3  <- select(top_terms_cuban_k6_p3,  -(beta))
top_terms_cuban_k6_p4  <- select(top_terms_cuban_k6_p4,  -(beta))

# add row name
top_terms_cuban_k6      <- add_row_numbers(top_terms_cuban_k6,      name = "n", zero_based = FALSE)
top_terms_cuban_k6_p1   <- add_row_numbers(top_terms_cuban_k6_p1,   name = "n", zero_based = FALSE)
top_terms_cuban_k6_p2   <- add_row_numbers(top_terms_cuban_k6_p2,   name = "n", zero_based = FALSE)
top_terms_cuban_k6_p3   <- add_row_numbers(top_terms_cuban_k6_p3,   name = "n", zero_based = FALSE)
top_terms_cuban_k6_p4   <- add_row_numbers(top_terms_cuban_k6_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_cuban_k6 <- inner_join(top_terms_cuban_k6, top_terms_cuban_k6_p1)
top_terms_cuban_k6 <- inner_join(top_terms_cuban_k6, top_terms_cuban_k6_p2)
top_terms_cuban_k6 <- inner_join(top_terms_cuban_k6, top_terms_cuban_k6_p3)
top_terms_cuban_k6 <- inner_join(top_terms_cuban_k6, top_terms_cuban_k6_p4)

write.table(top_terms_cuban_k6, "top_terms_cuban_k6.csv", row.names = FALSE)





# muslim
# column name
top_terms_muslim_k6_p1 <- rename(top_terms_muslim_k6_p1, term_p1 = term) 
top_terms_muslim_k6_p2 <- rename(top_terms_muslim_k6_p2, term_p2 = term) 
top_terms_muslim_k6_p3 <- rename(top_terms_muslim_k6_p3, term_p3 = term) 
top_terms_muslim_k6_p4 <- rename(top_terms_muslim_k6_p4, term_p4 = term) 

# remove beta

top_terms_muslim_k6     <- select(top_terms_muslim_k6,     -(beta))
top_terms_muslim_k6_p1  <- select(top_terms_muslim_k6_p1,  -(beta))
top_terms_muslim_k6_p2  <- select(top_terms_muslim_k6_p2,  -(beta))
top_terms_muslim_k6_p3  <- select(top_terms_muslim_k6_p3,  -(beta))
top_terms_muslim_k6_p4  <- select(top_terms_muslim_k6_p4,  -(beta))

# add row name
top_terms_muslim_k6      <- add_row_numbers(top_terms_muslim_k6,      name = "n", zero_based = FALSE)
top_terms_muslim_k6_p1   <- add_row_numbers(top_terms_muslim_k6_p1,   name = "n", zero_based = FALSE)
top_terms_muslim_k6_p2   <- add_row_numbers(top_terms_muslim_k6_p2,   name = "n", zero_based = FALSE)
top_terms_muslim_k6_p3   <- add_row_numbers(top_terms_muslim_k6_p3,   name = "n", zero_based = FALSE)
top_terms_muslim_k6_p4   <- add_row_numbers(top_terms_muslim_k6_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_muslim_k6 <- inner_join(top_terms_muslim_k6, top_terms_muslim_k6_p1)
top_terms_muslim_k6 <- inner_join(top_terms_muslim_k6, top_terms_muslim_k6_p2)
top_terms_muslim_k6 <- inner_join(top_terms_muslim_k6, top_terms_muslim_k6_p3)
top_terms_muslim_k6 <- inner_join(top_terms_muslim_k6, top_terms_muslim_k6_p4)

write.table(top_terms_muslim_k6, "top_terms_muslim_k6.csv", row.names = FALSE)





# chinese
# column name
top_terms_chinese_k6_p1 <- rename(top_terms_chinese_k6_p1, term_p1 = term) 
top_terms_chinese_k6_p2 <- rename(top_terms_chinese_k6_p2, term_p2 = term) 
top_terms_chinese_k6_p3 <- rename(top_terms_chinese_k6_p3, term_p3 = term) 
top_terms_chinese_k6_p4 <- rename(top_terms_chinese_k6_p4, term_p4 = term) 

# remove beta

top_terms_chinese_k6     <- select(top_terms_chinese_k6,     -(beta))
top_terms_chinese_k6_p1  <- select(top_terms_chinese_k6_p1,  -(beta))
top_terms_chinese_k6_p2  <- select(top_terms_chinese_k6_p2,  -(beta))
top_terms_chinese_k6_p3  <- select(top_terms_chinese_k6_p3,  -(beta))
top_terms_chinese_k6_p4  <- select(top_terms_chinese_k6_p4,  -(beta))

# add row name
top_terms_chinese_k6      <- add_row_numbers(top_terms_chinese_k6,      name = "n", zero_based = FALSE)
top_terms_chinese_k6_p1   <- add_row_numbers(top_terms_chinese_k6_p1,   name = "n", zero_based = FALSE)
top_terms_chinese_k6_p2   <- add_row_numbers(top_terms_chinese_k6_p2,   name = "n", zero_based = FALSE)
top_terms_chinese_k6_p3   <- add_row_numbers(top_terms_chinese_k6_p3,   name = "n", zero_based = FALSE)
top_terms_chinese_k6_p4   <- add_row_numbers(top_terms_chinese_k6_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_chinese_k6 <- inner_join(top_terms_chinese_k6, top_terms_chinese_k6_p1)
top_terms_chinese_k6 <- inner_join(top_terms_chinese_k6, top_terms_chinese_k6_p2)
top_terms_chinese_k6 <- inner_join(top_terms_chinese_k6, top_terms_chinese_k6_p3)
top_terms_chinese_k6 <- inner_join(top_terms_chinese_k6, top_terms_chinese_k6_p4)

write.table(top_terms_chinese_k6, "top_terms_chinese_k6.csv", row.names = FALSE)


# mexican
# column name
top_terms_mexican_k6_p1 <- rename(top_terms_mexican_k6_p1, term_p1 = term) 
top_terms_mexican_k6_p2 <- rename(top_terms_mexican_k6_p2, term_p2 = term) 
top_terms_mexican_k6_p3 <- rename(top_terms_mexican_k6_p3, term_p3 = term) 
top_terms_mexican_k6_p4 <- rename(top_terms_mexican_k6_p4, term_p4 = term) 

# remove beta

top_terms_mexican_k6     <- select(top_terms_mexican_k6,     -(beta))
top_terms_mexican_k6_p1  <- select(top_terms_mexican_k6_p1,  -(beta))
top_terms_mexican_k6_p2  <- select(top_terms_mexican_k6_p2,  -(beta))
top_terms_mexican_k6_p3  <- select(top_terms_mexican_k6_p3,  -(beta))
top_terms_mexican_k6_p4  <- select(top_terms_mexican_k6_p4,  -(beta))

# add row name
top_terms_mexican_k6      <- add_row_numbers(top_terms_mexican_k6,      name = "n", zero_based = FALSE)
top_terms_mexican_k6_p1   <- add_row_numbers(top_terms_mexican_k6_p1,   name = "n", zero_based = FALSE)
top_terms_mexican_k6_p2   <- add_row_numbers(top_terms_mexican_k6_p2,   name = "n", zero_based = FALSE)
top_terms_mexican_k6_p3   <- add_row_numbers(top_terms_mexican_k6_p3,   name = "n", zero_based = FALSE)
top_terms_mexican_k6_p4   <- add_row_numbers(top_terms_mexican_k6_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_mexican_k6 <- inner_join(top_terms_mexican_k6, top_terms_mexican_k6_p1)
top_terms_mexican_k6 <- inner_join(top_terms_mexican_k6, top_terms_mexican_k6_p2)
top_terms_mexican_k6 <- inner_join(top_terms_mexican_k6, top_terms_mexican_k6_p3)
top_terms_mexican_k6 <- inner_join(top_terms_mexican_k6, top_terms_mexican_k6_p4)

write.table(top_terms_mexican_k6, "top_terms_mexican_k6.csv", row.names = FALSE)






##### k7
# italian
# column name
top_terms_italian_k7_p1 <- rename(top_terms_italian_k7_p1, term_p1 = term) 
top_terms_italian_k7_p2 <- rename(top_terms_italian_k7_p2, term_p2 = term) 
top_terms_italian_k7_p3 <- rename(top_terms_italian_k7_p3, term_p3 = term) 
top_terms_italian_k7_p4 <- rename(top_terms_italian_k7_p4, term_p4 = term) 

# remove beta

top_terms_italian_k7     <- select(top_terms_italian_k7,     -(beta))
top_terms_italian_k7_p1  <- select(top_terms_italian_k7_p1,  -(beta))
top_terms_italian_k7_p2  <- select(top_terms_italian_k7_p2,  -(beta))
top_terms_italian_k7_p3  <- select(top_terms_italian_k7_p3,  -(beta))
top_terms_italian_k7_p4  <- select(top_terms_italian_k7_p4,  -(beta))

# add row name
top_terms_italian_k7      <- add_row_numbers(top_terms_italian_k7,      name = "n", zero_based = FALSE)
top_terms_italian_k7_p1   <- add_row_numbers(top_terms_italian_k7_p1,   name = "n", zero_based = FALSE)
top_terms_italian_k7_p2   <- add_row_numbers(top_terms_italian_k7_p2,   name = "n", zero_based = FALSE)
top_terms_italian_k7_p3   <- add_row_numbers(top_terms_italian_k7_p3,   name = "n", zero_based = FALSE)
top_terms_italian_k7_p4   <- add_row_numbers(top_terms_italian_k7_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_italian_k7 <- inner_join(top_terms_italian_k7, top_terms_italian_k7_p1)
top_terms_italian_k7 <- inner_join(top_terms_italian_k7, top_terms_italian_k7_p2)
top_terms_italian_k7 <- inner_join(top_terms_italian_k7, top_terms_italian_k7_p3)
top_terms_italian_k7 <- inner_join(top_terms_italian_k7, top_terms_italian_k7_p4)

write.table(top_terms_italian_k7, "top_terms_italian_k7.csv", row.names = FALSE)





# jewish
# column name
top_terms_jewish_k7_p1 <- rename(top_terms_jewish_k7_p1, term_p1 = term) 
top_terms_jewish_k7_p2 <- rename(top_terms_jewish_k7_p2, term_p2 = term) 
top_terms_jewish_k7_p3 <- rename(top_terms_jewish_k7_p3, term_p3 = term) 
top_terms_jewish_k7_p4 <- rename(top_terms_jewish_k7_p4, term_p4 = term) 

# remove beta

top_terms_jewish_k7     <- select(top_terms_jewish_k7,     -(beta))
top_terms_jewish_k7_p1  <- select(top_terms_jewish_k7_p1,  -(beta))
top_terms_jewish_k7_p2  <- select(top_terms_jewish_k7_p2,  -(beta))
top_terms_jewish_k7_p3  <- select(top_terms_jewish_k7_p3,  -(beta))
top_terms_jewish_k7_p4  <- select(top_terms_jewish_k7_p4,  -(beta))

# add row name
top_terms_jewish_k7      <- add_row_numbers(top_terms_jewish_k7,      name = "n", zero_based = FALSE)
top_terms_jewish_k7_p1   <- add_row_numbers(top_terms_jewish_k7_p1,   name = "n", zero_based = FALSE)
top_terms_jewish_k7_p2   <- add_row_numbers(top_terms_jewish_k7_p2,   name = "n", zero_based = FALSE)
top_terms_jewish_k7_p3   <- add_row_numbers(top_terms_jewish_k7_p3,   name = "n", zero_based = FALSE)
top_terms_jewish_k7_p4   <- add_row_numbers(top_terms_jewish_k7_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_jewish_k7 <- inner_join(top_terms_jewish_k7, top_terms_jewish_k7_p1)
top_terms_jewish_k7 <- inner_join(top_terms_jewish_k7, top_terms_jewish_k7_p2)
top_terms_jewish_k7 <- inner_join(top_terms_jewish_k7, top_terms_jewish_k7_p3)
top_terms_jewish_k7 <- inner_join(top_terms_jewish_k7, top_terms_jewish_k7_p4)

write.table(top_terms_jewish_k7, "top_terms_jewish_k7.csv", row.names = FALSE)





# catholic
# column name
top_terms_catholic_k7_p1 <- rename(top_terms_catholic_k7_p1, term_p1 = term) 
top_terms_catholic_k7_p2 <- rename(top_terms_catholic_k7_p2, term_p2 = term) 
top_terms_catholic_k7_p3 <- rename(top_terms_catholic_k7_p3, term_p3 = term) 
top_terms_catholic_k7_p4 <- rename(top_terms_catholic_k7_p4, term_p4 = term) 

# remove beta

top_terms_catholic_k7     <- select(top_terms_catholic_k7,     -(beta))
top_terms_catholic_k7_p1  <- select(top_terms_catholic_k7_p1,  -(beta))
top_terms_catholic_k7_p2  <- select(top_terms_catholic_k7_p2,  -(beta))
top_terms_catholic_k7_p3  <- select(top_terms_catholic_k7_p3,  -(beta))
top_terms_catholic_k7_p4  <- select(top_terms_catholic_k7_p4,  -(beta))

# add row name
top_terms_catholic_k7      <- add_row_numbers(top_terms_catholic_k7,      name = "n", zero_based = FALSE)
top_terms_catholic_k7_p1   <- add_row_numbers(top_terms_catholic_k7_p1,   name = "n", zero_based = FALSE)
top_terms_catholic_k7_p2   <- add_row_numbers(top_terms_catholic_k7_p2,   name = "n", zero_based = FALSE)
top_terms_catholic_k7_p3   <- add_row_numbers(top_terms_catholic_k7_p3,   name = "n", zero_based = FALSE)
top_terms_catholic_k7_p4   <- add_row_numbers(top_terms_catholic_k7_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_catholic_k7 <- inner_join(top_terms_catholic_k7, top_terms_catholic_k7_p1)
top_terms_catholic_k7 <- inner_join(top_terms_catholic_k7, top_terms_catholic_k7_p2)
top_terms_catholic_k7 <- inner_join(top_terms_catholic_k7, top_terms_catholic_k7_p3)
top_terms_catholic_k7 <- inner_join(top_terms_catholic_k7, top_terms_catholic_k7_p4)

write.table(top_terms_catholic_k7, "top_terms_catholic_k7.csv", row.names = FALSE)





# irish
# column name
top_terms_irish_k7_p1 <- rename(top_terms_irish_k7_p1, term_p1 = term) 
top_terms_irish_k7_p2 <- rename(top_terms_irish_k7_p2, term_p2 = term) 
top_terms_irish_k7_p3 <- rename(top_terms_irish_k7_p3, term_p3 = term) 
top_terms_irish_k7_p4 <- rename(top_terms_irish_k7_p4, term_p4 = term) 

# remove beta

top_terms_irish_k7     <- select(top_terms_irish_k7,     -(beta))
top_terms_irish_k7_p1  <- select(top_terms_irish_k7_p1,  -(beta))
top_terms_irish_k7_p2  <- select(top_terms_irish_k7_p2,  -(beta))
top_terms_irish_k7_p3  <- select(top_terms_irish_k7_p3,  -(beta))
top_terms_irish_k7_p4  <- select(top_terms_irish_k7_p4,  -(beta))

# add row name
top_terms_irish_k7      <- add_row_numbers(top_terms_irish_k7,      name = "n", zero_based = FALSE)
top_terms_irish_k7_p1   <- add_row_numbers(top_terms_irish_k7_p1,   name = "n", zero_based = FALSE)
top_terms_irish_k7_p2   <- add_row_numbers(top_terms_irish_k7_p2,   name = "n", zero_based = FALSE)
top_terms_irish_k7_p3   <- add_row_numbers(top_terms_irish_k7_p3,   name = "n", zero_based = FALSE)
top_terms_irish_k7_p4   <- add_row_numbers(top_terms_irish_k7_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_irish_k7 <- inner_join(top_terms_irish_k7, top_terms_irish_k7_p1)
top_terms_irish_k7 <- inner_join(top_terms_irish_k7, top_terms_irish_k7_p2)
top_terms_irish_k7 <- inner_join(top_terms_irish_k7, top_terms_irish_k7_p3)
top_terms_irish_k7 <- inner_join(top_terms_irish_k7, top_terms_irish_k7_p4)

write.table(top_terms_irish_k7, "top_terms_irish_k7.csv", row.names = FALSE)





# cuban
# column name
top_terms_cuban_k7_p1 <- rename(top_terms_cuban_k7_p1, term_p1 = term) 
top_terms_cuban_k7_p2 <- rename(top_terms_cuban_k7_p2, term_p2 = term) 
top_terms_cuban_k7_p3 <- rename(top_terms_cuban_k7_p3, term_p3 = term) 
top_terms_cuban_k7_p4 <- rename(top_terms_cuban_k7_p4, term_p4 = term) 

# remove beta

top_terms_cuban_k7     <- select(top_terms_cuban_k7,     -(beta))
top_terms_cuban_k7_p1  <- select(top_terms_cuban_k7_p1,  -(beta))
top_terms_cuban_k7_p2  <- select(top_terms_cuban_k7_p2,  -(beta))
top_terms_cuban_k7_p3  <- select(top_terms_cuban_k7_p3,  -(beta))
top_terms_cuban_k7_p4  <- select(top_terms_cuban_k7_p4,  -(beta))

# add row name
top_terms_cuban_k7      <- add_row_numbers(top_terms_cuban_k7,      name = "n", zero_based = FALSE)
top_terms_cuban_k7_p1   <- add_row_numbers(top_terms_cuban_k7_p1,   name = "n", zero_based = FALSE)
top_terms_cuban_k7_p2   <- add_row_numbers(top_terms_cuban_k7_p2,   name = "n", zero_based = FALSE)
top_terms_cuban_k7_p3   <- add_row_numbers(top_terms_cuban_k7_p3,   name = "n", zero_based = FALSE)
top_terms_cuban_k7_p4   <- add_row_numbers(top_terms_cuban_k7_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_cuban_k7 <- inner_join(top_terms_cuban_k7, top_terms_cuban_k7_p1)
top_terms_cuban_k7 <- inner_join(top_terms_cuban_k7, top_terms_cuban_k7_p2)
top_terms_cuban_k7 <- inner_join(top_terms_cuban_k7, top_terms_cuban_k7_p3)
top_terms_cuban_k7 <- inner_join(top_terms_cuban_k7, top_terms_cuban_k7_p4)

write.table(top_terms_cuban_k7, "top_terms_cuban_k7.csv", row.names = FALSE)





# muslim
# column name
top_terms_muslim_k7_p1 <- rename(top_terms_muslim_k7_p1, term_p1 = term) 
top_terms_muslim_k7_p2 <- rename(top_terms_muslim_k7_p2, term_p2 = term) 
top_terms_muslim_k7_p3 <- rename(top_terms_muslim_k7_p3, term_p3 = term) 
top_terms_muslim_k7_p4 <- rename(top_terms_muslim_k7_p4, term_p4 = term) 

# remove beta

top_terms_muslim_k7     <- select(top_terms_muslim_k7,     -(beta))
top_terms_muslim_k7_p1  <- select(top_terms_muslim_k7_p1,  -(beta))
top_terms_muslim_k7_p2  <- select(top_terms_muslim_k7_p2,  -(beta))
top_terms_muslim_k7_p3  <- select(top_terms_muslim_k7_p3,  -(beta))
top_terms_muslim_k7_p4  <- select(top_terms_muslim_k7_p4,  -(beta))

# add row name
top_terms_muslim_k7      <- add_row_numbers(top_terms_muslim_k7,      name = "n", zero_based = FALSE)
top_terms_muslim_k7_p1   <- add_row_numbers(top_terms_muslim_k7_p1,   name = "n", zero_based = FALSE)
top_terms_muslim_k7_p2   <- add_row_numbers(top_terms_muslim_k7_p2,   name = "n", zero_based = FALSE)
top_terms_muslim_k7_p3   <- add_row_numbers(top_terms_muslim_k7_p3,   name = "n", zero_based = FALSE)
top_terms_muslim_k7_p4   <- add_row_numbers(top_terms_muslim_k7_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_muslim_k7 <- inner_join(top_terms_muslim_k7, top_terms_muslim_k7_p1)
top_terms_muslim_k7 <- inner_join(top_terms_muslim_k7, top_terms_muslim_k7_p2)
top_terms_muslim_k7 <- inner_join(top_terms_muslim_k7, top_terms_muslim_k7_p3)
top_terms_muslim_k7 <- inner_join(top_terms_muslim_k7, top_terms_muslim_k7_p4)

write.table(top_terms_muslim_k7, "top_terms_muslim_k7.csv", row.names = FALSE)





# chinese
# column name
top_terms_chinese_k7_p1 <- rename(top_terms_chinese_k7_p1, term_p1 = term) 
top_terms_chinese_k7_p2 <- rename(top_terms_chinese_k7_p2, term_p2 = term) 
top_terms_chinese_k7_p3 <- rename(top_terms_chinese_k7_p3, term_p3 = term) 
top_terms_chinese_k7_p4 <- rename(top_terms_chinese_k7_p4, term_p4 = term) 

# remove beta

top_terms_chinese_k7     <- select(top_terms_chinese_k7,     -(beta))
top_terms_chinese_k7_p1  <- select(top_terms_chinese_k7_p1,  -(beta))
top_terms_chinese_k7_p2  <- select(top_terms_chinese_k7_p2,  -(beta))
top_terms_chinese_k7_p3  <- select(top_terms_chinese_k7_p3,  -(beta))
top_terms_chinese_k7_p4  <- select(top_terms_chinese_k7_p4,  -(beta))

# add row name
top_terms_chinese_k7      <- add_row_numbers(top_terms_chinese_k7,      name = "n", zero_based = FALSE)
top_terms_chinese_k7_p1   <- add_row_numbers(top_terms_chinese_k7_p1,   name = "n", zero_based = FALSE)
top_terms_chinese_k7_p2   <- add_row_numbers(top_terms_chinese_k7_p2,   name = "n", zero_based = FALSE)
top_terms_chinese_k7_p3   <- add_row_numbers(top_terms_chinese_k7_p3,   name = "n", zero_based = FALSE)
top_terms_chinese_k7_p4   <- add_row_numbers(top_terms_chinese_k7_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_chinese_k7 <- inner_join(top_terms_chinese_k7, top_terms_chinese_k7_p1)
top_terms_chinese_k7 <- inner_join(top_terms_chinese_k7, top_terms_chinese_k7_p2)
top_terms_chinese_k7 <- inner_join(top_terms_chinese_k7, top_terms_chinese_k7_p3)
top_terms_chinese_k7 <- inner_join(top_terms_chinese_k7, top_terms_chinese_k7_p4)

write.table(top_terms_chinese_k7, "top_terms_chinese_k7.csv", row.names = FALSE)


# mexican
# column name
top_terms_mexican_k7_p1 <- rename(top_terms_mexican_k7_p1, term_p1 = term) 
top_terms_mexican_k7_p2 <- rename(top_terms_mexican_k7_p2, term_p2 = term) 
top_terms_mexican_k7_p3 <- rename(top_terms_mexican_k7_p3, term_p3 = term) 
top_terms_mexican_k7_p4 <- rename(top_terms_mexican_k7_p4, term_p4 = term) 

# remove beta

top_terms_mexican_k7     <- select(top_terms_mexican_k7,     -(beta))
top_terms_mexican_k7_p1  <- select(top_terms_mexican_k7_p1,  -(beta))
top_terms_mexican_k7_p2  <- select(top_terms_mexican_k7_p2,  -(beta))
top_terms_mexican_k7_p3  <- select(top_terms_mexican_k7_p3,  -(beta))
top_terms_mexican_k7_p4  <- select(top_terms_mexican_k7_p4,  -(beta))

# add row name
top_terms_mexican_k7      <- add_row_numbers(top_terms_mexican_k7,      name = "n", zero_based = FALSE)
top_terms_mexican_k7_p1   <- add_row_numbers(top_terms_mexican_k7_p1,   name = "n", zero_based = FALSE)
top_terms_mexican_k7_p2   <- add_row_numbers(top_terms_mexican_k7_p2,   name = "n", zero_based = FALSE)
top_terms_mexican_k7_p3   <- add_row_numbers(top_terms_mexican_k7_p3,   name = "n", zero_based = FALSE)
top_terms_mexican_k7_p4   <- add_row_numbers(top_terms_mexican_k7_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_mexican_k7 <- inner_join(top_terms_mexican_k7, top_terms_mexican_k7_p1)
top_terms_mexican_k7 <- inner_join(top_terms_mexican_k7, top_terms_mexican_k7_p2)
top_terms_mexican_k7 <- inner_join(top_terms_mexican_k7, top_terms_mexican_k7_p3)
top_terms_mexican_k7 <- inner_join(top_terms_mexican_k7, top_terms_mexican_k7_p4)

write.table(top_terms_mexican_k7, "top_terms_mexican_k7.csv", row.names = FALSE)






##### k8
# italian
# column name
top_terms_italian_k8_p1 <- rename(top_terms_italian_k8_p1, term_p1 = term) 
top_terms_italian_k8_p2 <- rename(top_terms_italian_k8_p2, term_p2 = term) 
top_terms_italian_k8_p3 <- rename(top_terms_italian_k8_p3, term_p3 = term) 
top_terms_italian_k8_p4 <- rename(top_terms_italian_k8_p4, term_p4 = term) 

# remove beta

top_terms_italian_k8     <- select(top_terms_italian_k8,     -(beta))
top_terms_italian_k8_p1  <- select(top_terms_italian_k8_p1,  -(beta))
top_terms_italian_k8_p2  <- select(top_terms_italian_k8_p2,  -(beta))
top_terms_italian_k8_p3  <- select(top_terms_italian_k8_p3,  -(beta))
top_terms_italian_k8_p4  <- select(top_terms_italian_k8_p4,  -(beta))

# add row name
top_terms_italian_k8      <- add_row_numbers(top_terms_italian_k8,      name = "n", zero_based = FALSE)
top_terms_italian_k8_p1   <- add_row_numbers(top_terms_italian_k8_p1,   name = "n", zero_based = FALSE)
top_terms_italian_k8_p2   <- add_row_numbers(top_terms_italian_k8_p2,   name = "n", zero_based = FALSE)
top_terms_italian_k8_p3   <- add_row_numbers(top_terms_italian_k8_p3,   name = "n", zero_based = FALSE)
top_terms_italian_k8_p4   <- add_row_numbers(top_terms_italian_k8_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_italian_k8 <- inner_join(top_terms_italian_k8, top_terms_italian_k8_p1)
top_terms_italian_k8 <- inner_join(top_terms_italian_k8, top_terms_italian_k8_p2)
top_terms_italian_k8 <- inner_join(top_terms_italian_k8, top_terms_italian_k8_p3)
top_terms_italian_k8 <- inner_join(top_terms_italian_k8, top_terms_italian_k8_p4)

write.table(top_terms_italian_k8, "top_terms_italian_k8.csv", row.names = FALSE)





# jewish
# column name
top_terms_jewish_k8_p1 <- rename(top_terms_jewish_k8_p1, term_p1 = term) 
top_terms_jewish_k8_p2 <- rename(top_terms_jewish_k8_p2, term_p2 = term) 
top_terms_jewish_k8_p3 <- rename(top_terms_jewish_k8_p3, term_p3 = term) 
top_terms_jewish_k8_p4 <- rename(top_terms_jewish_k8_p4, term_p4 = term) 

# remove beta

top_terms_jewish_k8     <- select(top_terms_jewish_k8,     -(beta))
top_terms_jewish_k8_p1  <- select(top_terms_jewish_k8_p1,  -(beta))
top_terms_jewish_k8_p2  <- select(top_terms_jewish_k8_p2,  -(beta))
top_terms_jewish_k8_p3  <- select(top_terms_jewish_k8_p3,  -(beta))
top_terms_jewish_k8_p4  <- select(top_terms_jewish_k8_p4,  -(beta))

# add row name
top_terms_jewish_k8      <- add_row_numbers(top_terms_jewish_k8,      name = "n", zero_based = FALSE)
top_terms_jewish_k8_p1   <- add_row_numbers(top_terms_jewish_k8_p1,   name = "n", zero_based = FALSE)
top_terms_jewish_k8_p2   <- add_row_numbers(top_terms_jewish_k8_p2,   name = "n", zero_based = FALSE)
top_terms_jewish_k8_p3   <- add_row_numbers(top_terms_jewish_k8_p3,   name = "n", zero_based = FALSE)
top_terms_jewish_k8_p4   <- add_row_numbers(top_terms_jewish_k8_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_jewish_k8 <- inner_join(top_terms_jewish_k8, top_terms_jewish_k8_p1)
top_terms_jewish_k8 <- inner_join(top_terms_jewish_k8, top_terms_jewish_k8_p2)
top_terms_jewish_k8 <- inner_join(top_terms_jewish_k8, top_terms_jewish_k8_p3)
top_terms_jewish_k8 <- inner_join(top_terms_jewish_k8, top_terms_jewish_k8_p4)

write.table(top_terms_jewish_k8, "top_terms_jewish_k8.csv", row.names = FALSE)





# catholic
# column name
top_terms_catholic_k8_p1 <- rename(top_terms_catholic_k8_p1, term_p1 = term) 
top_terms_catholic_k8_p2 <- rename(top_terms_catholic_k8_p2, term_p2 = term) 
top_terms_catholic_k8_p3 <- rename(top_terms_catholic_k8_p3, term_p3 = term) 
top_terms_catholic_k8_p4 <- rename(top_terms_catholic_k8_p4, term_p4 = term) 

# remove beta

top_terms_catholic_k8     <- select(top_terms_catholic_k8,     -(beta))
top_terms_catholic_k8_p1  <- select(top_terms_catholic_k8_p1,  -(beta))
top_terms_catholic_k8_p2  <- select(top_terms_catholic_k8_p2,  -(beta))
top_terms_catholic_k8_p3  <- select(top_terms_catholic_k8_p3,  -(beta))
top_terms_catholic_k8_p4  <- select(top_terms_catholic_k8_p4,  -(beta))

# add row name
top_terms_catholic_k8      <- add_row_numbers(top_terms_catholic_k8,      name = "n", zero_based = FALSE)
top_terms_catholic_k8_p1   <- add_row_numbers(top_terms_catholic_k8_p1,   name = "n", zero_based = FALSE)
top_terms_catholic_k8_p2   <- add_row_numbers(top_terms_catholic_k8_p2,   name = "n", zero_based = FALSE)
top_terms_catholic_k8_p3   <- add_row_numbers(top_terms_catholic_k8_p3,   name = "n", zero_based = FALSE)
top_terms_catholic_k8_p4   <- add_row_numbers(top_terms_catholic_k8_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_catholic_k8 <- inner_join(top_terms_catholic_k8, top_terms_catholic_k8_p1)
top_terms_catholic_k8 <- inner_join(top_terms_catholic_k8, top_terms_catholic_k8_p2)
top_terms_catholic_k8 <- inner_join(top_terms_catholic_k8, top_terms_catholic_k8_p3)
top_terms_catholic_k8 <- inner_join(top_terms_catholic_k8, top_terms_catholic_k8_p4)

write.table(top_terms_catholic_k8, "top_terms_catholic_k8.csv", row.names = FALSE)





# irish
# column name
top_terms_irish_k8_p1 <- rename(top_terms_irish_k8_p1, term_p1 = term) 
top_terms_irish_k8_p2 <- rename(top_terms_irish_k8_p2, term_p2 = term) 
top_terms_irish_k8_p3 <- rename(top_terms_irish_k8_p3, term_p3 = term) 
top_terms_irish_k8_p4 <- rename(top_terms_irish_k8_p4, term_p4 = term) 

# remove beta

top_terms_irish_k8     <- select(top_terms_irish_k8,     -(beta))
top_terms_irish_k8_p1  <- select(top_terms_irish_k8_p1,  -(beta))
top_terms_irish_k8_p2  <- select(top_terms_irish_k8_p2,  -(beta))
top_terms_irish_k8_p3  <- select(top_terms_irish_k8_p3,  -(beta))
top_terms_irish_k8_p4  <- select(top_terms_irish_k8_p4,  -(beta))

# add row name
top_terms_irish_k8      <- add_row_numbers(top_terms_irish_k8,      name = "n", zero_based = FALSE)
top_terms_irish_k8_p1   <- add_row_numbers(top_terms_irish_k8_p1,   name = "n", zero_based = FALSE)
top_terms_irish_k8_p2   <- add_row_numbers(top_terms_irish_k8_p2,   name = "n", zero_based = FALSE)
top_terms_irish_k8_p3   <- add_row_numbers(top_terms_irish_k8_p3,   name = "n", zero_based = FALSE)
top_terms_irish_k8_p4   <- add_row_numbers(top_terms_irish_k8_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_irish_k8 <- inner_join(top_terms_irish_k8, top_terms_irish_k8_p1)
top_terms_irish_k8 <- inner_join(top_terms_irish_k8, top_terms_irish_k8_p2)
top_terms_irish_k8 <- inner_join(top_terms_irish_k8, top_terms_irish_k8_p3)
top_terms_irish_k8 <- inner_join(top_terms_irish_k8, top_terms_irish_k8_p4)

write.table(top_terms_irish_k8, "top_terms_irish_k8.csv", row.names = FALSE)





# cuban
# column name
top_terms_cuban_k8_p1 <- rename(top_terms_cuban_k8_p1, term_p1 = term) 
top_terms_cuban_k8_p2 <- rename(top_terms_cuban_k8_p2, term_p2 = term) 
top_terms_cuban_k8_p3 <- rename(top_terms_cuban_k8_p3, term_p3 = term) 
top_terms_cuban_k8_p4 <- rename(top_terms_cuban_k8_p4, term_p4 = term) 

# remove beta

top_terms_cuban_k8     <- select(top_terms_cuban_k8,     -(beta))
top_terms_cuban_k8_p1  <- select(top_terms_cuban_k8_p1,  -(beta))
top_terms_cuban_k8_p2  <- select(top_terms_cuban_k8_p2,  -(beta))
top_terms_cuban_k8_p3  <- select(top_terms_cuban_k8_p3,  -(beta))
top_terms_cuban_k8_p4  <- select(top_terms_cuban_k8_p4,  -(beta))

# add row name
top_terms_cuban_k8      <- add_row_numbers(top_terms_cuban_k8,      name = "n", zero_based = FALSE)
top_terms_cuban_k8_p1   <- add_row_numbers(top_terms_cuban_k8_p1,   name = "n", zero_based = FALSE)
top_terms_cuban_k8_p2   <- add_row_numbers(top_terms_cuban_k8_p2,   name = "n", zero_based = FALSE)
top_terms_cuban_k8_p3   <- add_row_numbers(top_terms_cuban_k8_p3,   name = "n", zero_based = FALSE)
top_terms_cuban_k8_p4   <- add_row_numbers(top_terms_cuban_k8_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_cuban_k8 <- inner_join(top_terms_cuban_k8, top_terms_cuban_k8_p1)
top_terms_cuban_k8 <- inner_join(top_terms_cuban_k8, top_terms_cuban_k8_p2)
top_terms_cuban_k8 <- inner_join(top_terms_cuban_k8, top_terms_cuban_k8_p3)
top_terms_cuban_k8 <- inner_join(top_terms_cuban_k8, top_terms_cuban_k8_p4)

write.table(top_terms_cuban_k8, "top_terms_cuban_k8.csv", row.names = FALSE)





# muslim
# column name
top_terms_muslim_k8_p1 <- rename(top_terms_muslim_k8_p1, term_p1 = term) 
top_terms_muslim_k8_p2 <- rename(top_terms_muslim_k8_p2, term_p2 = term) 
top_terms_muslim_k8_p3 <- rename(top_terms_muslim_k8_p3, term_p3 = term) 
top_terms_muslim_k8_p4 <- rename(top_terms_muslim_k8_p4, term_p4 = term) 

# remove beta

top_terms_muslim_k8     <- select(top_terms_muslim_k8,     -(beta))
top_terms_muslim_k8_p1  <- select(top_terms_muslim_k8_p1,  -(beta))
top_terms_muslim_k8_p2  <- select(top_terms_muslim_k8_p2,  -(beta))
top_terms_muslim_k8_p3  <- select(top_terms_muslim_k8_p3,  -(beta))
top_terms_muslim_k8_p4  <- select(top_terms_muslim_k8_p4,  -(beta))

# add row name
top_terms_muslim_k8      <- add_row_numbers(top_terms_muslim_k8,      name = "n", zero_based = FALSE)
top_terms_muslim_k8_p1   <- add_row_numbers(top_terms_muslim_k8_p1,   name = "n", zero_based = FALSE)
top_terms_muslim_k8_p2   <- add_row_numbers(top_terms_muslim_k8_p2,   name = "n", zero_based = FALSE)
top_terms_muslim_k8_p3   <- add_row_numbers(top_terms_muslim_k8_p3,   name = "n", zero_based = FALSE)
top_terms_muslim_k8_p4   <- add_row_numbers(top_terms_muslim_k8_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_muslim_k8 <- inner_join(top_terms_muslim_k8, top_terms_muslim_k8_p1)
top_terms_muslim_k8 <- inner_join(top_terms_muslim_k8, top_terms_muslim_k8_p2)
top_terms_muslim_k8 <- inner_join(top_terms_muslim_k8, top_terms_muslim_k8_p3)
top_terms_muslim_k8 <- inner_join(top_terms_muslim_k8, top_terms_muslim_k8_p4)

write.table(top_terms_muslim_k8, "top_terms_muslim_k8.csv", row.names = FALSE)





# chinese
# column name
top_terms_chinese_k8_p1 <- rename(top_terms_chinese_k8_p1, term_p1 = term) 
top_terms_chinese_k8_p2 <- rename(top_terms_chinese_k8_p2, term_p2 = term) 
top_terms_chinese_k8_p3 <- rename(top_terms_chinese_k8_p3, term_p3 = term) 
top_terms_chinese_k8_p4 <- rename(top_terms_chinese_k8_p4, term_p4 = term) 

# remove beta

top_terms_chinese_k8     <- select(top_terms_chinese_k8,     -(beta))
top_terms_chinese_k8_p1  <- select(top_terms_chinese_k8_p1,  -(beta))
top_terms_chinese_k8_p2  <- select(top_terms_chinese_k8_p2,  -(beta))
top_terms_chinese_k8_p3  <- select(top_terms_chinese_k8_p3,  -(beta))
top_terms_chinese_k8_p4  <- select(top_terms_chinese_k8_p4,  -(beta))

# add row name
top_terms_chinese_k8      <- add_row_numbers(top_terms_chinese_k8,      name = "n", zero_based = FALSE)
top_terms_chinese_k8_p1   <- add_row_numbers(top_terms_chinese_k8_p1,   name = "n", zero_based = FALSE)
top_terms_chinese_k8_p2   <- add_row_numbers(top_terms_chinese_k8_p2,   name = "n", zero_based = FALSE)
top_terms_chinese_k8_p3   <- add_row_numbers(top_terms_chinese_k8_p3,   name = "n", zero_based = FALSE)
top_terms_chinese_k8_p4   <- add_row_numbers(top_terms_chinese_k8_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_chinese_k8 <- inner_join(top_terms_chinese_k8, top_terms_chinese_k8_p1)
top_terms_chinese_k8 <- inner_join(top_terms_chinese_k8, top_terms_chinese_k8_p2)
top_terms_chinese_k8 <- inner_join(top_terms_chinese_k8, top_terms_chinese_k8_p3)
top_terms_chinese_k8 <- inner_join(top_terms_chinese_k8, top_terms_chinese_k8_p4)

write.table(top_terms_chinese_k8, "top_terms_chinese_k8.csv", row.names = FALSE)


# mexican
# column name
top_terms_mexican_k8_p1 <- rename(top_terms_mexican_k8_p1, term_p1 = term) 
top_terms_mexican_k8_p2 <- rename(top_terms_mexican_k8_p2, term_p2 = term) 
top_terms_mexican_k8_p3 <- rename(top_terms_mexican_k8_p3, term_p3 = term) 
top_terms_mexican_k8_p4 <- rename(top_terms_mexican_k8_p4, term_p4 = term) 

# remove beta

top_terms_mexican_k8     <- select(top_terms_mexican_k8,     -(beta))
top_terms_mexican_k8_p1  <- select(top_terms_mexican_k8_p1,  -(beta))
top_terms_mexican_k8_p2  <- select(top_terms_mexican_k8_p2,  -(beta))
top_terms_mexican_k8_p3  <- select(top_terms_mexican_k8_p3,  -(beta))
top_terms_mexican_k8_p4  <- select(top_terms_mexican_k8_p4,  -(beta))

# add row name
top_terms_mexican_k8      <- add_row_numbers(top_terms_mexican_k8,      name = "n", zero_based = FALSE)
top_terms_mexican_k8_p1   <- add_row_numbers(top_terms_mexican_k8_p1,   name = "n", zero_based = FALSE)
top_terms_mexican_k8_p2   <- add_row_numbers(top_terms_mexican_k8_p2,   name = "n", zero_based = FALSE)
top_terms_mexican_k8_p3   <- add_row_numbers(top_terms_mexican_k8_p3,   name = "n", zero_based = FALSE)
top_terms_mexican_k8_p4   <- add_row_numbers(top_terms_mexican_k8_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_mexican_k8 <- inner_join(top_terms_mexican_k8, top_terms_mexican_k8_p1)
top_terms_mexican_k8 <- inner_join(top_terms_mexican_k8, top_terms_mexican_k8_p2)
top_terms_mexican_k8 <- inner_join(top_terms_mexican_k8, top_terms_mexican_k8_p3)
top_terms_mexican_k8 <- inner_join(top_terms_mexican_k8, top_terms_mexican_k8_p4)

write.table(top_terms_mexican_k8, "top_terms_mexican_k8.csv", row.names = FALSE)





##### k9
# italian
# column name
top_terms_italian_k9_p1 <- rename(top_terms_italian_k9_p1, term_p1 = term) 
top_terms_italian_k9_p2 <- rename(top_terms_italian_k9_p2, term_p2 = term) 
top_terms_italian_k9_p3 <- rename(top_terms_italian_k9_p3, term_p3 = term) 
top_terms_italian_k9_p4 <- rename(top_terms_italian_k9_p4, term_p4 = term) 

# remove beta

top_terms_italian_k9     <- select(top_terms_italian_k9,     -(beta))
top_terms_italian_k9_p1  <- select(top_terms_italian_k9_p1,  -(beta))
top_terms_italian_k9_p2  <- select(top_terms_italian_k9_p2,  -(beta))
top_terms_italian_k9_p3  <- select(top_terms_italian_k9_p3,  -(beta))
top_terms_italian_k9_p4  <- select(top_terms_italian_k9_p4,  -(beta))

# add row name
top_terms_italian_k9      <- add_row_numbers(top_terms_italian_k9,      name = "n", zero_based = FALSE)
top_terms_italian_k9_p1   <- add_row_numbers(top_terms_italian_k9_p1,   name = "n", zero_based = FALSE)
top_terms_italian_k9_p2   <- add_row_numbers(top_terms_italian_k9_p2,   name = "n", zero_based = FALSE)
top_terms_italian_k9_p3   <- add_row_numbers(top_terms_italian_k9_p3,   name = "n", zero_based = FALSE)
top_terms_italian_k9_p4   <- add_row_numbers(top_terms_italian_k9_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_italian_k9 <- inner_join(top_terms_italian_k9, top_terms_italian_k9_p1)
top_terms_italian_k9 <- inner_join(top_terms_italian_k9, top_terms_italian_k9_p2)
top_terms_italian_k9 <- inner_join(top_terms_italian_k9, top_terms_italian_k9_p3)
top_terms_italian_k9 <- inner_join(top_terms_italian_k9, top_terms_italian_k9_p4)

write.table(top_terms_italian_k9, "top_terms_italian_k9.csv", row.names = FALSE)





# jewish
# column name
top_terms_jewish_k9_p1 <- rename(top_terms_jewish_k9_p1, term_p1 = term) 
top_terms_jewish_k9_p2 <- rename(top_terms_jewish_k9_p2, term_p2 = term) 
top_terms_jewish_k9_p3 <- rename(top_terms_jewish_k9_p3, term_p3 = term) 
top_terms_jewish_k9_p4 <- rename(top_terms_jewish_k9_p4, term_p4 = term) 

# remove beta

top_terms_jewish_k9     <- select(top_terms_jewish_k9,     -(beta))
top_terms_jewish_k9_p1  <- select(top_terms_jewish_k9_p1,  -(beta))
top_terms_jewish_k9_p2  <- select(top_terms_jewish_k9_p2,  -(beta))
top_terms_jewish_k9_p3  <- select(top_terms_jewish_k9_p3,  -(beta))
top_terms_jewish_k9_p4  <- select(top_terms_jewish_k9_p4,  -(beta))

# add row name
top_terms_jewish_k9      <- add_row_numbers(top_terms_jewish_k9,      name = "n", zero_based = FALSE)
top_terms_jewish_k9_p1   <- add_row_numbers(top_terms_jewish_k9_p1,   name = "n", zero_based = FALSE)
top_terms_jewish_k9_p2   <- add_row_numbers(top_terms_jewish_k9_p2,   name = "n", zero_based = FALSE)
top_terms_jewish_k9_p3   <- add_row_numbers(top_terms_jewish_k9_p3,   name = "n", zero_based = FALSE)
top_terms_jewish_k9_p4   <- add_row_numbers(top_terms_jewish_k9_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_jewish_k9 <- inner_join(top_terms_jewish_k9, top_terms_jewish_k9_p1)
top_terms_jewish_k9 <- inner_join(top_terms_jewish_k9, top_terms_jewish_k9_p2)
top_terms_jewish_k9 <- inner_join(top_terms_jewish_k9, top_terms_jewish_k9_p3)
top_terms_jewish_k9 <- inner_join(top_terms_jewish_k9, top_terms_jewish_k9_p4)

write.table(top_terms_jewish_k9, "top_terms_jewish_k9.csv", row.names = FALSE)





# catholic
# column name
top_terms_catholic_k9_p1 <- rename(top_terms_catholic_k9_p1, term_p1 = term) 
top_terms_catholic_k9_p2 <- rename(top_terms_catholic_k9_p2, term_p2 = term) 
top_terms_catholic_k9_p3 <- rename(top_terms_catholic_k9_p3, term_p3 = term) 
top_terms_catholic_k9_p4 <- rename(top_terms_catholic_k9_p4, term_p4 = term) 

# remove beta

top_terms_catholic_k9     <- select(top_terms_catholic_k9,     -(beta))
top_terms_catholic_k9_p1  <- select(top_terms_catholic_k9_p1,  -(beta))
top_terms_catholic_k9_p2  <- select(top_terms_catholic_k9_p2,  -(beta))
top_terms_catholic_k9_p3  <- select(top_terms_catholic_k9_p3,  -(beta))
top_terms_catholic_k9_p4  <- select(top_terms_catholic_k9_p4,  -(beta))

# add row name
top_terms_catholic_k9      <- add_row_numbers(top_terms_catholic_k9,      name = "n", zero_based = FALSE)
top_terms_catholic_k9_p1   <- add_row_numbers(top_terms_catholic_k9_p1,   name = "n", zero_based = FALSE)
top_terms_catholic_k9_p2   <- add_row_numbers(top_terms_catholic_k9_p2,   name = "n", zero_based = FALSE)
top_terms_catholic_k9_p3   <- add_row_numbers(top_terms_catholic_k9_p3,   name = "n", zero_based = FALSE)
top_terms_catholic_k9_p4   <- add_row_numbers(top_terms_catholic_k9_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_catholic_k9 <- inner_join(top_terms_catholic_k9, top_terms_catholic_k9_p1)
top_terms_catholic_k9 <- inner_join(top_terms_catholic_k9, top_terms_catholic_k9_p2)
top_terms_catholic_k9 <- inner_join(top_terms_catholic_k9, top_terms_catholic_k9_p3)
top_terms_catholic_k9 <- inner_join(top_terms_catholic_k9, top_terms_catholic_k9_p4)

write.table(top_terms_catholic_k9, "top_terms_catholic_k9.csv", row.names = FALSE)





# irish
# column name
top_terms_irish_k9_p1 <- rename(top_terms_irish_k9_p1, term_p1 = term) 
top_terms_irish_k9_p2 <- rename(top_terms_irish_k9_p2, term_p2 = term) 
top_terms_irish_k9_p3 <- rename(top_terms_irish_k9_p3, term_p3 = term) 
top_terms_irish_k9_p4 <- rename(top_terms_irish_k9_p4, term_p4 = term) 

# remove beta

top_terms_irish_k9     <- select(top_terms_irish_k9,     -(beta))
top_terms_irish_k9_p1  <- select(top_terms_irish_k9_p1,  -(beta))
top_terms_irish_k9_p2  <- select(top_terms_irish_k9_p2,  -(beta))
top_terms_irish_k9_p3  <- select(top_terms_irish_k9_p3,  -(beta))
top_terms_irish_k9_p4  <- select(top_terms_irish_k9_p4,  -(beta))

# add row name
top_terms_irish_k9      <- add_row_numbers(top_terms_irish_k9,      name = "n", zero_based = FALSE)
top_terms_irish_k9_p1   <- add_row_numbers(top_terms_irish_k9_p1,   name = "n", zero_based = FALSE)
top_terms_irish_k9_p2   <- add_row_numbers(top_terms_irish_k9_p2,   name = "n", zero_based = FALSE)
top_terms_irish_k9_p3   <- add_row_numbers(top_terms_irish_k9_p3,   name = "n", zero_based = FALSE)
top_terms_irish_k9_p4   <- add_row_numbers(top_terms_irish_k9_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_irish_k9 <- inner_join(top_terms_irish_k9, top_terms_irish_k9_p1)
top_terms_irish_k9 <- inner_join(top_terms_irish_k9, top_terms_irish_k9_p2)
top_terms_irish_k9 <- inner_join(top_terms_irish_k9, top_terms_irish_k9_p3)
top_terms_irish_k9 <- inner_join(top_terms_irish_k9, top_terms_irish_k9_p4)

write.table(top_terms_irish_k9, "top_terms_irish_k9.csv", row.names = FALSE)





# cuban
# column name
top_terms_cuban_k9_p1 <- rename(top_terms_cuban_k9_p1, term_p1 = term) 
top_terms_cuban_k9_p2 <- rename(top_terms_cuban_k9_p2, term_p2 = term) 
top_terms_cuban_k9_p3 <- rename(top_terms_cuban_k9_p3, term_p3 = term) 
top_terms_cuban_k9_p4 <- rename(top_terms_cuban_k9_p4, term_p4 = term) 

# remove beta

top_terms_cuban_k9     <- select(top_terms_cuban_k9,     -(beta))
top_terms_cuban_k9_p1  <- select(top_terms_cuban_k9_p1,  -(beta))
top_terms_cuban_k9_p2  <- select(top_terms_cuban_k9_p2,  -(beta))
top_terms_cuban_k9_p3  <- select(top_terms_cuban_k9_p3,  -(beta))
top_terms_cuban_k9_p4  <- select(top_terms_cuban_k9_p4,  -(beta))

# add row name
top_terms_cuban_k9      <- add_row_numbers(top_terms_cuban_k9,      name = "n", zero_based = FALSE)
top_terms_cuban_k9_p1   <- add_row_numbers(top_terms_cuban_k9_p1,   name = "n", zero_based = FALSE)
top_terms_cuban_k9_p2   <- add_row_numbers(top_terms_cuban_k9_p2,   name = "n", zero_based = FALSE)
top_terms_cuban_k9_p3   <- add_row_numbers(top_terms_cuban_k9_p3,   name = "n", zero_based = FALSE)
top_terms_cuban_k9_p4   <- add_row_numbers(top_terms_cuban_k9_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_cuban_k9 <- inner_join(top_terms_cuban_k9, top_terms_cuban_k9_p1)
top_terms_cuban_k9 <- inner_join(top_terms_cuban_k9, top_terms_cuban_k9_p2)
top_terms_cuban_k9 <- inner_join(top_terms_cuban_k9, top_terms_cuban_k9_p3)
top_terms_cuban_k9 <- inner_join(top_terms_cuban_k9, top_terms_cuban_k9_p4)

write.table(top_terms_cuban_k9, "top_terms_cuban_k9.csv", row.names = FALSE)





# muslim
# column name
top_terms_muslim_k9_p1 <- rename(top_terms_muslim_k9_p1, term_p1 = term) 
top_terms_muslim_k9_p2 <- rename(top_terms_muslim_k9_p2, term_p2 = term) 
top_terms_muslim_k9_p3 <- rename(top_terms_muslim_k9_p3, term_p3 = term) 
top_terms_muslim_k9_p4 <- rename(top_terms_muslim_k9_p4, term_p4 = term) 

# remove beta

top_terms_muslim_k9     <- select(top_terms_muslim_k9,     -(beta))
top_terms_muslim_k9_p1  <- select(top_terms_muslim_k9_p1,  -(beta))
top_terms_muslim_k9_p2  <- select(top_terms_muslim_k9_p2,  -(beta))
top_terms_muslim_k9_p3  <- select(top_terms_muslim_k9_p3,  -(beta))
top_terms_muslim_k9_p4  <- select(top_terms_muslim_k9_p4,  -(beta))

# add row name
top_terms_muslim_k9      <- add_row_numbers(top_terms_muslim_k9,      name = "n", zero_based = FALSE)
top_terms_muslim_k9_p1   <- add_row_numbers(top_terms_muslim_k9_p1,   name = "n", zero_based = FALSE)
top_terms_muslim_k9_p2   <- add_row_numbers(top_terms_muslim_k9_p2,   name = "n", zero_based = FALSE)
top_terms_muslim_k9_p3   <- add_row_numbers(top_terms_muslim_k9_p3,   name = "n", zero_based = FALSE)
top_terms_muslim_k9_p4   <- add_row_numbers(top_terms_muslim_k9_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_muslim_k9 <- inner_join(top_terms_muslim_k9, top_terms_muslim_k9_p1)
top_terms_muslim_k9 <- inner_join(top_terms_muslim_k9, top_terms_muslim_k9_p2)
top_terms_muslim_k9 <- inner_join(top_terms_muslim_k9, top_terms_muslim_k9_p3)
top_terms_muslim_k9 <- inner_join(top_terms_muslim_k9, top_terms_muslim_k9_p4)

write.table(top_terms_muslim_k9, "top_terms_muslim_k9.csv", row.names = FALSE)





# chinese
# column name
top_terms_chinese_k9_p1 <- rename(top_terms_chinese_k9_p1, term_p1 = term) 
top_terms_chinese_k9_p2 <- rename(top_terms_chinese_k9_p2, term_p2 = term) 
top_terms_chinese_k9_p3 <- rename(top_terms_chinese_k9_p3, term_p3 = term) 
top_terms_chinese_k9_p4 <- rename(top_terms_chinese_k9_p4, term_p4 = term) 

# remove beta

top_terms_chinese_k9     <- select(top_terms_chinese_k9,     -(beta))
top_terms_chinese_k9_p1  <- select(top_terms_chinese_k9_p1,  -(beta))
top_terms_chinese_k9_p2  <- select(top_terms_chinese_k9_p2,  -(beta))
top_terms_chinese_k9_p3  <- select(top_terms_chinese_k9_p3,  -(beta))
top_terms_chinese_k9_p4  <- select(top_terms_chinese_k9_p4,  -(beta))

# add row name
top_terms_chinese_k9      <- add_row_numbers(top_terms_chinese_k9,      name = "n", zero_based = FALSE)
top_terms_chinese_k9_p1   <- add_row_numbers(top_terms_chinese_k9_p1,   name = "n", zero_based = FALSE)
top_terms_chinese_k9_p2   <- add_row_numbers(top_terms_chinese_k9_p2,   name = "n", zero_based = FALSE)
top_terms_chinese_k9_p3   <- add_row_numbers(top_terms_chinese_k9_p3,   name = "n", zero_based = FALSE)
top_terms_chinese_k9_p4   <- add_row_numbers(top_terms_chinese_k9_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_chinese_k9 <- inner_join(top_terms_chinese_k9, top_terms_chinese_k9_p1)
top_terms_chinese_k9 <- inner_join(top_terms_chinese_k9, top_terms_chinese_k9_p2)
top_terms_chinese_k9 <- inner_join(top_terms_chinese_k9, top_terms_chinese_k9_p3)
top_terms_chinese_k9 <- inner_join(top_terms_chinese_k9, top_terms_chinese_k9_p4)

write.table(top_terms_chinese_k9, "top_terms_chinese_k9.csv", row.names = FALSE)


# mexican
# column name
top_terms_mexican_k9_p1 <- rename(top_terms_mexican_k9_p1, term_p1 = term) 
top_terms_mexican_k9_p2 <- rename(top_terms_mexican_k9_p2, term_p2 = term) 
top_terms_mexican_k9_p3 <- rename(top_terms_mexican_k9_p3, term_p3 = term) 
top_terms_mexican_k9_p4 <- rename(top_terms_mexican_k9_p4, term_p4 = term) 

# remove beta

top_terms_mexican_k9     <- select(top_terms_mexican_k9,     -(beta))
top_terms_mexican_k9_p1  <- select(top_terms_mexican_k9_p1,  -(beta))
top_terms_mexican_k9_p2  <- select(top_terms_mexican_k9_p2,  -(beta))
top_terms_mexican_k9_p3  <- select(top_terms_mexican_k9_p3,  -(beta))
top_terms_mexican_k9_p4  <- select(top_terms_mexican_k9_p4,  -(beta))

# add row name
top_terms_mexican_k9      <- add_row_numbers(top_terms_mexican_k9,      name = "n", zero_based = FALSE)
top_terms_mexican_k9_p1   <- add_row_numbers(top_terms_mexican_k9_p1,   name = "n", zero_based = FALSE)
top_terms_mexican_k9_p2   <- add_row_numbers(top_terms_mexican_k9_p2,   name = "n", zero_based = FALSE)
top_terms_mexican_k9_p3   <- add_row_numbers(top_terms_mexican_k9_p3,   name = "n", zero_based = FALSE)
top_terms_mexican_k9_p4   <- add_row_numbers(top_terms_mexican_k9_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_mexican_k9 <- inner_join(top_terms_mexican_k9, top_terms_mexican_k9_p1)
top_terms_mexican_k9 <- inner_join(top_terms_mexican_k9, top_terms_mexican_k9_p2)
top_terms_mexican_k9 <- inner_join(top_terms_mexican_k9, top_terms_mexican_k9_p3)
top_terms_mexican_k9 <- inner_join(top_terms_mexican_k9, top_terms_mexican_k9_p4)

write.table(top_terms_mexican_k9, "top_terms_mexican_k9.csv", row.names = FALSE)






##### k10
# italian
# column name
top_terms_italian_k10_p1 <- rename(top_terms_italian_k10_p1, term_p1 = term) 
top_terms_italian_k10_p2 <- rename(top_terms_italian_k10_p2, term_p2 = term) 
top_terms_italian_k10_p3 <- rename(top_terms_italian_k10_p3, term_p3 = term) 
top_terms_italian_k10_p4 <- rename(top_terms_italian_k10_p4, term_p4 = term) 

# remove beta

top_terms_italian_k10     <- select(top_terms_italian_k10,     -(beta))
top_terms_italian_k10_p1  <- select(top_terms_italian_k10_p1,  -(beta))
top_terms_italian_k10_p2  <- select(top_terms_italian_k10_p2,  -(beta))
top_terms_italian_k10_p3  <- select(top_terms_italian_k10_p3,  -(beta))
top_terms_italian_k10_p4  <- select(top_terms_italian_k10_p4,  -(beta))

# add row name
top_terms_italian_k10      <- add_row_numbers(top_terms_italian_k10,      name = "n", zero_based = FALSE)
top_terms_italian_k10_p1   <- add_row_numbers(top_terms_italian_k10_p1,   name = "n", zero_based = FALSE)
top_terms_italian_k10_p2   <- add_row_numbers(top_terms_italian_k10_p2,   name = "n", zero_based = FALSE)
top_terms_italian_k10_p3   <- add_row_numbers(top_terms_italian_k10_p3,   name = "n", zero_based = FALSE)
top_terms_italian_k10_p4   <- add_row_numbers(top_terms_italian_k10_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_italian_k10 <- inner_join(top_terms_italian_k10, top_terms_italian_k10_p1)
top_terms_italian_k10 <- inner_join(top_terms_italian_k10, top_terms_italian_k10_p2)
top_terms_italian_k10 <- inner_join(top_terms_italian_k10, top_terms_italian_k10_p3)
top_terms_italian_k10 <- inner_join(top_terms_italian_k10, top_terms_italian_k10_p4)

write.table(top_terms_italian_k10, "top_terms_italian_k10.csv", row.names = FALSE)





# jewish
# column name
top_terms_jewish_k10_p1 <- rename(top_terms_jewish_k10_p1, term_p1 = term) 
top_terms_jewish_k10_p2 <- rename(top_terms_jewish_k10_p2, term_p2 = term) 
top_terms_jewish_k10_p3 <- rename(top_terms_jewish_k10_p3, term_p3 = term) 
top_terms_jewish_k10_p4 <- rename(top_terms_jewish_k10_p4, term_p4 = term) 

# remove beta

top_terms_jewish_k10     <- select(top_terms_jewish_k10,     -(beta))
top_terms_jewish_k10_p1  <- select(top_terms_jewish_k10_p1,  -(beta))
top_terms_jewish_k10_p2  <- select(top_terms_jewish_k10_p2,  -(beta))
top_terms_jewish_k10_p3  <- select(top_terms_jewish_k10_p3,  -(beta))
top_terms_jewish_k10_p4  <- select(top_terms_jewish_k10_p4,  -(beta))

# add row name
top_terms_jewish_k10      <- add_row_numbers(top_terms_jewish_k10,      name = "n", zero_based = FALSE)
top_terms_jewish_k10_p1   <- add_row_numbers(top_terms_jewish_k10_p1,   name = "n", zero_based = FALSE)
top_terms_jewish_k10_p2   <- add_row_numbers(top_terms_jewish_k10_p2,   name = "n", zero_based = FALSE)
top_terms_jewish_k10_p3   <- add_row_numbers(top_terms_jewish_k10_p3,   name = "n", zero_based = FALSE)
top_terms_jewish_k10_p4   <- add_row_numbers(top_terms_jewish_k10_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_jewish_k10 <- inner_join(top_terms_jewish_k10, top_terms_jewish_k10_p1)
top_terms_jewish_k10 <- inner_join(top_terms_jewish_k10, top_terms_jewish_k10_p2)
top_terms_jewish_k10 <- inner_join(top_terms_jewish_k10, top_terms_jewish_k10_p3)
top_terms_jewish_k10 <- inner_join(top_terms_jewish_k10, top_terms_jewish_k10_p4)

write.table(top_terms_jewish_k10, "top_terms_jewish_k10.csv", row.names = FALSE)





# catholic
# column name
top_terms_catholic_k10_p1 <- rename(top_terms_catholic_k10_p1, term_p1 = term) 
top_terms_catholic_k10_p2 <- rename(top_terms_catholic_k10_p2, term_p2 = term) 
top_terms_catholic_k10_p3 <- rename(top_terms_catholic_k10_p3, term_p3 = term) 
top_terms_catholic_k10_p4 <- rename(top_terms_catholic_k10_p4, term_p4 = term) 

# remove beta

top_terms_catholic_k10     <- select(top_terms_catholic_k10,     -(beta))
top_terms_catholic_k10_p1  <- select(top_terms_catholic_k10_p1,  -(beta))
top_terms_catholic_k10_p2  <- select(top_terms_catholic_k10_p2,  -(beta))
top_terms_catholic_k10_p3  <- select(top_terms_catholic_k10_p3,  -(beta))
top_terms_catholic_k10_p4  <- select(top_terms_catholic_k10_p4,  -(beta))

# add row name
top_terms_catholic_k10      <- add_row_numbers(top_terms_catholic_k10,      name = "n", zero_based = FALSE)
top_terms_catholic_k10_p1   <- add_row_numbers(top_terms_catholic_k10_p1,   name = "n", zero_based = FALSE)
top_terms_catholic_k10_p2   <- add_row_numbers(top_terms_catholic_k10_p2,   name = "n", zero_based = FALSE)
top_terms_catholic_k10_p3   <- add_row_numbers(top_terms_catholic_k10_p3,   name = "n", zero_based = FALSE)
top_terms_catholic_k10_p4   <- add_row_numbers(top_terms_catholic_k10_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_catholic_k10 <- inner_join(top_terms_catholic_k10, top_terms_catholic_k10_p1)
top_terms_catholic_k10 <- inner_join(top_terms_catholic_k10, top_terms_catholic_k10_p2)
top_terms_catholic_k10 <- inner_join(top_terms_catholic_k10, top_terms_catholic_k10_p3)
top_terms_catholic_k10 <- inner_join(top_terms_catholic_k10, top_terms_catholic_k10_p4)

write.table(top_terms_catholic_k10, "top_terms_catholic_k10.csv", row.names = FALSE)





# irish
# column name
top_terms_irish_k10_p1 <- rename(top_terms_irish_k10_p1, term_p1 = term) 
top_terms_irish_k10_p2 <- rename(top_terms_irish_k10_p2, term_p2 = term) 
top_terms_irish_k10_p3 <- rename(top_terms_irish_k10_p3, term_p3 = term) 
top_terms_irish_k10_p4 <- rename(top_terms_irish_k10_p4, term_p4 = term) 

# remove beta

top_terms_irish_k10     <- select(top_terms_irish_k10,     -(beta))
top_terms_irish_k10_p1  <- select(top_terms_irish_k10_p1,  -(beta))
top_terms_irish_k10_p2  <- select(top_terms_irish_k10_p2,  -(beta))
top_terms_irish_k10_p3  <- select(top_terms_irish_k10_p3,  -(beta))
top_terms_irish_k10_p4  <- select(top_terms_irish_k10_p4,  -(beta))

# add row name
top_terms_irish_k10      <- add_row_numbers(top_terms_irish_k10,      name = "n", zero_based = FALSE)
top_terms_irish_k10_p1   <- add_row_numbers(top_terms_irish_k10_p1,   name = "n", zero_based = FALSE)
top_terms_irish_k10_p2   <- add_row_numbers(top_terms_irish_k10_p2,   name = "n", zero_based = FALSE)
top_terms_irish_k10_p3   <- add_row_numbers(top_terms_irish_k10_p3,   name = "n", zero_based = FALSE)
top_terms_irish_k10_p4   <- add_row_numbers(top_terms_irish_k10_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_irish_k10 <- inner_join(top_terms_irish_k10, top_terms_irish_k10_p1)
top_terms_irish_k10 <- inner_join(top_terms_irish_k10, top_terms_irish_k10_p2)
top_terms_irish_k10 <- inner_join(top_terms_irish_k10, top_terms_irish_k10_p3)
top_terms_irish_k10 <- inner_join(top_terms_irish_k10, top_terms_irish_k10_p4)

write.table(top_terms_irish_k10, "top_terms_irish_k10.csv", row.names = FALSE)





# cuban
# column name
top_terms_cuban_k10_p1 <- rename(top_terms_cuban_k10_p1, term_p1 = term) 
top_terms_cuban_k10_p2 <- rename(top_terms_cuban_k10_p2, term_p2 = term) 
top_terms_cuban_k10_p3 <- rename(top_terms_cuban_k10_p3, term_p3 = term) 
top_terms_cuban_k10_p4 <- rename(top_terms_cuban_k10_p4, term_p4 = term) 

# remove beta

top_terms_cuban_k10     <- select(top_terms_cuban_k10,     -(beta))
top_terms_cuban_k10_p1  <- select(top_terms_cuban_k10_p1,  -(beta))
top_terms_cuban_k10_p2  <- select(top_terms_cuban_k10_p2,  -(beta))
top_terms_cuban_k10_p3  <- select(top_terms_cuban_k10_p3,  -(beta))
top_terms_cuban_k10_p4  <- select(top_terms_cuban_k10_p4,  -(beta))

# add row name
top_terms_cuban_k10      <- add_row_numbers(top_terms_cuban_k10,      name = "n", zero_based = FALSE)
top_terms_cuban_k10_p1   <- add_row_numbers(top_terms_cuban_k10_p1,   name = "n", zero_based = FALSE)
top_terms_cuban_k10_p2   <- add_row_numbers(top_terms_cuban_k10_p2,   name = "n", zero_based = FALSE)
top_terms_cuban_k10_p3   <- add_row_numbers(top_terms_cuban_k10_p3,   name = "n", zero_based = FALSE)
top_terms_cuban_k10_p4   <- add_row_numbers(top_terms_cuban_k10_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_cuban_k10 <- inner_join(top_terms_cuban_k10, top_terms_cuban_k10_p1)
top_terms_cuban_k10 <- inner_join(top_terms_cuban_k10, top_terms_cuban_k10_p2)
top_terms_cuban_k10 <- inner_join(top_terms_cuban_k10, top_terms_cuban_k10_p3)
top_terms_cuban_k10 <- inner_join(top_terms_cuban_k10, top_terms_cuban_k10_p4)

write.table(top_terms_cuban_k10, "top_terms_cuban_k10.csv", row.names = FALSE)





# muslim
# column name
top_terms_muslim_k10_p1 <- rename(top_terms_muslim_k10_p1, term_p1 = term) 
top_terms_muslim_k10_p2 <- rename(top_terms_muslim_k10_p2, term_p2 = term) 
top_terms_muslim_k10_p3 <- rename(top_terms_muslim_k10_p3, term_p3 = term) 
top_terms_muslim_k10_p4 <- rename(top_terms_muslim_k10_p4, term_p4 = term) 

# remove beta

top_terms_muslim_k10     <- select(top_terms_muslim_k10,     -(beta))
top_terms_muslim_k10_p1  <- select(top_terms_muslim_k10_p1,  -(beta))
top_terms_muslim_k10_p2  <- select(top_terms_muslim_k10_p2,  -(beta))
top_terms_muslim_k10_p3  <- select(top_terms_muslim_k10_p3,  -(beta))
top_terms_muslim_k10_p4  <- select(top_terms_muslim_k10_p4,  -(beta))

# add row name
top_terms_muslim_k10      <- add_row_numbers(top_terms_muslim_k10,      name = "n", zero_based = FALSE)
top_terms_muslim_k10_p1   <- add_row_numbers(top_terms_muslim_k10_p1,   name = "n", zero_based = FALSE)
top_terms_muslim_k10_p2   <- add_row_numbers(top_terms_muslim_k10_p2,   name = "n", zero_based = FALSE)
top_terms_muslim_k10_p3   <- add_row_numbers(top_terms_muslim_k10_p3,   name = "n", zero_based = FALSE)
top_terms_muslim_k10_p4   <- add_row_numbers(top_terms_muslim_k10_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_muslim_k10 <- inner_join(top_terms_muslim_k10, top_terms_muslim_k10_p1)
top_terms_muslim_k10 <- inner_join(top_terms_muslim_k10, top_terms_muslim_k10_p2)
top_terms_muslim_k10 <- inner_join(top_terms_muslim_k10, top_terms_muslim_k10_p3)
top_terms_muslim_k10 <- inner_join(top_terms_muslim_k10, top_terms_muslim_k10_p4)

write.table(top_terms_muslim_k10, "top_terms_muslim_k10.csv", row.names = FALSE)





# chinese
# column name
top_terms_chinese_k10_p1 <- rename(top_terms_chinese_k10_p1, term_p1 = term) 
top_terms_chinese_k10_p2 <- rename(top_terms_chinese_k10_p2, term_p2 = term) 
top_terms_chinese_k10_p3 <- rename(top_terms_chinese_k10_p3, term_p3 = term) 
top_terms_chinese_k10_p4 <- rename(top_terms_chinese_k10_p4, term_p4 = term) 

# remove beta

top_terms_chinese_k10     <- select(top_terms_chinese_k10,     -(beta))
top_terms_chinese_k10_p1  <- select(top_terms_chinese_k10_p1,  -(beta))
top_terms_chinese_k10_p2  <- select(top_terms_chinese_k10_p2,  -(beta))
top_terms_chinese_k10_p3  <- select(top_terms_chinese_k10_p3,  -(beta))
top_terms_chinese_k10_p4  <- select(top_terms_chinese_k10_p4,  -(beta))

# add row name
top_terms_chinese_k10      <- add_row_numbers(top_terms_chinese_k10,      name = "n", zero_based = FALSE)
top_terms_chinese_k10_p1   <- add_row_numbers(top_terms_chinese_k10_p1,   name = "n", zero_based = FALSE)
top_terms_chinese_k10_p2   <- add_row_numbers(top_terms_chinese_k10_p2,   name = "n", zero_based = FALSE)
top_terms_chinese_k10_p3   <- add_row_numbers(top_terms_chinese_k10_p3,   name = "n", zero_based = FALSE)
top_terms_chinese_k10_p4   <- add_row_numbers(top_terms_chinese_k10_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_chinese_k10 <- inner_join(top_terms_chinese_k10, top_terms_chinese_k10_p1)
top_terms_chinese_k10 <- inner_join(top_terms_chinese_k10, top_terms_chinese_k10_p2)
top_terms_chinese_k10 <- inner_join(top_terms_chinese_k10, top_terms_chinese_k10_p3)
top_terms_chinese_k10 <- inner_join(top_terms_chinese_k10, top_terms_chinese_k10_p4)

write.table(top_terms_chinese_k10, "top_terms_chinese_k10.csv", row.names = FALSE)


# mexican
# column name
top_terms_mexican_k10_p1 <- rename(top_terms_mexican_k10_p1, term_p1 = term) 
top_terms_mexican_k10_p2 <- rename(top_terms_mexican_k10_p2, term_p2 = term) 
top_terms_mexican_k10_p3 <- rename(top_terms_mexican_k10_p3, term_p3 = term) 
top_terms_mexican_k10_p4 <- rename(top_terms_mexican_k10_p4, term_p4 = term) 

# remove beta

top_terms_mexican_k10     <- select(top_terms_mexican_k10,     -(beta))
top_terms_mexican_k10_p1  <- select(top_terms_mexican_k10_p1,  -(beta))
top_terms_mexican_k10_p2  <- select(top_terms_mexican_k10_p2,  -(beta))
top_terms_mexican_k10_p3  <- select(top_terms_mexican_k10_p3,  -(beta))
top_terms_mexican_k10_p4  <- select(top_terms_mexican_k10_p4,  -(beta))

# add row name
top_terms_mexican_k10      <- add_row_numbers(top_terms_mexican_k10,      name = "n", zero_based = FALSE)
top_terms_mexican_k10_p1   <- add_row_numbers(top_terms_mexican_k10_p1,   name = "n", zero_based = FALSE)
top_terms_mexican_k10_p2   <- add_row_numbers(top_terms_mexican_k10_p2,   name = "n", zero_based = FALSE)
top_terms_mexican_k10_p3   <- add_row_numbers(top_terms_mexican_k10_p3,   name = "n", zero_based = FALSE)
top_terms_mexican_k10_p4   <- add_row_numbers(top_terms_mexican_k10_p4,   name = "n", zero_based = FALSE)


# merge and export
top_terms_mexican_k10 <- inner_join(top_terms_mexican_k10, top_terms_mexican_k10_p1)
top_terms_mexican_k10 <- inner_join(top_terms_mexican_k10, top_terms_mexican_k10_p2)
top_terms_mexican_k10 <- inner_join(top_terms_mexican_k10, top_terms_mexican_k10_p3)
top_terms_mexican_k10 <- inner_join(top_terms_mexican_k10, top_terms_mexican_k10_p4)

write.table(top_terms_mexican_k10, "top_terms_mexican_k10.csv", row.names = FALSE)















































































































































































































































































































