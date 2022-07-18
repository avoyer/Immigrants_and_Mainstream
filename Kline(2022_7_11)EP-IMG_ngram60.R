
##  Reproduction package: Immigrants_and_mainstream
# Located as avoyer/immigrants_and_mainstream
# File Name:Kline(2022_7_11)EP-IMG
# Date:     txt_2022_3_28
# Who:      Voyer, Andrea, Zachary D. Kline, Madison Danton, and Tanja Volkova




#   1) copy txt to your R working directory
#             titled "txt_2022_3_28".
#     Note 1: Text comes from txt_2022_3_28. All changes to the corpus made
#                 after this date are excluded from analysis.

#   2)   Set Working Directory
setwd("C:/Users/zdk15001/Desktop/Academia/University_of_Connecticut/Emily_Post/posted/2022_7_11_repo/")
wd <- "C:/Users/zdk15001/Desktop/Academia/University_of_Connecticut/Emily_Post/posted/2022_7_11_repo/"

#   3)  Install and Load Packages (if not using remote server, you may only need to do this once)
#         Some of these packages are often updated, creating conflicts. 
#             Errors may be the result of commands that are 
#             defined differently in multiple packages

#install.packages("rlang")
#install.packages("dplyr")
#install.packages("tidytext")
#install.packages("tidyverse")
#install Rtools from https://cran.r-project.org/bin/windows/Rtools/
#install.packages("pacman")
#install.packages("devtools")
#install.packages('BiocManager')
#install.packages('stringr')
#install.packages('backports')
#install.packages("utils")
#install.packages("topicmodels")
#install.packages('text2map')



#devtools::install_github("UrbanInstitute/urbnthemes")
#devtools::install_github("statsmaths/fasttextM")           
#devtools::install_github("lionel-/ggstance")
#devtools::install_github("quanteda/quanteda.corpora")
#devtools::install_github("mukul13/rword2vec")
#install.packages('text2map')
#devtools::install_github("skgrange/threadr")



### Script begins here
#   Load in packages
library(rlang)
library(dplyr)
library(backports)
library(stringr)
library(tidytext)
library(tidyverse)
library(tokenizers)
library(tidyr)
library(ggplot2)
library(devtools)
library(pacman)
library(BiocManager)
library(text2map)
library(text2vec)
library(fasttextM)
library(urbnthemes)
library(utils)
library(topicmodels)
library(threadr)

pacman::p_load(text2vec,fasttextM,
               tm, Matrix,tidytext,
               textstem,dplyr,
               tidyverse,textclean,
               quanteda.corpora,
               stringr,extrafont,
               ggplot2,ggrepel,
               gutenbergr,ggstance,
               ggpubr,GGally,
               install=TRUE)



set_urbn_defaults()


# 4 Load appropriate word embeddings 
ngram_1960 <- readRDS("ngram_1960.Rds")








# 5  Load in and combine text into "source_EP"

# Folder where editions.txt are stored (Change as updated)
path2file <- "txt_2022_3_28"

# Create a list of editions
fileList <- list.files(path2file, full.names = TRUE) # Create a list of files in a folder

# Function to read in multiple texts and paste them into a tbl 
# TO DO HERE:
#   Identify how to tag chapters, sections, parts, ect 
#     Should be editions(parts(chapter(section)))
#   Identify how to change "filename"... can I just change from below "filename?"


readTextFiles <- function(file) { 
  message(file)
  rawText = paste(scan(file, sep="\n",what="raw",strip.white = TRUE))
  output = tibble(filename=gsub("data/sample_texts/","",file),text=rawText) %>% 
    group_by(filename) %>% 
    summarise(text = paste(rawText, collapse = " "))
  return(output)
}

# Run the function to create a tbl of combined files

source_EP <- tibble(filename=fileList) %>% 
  group_by(filename) %>% 
  do(readTextFiles(.$filename))  %>% 
  ungroup()

#   6 Create tidy data set

tidy_EP <- source_EP %>% 
  mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = replace_white(text)) %>%
  mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
  mutate(text = replace_number(text))  %>%
  unnest_tokens(word, text, to_lower = TRUE) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]+") ) 

#  Rename filename 

tidy_EP <- mutate(tidy_EP, filename =  
           ifelse(grepl("txt_2022_3_28/1922EditionFull.txt", filename), "1922",
           ifelse(grepl("txt_2022_3_28/1927EditionFull.txt", filename), "1927",
           ifelse(grepl("txt_2022_3_28/1931EditionFull.txt", filename), "1931",
           ifelse(grepl("txt_2022_3_28/1934EditionFull.txt", filename), "1934",
           ifelse(grepl("txt_2022_3_28/1937EditionFull.txt", filename), "1937",
           ifelse(grepl("txt_2022_3_28/1940EditionFull.txt", filename), "1940",
           ifelse(grepl("txt_2022_3_28/1942EditionFull.txt", filename), "1942",
           ifelse(grepl("txt_2022_3_28/1945EditionFull.txt", filename), "1945",
           ifelse(grepl("txt_2022_3_28/1950EditionFull.txt", filename), "1950",
           ifelse(grepl("txt_2022_3_28/1956EditionFull.txt", filename), "1956",
           ifelse(grepl("txt_2022_3_28/1960EditionFull.txt", filename), "1960",
           ifelse(grepl("txt_2022_3_28/1965EditionFull.txt", filename), "1965",
           ifelse(grepl("txt_2022_3_28/1969EditionFull.txt", filename), "1969",
           ifelse(grepl("txt_2022_3_28/1975EditionFull.txt", filename), "1975",
           ifelse(grepl("txt_2022_3_28/1984EditionFull.txt", filename), "1984",
           ifelse(grepl("txt_2022_3_28/1992EditionFull.txt", filename), "1992",
           ifelse(grepl("txt_2022_3_28/1997EditionFull.txt", filename), "1997",
           ifelse(grepl("txt_2022_3_28/2004EditionFull.txt", filename), "2004",
           ifelse(grepl("txt_2022_3_28/2011EditionFull.txt", filename), "2011",
           ifelse(grepl("txt_2022_3_28/2017EditionFull.txt", filename), "2017","x")))))))))))))))))))))





#   7 Chunk each edition into 150 word segments
#   150 word chunks

c1922 <- 150
c1927 <- 150
c1931 <- 150
c1934 <- 150
c1937 <- 150
c1940 <- 150
c1942 <- 150
c1945 <- 150
c1950 <- 150
c1956 <- 150
c1960 <- 150
c1965 <- 150
c1969 <- 150
c1975 <- 150
c1984 <- 150
c1992 <- 150
c1997 <- 150
c2004 <- 150
c2011 <- 150
c2017 <- 150


#  Create ID variables for each row

#1922
tidy_EP_1922 <- tidy_EP %>%
  filter(filename == "1922")

tidy_EP_1922 <- tidy_EP_1922 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1922))) %>%
  mutate(chunkx = as.numeric(id)/c1922 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1922*1000) %>%
  mutate(year = 1922)
  

#1927
tidy_EP_1927 <- tidy_EP %>%
  filter(filename == "1927")

tidy_EP_1927 <- tidy_EP_1927 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1927))) %>%
  mutate(chunkx = as.numeric(id)/c1927 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1927*1000) %>%
  mutate(year = 1927)

#1931
tidy_EP_1931 <- tidy_EP %>%
  filter(filename == "1931")

tidy_EP_1931 <- tidy_EP_1931 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1931))) %>%
  mutate(chunkx = as.numeric(id)/c1931 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1931*1000) %>%
  mutate(year = 1931)

#1934
tidy_EP_1934 <- tidy_EP %>%
  filter(filename == "1934")

tidy_EP_1934 <- tidy_EP_1934 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1934))) %>%
  mutate(chunkx = as.numeric(id)/c1934 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1934*1000) %>%
  mutate(year = 1934)

#1937
tidy_EP_1937 <- tidy_EP %>%
  filter(filename == "1937")

tidy_EP_1937 <- tidy_EP_1937 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1937))) %>%
  mutate(chunkx = as.numeric(id)/c1937 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 ))  %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1937*1000) %>%
  mutate(year = 1937)

#1940
tidy_EP_1940 <- tidy_EP %>%
  filter(filename == "1940")

tidy_EP_1940 <- tidy_EP_1940 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1940))) %>%
  mutate(chunkx = as.numeric(id)/c1940 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1940*1000) %>%
  mutate(year = 1940)

#1942
tidy_EP_1942 <- tidy_EP %>%
  filter(filename == "1942")

tidy_EP_1942 <- tidy_EP_1942 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1942))) %>%
  mutate(chunkx = as.numeric(id)/c1942 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1942*1000) %>%
  mutate(year = 1942)

#1945
tidy_EP_1945 <- tidy_EP %>%
  filter(filename == "1945")

tidy_EP_1945 <- tidy_EP_1945 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1945))) %>%
  mutate(chunkx = as.numeric(id)/c1945 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1945*1000) %>%
  mutate(year = 1945)

#1950
tidy_EP_1950 <- tidy_EP %>%
  filter(filename == "1950")

tidy_EP_1950 <- tidy_EP_1950 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1950))) %>%
  mutate(chunkx = as.numeric(id)/c1950 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1950*1000) %>%
  mutate(year = 1950)

#1956
tidy_EP_1956 <- tidy_EP %>%
  filter(filename == "1956")

tidy_EP_1956 <- tidy_EP_1956 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1956))) %>%
  mutate(chunkx = as.numeric(id)/c1956 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1956*1000) %>%
  mutate(year = 1956)


#1960
tidy_EP_1960 <- tidy_EP %>%
  filter(filename == "1960")

tidy_EP_1960 <- tidy_EP_1960 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1960))) %>%
  mutate(chunkx = as.numeric(id)/c1960 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1960*1000) %>%
  mutate(year = 1960)


#1965
tidy_EP_1965 <- tidy_EP %>%
  filter(filename == "1965")

tidy_EP_1965 <- tidy_EP_1965 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1965))) %>%
  mutate(chunkx = as.numeric(id)/c1965 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 ))  %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1965*1000) %>%
  mutate(year = 1965)


#1969
tidy_EP_1969 <- tidy_EP %>%
  filter(filename == "1969")

tidy_EP_1969 <- tidy_EP_1969 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1969))) %>%
  mutate(chunkx = as.numeric(id)/c1969 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 ))  %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1969*1000) %>%
  mutate(year = 1969)


#1975
tidy_EP_1975 <- tidy_EP %>%
  filter(filename == "1975")

tidy_EP_1975 <- tidy_EP_1975 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1975))) %>%
  mutate(chunkx = as.numeric(id)/c1975 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 ))  %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1975*1000) %>%
  mutate(year = 1975)


#1984
tidy_EP_1984 <- tidy_EP %>%
  filter(filename == "1984")

tidy_EP_1984 <- tidy_EP_1984 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1984))) %>%
  mutate(chunkx = as.numeric(id)/c1984 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1984*1000) %>%
  mutate(year = 1984)


#1992
tidy_EP_1992 <- tidy_EP %>%
  filter(filename == "1992")

tidy_EP_1992 <- tidy_EP_1992 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1992))) %>%
  mutate(chunkx = as.numeric(id)/c1992 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1992*1000) %>%
  mutate(year = 1992)


#1997
tidy_EP_1997 <- tidy_EP %>%
  filter(filename == "1997")

tidy_EP_1997 <- tidy_EP_1997 %>%
  mutate(id = as.numeric(rownames(tidy_EP_1997))) %>%
  mutate(chunkx = as.numeric(id)/c1997 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 1997*1000) %>%
  mutate(year = 1997)


#2004
tidy_EP_2004 <- tidy_EP %>%
  filter(filename == "2004")

tidy_EP_2004 <- tidy_EP_2004 %>%
  mutate(id = as.numeric(rownames(tidy_EP_2004))) %>%
  mutate(chunkx = as.numeric(id)/c2004 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 2004*1000) %>%
  mutate(year = 2004)


#2011
tidy_EP_2011 <- tidy_EP %>%
  filter(filename == "2011")

tidy_EP_2011 <- tidy_EP_2011 %>%
  mutate(id = as.numeric(rownames(tidy_EP_2011))) %>%
  mutate(chunkx = as.numeric(id)/c2011 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 2011*1000) %>%
  mutate(year = 2011)


#2017
tidy_EP_2017 <- tidy_EP %>%
  filter(filename == "2017")

tidy_EP_2017 <- tidy_EP_2017 %>%
  mutate(id = as.numeric(rownames(tidy_EP_2017))) %>%
  mutate(chunkx = as.numeric(id)/c2017 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0 )) %>% 
  count(chunk, word) %>%
  bind_tf_idf(word, chunk, n) %>%
  mutate(unique = chunk + 2017*1000) %>%
  mutate(year = 2017)

##  combine sets

tidy_EP_chunks <- full_join(tidy_EP_1922, tidy_EP_1927)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1931)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1934)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1937)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1940)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1942)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1945)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1950)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1956)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1960)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1965)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1969)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1975)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1984)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1992)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_1997)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_2004)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_2011)
tidy_EP_chunks <- full_join(tidy_EP_chunks, tidy_EP_2017)

















# 8 prepare sentiment lexicon
#Sentiment Preparation

lex1920 <- read.table(file = '1920.tsv', sep = '\t', header = FALSE)
lex1930 <- read.table(file = '1930.tsv', sep = '\t', header = FALSE)
lex1940 <- read.table(file = '1940.tsv', sep = '\t', header = FALSE)
lex1950 <- read.table(file = '1950.tsv', sep = '\t', header = FALSE)
lex1960 <- read.table(file = '1960.tsv', sep = '\t', header = FALSE)
lex1970 <- read.table(file = '1970.tsv', sep = '\t', header = FALSE)
lex1980 <- read.table(file = '1980.tsv', sep = '\t', header = FALSE)
lex1990 <- read.table(file = '1990.tsv', sep = '\t', header = FALSE)
lex2000 <- read.table(file = '2000.tsv', sep = '\t', header = FALSE)

#rename headers
lex1920 <- lex1920 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)
lex1930 <- lex1930 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)
lex1940 <- lex1940 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)
lex1950 <- lex1950 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)
lex1960 <- lex1960 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)
lex1970 <- lex1970 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)
lex1980 <- lex1980 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)
lex1990 <- lex1990 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)
lex2000 <- lex2000 %>%
  select(c("V1", "V2", "V3")) %>%
  rename('word' = "V1", 'mean_sentiment' = "V2", "std_sentiment" = V3)

#applying domain specific sentiments

tidy_EP_1922 <- tidy_EP_1922 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1920) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1922 %>%
              mutate(linenumber = row_number()))

tidy_EP_1927 <- tidy_EP_1927 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1920) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1927 %>%
              mutate(linenumber = row_number()))

tidy_EP_1931 <- tidy_EP_1931 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1930) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1931 %>%
              mutate(linenumber = row_number()))

tidy_EP_1934 <- tidy_EP_1934 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1930) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1934 %>%
              mutate(linenumber = row_number()))

tidy_EP_1937 <- tidy_EP_1937 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1930) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1937 %>%
              mutate(linenumber = row_number()))

tidy_EP_1940 <- tidy_EP_1940 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1940) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1940 %>%
              mutate(linenumber = row_number()))

tidy_EP_1942 <- tidy_EP_1942 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1940) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1942 %>%
              mutate(linenumber = row_number()))

tidy_EP_1945 <- tidy_EP_1945 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1940) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1945 %>%
              mutate(linenumber = row_number()))

tidy_EP_1950 <- tidy_EP_1950 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1950) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1950 %>%
              mutate(linenumber = row_number()))

tidy_EP_1956 <- tidy_EP_1956 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1950) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1956 %>%
              mutate(linenumber = row_number()))

tidy_EP_1960 <- tidy_EP_1960 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1960) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1960 %>%
              mutate(linenumber = row_number()))

tidy_EP_1965 <- tidy_EP_1965 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1960) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1965 %>%
              mutate(linenumber = row_number()))

tidy_EP_1969 <- tidy_EP_1969 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1960) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1969 %>%
              mutate(linenumber = row_number()))

tidy_EP_1975 <- tidy_EP_1975 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1970) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1975 %>%
              mutate(linenumber = row_number()))

tidy_EP_1984 <- tidy_EP_1984 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1980) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1984 %>%
              mutate(linenumber = row_number()))

tidy_EP_1992 <- tidy_EP_1992 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1990) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1992 %>%
              mutate(linenumber = row_number()))

tidy_EP_1997 <- tidy_EP_1997 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex1990) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_1997 %>%
              mutate(linenumber = row_number()))

tidy_EP_2004 <- tidy_EP_2004 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex2000) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_2004 %>%
              mutate(linenumber = row_number()))

tidy_EP_2011 <- tidy_EP_2011 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex2000) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_2011 %>%
              mutate(linenumber = row_number()))

tidy_EP_2017 <- tidy_EP_2017 %>%
  mutate(linenumber = row_number()) %>%
  inner_join(lex2000) %>% #inner join with historical lexicon to get polarity
  group_by(word) %>%
  left_join(tidy_EP_2017 %>%
              mutate(linenumber = row_number()))























#   9 Use CMDist to create pseudocorpera

# create  direction (strange vs normal)
additions <- c("normal")
subtracts  <- c("strange")
pairs <- cbind(additions, subtracts)
immigrant.sd <- get_direction(pairs, wv = ngram_1960)


#  Italian


tidy_EP_italian_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)


tidy_EP_italian_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)


tidy_EP_italian_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv = ngram_1960)

tidy_EP_italian_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=ngram_1960)

tidy_EP_italian_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=ngram_1960)

tidy_EP_italian_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=ngram_1960)


# combine, need to add "year"; also add unique

tidy_EP_italian_1922 <- tidy_EP_italian_1922 %>%
  mutate(year = 1922) 

tidy_EP_italian_1927 <- tidy_EP_italian_1927 %>%
  mutate(year = 1927)

tidy_EP_italian_1931 <- tidy_EP_italian_1931 %>%
  mutate(year = 1931)

tidy_EP_italian_1934 <- tidy_EP_italian_1934 %>%
  mutate(year = 1934)

tidy_EP_italian_1937 <- tidy_EP_italian_1937 %>%
  mutate(year = 1937)

tidy_EP_italian_1940 <- tidy_EP_italian_1940 %>%
  mutate(year = 1940)

tidy_EP_italian_1942 <- tidy_EP_italian_1942 %>%
  mutate(year = 1942)

tidy_EP_italian_1945 <- tidy_EP_italian_1945 %>%
  mutate(year = 1945)

tidy_EP_italian_1950 <- tidy_EP_italian_1950 %>%
  mutate(year = 1950)

tidy_EP_italian_1956 <- tidy_EP_italian_1956 %>%
  mutate(year = 1956)

tidy_EP_italian_1960 <- tidy_EP_italian_1960 %>%
  mutate(year = 1960)

tidy_EP_italian_1965 <- tidy_EP_italian_1965 %>%
  mutate(year = 1965)


tidy_EP_italian_1969 <- tidy_EP_italian_1969 %>%
  mutate(year = 1969)

tidy_EP_italian_1975 <- tidy_EP_italian_1975 %>%
  mutate(year = 1975)

tidy_EP_italian_1984 <- tidy_EP_italian_1984 %>%
  mutate(year = 1984)

tidy_EP_italian_1992 <- tidy_EP_italian_1992 %>%
  mutate(year = 1992)

tidy_EP_italian_1997 <- tidy_EP_italian_1997 %>%
  mutate(year = 1997)

tidy_EP_italian_2004 <- tidy_EP_italian_2004 %>%
  mutate(year = 2004)

tidy_EP_italian_2011 <- tidy_EP_italian_2011 %>%
  mutate(year = 2011)

tidy_EP_italian_2017 <- tidy_EP_italian_2017 %>%
  mutate(year = 2017)



# select on CMD > cv for each edition, gen unique

tidy_EP_italian_1922_salient <- tidy_EP_italian_1922[tidy_EP_italian_1922$italian > 1.65, ]
tidy_EP_italian_1927_salient <- tidy_EP_italian_1927[tidy_EP_italian_1927$italian > 1.65, ]
tidy_EP_italian_1931_salient <- tidy_EP_italian_1931[tidy_EP_italian_1931$italian > 1.65, ]
tidy_EP_italian_1934_salient <- tidy_EP_italian_1934[tidy_EP_italian_1934$italian > 1.65, ]
tidy_EP_italian_1937_salient <- tidy_EP_italian_1937[tidy_EP_italian_1937$italian > 1.65, ]
tidy_EP_italian_1940_salient <- tidy_EP_italian_1940[tidy_EP_italian_1940$italian > 1.65, ]
tidy_EP_italian_1942_salient <- tidy_EP_italian_1942[tidy_EP_italian_1942$italian > 1.65, ]
tidy_EP_italian_1945_salient <- tidy_EP_italian_1945[tidy_EP_italian_1945$italian > 1.65, ]
tidy_EP_italian_1950_salient <- tidy_EP_italian_1950[tidy_EP_italian_1950$italian > 1.65, ]
tidy_EP_italian_1956_salient <- tidy_EP_italian_1956[tidy_EP_italian_1956$italian > 1.65, ]
tidy_EP_italian_1960_salient <- tidy_EP_italian_1960[tidy_EP_italian_1960$italian > 1.65, ]
tidy_EP_italian_1965_salient <- tidy_EP_italian_1965[tidy_EP_italian_1965$italian > 1.65, ]
tidy_EP_italian_1969_salient <- tidy_EP_italian_1969[tidy_EP_italian_1969$italian > 1.65, ]
tidy_EP_italian_1975_salient <- tidy_EP_italian_1975[tidy_EP_italian_1975$italian > 1.65, ]
tidy_EP_italian_1984_salient <- tidy_EP_italian_1984[tidy_EP_italian_1984$italian > 1.65, ]
tidy_EP_italian_1992_salient <- tidy_EP_italian_1992[tidy_EP_italian_1992$italian > 1.65, ]
tidy_EP_italian_1997_salient <- tidy_EP_italian_1997[tidy_EP_italian_1997$italian > 1.65, ]
tidy_EP_italian_2004_salient <- tidy_EP_italian_2004[tidy_EP_italian_2004$italian > 1.65, ]
tidy_EP_italian_2011_salient <- tidy_EP_italian_2011[tidy_EP_italian_2011$italian > 1.65, ]
tidy_EP_italian_2017_salient <- tidy_EP_italian_2017[tidy_EP_italian_2017$italian > 1.65, ]



# list of salient
italian_chunk_1922 <- print(tidy_EP_italian_1922_salient$doc_id)
italian_chunk_1927 <- print(tidy_EP_italian_1927_salient$doc_id)
italian_chunk_1931 <- print(tidy_EP_italian_1931_salient$doc_id)
italian_chunk_1934 <- print(tidy_EP_italian_1934_salient$doc_id)
italian_chunk_1937 <- print(tidy_EP_italian_1937_salient$doc_id)
italian_chunk_1940 <- print(tidy_EP_italian_1940_salient$doc_id)
italian_chunk_1942 <- print(tidy_EP_italian_1942_salient$doc_id)
italian_chunk_1945 <- print(tidy_EP_italian_1945_salient$doc_id)
italian_chunk_1950 <- print(tidy_EP_italian_1950_salient$doc_id)
italian_chunk_1956 <- print(tidy_EP_italian_1956_salient$doc_id)
italian_chunk_1960 <- print(tidy_EP_italian_1960_salient$doc_id)
italian_chunk_1965 <- print(tidy_EP_italian_1965_salient$doc_id)
italian_chunk_1969 <- print(tidy_EP_italian_1969_salient$doc_id)
italian_chunk_1975 <- print(tidy_EP_italian_1975_salient$doc_id)
italian_chunk_1984 <- print(tidy_EP_italian_1984_salient$doc_id)
italian_chunk_1992 <- print(tidy_EP_italian_1992_salient$doc_id)
italian_chunk_1997 <- print(tidy_EP_italian_1997_salient$doc_id)
italian_chunk_2004 <- print(tidy_EP_italian_2004_salient$doc_id)
italian_chunk_2011 <- print(tidy_EP_italian_2011_salient$doc_id)
italian_chunk_2017 <- print(tidy_EP_italian_2017_salient$doc_id)



# select if in chunk
tidy_EP_italian_1922_select <- filter(tidy_EP_1922, chunk %in% italian_chunk_1922)
tidy_EP_italian_1927_select <- filter(tidy_EP_1927, chunk %in% italian_chunk_1927)
tidy_EP_italian_1931_select <- filter(tidy_EP_1931, chunk %in% italian_chunk_1931)
tidy_EP_italian_1934_select <- filter(tidy_EP_1934, chunk %in% italian_chunk_1934)
tidy_EP_italian_1937_select <- filter(tidy_EP_1937, chunk %in% italian_chunk_1937)
tidy_EP_italian_1940_select <- filter(tidy_EP_1940, chunk %in% italian_chunk_1940)
tidy_EP_italian_1942_select <- filter(tidy_EP_1942, chunk %in% italian_chunk_1942)
tidy_EP_italian_1945_select <- filter(tidy_EP_1945, chunk %in% italian_chunk_1945)
tidy_EP_italian_1950_select <- filter(tidy_EP_1950, chunk %in% italian_chunk_1950)
tidy_EP_italian_1956_select <- filter(tidy_EP_1956, chunk %in% italian_chunk_1956)
tidy_EP_italian_1960_select <- filter(tidy_EP_1960, chunk %in% italian_chunk_1960)
tidy_EP_italian_1965_select <- filter(tidy_EP_1965, chunk %in% italian_chunk_1965)
tidy_EP_italian_1969_select <- filter(tidy_EP_1969, chunk %in% italian_chunk_1969)
tidy_EP_italian_1975_select <- filter(tidy_EP_1975, chunk %in% italian_chunk_1975)
tidy_EP_italian_1984_select <- filter(tidy_EP_1984, chunk %in% italian_chunk_1984)
tidy_EP_italian_1992_select <- filter(tidy_EP_1992, chunk %in% italian_chunk_1992)
tidy_EP_italian_1997_select <- filter(tidy_EP_1997, chunk %in% italian_chunk_1997)
tidy_EP_italian_2004_select <- filter(tidy_EP_2004, chunk %in% italian_chunk_2004)
tidy_EP_italian_2011_select <- filter(tidy_EP_2011, chunk %in% italian_chunk_2011)
tidy_EP_italian_2017_select <- filter(tidy_EP_2017, chunk %in% italian_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_italian <- full_join(tidy_EP_italian_1922_select, tidy_EP_italian_1927_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1931_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1934_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1937_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1940_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1942_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1945_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1950_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1956_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1960_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1965_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1969_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1975_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1984_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1992_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_1997_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_2004_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_2011_select)
tidy_EP_chunks_italian <- full_join(tidy_EP_chunks_italian, tidy_EP_italian_2017_select)



#   build cmdist for antonym pair, italian

italian_closeness <- tidy_EP_chunks_italian %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=ngram_1960 )





#   jewish


tidy_EP_jewish_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)


tidy_EP_jewish_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)


tidy_EP_jewish_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv = ngram_1960)

tidy_EP_jewish_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=ngram_1960)

tidy_EP_jewish_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=ngram_1960)

tidy_EP_jewish_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=ngram_1960)


# combine, need to add "year"; also add unique

tidy_EP_jewish_1922 <- tidy_EP_jewish_1922 %>%
  mutate(year = 1922) 

tidy_EP_jewish_1927 <- tidy_EP_jewish_1927 %>%
  mutate(year = 1927)

tidy_EP_jewish_1931 <- tidy_EP_jewish_1931 %>%
  mutate(year = 1931)

tidy_EP_jewish_1934 <- tidy_EP_jewish_1934 %>%
  mutate(year = 1934)

tidy_EP_jewish_1937 <- tidy_EP_jewish_1937 %>%
  mutate(year = 1937)

tidy_EP_jewish_1940 <- tidy_EP_jewish_1940 %>%
  mutate(year = 1940)

tidy_EP_jewish_1942 <- tidy_EP_jewish_1942 %>%
  mutate(year = 1942)

tidy_EP_jewish_1945 <- tidy_EP_jewish_1945 %>%
  mutate(year = 1945)

tidy_EP_jewish_1950 <- tidy_EP_jewish_1950 %>%
  mutate(year = 1950)

tidy_EP_jewish_1956 <- tidy_EP_jewish_1956 %>%
  mutate(year = 1956)

tidy_EP_jewish_1960 <- tidy_EP_jewish_1960 %>%
  mutate(year = 1960)

tidy_EP_jewish_1965 <- tidy_EP_jewish_1965 %>%
  mutate(year = 1965)


tidy_EP_jewish_1969 <- tidy_EP_jewish_1969 %>%
  mutate(year = 1969)

tidy_EP_jewish_1975 <- tidy_EP_jewish_1975 %>%
  mutate(year = 1975)

tidy_EP_jewish_1984 <- tidy_EP_jewish_1984 %>%
  mutate(year = 1984)

tidy_EP_jewish_1992 <- tidy_EP_jewish_1992 %>%
  mutate(year = 1992)

tidy_EP_jewish_1997 <- tidy_EP_jewish_1997 %>%
  mutate(year = 1997)

tidy_EP_jewish_2004 <- tidy_EP_jewish_2004 %>%
  mutate(year = 2004)

tidy_EP_jewish_2011 <- tidy_EP_jewish_2011 %>%
  mutate(year = 2011)

tidy_EP_jewish_2017 <- tidy_EP_jewish_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_jewish_1922_salient <- tidy_EP_jewish_1922[tidy_EP_jewish_1922$jewish > 1.65, ]
tidy_EP_jewish_1927_salient <- tidy_EP_jewish_1927[tidy_EP_jewish_1927$jewish > 1.65, ]
tidy_EP_jewish_1931_salient <- tidy_EP_jewish_1931[tidy_EP_jewish_1931$jewish > 1.65, ]
tidy_EP_jewish_1934_salient <- tidy_EP_jewish_1934[tidy_EP_jewish_1934$jewish > 1.65, ]
tidy_EP_jewish_1937_salient <- tidy_EP_jewish_1937[tidy_EP_jewish_1937$jewish > 1.65, ]
tidy_EP_jewish_1940_salient <- tidy_EP_jewish_1940[tidy_EP_jewish_1940$jewish > 1.65, ]
tidy_EP_jewish_1942_salient <- tidy_EP_jewish_1942[tidy_EP_jewish_1942$jewish > 1.65, ]
tidy_EP_jewish_1945_salient <- tidy_EP_jewish_1945[tidy_EP_jewish_1945$jewish > 1.65, ]
tidy_EP_jewish_1950_salient <- tidy_EP_jewish_1950[tidy_EP_jewish_1950$jewish > 1.65, ]
tidy_EP_jewish_1956_salient <- tidy_EP_jewish_1956[tidy_EP_jewish_1956$jewish > 1.65, ]
tidy_EP_jewish_1960_salient <- tidy_EP_jewish_1960[tidy_EP_jewish_1960$jewish > 1.65, ]
tidy_EP_jewish_1965_salient <- tidy_EP_jewish_1965[tidy_EP_jewish_1965$jewish > 1.65, ]
tidy_EP_jewish_1969_salient <- tidy_EP_jewish_1969[tidy_EP_jewish_1969$jewish > 1.65, ]
tidy_EP_jewish_1975_salient <- tidy_EP_jewish_1975[tidy_EP_jewish_1975$jewish > 1.65, ]
tidy_EP_jewish_1984_salient <- tidy_EP_jewish_1984[tidy_EP_jewish_1984$jewish > 1.65, ]
tidy_EP_jewish_1992_salient <- tidy_EP_jewish_1992[tidy_EP_jewish_1992$jewish > 1.65, ]
tidy_EP_jewish_1997_salient <- tidy_EP_jewish_1997[tidy_EP_jewish_1997$jewish > 1.65, ]
tidy_EP_jewish_2004_salient <- tidy_EP_jewish_2004[tidy_EP_jewish_2004$jewish > 1.65, ]
tidy_EP_jewish_2011_salient <- tidy_EP_jewish_2011[tidy_EP_jewish_2011$jewish > 1.65, ]
tidy_EP_jewish_2017_salient <- tidy_EP_jewish_2017[tidy_EP_jewish_2017$jewish > 1.65, ]



# list of salient
jewish_chunk_1922 <- print(tidy_EP_jewish_1922_salient$doc_id)
jewish_chunk_1927 <- print(tidy_EP_jewish_1927_salient$doc_id)
jewish_chunk_1931 <- print(tidy_EP_jewish_1931_salient$doc_id)
jewish_chunk_1934 <- print(tidy_EP_jewish_1934_salient$doc_id)
jewish_chunk_1937 <- print(tidy_EP_jewish_1937_salient$doc_id)
jewish_chunk_1940 <- print(tidy_EP_jewish_1940_salient$doc_id)
jewish_chunk_1942 <- print(tidy_EP_jewish_1942_salient$doc_id)
jewish_chunk_1945 <- print(tidy_EP_jewish_1945_salient$doc_id)
jewish_chunk_1950 <- print(tidy_EP_jewish_1950_salient$doc_id)
jewish_chunk_1956 <- print(tidy_EP_jewish_1956_salient$doc_id)
jewish_chunk_1960 <- print(tidy_EP_jewish_1960_salient$doc_id)
jewish_chunk_1965 <- print(tidy_EP_jewish_1965_salient$doc_id)
jewish_chunk_1969 <- print(tidy_EP_jewish_1969_salient$doc_id)
jewish_chunk_1975 <- print(tidy_EP_jewish_1975_salient$doc_id)
jewish_chunk_1984 <- print(tidy_EP_jewish_1984_salient$doc_id)
jewish_chunk_1992 <- print(tidy_EP_jewish_1992_salient$doc_id)
jewish_chunk_1997 <- print(tidy_EP_jewish_1997_salient$doc_id)
jewish_chunk_2004 <- print(tidy_EP_jewish_2004_salient$doc_id)
jewish_chunk_2011 <- print(tidy_EP_jewish_2011_salient$doc_id)
jewish_chunk_2017 <- print(tidy_EP_jewish_2017_salient$doc_id)



# select if in chunk
tidy_EP_jewish_1922_select <- filter(tidy_EP_1922, chunk %in% jewish_chunk_1922)
tidy_EP_jewish_1927_select <- filter(tidy_EP_1927, chunk %in% jewish_chunk_1927)
tidy_EP_jewish_1931_select <- filter(tidy_EP_1931, chunk %in% jewish_chunk_1931)
tidy_EP_jewish_1934_select <- filter(tidy_EP_1934, chunk %in% jewish_chunk_1934)
tidy_EP_jewish_1937_select <- filter(tidy_EP_1937, chunk %in% jewish_chunk_1937)
tidy_EP_jewish_1940_select <- filter(tidy_EP_1940, chunk %in% jewish_chunk_1940)
tidy_EP_jewish_1942_select <- filter(tidy_EP_1942, chunk %in% jewish_chunk_1942)
tidy_EP_jewish_1945_select <- filter(tidy_EP_1945, chunk %in% jewish_chunk_1945)
tidy_EP_jewish_1950_select <- filter(tidy_EP_1950, chunk %in% jewish_chunk_1950)
tidy_EP_jewish_1956_select <- filter(tidy_EP_1956, chunk %in% jewish_chunk_1956)
tidy_EP_jewish_1960_select <- filter(tidy_EP_1960, chunk %in% jewish_chunk_1960)
tidy_EP_jewish_1965_select <- filter(tidy_EP_1965, chunk %in% jewish_chunk_1965)
tidy_EP_jewish_1969_select <- filter(tidy_EP_1969, chunk %in% jewish_chunk_1969)
tidy_EP_jewish_1975_select <- filter(tidy_EP_1975, chunk %in% jewish_chunk_1975)
tidy_EP_jewish_1984_select <- filter(tidy_EP_1984, chunk %in% jewish_chunk_1984)
tidy_EP_jewish_1992_select <- filter(tidy_EP_1992, chunk %in% jewish_chunk_1992)
tidy_EP_jewish_1997_select <- filter(tidy_EP_1997, chunk %in% jewish_chunk_1997)
tidy_EP_jewish_2004_select <- filter(tidy_EP_2004, chunk %in% jewish_chunk_2004)
tidy_EP_jewish_2011_select <- filter(tidy_EP_2011, chunk %in% jewish_chunk_2011)
tidy_EP_jewish_2017_select <- filter(tidy_EP_2017, chunk %in% jewish_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_jewish <- full_join(tidy_EP_jewish_1922_select, tidy_EP_jewish_1927_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1931_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1934_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1937_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1940_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1942_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1945_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1950_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1956_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1960_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1965_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1969_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1975_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1984_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1992_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_1997_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_2004_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_2011_select)
tidy_EP_chunks_jewish <- full_join(tidy_EP_chunks_jewish, tidy_EP_jewish_2017_select)



#   build cmdist for antonym pair, jewish

jewish_closeness <- tidy_EP_chunks_jewish %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=ngram_1960 )











#   irish


tidy_EP_irish_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)


tidy_EP_irish_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)


tidy_EP_irish_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv = ngram_1960)

tidy_EP_irish_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=ngram_1960)

tidy_EP_irish_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=ngram_1960)

tidy_EP_irish_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=ngram_1960)


# combine, need to add "year"; also add unique

tidy_EP_irish_1922 <- tidy_EP_irish_1922 %>%
  mutate(year = 1922) 

tidy_EP_irish_1927 <- tidy_EP_irish_1927 %>%
  mutate(year = 1927)

tidy_EP_irish_1931 <- tidy_EP_irish_1931 %>%
  mutate(year = 1931)

tidy_EP_irish_1934 <- tidy_EP_irish_1934 %>%
  mutate(year = 1934)

tidy_EP_irish_1937 <- tidy_EP_irish_1937 %>%
  mutate(year = 1937)

tidy_EP_irish_1940 <- tidy_EP_irish_1940 %>%
  mutate(year = 1940)

tidy_EP_irish_1942 <- tidy_EP_irish_1942 %>%
  mutate(year = 1942)

tidy_EP_irish_1945 <- tidy_EP_irish_1945 %>%
  mutate(year = 1945)

tidy_EP_irish_1950 <- tidy_EP_irish_1950 %>%
  mutate(year = 1950)

tidy_EP_irish_1956 <- tidy_EP_irish_1956 %>%
  mutate(year = 1956)

tidy_EP_irish_1960 <- tidy_EP_irish_1960 %>%
  mutate(year = 1960)

tidy_EP_irish_1965 <- tidy_EP_irish_1965 %>%
  mutate(year = 1965)


tidy_EP_irish_1969 <- tidy_EP_irish_1969 %>%
  mutate(year = 1969)

tidy_EP_irish_1975 <- tidy_EP_irish_1975 %>%
  mutate(year = 1975)

tidy_EP_irish_1984 <- tidy_EP_irish_1984 %>%
  mutate(year = 1984)

tidy_EP_irish_1992 <- tidy_EP_irish_1992 %>%
  mutate(year = 1992)

tidy_EP_irish_1997 <- tidy_EP_irish_1997 %>%
  mutate(year = 1997)

tidy_EP_irish_2004 <- tidy_EP_irish_2004 %>%
  mutate(year = 2004)

tidy_EP_irish_2011 <- tidy_EP_irish_2011 %>%
  mutate(year = 2011)

tidy_EP_irish_2017 <- tidy_EP_irish_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_irish_1922_salient <- tidy_EP_irish_1922[tidy_EP_irish_1922$irish > 1.65, ]
tidy_EP_irish_1927_salient <- tidy_EP_irish_1927[tidy_EP_irish_1927$irish > 1.65, ]
tidy_EP_irish_1931_salient <- tidy_EP_irish_1931[tidy_EP_irish_1931$irish > 1.65, ]
tidy_EP_irish_1934_salient <- tidy_EP_irish_1934[tidy_EP_irish_1934$irish > 1.65, ]
tidy_EP_irish_1937_salient <- tidy_EP_irish_1937[tidy_EP_irish_1937$irish > 1.65, ]
tidy_EP_irish_1940_salient <- tidy_EP_irish_1940[tidy_EP_irish_1940$irish > 1.65, ]
tidy_EP_irish_1942_salient <- tidy_EP_irish_1942[tidy_EP_irish_1942$irish > 1.65, ]
tidy_EP_irish_1945_salient <- tidy_EP_irish_1945[tidy_EP_irish_1945$irish > 1.65, ]
tidy_EP_irish_1950_salient <- tidy_EP_irish_1950[tidy_EP_irish_1950$irish > 1.65, ]
tidy_EP_irish_1956_salient <- tidy_EP_irish_1956[tidy_EP_irish_1956$irish > 1.65, ]
tidy_EP_irish_1960_salient <- tidy_EP_irish_1960[tidy_EP_irish_1960$irish > 1.65, ]
tidy_EP_irish_1965_salient <- tidy_EP_irish_1965[tidy_EP_irish_1965$irish > 1.65, ]
tidy_EP_irish_1969_salient <- tidy_EP_irish_1969[tidy_EP_irish_1969$irish > 1.65, ]
tidy_EP_irish_1975_salient <- tidy_EP_irish_1975[tidy_EP_irish_1975$irish > 1.65, ]
tidy_EP_irish_1984_salient <- tidy_EP_irish_1984[tidy_EP_irish_1984$irish > 1.65, ]
tidy_EP_irish_1992_salient <- tidy_EP_irish_1992[tidy_EP_irish_1992$irish > 1.65, ]
tidy_EP_irish_1997_salient <- tidy_EP_irish_1997[tidy_EP_irish_1997$irish > 1.65, ]
tidy_EP_irish_2004_salient <- tidy_EP_irish_2004[tidy_EP_irish_2004$irish > 1.65, ]
tidy_EP_irish_2011_salient <- tidy_EP_irish_2011[tidy_EP_irish_2011$irish > 1.65, ]
tidy_EP_irish_2017_salient <- tidy_EP_irish_2017[tidy_EP_irish_2017$irish > 1.65, ]



# list of salient
irish_chunk_1922 <- print(tidy_EP_irish_1922_salient$doc_id)
irish_chunk_1927 <- print(tidy_EP_irish_1927_salient$doc_id)
irish_chunk_1931 <- print(tidy_EP_irish_1931_salient$doc_id)
irish_chunk_1934 <- print(tidy_EP_irish_1934_salient$doc_id)
irish_chunk_1937 <- print(tidy_EP_irish_1937_salient$doc_id)
irish_chunk_1940 <- print(tidy_EP_irish_1940_salient$doc_id)
irish_chunk_1942 <- print(tidy_EP_irish_1942_salient$doc_id)
irish_chunk_1945 <- print(tidy_EP_irish_1945_salient$doc_id)
irish_chunk_1950 <- print(tidy_EP_irish_1950_salient$doc_id)
irish_chunk_1956 <- print(tidy_EP_irish_1956_salient$doc_id)
irish_chunk_1960 <- print(tidy_EP_irish_1960_salient$doc_id)
irish_chunk_1965 <- print(tidy_EP_irish_1965_salient$doc_id)
irish_chunk_1969 <- print(tidy_EP_irish_1969_salient$doc_id)
irish_chunk_1975 <- print(tidy_EP_irish_1975_salient$doc_id)
irish_chunk_1984 <- print(tidy_EP_irish_1984_salient$doc_id)
irish_chunk_1992 <- print(tidy_EP_irish_1992_salient$doc_id)
irish_chunk_1997 <- print(tidy_EP_irish_1997_salient$doc_id)
irish_chunk_2004 <- print(tidy_EP_irish_2004_salient$doc_id)
irish_chunk_2011 <- print(tidy_EP_irish_2011_salient$doc_id)
irish_chunk_2017 <- print(tidy_EP_irish_2017_salient$doc_id)



# select if in chunk
tidy_EP_irish_1922_select <- filter(tidy_EP_1922, chunk %in% irish_chunk_1922)
tidy_EP_irish_1927_select <- filter(tidy_EP_1927, chunk %in% irish_chunk_1927)
tidy_EP_irish_1931_select <- filter(tidy_EP_1931, chunk %in% irish_chunk_1931)
tidy_EP_irish_1934_select <- filter(tidy_EP_1934, chunk %in% irish_chunk_1934)
tidy_EP_irish_1937_select <- filter(tidy_EP_1937, chunk %in% irish_chunk_1937)
tidy_EP_irish_1940_select <- filter(tidy_EP_1940, chunk %in% irish_chunk_1940)
tidy_EP_irish_1942_select <- filter(tidy_EP_1942, chunk %in% irish_chunk_1942)
tidy_EP_irish_1945_select <- filter(tidy_EP_1945, chunk %in% irish_chunk_1945)
tidy_EP_irish_1950_select <- filter(tidy_EP_1950, chunk %in% irish_chunk_1950)
tidy_EP_irish_1956_select <- filter(tidy_EP_1956, chunk %in% irish_chunk_1956)
tidy_EP_irish_1960_select <- filter(tidy_EP_1960, chunk %in% irish_chunk_1960)
tidy_EP_irish_1965_select <- filter(tidy_EP_1965, chunk %in% irish_chunk_1965)
tidy_EP_irish_1969_select <- filter(tidy_EP_1969, chunk %in% irish_chunk_1969)
tidy_EP_irish_1975_select <- filter(tidy_EP_1975, chunk %in% irish_chunk_1975)
tidy_EP_irish_1984_select <- filter(tidy_EP_1984, chunk %in% irish_chunk_1984)
tidy_EP_irish_1992_select <- filter(tidy_EP_1992, chunk %in% irish_chunk_1992)
tidy_EP_irish_1997_select <- filter(tidy_EP_1997, chunk %in% irish_chunk_1997)
tidy_EP_irish_2004_select <- filter(tidy_EP_2004, chunk %in% irish_chunk_2004)
tidy_EP_irish_2011_select <- filter(tidy_EP_2011, chunk %in% irish_chunk_2011)
tidy_EP_irish_2017_select <- filter(tidy_EP_2017, chunk %in% irish_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_irish <- full_join(tidy_EP_irish_1922_select, tidy_EP_irish_1927_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1931_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1934_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1937_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1940_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1942_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1945_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1950_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1956_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1960_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1965_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1969_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1975_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1984_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1992_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_1997_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_2004_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_2011_select)
tidy_EP_chunks_irish <- full_join(tidy_EP_chunks_irish, tidy_EP_irish_2017_select)



#   build cmdist for antonym pair, irish

irish_closeness <- tidy_EP_chunks_irish %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=ngram_1960 )


















#    catholic


tidy_EP_catholic_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)


tidy_EP_catholic_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)


tidy_EP_catholic_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv = ngram_1960)

tidy_EP_catholic_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=ngram_1960)

tidy_EP_catholic_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=ngram_1960)

tidy_EP_catholic_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=ngram_1960)


# combine, need to add "year"; also add unique

tidy_EP_catholic_1922 <- tidy_EP_catholic_1922 %>%
  mutate(year = 1922) 

tidy_EP_catholic_1927 <- tidy_EP_catholic_1927 %>%
  mutate(year = 1927)

tidy_EP_catholic_1931 <- tidy_EP_catholic_1931 %>%
  mutate(year = 1931)

tidy_EP_catholic_1934 <- tidy_EP_catholic_1934 %>%
  mutate(year = 1934)

tidy_EP_catholic_1937 <- tidy_EP_catholic_1937 %>%
  mutate(year = 1937)

tidy_EP_catholic_1940 <- tidy_EP_catholic_1940 %>%
  mutate(year = 1940)

tidy_EP_catholic_1942 <- tidy_EP_catholic_1942 %>%
  mutate(year = 1942)

tidy_EP_catholic_1945 <- tidy_EP_catholic_1945 %>%
  mutate(year = 1945)

tidy_EP_catholic_1950 <- tidy_EP_catholic_1950 %>%
  mutate(year = 1950)

tidy_EP_catholic_1956 <- tidy_EP_catholic_1956 %>%
  mutate(year = 1956)

tidy_EP_catholic_1960 <- tidy_EP_catholic_1960 %>%
  mutate(year = 1960)

tidy_EP_catholic_1965 <- tidy_EP_catholic_1965 %>%
  mutate(year = 1965)


tidy_EP_catholic_1969 <- tidy_EP_catholic_1969 %>%
  mutate(year = 1969)

tidy_EP_catholic_1975 <- tidy_EP_catholic_1975 %>%
  mutate(year = 1975)

tidy_EP_catholic_1984 <- tidy_EP_catholic_1984 %>%
  mutate(year = 1984)

tidy_EP_catholic_1992 <- tidy_EP_catholic_1992 %>%
  mutate(year = 1992)

tidy_EP_catholic_1997 <- tidy_EP_catholic_1997 %>%
  mutate(year = 1997)

tidy_EP_catholic_2004 <- tidy_EP_catholic_2004 %>%
  mutate(year = 2004)

tidy_EP_catholic_2011 <- tidy_EP_catholic_2011 %>%
  mutate(year = 2011)

tidy_EP_catholic_2017 <- tidy_EP_catholic_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_catholic_1922_salient <- tidy_EP_catholic_1922[tidy_EP_catholic_1922$catholic > 1.65, ]
tidy_EP_catholic_1927_salient <- tidy_EP_catholic_1927[tidy_EP_catholic_1927$catholic > 1.65, ]
tidy_EP_catholic_1931_salient <- tidy_EP_catholic_1931[tidy_EP_catholic_1931$catholic > 1.65, ]
tidy_EP_catholic_1934_salient <- tidy_EP_catholic_1934[tidy_EP_catholic_1934$catholic > 1.65, ]
tidy_EP_catholic_1937_salient <- tidy_EP_catholic_1937[tidy_EP_catholic_1937$catholic > 1.65, ]
tidy_EP_catholic_1940_salient <- tidy_EP_catholic_1940[tidy_EP_catholic_1940$catholic > 1.65, ]
tidy_EP_catholic_1942_salient <- tidy_EP_catholic_1942[tidy_EP_catholic_1942$catholic > 1.65, ]
tidy_EP_catholic_1945_salient <- tidy_EP_catholic_1945[tidy_EP_catholic_1945$catholic > 1.65, ]
tidy_EP_catholic_1950_salient <- tidy_EP_catholic_1950[tidy_EP_catholic_1950$catholic > 1.65, ]
tidy_EP_catholic_1956_salient <- tidy_EP_catholic_1956[tidy_EP_catholic_1956$catholic > 1.65, ]
tidy_EP_catholic_1960_salient <- tidy_EP_catholic_1960[tidy_EP_catholic_1960$catholic > 1.65, ]
tidy_EP_catholic_1965_salient <- tidy_EP_catholic_1965[tidy_EP_catholic_1965$catholic > 1.65, ]
tidy_EP_catholic_1969_salient <- tidy_EP_catholic_1969[tidy_EP_catholic_1969$catholic > 1.65, ]
tidy_EP_catholic_1975_salient <- tidy_EP_catholic_1975[tidy_EP_catholic_1975$catholic > 1.65, ]
tidy_EP_catholic_1984_salient <- tidy_EP_catholic_1984[tidy_EP_catholic_1984$catholic > 1.65, ]
tidy_EP_catholic_1992_salient <- tidy_EP_catholic_1992[tidy_EP_catholic_1992$catholic > 1.65, ]
tidy_EP_catholic_1997_salient <- tidy_EP_catholic_1997[tidy_EP_catholic_1997$catholic > 1.65, ]
tidy_EP_catholic_2004_salient <- tidy_EP_catholic_2004[tidy_EP_catholic_2004$catholic > 1.65, ]
tidy_EP_catholic_2011_salient <- tidy_EP_catholic_2011[tidy_EP_catholic_2011$catholic > 1.65, ]
tidy_EP_catholic_2017_salient <- tidy_EP_catholic_2017[tidy_EP_catholic_2017$catholic > 1.65, ]



# list of salient
catholic_chunk_1922 <- print(tidy_EP_catholic_1922_salient$doc_id)
catholic_chunk_1927 <- print(tidy_EP_catholic_1927_salient$doc_id)
catholic_chunk_1931 <- print(tidy_EP_catholic_1931_salient$doc_id)
catholic_chunk_1934 <- print(tidy_EP_catholic_1934_salient$doc_id)
catholic_chunk_1937 <- print(tidy_EP_catholic_1937_salient$doc_id)
catholic_chunk_1940 <- print(tidy_EP_catholic_1940_salient$doc_id)
catholic_chunk_1942 <- print(tidy_EP_catholic_1942_salient$doc_id)
catholic_chunk_1945 <- print(tidy_EP_catholic_1945_salient$doc_id)
catholic_chunk_1950 <- print(tidy_EP_catholic_1950_salient$doc_id)
catholic_chunk_1956 <- print(tidy_EP_catholic_1956_salient$doc_id)
catholic_chunk_1960 <- print(tidy_EP_catholic_1960_salient$doc_id)
catholic_chunk_1965 <- print(tidy_EP_catholic_1965_salient$doc_id)
catholic_chunk_1969 <- print(tidy_EP_catholic_1969_salient$doc_id)
catholic_chunk_1975 <- print(tidy_EP_catholic_1975_salient$doc_id)
catholic_chunk_1984 <- print(tidy_EP_catholic_1984_salient$doc_id)
catholic_chunk_1992 <- print(tidy_EP_catholic_1992_salient$doc_id)
catholic_chunk_1997 <- print(tidy_EP_catholic_1997_salient$doc_id)
catholic_chunk_2004 <- print(tidy_EP_catholic_2004_salient$doc_id)
catholic_chunk_2011 <- print(tidy_EP_catholic_2011_salient$doc_id)
catholic_chunk_2017 <- print(tidy_EP_catholic_2017_salient$doc_id)



# select if in chunk
tidy_EP_catholic_1922_select <- filter(tidy_EP_1922, chunk %in% catholic_chunk_1922)
tidy_EP_catholic_1927_select <- filter(tidy_EP_1927, chunk %in% catholic_chunk_1927)
tidy_EP_catholic_1931_select <- filter(tidy_EP_1931, chunk %in% catholic_chunk_1931)
tidy_EP_catholic_1934_select <- filter(tidy_EP_1934, chunk %in% catholic_chunk_1934)
tidy_EP_catholic_1937_select <- filter(tidy_EP_1937, chunk %in% catholic_chunk_1937)
tidy_EP_catholic_1940_select <- filter(tidy_EP_1940, chunk %in% catholic_chunk_1940)
tidy_EP_catholic_1942_select <- filter(tidy_EP_1942, chunk %in% catholic_chunk_1942)
tidy_EP_catholic_1945_select <- filter(tidy_EP_1945, chunk %in% catholic_chunk_1945)
tidy_EP_catholic_1950_select <- filter(tidy_EP_1950, chunk %in% catholic_chunk_1950)
tidy_EP_catholic_1956_select <- filter(tidy_EP_1956, chunk %in% catholic_chunk_1956)
tidy_EP_catholic_1960_select <- filter(tidy_EP_1960, chunk %in% catholic_chunk_1960)
tidy_EP_catholic_1965_select <- filter(tidy_EP_1965, chunk %in% catholic_chunk_1965)
tidy_EP_catholic_1969_select <- filter(tidy_EP_1969, chunk %in% catholic_chunk_1969)
tidy_EP_catholic_1975_select <- filter(tidy_EP_1975, chunk %in% catholic_chunk_1975)
tidy_EP_catholic_1984_select <- filter(tidy_EP_1984, chunk %in% catholic_chunk_1984)
tidy_EP_catholic_1992_select <- filter(tidy_EP_1992, chunk %in% catholic_chunk_1992)
tidy_EP_catholic_1997_select <- filter(tidy_EP_1997, chunk %in% catholic_chunk_1997)
tidy_EP_catholic_2004_select <- filter(tidy_EP_2004, chunk %in% catholic_chunk_2004)
tidy_EP_catholic_2011_select <- filter(tidy_EP_2011, chunk %in% catholic_chunk_2011)
tidy_EP_catholic_2017_select <- filter(tidy_EP_2017, chunk %in% catholic_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_catholic <- full_join(tidy_EP_catholic_1922_select, tidy_EP_catholic_1927_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1931_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1934_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1937_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1940_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1942_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1945_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1950_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1956_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1960_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1965_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1969_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1975_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1984_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1992_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_1997_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_2004_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_2011_select)
tidy_EP_chunks_catholic <- full_join(tidy_EP_chunks_catholic, tidy_EP_catholic_2017_select)



#   build cmdist for antonym pair, catholic

catholic_closeness <- tidy_EP_chunks_catholic %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=ngram_1960 )













#   mexican


tidy_EP_mexican_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)


tidy_EP_mexican_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)


tidy_EP_mexican_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv = ngram_1960)

tidy_EP_mexican_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=ngram_1960)

tidy_EP_mexican_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=ngram_1960)

tidy_EP_mexican_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=ngram_1960)


# combine, need to add "year"; also add unique

tidy_EP_mexican_1922 <- tidy_EP_mexican_1922 %>%
  mutate(year = 1922) 

tidy_EP_mexican_1927 <- tidy_EP_mexican_1927 %>%
  mutate(year = 1927)

tidy_EP_mexican_1931 <- tidy_EP_mexican_1931 %>%
  mutate(year = 1931)

tidy_EP_mexican_1934 <- tidy_EP_mexican_1934 %>%
  mutate(year = 1934)

tidy_EP_mexican_1937 <- tidy_EP_mexican_1937 %>%
  mutate(year = 1937)

tidy_EP_mexican_1940 <- tidy_EP_mexican_1940 %>%
  mutate(year = 1940)

tidy_EP_mexican_1942 <- tidy_EP_mexican_1942 %>%
  mutate(year = 1942)

tidy_EP_mexican_1945 <- tidy_EP_mexican_1945 %>%
  mutate(year = 1945)

tidy_EP_mexican_1950 <- tidy_EP_mexican_1950 %>%
  mutate(year = 1950)

tidy_EP_mexican_1956 <- tidy_EP_mexican_1956 %>%
  mutate(year = 1956)

tidy_EP_mexican_1960 <- tidy_EP_mexican_1960 %>%
  mutate(year = 1960)

tidy_EP_mexican_1965 <- tidy_EP_mexican_1965 %>%
  mutate(year = 1965)


tidy_EP_mexican_1969 <- tidy_EP_mexican_1969 %>%
  mutate(year = 1969)

tidy_EP_mexican_1975 <- tidy_EP_mexican_1975 %>%
  mutate(year = 1975)

tidy_EP_mexican_1984 <- tidy_EP_mexican_1984 %>%
  mutate(year = 1984)

tidy_EP_mexican_1992 <- tidy_EP_mexican_1992 %>%
  mutate(year = 1992)

tidy_EP_mexican_1997 <- tidy_EP_mexican_1997 %>%
  mutate(year = 1997)

tidy_EP_mexican_2004 <- tidy_EP_mexican_2004 %>%
  mutate(year = 2004)

tidy_EP_mexican_2011 <- tidy_EP_mexican_2011 %>%
  mutate(year = 2011)

tidy_EP_mexican_2017 <- tidy_EP_mexican_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_mexican_1922_salient <- tidy_EP_mexican_1922[tidy_EP_mexican_1922$mexican > 1.65, ]
tidy_EP_mexican_1927_salient <- tidy_EP_mexican_1927[tidy_EP_mexican_1927$mexican > 1.65, ]
tidy_EP_mexican_1931_salient <- tidy_EP_mexican_1931[tidy_EP_mexican_1931$mexican > 1.65, ]
tidy_EP_mexican_1934_salient <- tidy_EP_mexican_1934[tidy_EP_mexican_1934$mexican > 1.65, ]
tidy_EP_mexican_1937_salient <- tidy_EP_mexican_1937[tidy_EP_mexican_1937$mexican > 1.65, ]
tidy_EP_mexican_1940_salient <- tidy_EP_mexican_1940[tidy_EP_mexican_1940$mexican > 1.65, ]
tidy_EP_mexican_1942_salient <- tidy_EP_mexican_1942[tidy_EP_mexican_1942$mexican > 1.65, ]
tidy_EP_mexican_1945_salient <- tidy_EP_mexican_1945[tidy_EP_mexican_1945$mexican > 1.65, ]
tidy_EP_mexican_1950_salient <- tidy_EP_mexican_1950[tidy_EP_mexican_1950$mexican > 1.65, ]
tidy_EP_mexican_1956_salient <- tidy_EP_mexican_1956[tidy_EP_mexican_1956$mexican > 1.65, ]
tidy_EP_mexican_1960_salient <- tidy_EP_mexican_1960[tidy_EP_mexican_1960$mexican > 1.65, ]
tidy_EP_mexican_1965_salient <- tidy_EP_mexican_1965[tidy_EP_mexican_1965$mexican > 1.65, ]
tidy_EP_mexican_1969_salient <- tidy_EP_mexican_1969[tidy_EP_mexican_1969$mexican > 1.65, ]
tidy_EP_mexican_1975_salient <- tidy_EP_mexican_1975[tidy_EP_mexican_1975$mexican > 1.65, ]
tidy_EP_mexican_1984_salient <- tidy_EP_mexican_1984[tidy_EP_mexican_1984$mexican > 1.65, ]
tidy_EP_mexican_1992_salient <- tidy_EP_mexican_1992[tidy_EP_mexican_1992$mexican > 1.65, ]
tidy_EP_mexican_1997_salient <- tidy_EP_mexican_1997[tidy_EP_mexican_1997$mexican > 1.65, ]
tidy_EP_mexican_2004_salient <- tidy_EP_mexican_2004[tidy_EP_mexican_2004$mexican > 1.65, ]
tidy_EP_mexican_2011_salient <- tidy_EP_mexican_2011[tidy_EP_mexican_2011$mexican > 1.65, ]
tidy_EP_mexican_2017_salient <- tidy_EP_mexican_2017[tidy_EP_mexican_2017$mexican > 1.65, ]



# list of salient
mexican_chunk_1922 <- print(tidy_EP_mexican_1922_salient$doc_id)
mexican_chunk_1927 <- print(tidy_EP_mexican_1927_salient$doc_id)
mexican_chunk_1931 <- print(tidy_EP_mexican_1931_salient$doc_id)
mexican_chunk_1934 <- print(tidy_EP_mexican_1934_salient$doc_id)
mexican_chunk_1937 <- print(tidy_EP_mexican_1937_salient$doc_id)
mexican_chunk_1940 <- print(tidy_EP_mexican_1940_salient$doc_id)
mexican_chunk_1942 <- print(tidy_EP_mexican_1942_salient$doc_id)
mexican_chunk_1945 <- print(tidy_EP_mexican_1945_salient$doc_id)
mexican_chunk_1950 <- print(tidy_EP_mexican_1950_salient$doc_id)
mexican_chunk_1956 <- print(tidy_EP_mexican_1956_salient$doc_id)
mexican_chunk_1960 <- print(tidy_EP_mexican_1960_salient$doc_id)
mexican_chunk_1965 <- print(tidy_EP_mexican_1965_salient$doc_id)
mexican_chunk_1969 <- print(tidy_EP_mexican_1969_salient$doc_id)
mexican_chunk_1975 <- print(tidy_EP_mexican_1975_salient$doc_id)
mexican_chunk_1984 <- print(tidy_EP_mexican_1984_salient$doc_id)
mexican_chunk_1992 <- print(tidy_EP_mexican_1992_salient$doc_id)
mexican_chunk_1997 <- print(tidy_EP_mexican_1997_salient$doc_id)
mexican_chunk_2004 <- print(tidy_EP_mexican_2004_salient$doc_id)
mexican_chunk_2011 <- print(tidy_EP_mexican_2011_salient$doc_id)
mexican_chunk_2017 <- print(tidy_EP_mexican_2017_salient$doc_id)



# select if in chunk
tidy_EP_mexican_1922_select <- filter(tidy_EP_1922, chunk %in% mexican_chunk_1922)
tidy_EP_mexican_1927_select <- filter(tidy_EP_1927, chunk %in% mexican_chunk_1927)
tidy_EP_mexican_1931_select <- filter(tidy_EP_1931, chunk %in% mexican_chunk_1931)
tidy_EP_mexican_1934_select <- filter(tidy_EP_1934, chunk %in% mexican_chunk_1934)
tidy_EP_mexican_1937_select <- filter(tidy_EP_1937, chunk %in% mexican_chunk_1937)
tidy_EP_mexican_1940_select <- filter(tidy_EP_1940, chunk %in% mexican_chunk_1940)
tidy_EP_mexican_1942_select <- filter(tidy_EP_1942, chunk %in% mexican_chunk_1942)
tidy_EP_mexican_1945_select <- filter(tidy_EP_1945, chunk %in% mexican_chunk_1945)
tidy_EP_mexican_1950_select <- filter(tidy_EP_1950, chunk %in% mexican_chunk_1950)
tidy_EP_mexican_1956_select <- filter(tidy_EP_1956, chunk %in% mexican_chunk_1956)
tidy_EP_mexican_1960_select <- filter(tidy_EP_1960, chunk %in% mexican_chunk_1960)
tidy_EP_mexican_1965_select <- filter(tidy_EP_1965, chunk %in% mexican_chunk_1965)
tidy_EP_mexican_1969_select <- filter(tidy_EP_1969, chunk %in% mexican_chunk_1969)
tidy_EP_mexican_1975_select <- filter(tidy_EP_1975, chunk %in% mexican_chunk_1975)
tidy_EP_mexican_1984_select <- filter(tidy_EP_1984, chunk %in% mexican_chunk_1984)
tidy_EP_mexican_1992_select <- filter(tidy_EP_1992, chunk %in% mexican_chunk_1992)
tidy_EP_mexican_1997_select <- filter(tidy_EP_1997, chunk %in% mexican_chunk_1997)
tidy_EP_mexican_2004_select <- filter(tidy_EP_2004, chunk %in% mexican_chunk_2004)
tidy_EP_mexican_2011_select <- filter(tidy_EP_2011, chunk %in% mexican_chunk_2011)
tidy_EP_mexican_2017_select <- filter(tidy_EP_2017, chunk %in% mexican_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_mexican <- full_join(tidy_EP_mexican_1922_select, tidy_EP_mexican_1927_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1931_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1934_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1937_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1940_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1942_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1945_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1950_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1956_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1960_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1965_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1969_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1975_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1984_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1992_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_1997_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_2004_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_2011_select)
tidy_EP_chunks_mexican <- full_join(tidy_EP_chunks_mexican, tidy_EP_mexican_2017_select)



#   build cmdist for antonym pair, mexican

mexican_closeness <- tidy_EP_chunks_mexican %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=ngram_1960 )










#    chinese


tidy_EP_chinese_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)


tidy_EP_chinese_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)


tidy_EP_chinese_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv = ngram_1960)

tidy_EP_chinese_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=ngram_1960)

tidy_EP_chinese_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=ngram_1960)

tidy_EP_chinese_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=ngram_1960)


# combine, need to add "year"; also add unique

tidy_EP_chinese_1922 <- tidy_EP_chinese_1922 %>%
  mutate(year = 1922) 

tidy_EP_chinese_1927 <- tidy_EP_chinese_1927 %>%
  mutate(year = 1927)

tidy_EP_chinese_1931 <- tidy_EP_chinese_1931 %>%
  mutate(year = 1931)

tidy_EP_chinese_1934 <- tidy_EP_chinese_1934 %>%
  mutate(year = 1934)

tidy_EP_chinese_1937 <- tidy_EP_chinese_1937 %>%
  mutate(year = 1937)

tidy_EP_chinese_1940 <- tidy_EP_chinese_1940 %>%
  mutate(year = 1940)

tidy_EP_chinese_1942 <- tidy_EP_chinese_1942 %>%
  mutate(year = 1942)

tidy_EP_chinese_1945 <- tidy_EP_chinese_1945 %>%
  mutate(year = 1945)

tidy_EP_chinese_1950 <- tidy_EP_chinese_1950 %>%
  mutate(year = 1950)

tidy_EP_chinese_1956 <- tidy_EP_chinese_1956 %>%
  mutate(year = 1956)

tidy_EP_chinese_1960 <- tidy_EP_chinese_1960 %>%
  mutate(year = 1960)

tidy_EP_chinese_1965 <- tidy_EP_chinese_1965 %>%
  mutate(year = 1965)


tidy_EP_chinese_1969 <- tidy_EP_chinese_1969 %>%
  mutate(year = 1969)

tidy_EP_chinese_1975 <- tidy_EP_chinese_1975 %>%
  mutate(year = 1975)

tidy_EP_chinese_1984 <- tidy_EP_chinese_1984 %>%
  mutate(year = 1984)

tidy_EP_chinese_1992 <- tidy_EP_chinese_1992 %>%
  mutate(year = 1992)

tidy_EP_chinese_1997 <- tidy_EP_chinese_1997 %>%
  mutate(year = 1997)

tidy_EP_chinese_2004 <- tidy_EP_chinese_2004 %>%
  mutate(year = 2004)

tidy_EP_chinese_2011 <- tidy_EP_chinese_2011 %>%
  mutate(year = 2011)

tidy_EP_chinese_2017 <- tidy_EP_chinese_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_chinese_1922_salient <- tidy_EP_chinese_1922[tidy_EP_chinese_1922$chinese > 1.65, ]
tidy_EP_chinese_1927_salient <- tidy_EP_chinese_1927[tidy_EP_chinese_1927$chinese > 1.65, ]
tidy_EP_chinese_1931_salient <- tidy_EP_chinese_1931[tidy_EP_chinese_1931$chinese > 1.65, ]
tidy_EP_chinese_1934_salient <- tidy_EP_chinese_1934[tidy_EP_chinese_1934$chinese > 1.65, ]
tidy_EP_chinese_1937_salient <- tidy_EP_chinese_1937[tidy_EP_chinese_1937$chinese > 1.65, ]
tidy_EP_chinese_1940_salient <- tidy_EP_chinese_1940[tidy_EP_chinese_1940$chinese > 1.65, ]
tidy_EP_chinese_1942_salient <- tidy_EP_chinese_1942[tidy_EP_chinese_1942$chinese > 1.65, ]
tidy_EP_chinese_1945_salient <- tidy_EP_chinese_1945[tidy_EP_chinese_1945$chinese > 1.65, ]
tidy_EP_chinese_1950_salient <- tidy_EP_chinese_1950[tidy_EP_chinese_1950$chinese > 1.65, ]
tidy_EP_chinese_1956_salient <- tidy_EP_chinese_1956[tidy_EP_chinese_1956$chinese > 1.65, ]
tidy_EP_chinese_1960_salient <- tidy_EP_chinese_1960[tidy_EP_chinese_1960$chinese > 1.65, ]
tidy_EP_chinese_1965_salient <- tidy_EP_chinese_1965[tidy_EP_chinese_1965$chinese > 1.65, ]
tidy_EP_chinese_1969_salient <- tidy_EP_chinese_1969[tidy_EP_chinese_1969$chinese > 1.65, ]
tidy_EP_chinese_1975_salient <- tidy_EP_chinese_1975[tidy_EP_chinese_1975$chinese > 1.65, ]
tidy_EP_chinese_1984_salient <- tidy_EP_chinese_1984[tidy_EP_chinese_1984$chinese > 1.65, ]
tidy_EP_chinese_1992_salient <- tidy_EP_chinese_1992[tidy_EP_chinese_1992$chinese > 1.65, ]
tidy_EP_chinese_1997_salient <- tidy_EP_chinese_1997[tidy_EP_chinese_1997$chinese > 1.65, ]
tidy_EP_chinese_2004_salient <- tidy_EP_chinese_2004[tidy_EP_chinese_2004$chinese > 1.65, ]
tidy_EP_chinese_2011_salient <- tidy_EP_chinese_2011[tidy_EP_chinese_2011$chinese > 1.65, ]
tidy_EP_chinese_2017_salient <- tidy_EP_chinese_2017[tidy_EP_chinese_2017$chinese > 1.65, ]



# list of salient
chinese_chunk_1922 <- print(tidy_EP_chinese_1922_salient$doc_id)
chinese_chunk_1927 <- print(tidy_EP_chinese_1927_salient$doc_id)
chinese_chunk_1931 <- print(tidy_EP_chinese_1931_salient$doc_id)
chinese_chunk_1934 <- print(tidy_EP_chinese_1934_salient$doc_id)
chinese_chunk_1937 <- print(tidy_EP_chinese_1937_salient$doc_id)
chinese_chunk_1940 <- print(tidy_EP_chinese_1940_salient$doc_id)
chinese_chunk_1942 <- print(tidy_EP_chinese_1942_salient$doc_id)
chinese_chunk_1945 <- print(tidy_EP_chinese_1945_salient$doc_id)
chinese_chunk_1950 <- print(tidy_EP_chinese_1950_salient$doc_id)
chinese_chunk_1956 <- print(tidy_EP_chinese_1956_salient$doc_id)
chinese_chunk_1960 <- print(tidy_EP_chinese_1960_salient$doc_id)
chinese_chunk_1965 <- print(tidy_EP_chinese_1965_salient$doc_id)
chinese_chunk_1969 <- print(tidy_EP_chinese_1969_salient$doc_id)
chinese_chunk_1975 <- print(tidy_EP_chinese_1975_salient$doc_id)
chinese_chunk_1984 <- print(tidy_EP_chinese_1984_salient$doc_id)
chinese_chunk_1992 <- print(tidy_EP_chinese_1992_salient$doc_id)
chinese_chunk_1997 <- print(tidy_EP_chinese_1997_salient$doc_id)
chinese_chunk_2004 <- print(tidy_EP_chinese_2004_salient$doc_id)
chinese_chunk_2011 <- print(tidy_EP_chinese_2011_salient$doc_id)
chinese_chunk_2017 <- print(tidy_EP_chinese_2017_salient$doc_id)



# select if in chunk
tidy_EP_chinese_1922_select <- filter(tidy_EP_1922, chunk %in% chinese_chunk_1922)
tidy_EP_chinese_1927_select <- filter(tidy_EP_1927, chunk %in% chinese_chunk_1927)
tidy_EP_chinese_1931_select <- filter(tidy_EP_1931, chunk %in% chinese_chunk_1931)
tidy_EP_chinese_1934_select <- filter(tidy_EP_1934, chunk %in% chinese_chunk_1934)
tidy_EP_chinese_1937_select <- filter(tidy_EP_1937, chunk %in% chinese_chunk_1937)
tidy_EP_chinese_1940_select <- filter(tidy_EP_1940, chunk %in% chinese_chunk_1940)
tidy_EP_chinese_1942_select <- filter(tidy_EP_1942, chunk %in% chinese_chunk_1942)
tidy_EP_chinese_1945_select <- filter(tidy_EP_1945, chunk %in% chinese_chunk_1945)
tidy_EP_chinese_1950_select <- filter(tidy_EP_1950, chunk %in% chinese_chunk_1950)
tidy_EP_chinese_1956_select <- filter(tidy_EP_1956, chunk %in% chinese_chunk_1956)
tidy_EP_chinese_1960_select <- filter(tidy_EP_1960, chunk %in% chinese_chunk_1960)
tidy_EP_chinese_1965_select <- filter(tidy_EP_1965, chunk %in% chinese_chunk_1965)
tidy_EP_chinese_1969_select <- filter(tidy_EP_1969, chunk %in% chinese_chunk_1969)
tidy_EP_chinese_1975_select <- filter(tidy_EP_1975, chunk %in% chinese_chunk_1975)
tidy_EP_chinese_1984_select <- filter(tidy_EP_1984, chunk %in% chinese_chunk_1984)
tidy_EP_chinese_1992_select <- filter(tidy_EP_1992, chunk %in% chinese_chunk_1992)
tidy_EP_chinese_1997_select <- filter(tidy_EP_1997, chunk %in% chinese_chunk_1997)
tidy_EP_chinese_2004_select <- filter(tidy_EP_2004, chunk %in% chinese_chunk_2004)
tidy_EP_chinese_2011_select <- filter(tidy_EP_2011, chunk %in% chinese_chunk_2011)
tidy_EP_chinese_2017_select <- filter(tidy_EP_2017, chunk %in% chinese_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_chinese <- full_join(tidy_EP_chinese_1922_select, tidy_EP_chinese_1927_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1931_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1934_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1937_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1940_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1942_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1945_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1950_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1956_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1960_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1965_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1969_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1975_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1984_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1992_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_1997_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_2004_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_2011_select)
tidy_EP_chunks_chinese <- full_join(tidy_EP_chunks_chinese, tidy_EP_chinese_2017_select)



#   build cmdist for antonym pair, chinese

chinese_closeness <- tidy_EP_chunks_chinese %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=ngram_1960 )




















#  muslim



tidy_EP_muslim_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)


tidy_EP_muslim_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)


tidy_EP_muslim_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv = ngram_1960)

tidy_EP_muslim_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=ngram_1960)

tidy_EP_muslim_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=ngram_1960)

tidy_EP_muslim_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=ngram_1960)


# combine, need to add "year"; also add unique

tidy_EP_muslim_1922 <- tidy_EP_muslim_1922 %>%
  mutate(year = 1922) 

tidy_EP_muslim_1927 <- tidy_EP_muslim_1927 %>%
  mutate(year = 1927)

tidy_EP_muslim_1931 <- tidy_EP_muslim_1931 %>%
  mutate(year = 1931)

tidy_EP_muslim_1934 <- tidy_EP_muslim_1934 %>%
  mutate(year = 1934)

tidy_EP_muslim_1937 <- tidy_EP_muslim_1937 %>%
  mutate(year = 1937)

tidy_EP_muslim_1940 <- tidy_EP_muslim_1940 %>%
  mutate(year = 1940)

tidy_EP_muslim_1942 <- tidy_EP_muslim_1942 %>%
  mutate(year = 1942)

tidy_EP_muslim_1945 <- tidy_EP_muslim_1945 %>%
  mutate(year = 1945)

tidy_EP_muslim_1950 <- tidy_EP_muslim_1950 %>%
  mutate(year = 1950)

tidy_EP_muslim_1956 <- tidy_EP_muslim_1956 %>%
  mutate(year = 1956)

tidy_EP_muslim_1960 <- tidy_EP_muslim_1960 %>%
  mutate(year = 1960)

tidy_EP_muslim_1965 <- tidy_EP_muslim_1965 %>%
  mutate(year = 1965)


tidy_EP_muslim_1969 <- tidy_EP_muslim_1969 %>%
  mutate(year = 1969)

tidy_EP_muslim_1975 <- tidy_EP_muslim_1975 %>%
  mutate(year = 1975)

tidy_EP_muslim_1984 <- tidy_EP_muslim_1984 %>%
  mutate(year = 1984)

tidy_EP_muslim_1992 <- tidy_EP_muslim_1992 %>%
  mutate(year = 1992)

tidy_EP_muslim_1997 <- tidy_EP_muslim_1997 %>%
  mutate(year = 1997)

tidy_EP_muslim_2004 <- tidy_EP_muslim_2004 %>%
  mutate(year = 2004)

tidy_EP_muslim_2011 <- tidy_EP_muslim_2011 %>%
  mutate(year = 2011)

tidy_EP_muslim_2017 <- tidy_EP_muslim_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_muslim_1922_salient <- tidy_EP_muslim_1922[tidy_EP_muslim_1922$muslim > 1.65, ]
tidy_EP_muslim_1927_salient <- tidy_EP_muslim_1927[tidy_EP_muslim_1927$muslim > 1.65, ]
tidy_EP_muslim_1931_salient <- tidy_EP_muslim_1931[tidy_EP_muslim_1931$muslim > 1.65, ]
tidy_EP_muslim_1934_salient <- tidy_EP_muslim_1934[tidy_EP_muslim_1934$muslim > 1.65, ]
tidy_EP_muslim_1937_salient <- tidy_EP_muslim_1937[tidy_EP_muslim_1937$muslim > 1.65, ]
tidy_EP_muslim_1940_salient <- tidy_EP_muslim_1940[tidy_EP_muslim_1940$muslim > 1.65, ]
tidy_EP_muslim_1942_salient <- tidy_EP_muslim_1942[tidy_EP_muslim_1942$muslim > 1.65, ]
tidy_EP_muslim_1945_salient <- tidy_EP_muslim_1945[tidy_EP_muslim_1945$muslim > 1.65, ]
tidy_EP_muslim_1950_salient <- tidy_EP_muslim_1950[tidy_EP_muslim_1950$muslim > 1.65, ]
tidy_EP_muslim_1956_salient <- tidy_EP_muslim_1956[tidy_EP_muslim_1956$muslim > 1.65, ]
tidy_EP_muslim_1960_salient <- tidy_EP_muslim_1960[tidy_EP_muslim_1960$muslim > 1.65, ]
tidy_EP_muslim_1965_salient <- tidy_EP_muslim_1965[tidy_EP_muslim_1965$muslim > 1.65, ]
tidy_EP_muslim_1969_salient <- tidy_EP_muslim_1969[tidy_EP_muslim_1969$muslim > 1.65, ]
tidy_EP_muslim_1975_salient <- tidy_EP_muslim_1975[tidy_EP_muslim_1975$muslim > 1.65, ]
tidy_EP_muslim_1984_salient <- tidy_EP_muslim_1984[tidy_EP_muslim_1984$muslim > 1.65, ]
tidy_EP_muslim_1992_salient <- tidy_EP_muslim_1992[tidy_EP_muslim_1992$muslim > 1.65, ]
tidy_EP_muslim_1997_salient <- tidy_EP_muslim_1997[tidy_EP_muslim_1997$muslim > 1.65, ]
tidy_EP_muslim_2004_salient <- tidy_EP_muslim_2004[tidy_EP_muslim_2004$muslim > 1.65, ]
tidy_EP_muslim_2011_salient <- tidy_EP_muslim_2011[tidy_EP_muslim_2011$muslim > 1.65, ]
tidy_EP_muslim_2017_salient <- tidy_EP_muslim_2017[tidy_EP_muslim_2017$muslim > 1.65, ]



# list of salient
muslim_chunk_1922 <- print(tidy_EP_muslim_1922_salient$doc_id)
muslim_chunk_1927 <- print(tidy_EP_muslim_1927_salient$doc_id)
muslim_chunk_1931 <- print(tidy_EP_muslim_1931_salient$doc_id)
muslim_chunk_1934 <- print(tidy_EP_muslim_1934_salient$doc_id)
muslim_chunk_1937 <- print(tidy_EP_muslim_1937_salient$doc_id)
muslim_chunk_1940 <- print(tidy_EP_muslim_1940_salient$doc_id)
muslim_chunk_1942 <- print(tidy_EP_muslim_1942_salient$doc_id)
muslim_chunk_1945 <- print(tidy_EP_muslim_1945_salient$doc_id)
muslim_chunk_1950 <- print(tidy_EP_muslim_1950_salient$doc_id)
muslim_chunk_1956 <- print(tidy_EP_muslim_1956_salient$doc_id)
muslim_chunk_1960 <- print(tidy_EP_muslim_1960_salient$doc_id)
muslim_chunk_1965 <- print(tidy_EP_muslim_1965_salient$doc_id)
muslim_chunk_1969 <- print(tidy_EP_muslim_1969_salient$doc_id)
muslim_chunk_1975 <- print(tidy_EP_muslim_1975_salient$doc_id)
muslim_chunk_1984 <- print(tidy_EP_muslim_1984_salient$doc_id)
muslim_chunk_1992 <- print(tidy_EP_muslim_1992_salient$doc_id)
muslim_chunk_1997 <- print(tidy_EP_muslim_1997_salient$doc_id)
muslim_chunk_2004 <- print(tidy_EP_muslim_2004_salient$doc_id)
muslim_chunk_2011 <- print(tidy_EP_muslim_2011_salient$doc_id)
muslim_chunk_2017 <- print(tidy_EP_muslim_2017_salient$doc_id)



# select if in chunk
tidy_EP_muslim_1922_select <- filter(tidy_EP_1922, chunk %in% muslim_chunk_1922)
tidy_EP_muslim_1927_select <- filter(tidy_EP_1927, chunk %in% muslim_chunk_1927)
tidy_EP_muslim_1931_select <- filter(tidy_EP_1931, chunk %in% muslim_chunk_1931)
tidy_EP_muslim_1934_select <- filter(tidy_EP_1934, chunk %in% muslim_chunk_1934)
tidy_EP_muslim_1937_select <- filter(tidy_EP_1937, chunk %in% muslim_chunk_1937)
tidy_EP_muslim_1940_select <- filter(tidy_EP_1940, chunk %in% muslim_chunk_1940)
tidy_EP_muslim_1942_select <- filter(tidy_EP_1942, chunk %in% muslim_chunk_1942)
tidy_EP_muslim_1945_select <- filter(tidy_EP_1945, chunk %in% muslim_chunk_1945)
tidy_EP_muslim_1950_select <- filter(tidy_EP_1950, chunk %in% muslim_chunk_1950)
tidy_EP_muslim_1956_select <- filter(tidy_EP_1956, chunk %in% muslim_chunk_1956)
tidy_EP_muslim_1960_select <- filter(tidy_EP_1960, chunk %in% muslim_chunk_1960)
tidy_EP_muslim_1965_select <- filter(tidy_EP_1965, chunk %in% muslim_chunk_1965)
tidy_EP_muslim_1969_select <- filter(tidy_EP_1969, chunk %in% muslim_chunk_1969)
tidy_EP_muslim_1975_select <- filter(tidy_EP_1975, chunk %in% muslim_chunk_1975)
tidy_EP_muslim_1984_select <- filter(tidy_EP_1984, chunk %in% muslim_chunk_1984)
tidy_EP_muslim_1992_select <- filter(tidy_EP_1992, chunk %in% muslim_chunk_1992)
tidy_EP_muslim_1997_select <- filter(tidy_EP_1997, chunk %in% muslim_chunk_1997)
tidy_EP_muslim_2004_select <- filter(tidy_EP_2004, chunk %in% muslim_chunk_2004)
tidy_EP_muslim_2011_select <- filter(tidy_EP_2011, chunk %in% muslim_chunk_2011)
tidy_EP_muslim_2017_select <- filter(tidy_EP_2017, chunk %in% muslim_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_muslim <- full_join(tidy_EP_muslim_1922_select, tidy_EP_muslim_1927_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1931_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1934_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1937_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1940_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1942_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1945_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1950_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1956_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1960_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1965_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1969_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1975_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1984_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1992_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_1997_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_2004_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_2011_select)
tidy_EP_chunks_muslim <- full_join(tidy_EP_chunks_muslim, tidy_EP_muslim_2017_select)



#   build cmdist for antonym pair, muslim

muslim_closeness <- tidy_EP_chunks_muslim %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=ngram_1960 )








#  Make CSV, rename first

italian_closeness <- mutate(italian_closeness,  italian.normal  = normal_pole)
jewish_closeness  <- mutate(jewish_closeness,   jewish.normal   = normal_pole)
irish_closeness   <- mutate(irish_closeness,    irish.normal    = normal_pole)
catholic_closeness<- mutate(catholic_closeness, catholic.normal = normal_pole)
mexican_closeness <- mutate(mexican_closeness,  mexican.normal  = normal_pole)
chinese_closeness <- mutate(chinese_closeness,  chinese.normal  = normal_pole)
muslim_closeness  <- mutate(muslim_closeness,   muslim.normal   = normal_pole)


italian_closeness  <- select(italian_closeness,  -(normal_pole))
jewish_closeness   <- select(jewish_closeness,   -(normal_pole))
irish_closeness    <- select(irish_closeness,    -(normal_pole))
catholic_closeness <- select(catholic_closeness, -(normal_pole))
mexican_closeness  <- select(mexican_closeness,  -(normal_pole))
chinese_closeness  <- select(chinese_closeness,  -(normal_pole))
muslim_closeness   <- select(muslim_closeness,   -(normal_pole))


immigrant_closeness <- full_join(italian_closeness,   jewish_closeness)
immigrant_closeness <- full_join(immigrant_closeness, irish_closeness)
immigrant_closeness <- full_join(immigrant_closeness, catholic_closeness)
immigrant_closeness <- full_join(immigrant_closeness, mexican_closeness)
immigrant_closeness <- full_join(immigrant_closeness, chinese_closeness)
immigrant_closeness <- full_join(immigrant_closeness, muslim_closeness)


















# 10 combine closeness and standardize scores across all pseudo-documents
tidy_EP_chunks_italian <-   mutate(tidy_EP_chunks_italian,   chunk_unique = year+.1)
tidy_EP_chunks_jewish <-    mutate(tidy_EP_chunks_jewish,    chunk_unique = year+.2)
tidy_EP_chunks_irish <-     mutate(tidy_EP_chunks_irish,     chunk_unique = year+.3)
tidy_EP_chunks_catholic <-  mutate(tidy_EP_chunks_catholic,  chunk_unique = year+.4)
tidy_EP_chunks_mexican <-   mutate(tidy_EP_chunks_mexican,   chunk_unique = year+.5)
tidy_EP_chunks_chinese <-   mutate(tidy_EP_chunks_chinese,   chunk_unique = year+.6)
tidy_EP_chunks_muslim <-    mutate(tidy_EP_chunks_muslim,    chunk_unique = year+.8)



tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_italian, tidy_EP_chunks_jewish)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_irish)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_catholic)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_mexican)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_chinese)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_muslim)


#combine full set


tidy_EP_chunks_combined_closeness <- tidy_EP_chunks_combined %>% 
  cast_dtm(term = word, 
           document = chunk_unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=ngram_1960 )

tidy_EP_chunks_combined_closeness <- mutate(tidy_EP_chunks_combined_closeness, year = substr(doc_id, 1, 4))
tidy_EP_chunks_combined_closeness <- mutate(tidy_EP_chunks_combined_closeness, group = -100 * (as.numeric(year)-as.numeric(doc_id)))





#    11 determine sentiment scores


#Create dataframe with sentiment scores

sent_scores <- data.frame("year" = c(1922, 1927, 1931, 1934, 
                                     1937, 1940, 1942, 1945, 
                                     1950, 1956, 1960, 1965, 
                                     1969, 1975, 1984, 1992, 
                                     1997, 2004, 2011, 2017),
"catholic_group" = 
  c(mean(tidy_EP_catholic_1922_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment), 
    mean(tidy_EP_catholic_1927_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1931_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1934_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1937_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1940_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1942_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1945_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1950_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1956_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1960_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1965_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1969_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1975_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1984_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1992_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_1997_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_2004_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_2011_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment),
    mean(tidy_EP_catholic_2017_select$mean_sentiment- tidy_EP_chunks_catholic$mean_sentiment)),
"chinese_group" =  
  c(mean(tidy_EP_chinese_1922_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ), 
    mean(tidy_EP_chinese_1927_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1931_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1934_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1937_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1940_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1942_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1945_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1950_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1956_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1960_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1965_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1969_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1975_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1984_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1992_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_1997_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_2004_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_2011_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment ),
    mean(tidy_EP_chinese_2017_select$mean_sentiment - tidy_EP_chunks_chinese$mean_sentiment )),
"irish_group" =    
 c(mean(tidy_EP_irish_1922_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ), 
   mean(tidy_EP_irish_1927_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1931_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1934_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1937_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1940_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1942_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1945_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1950_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1956_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1960_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1965_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1969_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1975_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1984_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1992_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_1997_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_2004_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_2011_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   ),
   mean(tidy_EP_irish_2017_select$mean_sentiment   - tidy_EP_chunks_irish$mean_sentiment   )),
"italian_group" =  
  c(mean(tidy_EP_italian_1922_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ), 
    mean(tidy_EP_italian_1927_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1931_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1934_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1937_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1940_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1942_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1945_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1950_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1956_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1960_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1965_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1969_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1975_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1984_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1992_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_1997_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_2004_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_2011_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment ),
    mean(tidy_EP_italian_2017_select$mean_sentiment - tidy_EP_chunks_italian$mean_sentiment )),
"jewish_group" =   
  c(mean(tidy_EP_jewish_1922_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ), 
    mean(tidy_EP_jewish_1927_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1931_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1934_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1937_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1940_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1942_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1945_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1950_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1956_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1960_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1965_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1969_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1975_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1984_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1992_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_1997_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_2004_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_2011_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  ),
    mean(tidy_EP_jewish_2017_select$mean_sentiment  - tidy_EP_chunks_jewish$mean_sentiment  )),
"mexican_group" =  
  c(mean(tidy_EP_mexican_1922_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ), 
    mean(tidy_EP_mexican_1927_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1931_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1934_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1937_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1940_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1942_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1945_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1950_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1956_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1960_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1965_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1969_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1975_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1984_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1992_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_1997_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_2004_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_2011_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment ),
    mean(tidy_EP_mexican_2017_select$mean_sentiment - tidy_EP_chunks_mexican$mean_sentiment )),
"muslim_group" =   
  c(mean(tidy_EP_muslim_1922_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ), 
    mean(tidy_EP_muslim_1927_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1931_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1934_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1937_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1940_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1942_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1945_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1950_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1956_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1960_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1965_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1969_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1975_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1984_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1992_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_1997_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_2004_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_2011_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  ),
    mean(tidy_EP_muslim_2017_select$mean_sentiment  - tidy_EP_chunks_muslim$mean_sentiment  )),
"catholic_combined" = 
  c(mean(tidy_EP_catholic_1922_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment), 
    mean(tidy_EP_catholic_1927_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1931_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1934_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1937_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1940_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1942_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1945_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1950_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1956_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1960_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1965_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1969_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1975_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1984_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1992_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_1997_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_2004_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_2011_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_catholic_2017_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment)),
"chinese_combined" =  
  c(mean(tidy_EP_chinese_1922_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ), 
    mean(tidy_EP_chinese_1927_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1931_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1934_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1937_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1940_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1942_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1945_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1950_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1956_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1960_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1965_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1969_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1975_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1984_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1992_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_1997_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_2004_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_2011_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_chinese_2017_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment )),
"irish_combined" =    
  c(mean(tidy_EP_irish_1922_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ), 
    mean(tidy_EP_irish_1927_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1931_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1934_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1937_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1940_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1942_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1945_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1950_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1956_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1960_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1965_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1969_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1975_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1984_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1992_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_1997_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_2004_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_2011_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_irish_2017_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   )),
"italian_combined" =  
  c(mean(tidy_EP_italian_1922_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ), 
    mean(tidy_EP_italian_1927_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1931_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1934_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1937_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1940_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1942_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1945_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1950_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1956_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1960_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1965_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1969_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1975_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1984_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1992_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_1997_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_2004_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_2011_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_italian_2017_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment )),
"jewish_combined" =   
  c(mean(tidy_EP_jewish_1922_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ), 
    mean(tidy_EP_jewish_1927_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1931_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1934_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1937_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1940_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1942_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1945_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1950_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1956_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1960_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1965_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1969_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1975_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1984_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1992_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_1997_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_2004_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_2011_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_jewish_2017_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  )),
"mexican_combined" =  
  c(mean(tidy_EP_mexican_1922_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ), 
    mean(tidy_EP_mexican_1927_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1931_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1934_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1937_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1940_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1942_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1945_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1950_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1956_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1960_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1965_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1969_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1975_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1984_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1992_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_1997_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_2004_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_2011_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_mexican_2017_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment )),
"muslim_combined" =   
  c(mean(tidy_EP_muslim_1922_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ), 
    mean(tidy_EP_muslim_1927_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1931_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1934_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1937_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1940_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1942_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1945_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1950_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1956_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1960_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1965_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1969_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1975_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1984_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1992_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_1997_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_2004_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_2011_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_muslim_2017_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  )))

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

sent_scores[is.nan(sent_scores)] <- NA

# create tidy data frame for plotting




sent_scores_group  <- select(sent_scores,  "year", 
                             "catholic_group", "irish_group", 
                             "italian_group", "jewish_group", 
                             "chinese_group",
                             "mexican_group", "muslim_group")


sent_scores_tidy_group <- reshape2::melt(sent_scores, id.var = "year")



sent_scores_combined  <- select(sent_scores,  "year", 
                                "catholic_combined", "irish_combined", 
                                "italian_combined", "jewish_combined", 
                                "chinese_combined", 
                                "mexican_combined", "muslim_combined")


sent_scores_tidy_combined <- reshape2::melt(sent_scores_combined, id.var = "year")


# groups
sent_scores_tidy_catholic <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="catholic_group"),]
sent_scores_tidy_chinese <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="chinese_group"),]
sent_scores_tidy_irish <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="irish_group"),]
sent_scores_tidy_italian <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="italian_group"),] 
sent_scores_tidy_jewish <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="jewish_group"),]
sent_scores_tidy_mexican <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="mexican_group"),]
sent_scores_tidy_muslim <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="muslim_group"),] 
















## 12 Document similarity score: Used for Table



similarity <- source_EP %>% 
  mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = replace_white(text)) %>%
  mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
  mutate(text = replace_number(text))  %>%
  filter(!str_detect(text, "[0-9]+") ) 


similarity$text_clean = similarity$text







similarity <- mutate(similarity, filename =  
          ifelse(grepl("txt_2022_3_28/1922EditionFull.txt", filename), "1922",
          ifelse(grepl("txt_2022_3_28/1927EditionFull.txt", filename), "1927",
          ifelse(grepl("txt_2022_3_28/1931EditionFull.txt", filename), "1931",
          ifelse(grepl("txt_2022_3_28/1934EditionFull.txt", filename), "1934",
          ifelse(grepl("txt_2022_3_28/1937EditionFull.txt", filename), "1937",
          ifelse(grepl("txt_2022_3_28/1940EditionFull.txt", filename), "1940",
          ifelse(grepl("txt_2022_3_28/1942EditionFull.txt", filename), "1942",
          ifelse(grepl("txt_2022_3_28/1945EditionFull.txt", filename), "1945",
          ifelse(grepl("txt_2022_3_28/1950EditionFull.txt", filename), "1950",
          ifelse(grepl("txt_2022_3_28/1956EditionFull.txt", filename), "1956",
          ifelse(grepl("txt_2022_3_28/1960EditionFull.txt", filename), "1960",
          ifelse(grepl("txt_2022_3_28/1965EditionFull.txt", filename), "1965",
          ifelse(grepl("txt_2022_3_28/1969EditionFull.txt", filename), "1969",
          ifelse(grepl("txt_2022_3_28/1975EditionFull.txt", filename), "1975",
          ifelse(grepl("txt_2022_3_28/1984EditionFull.txt", filename), "1984",
          ifelse(grepl("txt_2022_3_28/1992EditionFull.txt", filename), "1992",
          ifelse(grepl("txt_2022_3_28/1997EditionFull.txt", filename), "1997",
          ifelse(grepl("txt_2022_3_28/2004EditionFull.txt", filename), "2004",
          ifelse(grepl("txt_2022_3_28/2011EditionFull.txt", filename), "2011",
          ifelse(grepl("txt_2022_3_28/2017EditionFull.txt", filename), "2017","x")))))))))))))))))))))

##  split


similarity_1922 <- filter(similarity, filename == "1922")
similarity_1927 <- filter(similarity, filename == "1927")
similarity_1931 <- filter(similarity, filename == "1931")
similarity_1934 <- filter(similarity, filename == "1934")
similarity_1937 <- filter(similarity, filename == "1937")
similarity_1940 <- filter(similarity, filename == "1940")
similarity_1942 <- filter(similarity, filename == "1942")
similarity_1945 <- filter(similarity, filename == "1945")
similarity_1950 <- filter(similarity, filename == "1950")
similarity_1956 <- filter(similarity, filename == "1956")
similarity_1960 <- filter(similarity, filename == "1960")
similarity_1965 <- filter(similarity, filename == "1965")
similarity_1969 <- filter(similarity, filename == "1969")
similarity_1975 <- filter(similarity, filename == "1975")
similarity_1984 <- filter(similarity, filename == "1984")
similarity_1992 <- filter(similarity, filename == "1992")
similarity_1997 <- filter(similarity, filename == "1997")
similarity_2004 <- filter(similarity, filename == "2004")
similarity_2011 <- filter(similarity, filename == "2011")
similarity_2017 <- filter(similarity, filename == "2017")

##  token
it1922 <- itoken(similarity_1922$text_clean, progressbar = FALSE)
it1927 <- itoken(similarity_1927$text_clean, progressbar = FALSE)
it1931 <- itoken(similarity_1931$text_clean, progressbar = FALSE)
it1934 <- itoken(similarity_1934$text_clean, progressbar = FALSE)
it1937 <- itoken(similarity_1937$text_clean, progressbar = FALSE)
it1940 <- itoken(similarity_1940$text_clean, progressbar = FALSE)
it1942 <- itoken(similarity_1942$text_clean, progressbar = FALSE)
it1945 <- itoken(similarity_1945$text_clean, progressbar = FALSE)
it1950 <- itoken(similarity_1950$text_clean, progressbar = FALSE)
it1956 <- itoken(similarity_1956$text_clean, progressbar = FALSE)
it1960 <- itoken(similarity_1960$text_clean, progressbar = FALSE)
it1965 <- itoken(similarity_1965$text_clean, progressbar = FALSE)
it1969 <- itoken(similarity_1969$text_clean, progressbar = FALSE)
it1975 <- itoken(similarity_1975$text_clean, progressbar = FALSE)
it1984 <- itoken(similarity_1984$text_clean, progressbar = FALSE)
it1992 <- itoken(similarity_1992$text_clean, progressbar = FALSE)
it1997 <- itoken(similarity_1997$text_clean, progressbar = FALSE)
it2004 <- itoken(similarity_2004$text_clean, progressbar = FALSE)
it2011 <- itoken(similarity_2011$text_clean, progressbar = FALSE)
it2017 <- itoken(similarity_2017$text_clean, progressbar = FALSE)

# total document cosine



it_similarity <- itoken(similarity$text_clean, progressbar = FALSE)
stop_words <- "stop_words"
v = create_vocabulary(it_similarity, stopwords = stop_words)
# trim less than 10 words and 99 percentile

v99 <- as.numeric(quantile(v$term_count, probs = c(.99)))

v = prune_vocabulary(v, term_count_min = 10, term_count_max = v99)
vectorizer = vocab_vectorizer(v)


### each year vs full corpus

dtmEP = create_dtm(it_similarity, vectorizer)
dim(dtmEP)    

# add weights

tfidf = TfIdf$new()
dtmEP_tfidf = fit_transform(dtmEP, tfidf)

tfidf_EP_cos_sim = sim2(x = dtmEP_tfidf, method = "cosine", norm = "l2")
dim(tfidf_EP_cos_sim)
tfidf_EP_cos_sim[1:5, 1:20]

# put in excel
tfidf_EP_cos_sim_df <- tfidf_EP_cos_sim %>%
  as.matrix %>% as.data.frame













# 13 Plot CMDist scores 

tidy_EP_chunks_combined_closeness_plot <- tidy_EP_chunks_combined_closeness %>% 
  ggplot(aes(x = as.numeric(year), y = as.numeric(normal_pole))) +
  geom_point(aes(color = as.numeric(normal_pole)), size = 2) +
  ylim(-3,3) +
  geom_smooth(color="grey50", se = TRUE ) + 
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))
print(tidy_EP_chunks_combined_closeness_plot)







# Grouped graphs, separate

plot_italian_normal <- italian_closeness %>% 
  ggplot(aes(x = as.numeric(doc_id), y = as.numeric(italian.normal))) +
  geom_point(aes(color = as.numeric(italian.normal)), size = 2) +
  geom_smooth(color="grey50", se = TRUE ) + 
  ylim(-3,3) +
  xlab("Year") +
  ggtitle("Italian") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0)) 
  


print (plot_italian_normal)






plot_jewish_normal <- jewish_closeness %>% 
  ggplot(aes(x = as.numeric(doc_id), y = as.numeric(jewish.normal))) +
  geom_point(aes(color = as.numeric(jewish.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  ggtitle("Jewish") +
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))




plot_irish_normal <- irish_closeness %>% 
  ggplot(aes(x = as.numeric(doc_id), y = as.numeric(irish.normal))) +
  geom_point(aes(color = as.numeric(irish.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  ggtitle("Irish") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))



plot_catholic_normal <- catholic_closeness %>% 
  ggplot(aes(x = as.numeric(doc_id), y = as.numeric(catholic.normal))) +
  geom_point(aes(color = as.numeric(catholic.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  ggtitle("Catholic") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))



plot_mexican_normal <- mexican_closeness %>% 
  ggplot(aes(x = as.numeric(doc_id), y = as.numeric(mexican.normal))) +
  geom_point(aes(color = as.numeric(mexican.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  ggtitle("Mexican") +
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))



plot_chinese_normal <- chinese_closeness %>% 
  ggplot(aes(x = as.numeric(doc_id), y = as.numeric(chinese.normal))) +
  geom_point(aes(color = as.numeric(chinese.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  ggtitle("Chinese") +
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))




plot_muslim_normal <- muslim_closeness %>% 
  ggplot(aes(x = as.numeric(doc_id), y = as.numeric(muslim.normal))) +
  geom_point(aes(color = as.numeric(muslim.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  ggtitle("Muslim") + 
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))



## Group 1: italian, jewish, catholic, irish


plot_group1_meaning <- ggarrange(plot_italian_normal, 
          plot_jewish_normal,  
          plot_catholic_normal,  
          plot_irish_normal, 
          nrow = 2, ncol = 2,
          common.legend = TRUE,
          legend = "bottom")


print(plot_group1_meaning)

## Group 2: curban, muslim, chinese, mexican 
plot_group2_meaning <- ggarrange(plot_muslim_normal,  
                         plot_chinese_normal,  
                         plot_mexican_normal, 
                         nrow = 2, ncol = 2,
                         common.legend = TRUE,
                         legend = "bottom")


print(plot_group2_meaning)







#14 create plot of sentiment scores

#combined
sent_scores_plot_combined <- sent_scores_tidy_combined %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.2, .2) +  xlab("Year") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
  limits = c(-.4, .4),                        
  breaks = c(-.4, .4), 
  labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
  legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))


print(sent_scores_plot_combined)


#groups



sent_scores_plot_italian <- sent_scores_tidy_italian %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.2, .2) +  xlab("Year") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  ggtitle("Italian") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))




sent_scores_plot_jewish <- sent_scores_tidy_jewish %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.2, .2) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  ggtitle("Jewish") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))




sent_scores_plot_irish <- sent_scores_tidy_irish %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.2, .2) +  xlab("Year") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  ggtitle("Irish") +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))



sent_scores_plot_catholic <- sent_scores_tidy_catholic %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.2, .2) +  xlab("Year") +  
  ggtitle("Catholic") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))




sent_scores_plot_mexican <- sent_scores_tidy_mexican %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.2, .2) +  xlab("Year") +  
  ggtitle("Mexican") + 
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))




sent_scores_plot_chinese <- sent_scores_tidy_chinese %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.2, .2) +  xlab("Year") +
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +
  ggtitle("Chinese") +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))





sent_scores_plot_muslim <- sent_scores_tidy_muslim %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.2, .2) +  xlab("Year") +  
  ggtitle("Muslim") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))

# print all

print(sent_scores_plot_combined)

print (sent_scores_plot_italian)
print (sent_scores_plot_jewish)
print (sent_scores_plot_irish)
print (sent_scores_plot_catholic)
print (sent_scores_plot_mexican)
print (sent_scores_plot_chinese)
print (sent_scores_plot_muslim)


# combine plots into groups


plot_group1_sent <- ggarrange(sent_scores_plot_italian, 
                              sent_scores_plot_jewish,  
                              sent_scores_plot_catholic,  
                              sent_scores_plot_irish, 
                         nrow = 2, ncol = 2,
                         common.legend = TRUE,
                         legend = "bottom")


print(plot_group1_sent)

## Group 2: curban, muslim, chinese, mexican 
plot_group2_sent <- ggarrange(sent_scores_plot_muslim,  
                         sent_scores_plot_chinese,  
                         sent_scores_plot_mexican, 
                         nrow = 2, ncol = 2,
                         common.legend = TRUE,
                         legend = "bottom")


print(plot_group2_sent)






############ Table and Figure summary ############

# Table 1 reported in tfidf_cos_sim.csv
#     Prepared in Excel and exported to word

# Table 2 reported in count_prop_list.csv
#     Prepared in Excel and exported to word

# Figure 2: Combined Sentiment
print(sent_scores_plot_combined)
# Figure 3+4 Grouped Sentiment

print(plot_group1_sent)
print(plot_group2_sent)

# Figure 4: Combined Meaning
print(tidy_EP_chunks_combined_closeness_plot)

# Figure 5+6 Grouped Meaning
print(plot_group1_meaning)
print(plot_group2_meaning)


# print all figures to PDF

pdf("kline(2022_7_11)EP-img_ngram1960_fig2.pdf")
sent_scores_plot_combined
dev.off()

pdf("kline(2022_7_11)EP-img_ngram1960_fig3.pdf")
plot_group1_sent
dev.off()

pdf("kline(2022_7_11)EP-img_ngram1960_fig4.pdf")
plot_group2_sent
dev.off()

pdf("kline(2022_7_11)EP-img_ngram1960_fig5.pdf")
tidy_EP_chunks_combined_closeness_plot
dev.off()

pdf("kline(2022_7_11)EP-img_ngram1960_fig6.pdf")
plot_group1_meaning
dev.off()

pdf("kline(2022_7_11)EP-img_ngram1960_fig7.pdf")
plot_group2_meaning
dev.off()


pdf("kline(2022_7_11)EP-img_ngram1960.pdf")
sent_scores_plot_combined
plot_group1_sent
plot_group2_sent
tidy_EP_chunks_combined_closeness_plot
plot_group1_meaning
plot_group2_meaning
dev.off()


############

sent_combined_ngram1960  <- sent_scores_plot_combined
meaning_combined_ngram1960 <- tidy_EP_chunks_combined_closeness_plot

save(sent_combined_ngram1960, file = "sent_combined_ngram1960.Rdata")
save(meaning_combined_ngram1960, file = "meaning_combined_ngram1960.Rdata")
