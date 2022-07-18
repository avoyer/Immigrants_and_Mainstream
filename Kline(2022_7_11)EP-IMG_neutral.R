
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
#


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
# only need to run once per directory; gathers word embeddings from fasttextM
#ft_download_model("en", mb = 3000, location = wd)
#ft_load_model("en", location = wd)

fastTM <- readRDS("en.Rds")

# load embeddings from google n-grams
ngram_1920 <- readRDS("ngram_1920.Rds")
ngram_1930 <- readRDS("ngram_1930.Rds")
ngram_1940 <- readRDS("ngram_1940.Rds")
ngram_1950 <- readRDS("ngram_1950.Rds")
ngram_1960 <- readRDS("ngram_1960.Rds")
ngram_1970 <- readRDS("ngram_1970.Rds")
ngram_1980 <- readRDS("ngram_1980.Rds")
ngram_1990 <- readRDS("ngram_1990.Rds")






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
immigrant.sd <- get_direction(pairs, fastTM)


#  table


tidy_EP_table_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1920)

tidy_EP_table_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1920)

tidy_EP_table_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1930)

tidy_EP_table_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1930)

tidy_EP_table_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1930)

tidy_EP_table_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1940)

tidy_EP_table_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1940)

tidy_EP_table_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1940)

tidy_EP_table_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1950)

tidy_EP_table_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1950)

tidy_EP_table_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1960)

tidy_EP_table_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1960)

tidy_EP_table_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1960)


tidy_EP_table_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1970)

tidy_EP_table_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1980)

tidy_EP_table_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1990)


tidy_EP_table_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=ngram_1990)

tidy_EP_table_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=fastTM)

tidy_EP_table_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=fastTM)

tidy_EP_table_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("table"), wv=fastTM)


# combine, need to add "year"; also add unique

tidy_EP_table_1922 <- tidy_EP_table_1922 %>%
  mutate(year = 1922) 

tidy_EP_table_1927 <- tidy_EP_table_1927 %>%
  mutate(year = 1927)

tidy_EP_table_1931 <- tidy_EP_table_1931 %>%
  mutate(year = 1931)

tidy_EP_table_1934 <- tidy_EP_table_1934 %>%
  mutate(year = 1934)

tidy_EP_table_1937 <- tidy_EP_table_1937 %>%
  mutate(year = 1937)

tidy_EP_table_1940 <- tidy_EP_table_1940 %>%
  mutate(year = 1940)

tidy_EP_table_1942 <- tidy_EP_table_1942 %>%
  mutate(year = 1942)

tidy_EP_table_1945 <- tidy_EP_table_1945 %>%
  mutate(year = 1945)

tidy_EP_table_1950 <- tidy_EP_table_1950 %>%
  mutate(year = 1950)

tidy_EP_table_1956 <- tidy_EP_table_1956 %>%
  mutate(year = 1956)

tidy_EP_table_1960 <- tidy_EP_table_1960 %>%
  mutate(year = 1960)

tidy_EP_table_1965 <- tidy_EP_table_1965 %>%
  mutate(year = 1965)


tidy_EP_table_1969 <- tidy_EP_table_1969 %>%
  mutate(year = 1969)

tidy_EP_table_1975 <- tidy_EP_table_1975 %>%
  mutate(year = 1975)

tidy_EP_table_1984 <- tidy_EP_table_1984 %>%
  mutate(year = 1984)

tidy_EP_table_1992 <- tidy_EP_table_1992 %>%
  mutate(year = 1992)

tidy_EP_table_1997 <- tidy_EP_table_1997 %>%
  mutate(year = 1997)

tidy_EP_table_2004 <- tidy_EP_table_2004 %>%
  mutate(year = 2004)

tidy_EP_table_2011 <- tidy_EP_table_2011 %>%
  mutate(year = 2011)

tidy_EP_table_2017 <- tidy_EP_table_2017 %>%
  mutate(year = 2017)



# select on CMD > cv for each edition, gen unique

tidy_EP_table_1922_salient <- tidy_EP_table_1922[tidy_EP_table_1922$table > 1.65, ]
tidy_EP_table_1927_salient <- tidy_EP_table_1927[tidy_EP_table_1927$table > 1.65, ]
tidy_EP_table_1931_salient <- tidy_EP_table_1931[tidy_EP_table_1931$table > 1.65, ]
tidy_EP_table_1934_salient <- tidy_EP_table_1934[tidy_EP_table_1934$table > 1.65, ]
tidy_EP_table_1937_salient <- tidy_EP_table_1937[tidy_EP_table_1937$table > 1.65, ]
tidy_EP_table_1940_salient <- tidy_EP_table_1940[tidy_EP_table_1940$table > 1.65, ]
tidy_EP_table_1942_salient <- tidy_EP_table_1942[tidy_EP_table_1942$table > 1.65, ]
tidy_EP_table_1945_salient <- tidy_EP_table_1945[tidy_EP_table_1945$table > 1.65, ]
tidy_EP_table_1950_salient <- tidy_EP_table_1950[tidy_EP_table_1950$table > 1.65, ]
tidy_EP_table_1956_salient <- tidy_EP_table_1956[tidy_EP_table_1956$table > 1.65, ]
tidy_EP_table_1960_salient <- tidy_EP_table_1960[tidy_EP_table_1960$table > 1.65, ]
tidy_EP_table_1965_salient <- tidy_EP_table_1965[tidy_EP_table_1965$table > 1.65, ]
tidy_EP_table_1969_salient <- tidy_EP_table_1969[tidy_EP_table_1969$table > 1.65, ]
tidy_EP_table_1975_salient <- tidy_EP_table_1975[tidy_EP_table_1975$table > 1.65, ]
tidy_EP_table_1984_salient <- tidy_EP_table_1984[tidy_EP_table_1984$table > 1.65, ]
tidy_EP_table_1992_salient <- tidy_EP_table_1992[tidy_EP_table_1992$table > 1.65, ]
tidy_EP_table_1997_salient <- tidy_EP_table_1997[tidy_EP_table_1997$table > 1.65, ]
tidy_EP_table_2004_salient <- tidy_EP_table_2004[tidy_EP_table_2004$table > 1.65, ]
tidy_EP_table_2011_salient <- tidy_EP_table_2011[tidy_EP_table_2011$table > 1.65, ]
tidy_EP_table_2017_salient <- tidy_EP_table_2017[tidy_EP_table_2017$table > 1.65, ]



# list of salient
table_chunk_1922 <- print(tidy_EP_table_1922_salient$doc_id)
table_chunk_1927 <- print(tidy_EP_table_1927_salient$doc_id)
table_chunk_1931 <- print(tidy_EP_table_1931_salient$doc_id)
table_chunk_1934 <- print(tidy_EP_table_1934_salient$doc_id)
table_chunk_1937 <- print(tidy_EP_table_1937_salient$doc_id)
table_chunk_1940 <- print(tidy_EP_table_1940_salient$doc_id)
table_chunk_1942 <- print(tidy_EP_table_1942_salient$doc_id)
table_chunk_1945 <- print(tidy_EP_table_1945_salient$doc_id)
table_chunk_1950 <- print(tidy_EP_table_1950_salient$doc_id)
table_chunk_1956 <- print(tidy_EP_table_1956_salient$doc_id)
table_chunk_1960 <- print(tidy_EP_table_1960_salient$doc_id)
table_chunk_1965 <- print(tidy_EP_table_1965_salient$doc_id)
table_chunk_1969 <- print(tidy_EP_table_1969_salient$doc_id)
table_chunk_1975 <- print(tidy_EP_table_1975_salient$doc_id)
table_chunk_1984 <- print(tidy_EP_table_1984_salient$doc_id)
table_chunk_1992 <- print(tidy_EP_table_1992_salient$doc_id)
table_chunk_1997 <- print(tidy_EP_table_1997_salient$doc_id)
table_chunk_2004 <- print(tidy_EP_table_2004_salient$doc_id)
table_chunk_2011 <- print(tidy_EP_table_2011_salient$doc_id)
table_chunk_2017 <- print(tidy_EP_table_2017_salient$doc_id)



# select if in chunk
tidy_EP_table_1922_select <- filter(tidy_EP_1922, chunk %in% table_chunk_1922)
tidy_EP_table_1927_select <- filter(tidy_EP_1927, chunk %in% table_chunk_1927)
tidy_EP_table_1931_select <- filter(tidy_EP_1931, chunk %in% table_chunk_1931)
tidy_EP_table_1934_select <- filter(tidy_EP_1934, chunk %in% table_chunk_1934)
tidy_EP_table_1937_select <- filter(tidy_EP_1937, chunk %in% table_chunk_1937)
tidy_EP_table_1940_select <- filter(tidy_EP_1940, chunk %in% table_chunk_1940)
tidy_EP_table_1942_select <- filter(tidy_EP_1942, chunk %in% table_chunk_1942)
tidy_EP_table_1945_select <- filter(tidy_EP_1945, chunk %in% table_chunk_1945)
tidy_EP_table_1950_select <- filter(tidy_EP_1950, chunk %in% table_chunk_1950)
tidy_EP_table_1956_select <- filter(tidy_EP_1956, chunk %in% table_chunk_1956)
tidy_EP_table_1960_select <- filter(tidy_EP_1960, chunk %in% table_chunk_1960)
tidy_EP_table_1965_select <- filter(tidy_EP_1965, chunk %in% table_chunk_1965)
tidy_EP_table_1969_select <- filter(tidy_EP_1969, chunk %in% table_chunk_1969)
tidy_EP_table_1975_select <- filter(tidy_EP_1975, chunk %in% table_chunk_1975)
tidy_EP_table_1984_select <- filter(tidy_EP_1984, chunk %in% table_chunk_1984)
tidy_EP_table_1992_select <- filter(tidy_EP_1992, chunk %in% table_chunk_1992)
tidy_EP_table_1997_select <- filter(tidy_EP_1997, chunk %in% table_chunk_1997)
tidy_EP_table_2004_select <- filter(tidy_EP_2004, chunk %in% table_chunk_2004)
tidy_EP_table_2011_select <- filter(tidy_EP_2011, chunk %in% table_chunk_2011)
tidy_EP_table_2017_select <- filter(tidy_EP_2017, chunk %in% table_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_table <- full_join(tidy_EP_table_1922_select, tidy_EP_table_1927_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1931_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1934_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1937_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1940_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1942_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1945_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1950_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1956_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1960_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1965_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1969_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1975_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1984_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1992_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_1997_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_2004_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_2011_select)
tidy_EP_chunks_table <- full_join(tidy_EP_chunks_table, tidy_EP_table_2017_select)



#   build cmdist for antonym pair, table

table_closeness <- tidy_EP_chunks_table %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )





#   friend


tidy_EP_friend_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1920)

tidy_EP_friend_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1920)

tidy_EP_friend_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1930)

tidy_EP_friend_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1930)

tidy_EP_friend_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1930)

tidy_EP_friend_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1940)

tidy_EP_friend_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1940)

tidy_EP_friend_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1940)

tidy_EP_friend_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1950)

tidy_EP_friend_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1950)

tidy_EP_friend_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1960)

tidy_EP_friend_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1960)

tidy_EP_friend_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1960)


tidy_EP_friend_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1970)

tidy_EP_friend_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1980)

tidy_EP_friend_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1990)


tidy_EP_friend_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=ngram_1990)

tidy_EP_friend_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=fastTM)

tidy_EP_friend_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=fastTM)

tidy_EP_friend_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("friend"), wv=fastTM)


# combine, need to add "year"; also add unique

tidy_EP_friend_1922 <- tidy_EP_friend_1922 %>%
  mutate(year = 1922) 

tidy_EP_friend_1927 <- tidy_EP_friend_1927 %>%
  mutate(year = 1927)

tidy_EP_friend_1931 <- tidy_EP_friend_1931 %>%
  mutate(year = 1931)

tidy_EP_friend_1934 <- tidy_EP_friend_1934 %>%
  mutate(year = 1934)

tidy_EP_friend_1937 <- tidy_EP_friend_1937 %>%
  mutate(year = 1937)

tidy_EP_friend_1940 <- tidy_EP_friend_1940 %>%
  mutate(year = 1940)

tidy_EP_friend_1942 <- tidy_EP_friend_1942 %>%
  mutate(year = 1942)

tidy_EP_friend_1945 <- tidy_EP_friend_1945 %>%
  mutate(year = 1945)

tidy_EP_friend_1950 <- tidy_EP_friend_1950 %>%
  mutate(year = 1950)

tidy_EP_friend_1956 <- tidy_EP_friend_1956 %>%
  mutate(year = 1956)

tidy_EP_friend_1960 <- tidy_EP_friend_1960 %>%
  mutate(year = 1960)

tidy_EP_friend_1965 <- tidy_EP_friend_1965 %>%
  mutate(year = 1965)


tidy_EP_friend_1969 <- tidy_EP_friend_1969 %>%
  mutate(year = 1969)

tidy_EP_friend_1975 <- tidy_EP_friend_1975 %>%
  mutate(year = 1975)

tidy_EP_friend_1984 <- tidy_EP_friend_1984 %>%
  mutate(year = 1984)

tidy_EP_friend_1992 <- tidy_EP_friend_1992 %>%
  mutate(year = 1992)

tidy_EP_friend_1997 <- tidy_EP_friend_1997 %>%
  mutate(year = 1997)

tidy_EP_friend_2004 <- tidy_EP_friend_2004 %>%
  mutate(year = 2004)

tidy_EP_friend_2011 <- tidy_EP_friend_2011 %>%
  mutate(year = 2011)

tidy_EP_friend_2017 <- tidy_EP_friend_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_friend_1922_salient <- tidy_EP_friend_1922[tidy_EP_friend_1922$friend > 1.65, ]
tidy_EP_friend_1927_salient <- tidy_EP_friend_1927[tidy_EP_friend_1927$friend > 1.65, ]
tidy_EP_friend_1931_salient <- tidy_EP_friend_1931[tidy_EP_friend_1931$friend > 1.65, ]
tidy_EP_friend_1934_salient <- tidy_EP_friend_1934[tidy_EP_friend_1934$friend > 1.65, ]
tidy_EP_friend_1937_salient <- tidy_EP_friend_1937[tidy_EP_friend_1937$friend > 1.65, ]
tidy_EP_friend_1940_salient <- tidy_EP_friend_1940[tidy_EP_friend_1940$friend > 1.65, ]
tidy_EP_friend_1942_salient <- tidy_EP_friend_1942[tidy_EP_friend_1942$friend > 1.65, ]
tidy_EP_friend_1945_salient <- tidy_EP_friend_1945[tidy_EP_friend_1945$friend > 1.65, ]
tidy_EP_friend_1950_salient <- tidy_EP_friend_1950[tidy_EP_friend_1950$friend > 1.65, ]
tidy_EP_friend_1956_salient <- tidy_EP_friend_1956[tidy_EP_friend_1956$friend > 1.65, ]
tidy_EP_friend_1960_salient <- tidy_EP_friend_1960[tidy_EP_friend_1960$friend > 1.65, ]
tidy_EP_friend_1965_salient <- tidy_EP_friend_1965[tidy_EP_friend_1965$friend > 1.65, ]
tidy_EP_friend_1969_salient <- tidy_EP_friend_1969[tidy_EP_friend_1969$friend > 1.65, ]
tidy_EP_friend_1975_salient <- tidy_EP_friend_1975[tidy_EP_friend_1975$friend > 1.65, ]
tidy_EP_friend_1984_salient <- tidy_EP_friend_1984[tidy_EP_friend_1984$friend > 1.65, ]
tidy_EP_friend_1992_salient <- tidy_EP_friend_1992[tidy_EP_friend_1992$friend > 1.65, ]
tidy_EP_friend_1997_salient <- tidy_EP_friend_1997[tidy_EP_friend_1997$friend > 1.65, ]
tidy_EP_friend_2004_salient <- tidy_EP_friend_2004[tidy_EP_friend_2004$friend > 1.65, ]
tidy_EP_friend_2011_salient <- tidy_EP_friend_2011[tidy_EP_friend_2011$friend > 1.65, ]
tidy_EP_friend_2017_salient <- tidy_EP_friend_2017[tidy_EP_friend_2017$friend > 1.65, ]



# list of salient
friend_chunk_1922 <- print(tidy_EP_friend_1922_salient$doc_id)
friend_chunk_1927 <- print(tidy_EP_friend_1927_salient$doc_id)
friend_chunk_1931 <- print(tidy_EP_friend_1931_salient$doc_id)
friend_chunk_1934 <- print(tidy_EP_friend_1934_salient$doc_id)
friend_chunk_1937 <- print(tidy_EP_friend_1937_salient$doc_id)
friend_chunk_1940 <- print(tidy_EP_friend_1940_salient$doc_id)
friend_chunk_1942 <- print(tidy_EP_friend_1942_salient$doc_id)
friend_chunk_1945 <- print(tidy_EP_friend_1945_salient$doc_id)
friend_chunk_1950 <- print(tidy_EP_friend_1950_salient$doc_id)
friend_chunk_1956 <- print(tidy_EP_friend_1956_salient$doc_id)
friend_chunk_1960 <- print(tidy_EP_friend_1960_salient$doc_id)
friend_chunk_1965 <- print(tidy_EP_friend_1965_salient$doc_id)
friend_chunk_1969 <- print(tidy_EP_friend_1969_salient$doc_id)
friend_chunk_1975 <- print(tidy_EP_friend_1975_salient$doc_id)
friend_chunk_1984 <- print(tidy_EP_friend_1984_salient$doc_id)
friend_chunk_1992 <- print(tidy_EP_friend_1992_salient$doc_id)
friend_chunk_1997 <- print(tidy_EP_friend_1997_salient$doc_id)
friend_chunk_2004 <- print(tidy_EP_friend_2004_salient$doc_id)
friend_chunk_2011 <- print(tidy_EP_friend_2011_salient$doc_id)
friend_chunk_2017 <- print(tidy_EP_friend_2017_salient$doc_id)



# select if in chunk
tidy_EP_friend_1922_select <- filter(tidy_EP_1922, chunk %in% friend_chunk_1922)
tidy_EP_friend_1927_select <- filter(tidy_EP_1927, chunk %in% friend_chunk_1927)
tidy_EP_friend_1931_select <- filter(tidy_EP_1931, chunk %in% friend_chunk_1931)
tidy_EP_friend_1934_select <- filter(tidy_EP_1934, chunk %in% friend_chunk_1934)
tidy_EP_friend_1937_select <- filter(tidy_EP_1937, chunk %in% friend_chunk_1937)
tidy_EP_friend_1940_select <- filter(tidy_EP_1940, chunk %in% friend_chunk_1940)
tidy_EP_friend_1942_select <- filter(tidy_EP_1942, chunk %in% friend_chunk_1942)
tidy_EP_friend_1945_select <- filter(tidy_EP_1945, chunk %in% friend_chunk_1945)
tidy_EP_friend_1950_select <- filter(tidy_EP_1950, chunk %in% friend_chunk_1950)
tidy_EP_friend_1956_select <- filter(tidy_EP_1956, chunk %in% friend_chunk_1956)
tidy_EP_friend_1960_select <- filter(tidy_EP_1960, chunk %in% friend_chunk_1960)
tidy_EP_friend_1965_select <- filter(tidy_EP_1965, chunk %in% friend_chunk_1965)
tidy_EP_friend_1969_select <- filter(tidy_EP_1969, chunk %in% friend_chunk_1969)
tidy_EP_friend_1975_select <- filter(tidy_EP_1975, chunk %in% friend_chunk_1975)
tidy_EP_friend_1984_select <- filter(tidy_EP_1984, chunk %in% friend_chunk_1984)
tidy_EP_friend_1992_select <- filter(tidy_EP_1992, chunk %in% friend_chunk_1992)
tidy_EP_friend_1997_select <- filter(tidy_EP_1997, chunk %in% friend_chunk_1997)
tidy_EP_friend_2004_select <- filter(tidy_EP_2004, chunk %in% friend_chunk_2004)
tidy_EP_friend_2011_select <- filter(tidy_EP_2011, chunk %in% friend_chunk_2011)
tidy_EP_friend_2017_select <- filter(tidy_EP_2017, chunk %in% friend_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_friend <- full_join(tidy_EP_friend_1922_select, tidy_EP_friend_1927_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1931_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1934_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1937_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1940_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1942_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1945_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1950_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1956_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1960_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1965_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1969_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1975_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1984_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1992_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_1997_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_2004_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_2011_select)
tidy_EP_chunks_friend <- full_join(tidy_EP_chunks_friend, tidy_EP_friend_2017_select)



#   build cmdist for antonym pair, friend

friend_closeness <- tidy_EP_chunks_friend %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )











#   book


tidy_EP_book_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1920)

tidy_EP_book_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1920)

tidy_EP_book_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1930)

tidy_EP_book_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1930)

tidy_EP_book_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1930)

tidy_EP_book_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1940)

tidy_EP_book_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1940)

tidy_EP_book_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1940)

tidy_EP_book_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1950)

tidy_EP_book_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1950)

tidy_EP_book_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1960)

tidy_EP_book_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1960)

tidy_EP_book_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1960)


tidy_EP_book_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1970)

tidy_EP_book_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1980)

tidy_EP_book_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1990)


tidy_EP_book_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=ngram_1990)

tidy_EP_book_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=fastTM)

tidy_EP_book_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=fastTM)

tidy_EP_book_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("book"), wv=fastTM)


# combine, need to add "year"; also add unique

tidy_EP_book_1922 <- tidy_EP_book_1922 %>%
  mutate(year = 1922) 

tidy_EP_book_1927 <- tidy_EP_book_1927 %>%
  mutate(year = 1927)

tidy_EP_book_1931 <- tidy_EP_book_1931 %>%
  mutate(year = 1931)

tidy_EP_book_1934 <- tidy_EP_book_1934 %>%
  mutate(year = 1934)

tidy_EP_book_1937 <- tidy_EP_book_1937 %>%
  mutate(year = 1937)

tidy_EP_book_1940 <- tidy_EP_book_1940 %>%
  mutate(year = 1940)

tidy_EP_book_1942 <- tidy_EP_book_1942 %>%
  mutate(year = 1942)

tidy_EP_book_1945 <- tidy_EP_book_1945 %>%
  mutate(year = 1945)

tidy_EP_book_1950 <- tidy_EP_book_1950 %>%
  mutate(year = 1950)

tidy_EP_book_1956 <- tidy_EP_book_1956 %>%
  mutate(year = 1956)

tidy_EP_book_1960 <- tidy_EP_book_1960 %>%
  mutate(year = 1960)

tidy_EP_book_1965 <- tidy_EP_book_1965 %>%
  mutate(year = 1965)


tidy_EP_book_1969 <- tidy_EP_book_1969 %>%
  mutate(year = 1969)

tidy_EP_book_1975 <- tidy_EP_book_1975 %>%
  mutate(year = 1975)

tidy_EP_book_1984 <- tidy_EP_book_1984 %>%
  mutate(year = 1984)

tidy_EP_book_1992 <- tidy_EP_book_1992 %>%
  mutate(year = 1992)

tidy_EP_book_1997 <- tidy_EP_book_1997 %>%
  mutate(year = 1997)

tidy_EP_book_2004 <- tidy_EP_book_2004 %>%
  mutate(year = 2004)

tidy_EP_book_2011 <- tidy_EP_book_2011 %>%
  mutate(year = 2011)

tidy_EP_book_2017 <- tidy_EP_book_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_book_1922_salient <- tidy_EP_book_1922[tidy_EP_book_1922$book > 1.65, ]
tidy_EP_book_1927_salient <- tidy_EP_book_1927[tidy_EP_book_1927$book > 1.65, ]
tidy_EP_book_1931_salient <- tidy_EP_book_1931[tidy_EP_book_1931$book > 1.65, ]
tidy_EP_book_1934_salient <- tidy_EP_book_1934[tidy_EP_book_1934$book > 1.65, ]
tidy_EP_book_1937_salient <- tidy_EP_book_1937[tidy_EP_book_1937$book > 1.65, ]
tidy_EP_book_1940_salient <- tidy_EP_book_1940[tidy_EP_book_1940$book > 1.65, ]
tidy_EP_book_1942_salient <- tidy_EP_book_1942[tidy_EP_book_1942$book > 1.65, ]
tidy_EP_book_1945_salient <- tidy_EP_book_1945[tidy_EP_book_1945$book > 1.65, ]
tidy_EP_book_1950_salient <- tidy_EP_book_1950[tidy_EP_book_1950$book > 1.65, ]
tidy_EP_book_1956_salient <- tidy_EP_book_1956[tidy_EP_book_1956$book > 1.65, ]
tidy_EP_book_1960_salient <- tidy_EP_book_1960[tidy_EP_book_1960$book > 1.65, ]
tidy_EP_book_1965_salient <- tidy_EP_book_1965[tidy_EP_book_1965$book > 1.65, ]
tidy_EP_book_1969_salient <- tidy_EP_book_1969[tidy_EP_book_1969$book > 1.65, ]
tidy_EP_book_1975_salient <- tidy_EP_book_1975[tidy_EP_book_1975$book > 1.65, ]
tidy_EP_book_1984_salient <- tidy_EP_book_1984[tidy_EP_book_1984$book > 1.65, ]
tidy_EP_book_1992_salient <- tidy_EP_book_1992[tidy_EP_book_1992$book > 1.65, ]
tidy_EP_book_1997_salient <- tidy_EP_book_1997[tidy_EP_book_1997$book > 1.65, ]
tidy_EP_book_2004_salient <- tidy_EP_book_2004[tidy_EP_book_2004$book > 1.65, ]
tidy_EP_book_2011_salient <- tidy_EP_book_2011[tidy_EP_book_2011$book > 1.65, ]
tidy_EP_book_2017_salient <- tidy_EP_book_2017[tidy_EP_book_2017$book > 1.65, ]



# list of salient
book_chunk_1922 <- print(tidy_EP_book_1922_salient$doc_id)
book_chunk_1927 <- print(tidy_EP_book_1927_salient$doc_id)
book_chunk_1931 <- print(tidy_EP_book_1931_salient$doc_id)
book_chunk_1934 <- print(tidy_EP_book_1934_salient$doc_id)
book_chunk_1937 <- print(tidy_EP_book_1937_salient$doc_id)
book_chunk_1940 <- print(tidy_EP_book_1940_salient$doc_id)
book_chunk_1942 <- print(tidy_EP_book_1942_salient$doc_id)
book_chunk_1945 <- print(tidy_EP_book_1945_salient$doc_id)
book_chunk_1950 <- print(tidy_EP_book_1950_salient$doc_id)
book_chunk_1956 <- print(tidy_EP_book_1956_salient$doc_id)
book_chunk_1960 <- print(tidy_EP_book_1960_salient$doc_id)
book_chunk_1965 <- print(tidy_EP_book_1965_salient$doc_id)
book_chunk_1969 <- print(tidy_EP_book_1969_salient$doc_id)
book_chunk_1975 <- print(tidy_EP_book_1975_salient$doc_id)
book_chunk_1984 <- print(tidy_EP_book_1984_salient$doc_id)
book_chunk_1992 <- print(tidy_EP_book_1992_salient$doc_id)
book_chunk_1997 <- print(tidy_EP_book_1997_salient$doc_id)
book_chunk_2004 <- print(tidy_EP_book_2004_salient$doc_id)
book_chunk_2011 <- print(tidy_EP_book_2011_salient$doc_id)
book_chunk_2017 <- print(tidy_EP_book_2017_salient$doc_id)



# select if in chunk
tidy_EP_book_1922_select <- filter(tidy_EP_1922, chunk %in% book_chunk_1922)
tidy_EP_book_1927_select <- filter(tidy_EP_1927, chunk %in% book_chunk_1927)
tidy_EP_book_1931_select <- filter(tidy_EP_1931, chunk %in% book_chunk_1931)
tidy_EP_book_1934_select <- filter(tidy_EP_1934, chunk %in% book_chunk_1934)
tidy_EP_book_1937_select <- filter(tidy_EP_1937, chunk %in% book_chunk_1937)
tidy_EP_book_1940_select <- filter(tidy_EP_1940, chunk %in% book_chunk_1940)
tidy_EP_book_1942_select <- filter(tidy_EP_1942, chunk %in% book_chunk_1942)
tidy_EP_book_1945_select <- filter(tidy_EP_1945, chunk %in% book_chunk_1945)
tidy_EP_book_1950_select <- filter(tidy_EP_1950, chunk %in% book_chunk_1950)
tidy_EP_book_1956_select <- filter(tidy_EP_1956, chunk %in% book_chunk_1956)
tidy_EP_book_1960_select <- filter(tidy_EP_1960, chunk %in% book_chunk_1960)
tidy_EP_book_1965_select <- filter(tidy_EP_1965, chunk %in% book_chunk_1965)
tidy_EP_book_1969_select <- filter(tidy_EP_1969, chunk %in% book_chunk_1969)
tidy_EP_book_1975_select <- filter(tidy_EP_1975, chunk %in% book_chunk_1975)
tidy_EP_book_1984_select <- filter(tidy_EP_1984, chunk %in% book_chunk_1984)
tidy_EP_book_1992_select <- filter(tidy_EP_1992, chunk %in% book_chunk_1992)
tidy_EP_book_1997_select <- filter(tidy_EP_1997, chunk %in% book_chunk_1997)
tidy_EP_book_2004_select <- filter(tidy_EP_2004, chunk %in% book_chunk_2004)
tidy_EP_book_2011_select <- filter(tidy_EP_2011, chunk %in% book_chunk_2011)
tidy_EP_book_2017_select <- filter(tidy_EP_2017, chunk %in% book_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_book <- full_join(tidy_EP_book_1922_select, tidy_EP_book_1927_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1931_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1934_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1937_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1940_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1942_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1945_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1950_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1956_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1960_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1965_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1969_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1975_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1984_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1992_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_1997_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_2004_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_2011_select)
tidy_EP_chunks_book <- full_join(tidy_EP_chunks_book, tidy_EP_book_2017_select)



#   build cmdist for antonym pair, book

book_closeness <- tidy_EP_chunks_book %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )


















#    rain


tidy_EP_rain_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1920)

tidy_EP_rain_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1920)

tidy_EP_rain_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1930)

tidy_EP_rain_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1930)

tidy_EP_rain_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1930)

tidy_EP_rain_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1940)

tidy_EP_rain_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1940)

tidy_EP_rain_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1940)

tidy_EP_rain_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1950)

tidy_EP_rain_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1950)

tidy_EP_rain_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1960)

tidy_EP_rain_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1960)

tidy_EP_rain_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1960)


tidy_EP_rain_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1970)

tidy_EP_rain_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1980)

tidy_EP_rain_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1990)


tidy_EP_rain_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=ngram_1990)

tidy_EP_rain_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=fastTM)

tidy_EP_rain_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=fastTM)

tidy_EP_rain_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("rain"), wv=fastTM)


# combine, need to add "year"; also add unique

tidy_EP_rain_1922 <- tidy_EP_rain_1922 %>%
  mutate(year = 1922) 

tidy_EP_rain_1927 <- tidy_EP_rain_1927 %>%
  mutate(year = 1927)

tidy_EP_rain_1931 <- tidy_EP_rain_1931 %>%
  mutate(year = 1931)

tidy_EP_rain_1934 <- tidy_EP_rain_1934 %>%
  mutate(year = 1934)

tidy_EP_rain_1937 <- tidy_EP_rain_1937 %>%
  mutate(year = 1937)

tidy_EP_rain_1940 <- tidy_EP_rain_1940 %>%
  mutate(year = 1940)

tidy_EP_rain_1942 <- tidy_EP_rain_1942 %>%
  mutate(year = 1942)

tidy_EP_rain_1945 <- tidy_EP_rain_1945 %>%
  mutate(year = 1945)

tidy_EP_rain_1950 <- tidy_EP_rain_1950 %>%
  mutate(year = 1950)

tidy_EP_rain_1956 <- tidy_EP_rain_1956 %>%
  mutate(year = 1956)

tidy_EP_rain_1960 <- tidy_EP_rain_1960 %>%
  mutate(year = 1960)

tidy_EP_rain_1965 <- tidy_EP_rain_1965 %>%
  mutate(year = 1965)


tidy_EP_rain_1969 <- tidy_EP_rain_1969 %>%
  mutate(year = 1969)

tidy_EP_rain_1975 <- tidy_EP_rain_1975 %>%
  mutate(year = 1975)

tidy_EP_rain_1984 <- tidy_EP_rain_1984 %>%
  mutate(year = 1984)

tidy_EP_rain_1992 <- tidy_EP_rain_1992 %>%
  mutate(year = 1992)

tidy_EP_rain_1997 <- tidy_EP_rain_1997 %>%
  mutate(year = 1997)

tidy_EP_rain_2004 <- tidy_EP_rain_2004 %>%
  mutate(year = 2004)

tidy_EP_rain_2011 <- tidy_EP_rain_2011 %>%
  mutate(year = 2011)

tidy_EP_rain_2017 <- tidy_EP_rain_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_rain_1922_salient <- tidy_EP_rain_1922[tidy_EP_rain_1922$rain > 1.65, ]
tidy_EP_rain_1927_salient <- tidy_EP_rain_1927[tidy_EP_rain_1927$rain > 1.65, ]
tidy_EP_rain_1931_salient <- tidy_EP_rain_1931[tidy_EP_rain_1931$rain > 1.65, ]
tidy_EP_rain_1934_salient <- tidy_EP_rain_1934[tidy_EP_rain_1934$rain > 1.65, ]
tidy_EP_rain_1937_salient <- tidy_EP_rain_1937[tidy_EP_rain_1937$rain > 1.65, ]
tidy_EP_rain_1940_salient <- tidy_EP_rain_1940[tidy_EP_rain_1940$rain > 1.65, ]
tidy_EP_rain_1942_salient <- tidy_EP_rain_1942[tidy_EP_rain_1942$rain > 1.65, ]
tidy_EP_rain_1945_salient <- tidy_EP_rain_1945[tidy_EP_rain_1945$rain > 1.65, ]
tidy_EP_rain_1950_salient <- tidy_EP_rain_1950[tidy_EP_rain_1950$rain > 1.65, ]
tidy_EP_rain_1956_salient <- tidy_EP_rain_1956[tidy_EP_rain_1956$rain > 1.65, ]
tidy_EP_rain_1960_salient <- tidy_EP_rain_1960[tidy_EP_rain_1960$rain > 1.65, ]
tidy_EP_rain_1965_salient <- tidy_EP_rain_1965[tidy_EP_rain_1965$rain > 1.65, ]
tidy_EP_rain_1969_salient <- tidy_EP_rain_1969[tidy_EP_rain_1969$rain > 1.65, ]
tidy_EP_rain_1975_salient <- tidy_EP_rain_1975[tidy_EP_rain_1975$rain > 1.65, ]
tidy_EP_rain_1984_salient <- tidy_EP_rain_1984[tidy_EP_rain_1984$rain > 1.65, ]
tidy_EP_rain_1992_salient <- tidy_EP_rain_1992[tidy_EP_rain_1992$rain > 1.65, ]
tidy_EP_rain_1997_salient <- tidy_EP_rain_1997[tidy_EP_rain_1997$rain > 1.65, ]
tidy_EP_rain_2004_salient <- tidy_EP_rain_2004[tidy_EP_rain_2004$rain > 1.65, ]
tidy_EP_rain_2011_salient <- tidy_EP_rain_2011[tidy_EP_rain_2011$rain > 1.65, ]
tidy_EP_rain_2017_salient <- tidy_EP_rain_2017[tidy_EP_rain_2017$rain > 1.65, ]



# list of salient
rain_chunk_1922 <- print(tidy_EP_rain_1922_salient$doc_id)
rain_chunk_1927 <- print(tidy_EP_rain_1927_salient$doc_id)
rain_chunk_1931 <- print(tidy_EP_rain_1931_salient$doc_id)
rain_chunk_1934 <- print(tidy_EP_rain_1934_salient$doc_id)
rain_chunk_1937 <- print(tidy_EP_rain_1937_salient$doc_id)
rain_chunk_1940 <- print(tidy_EP_rain_1940_salient$doc_id)
rain_chunk_1942 <- print(tidy_EP_rain_1942_salient$doc_id)
rain_chunk_1945 <- print(tidy_EP_rain_1945_salient$doc_id)
rain_chunk_1950 <- print(tidy_EP_rain_1950_salient$doc_id)
rain_chunk_1956 <- print(tidy_EP_rain_1956_salient$doc_id)
rain_chunk_1960 <- print(tidy_EP_rain_1960_salient$doc_id)
rain_chunk_1965 <- print(tidy_EP_rain_1965_salient$doc_id)
rain_chunk_1969 <- print(tidy_EP_rain_1969_salient$doc_id)
rain_chunk_1975 <- print(tidy_EP_rain_1975_salient$doc_id)
rain_chunk_1984 <- print(tidy_EP_rain_1984_salient$doc_id)
rain_chunk_1992 <- print(tidy_EP_rain_1992_salient$doc_id)
rain_chunk_1997 <- print(tidy_EP_rain_1997_salient$doc_id)
rain_chunk_2004 <- print(tidy_EP_rain_2004_salient$doc_id)
rain_chunk_2011 <- print(tidy_EP_rain_2011_salient$doc_id)
rain_chunk_2017 <- print(tidy_EP_rain_2017_salient$doc_id)



# select if in chunk
tidy_EP_rain_1922_select <- filter(tidy_EP_1922, chunk %in% rain_chunk_1922)
tidy_EP_rain_1927_select <- filter(tidy_EP_1927, chunk %in% rain_chunk_1927)
tidy_EP_rain_1931_select <- filter(tidy_EP_1931, chunk %in% rain_chunk_1931)
tidy_EP_rain_1934_select <- filter(tidy_EP_1934, chunk %in% rain_chunk_1934)
tidy_EP_rain_1937_select <- filter(tidy_EP_1937, chunk %in% rain_chunk_1937)
tidy_EP_rain_1940_select <- filter(tidy_EP_1940, chunk %in% rain_chunk_1940)
tidy_EP_rain_1942_select <- filter(tidy_EP_1942, chunk %in% rain_chunk_1942)
tidy_EP_rain_1945_select <- filter(tidy_EP_1945, chunk %in% rain_chunk_1945)
tidy_EP_rain_1950_select <- filter(tidy_EP_1950, chunk %in% rain_chunk_1950)
tidy_EP_rain_1956_select <- filter(tidy_EP_1956, chunk %in% rain_chunk_1956)
tidy_EP_rain_1960_select <- filter(tidy_EP_1960, chunk %in% rain_chunk_1960)
tidy_EP_rain_1965_select <- filter(tidy_EP_1965, chunk %in% rain_chunk_1965)
tidy_EP_rain_1969_select <- filter(tidy_EP_1969, chunk %in% rain_chunk_1969)
tidy_EP_rain_1975_select <- filter(tidy_EP_1975, chunk %in% rain_chunk_1975)
tidy_EP_rain_1984_select <- filter(tidy_EP_1984, chunk %in% rain_chunk_1984)
tidy_EP_rain_1992_select <- filter(tidy_EP_1992, chunk %in% rain_chunk_1992)
tidy_EP_rain_1997_select <- filter(tidy_EP_1997, chunk %in% rain_chunk_1997)
tidy_EP_rain_2004_select <- filter(tidy_EP_2004, chunk %in% rain_chunk_2004)
tidy_EP_rain_2011_select <- filter(tidy_EP_2011, chunk %in% rain_chunk_2011)
tidy_EP_rain_2017_select <- filter(tidy_EP_2017, chunk %in% rain_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_rain <- full_join(tidy_EP_rain_1922_select, tidy_EP_rain_1927_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1931_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1934_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1937_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1940_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1942_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1945_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1950_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1956_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1960_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1965_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1969_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1975_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1984_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1992_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_1997_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_2004_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_2011_select)
tidy_EP_chunks_rain <- full_join(tidy_EP_chunks_rain, tidy_EP_rain_2017_select)



#   build cmdist for antonym pair, rain

rain_closeness <- tidy_EP_chunks_rain %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )













#   immigrant


tidy_EP_immigrant_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1920)

tidy_EP_immigrant_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1920)

tidy_EP_immigrant_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1930)

tidy_EP_immigrant_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1930)

tidy_EP_immigrant_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1930)

tidy_EP_immigrant_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1940)

tidy_EP_immigrant_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1940)

tidy_EP_immigrant_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1940)

tidy_EP_immigrant_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1950)

tidy_EP_immigrant_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1950)

tidy_EP_immigrant_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1960)

tidy_EP_immigrant_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1960)

tidy_EP_immigrant_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1960)


tidy_EP_immigrant_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1970)

tidy_EP_immigrant_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1980)

tidy_EP_immigrant_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1990)


tidy_EP_immigrant_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=ngram_1990)

tidy_EP_immigrant_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=fastTM)

tidy_EP_immigrant_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=fastTM)

tidy_EP_immigrant_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("immigrant"), wv=fastTM)


# combine, need to add "year"; also add unique

tidy_EP_immigrant_1922 <- tidy_EP_immigrant_1922 %>%
  mutate(year = 1922) 

tidy_EP_immigrant_1927 <- tidy_EP_immigrant_1927 %>%
  mutate(year = 1927)

tidy_EP_immigrant_1931 <- tidy_EP_immigrant_1931 %>%
  mutate(year = 1931)

tidy_EP_immigrant_1934 <- tidy_EP_immigrant_1934 %>%
  mutate(year = 1934)

tidy_EP_immigrant_1937 <- tidy_EP_immigrant_1937 %>%
  mutate(year = 1937)

tidy_EP_immigrant_1940 <- tidy_EP_immigrant_1940 %>%
  mutate(year = 1940)

tidy_EP_immigrant_1942 <- tidy_EP_immigrant_1942 %>%
  mutate(year = 1942)

tidy_EP_immigrant_1945 <- tidy_EP_immigrant_1945 %>%
  mutate(year = 1945)

tidy_EP_immigrant_1950 <- tidy_EP_immigrant_1950 %>%
  mutate(year = 1950)

tidy_EP_immigrant_1956 <- tidy_EP_immigrant_1956 %>%
  mutate(year = 1956)

tidy_EP_immigrant_1960 <- tidy_EP_immigrant_1960 %>%
  mutate(year = 1960)

tidy_EP_immigrant_1965 <- tidy_EP_immigrant_1965 %>%
  mutate(year = 1965)


tidy_EP_immigrant_1969 <- tidy_EP_immigrant_1969 %>%
  mutate(year = 1969)

tidy_EP_immigrant_1975 <- tidy_EP_immigrant_1975 %>%
  mutate(year = 1975)

tidy_EP_immigrant_1984 <- tidy_EP_immigrant_1984 %>%
  mutate(year = 1984)

tidy_EP_immigrant_1992 <- tidy_EP_immigrant_1992 %>%
  mutate(year = 1992)

tidy_EP_immigrant_1997 <- tidy_EP_immigrant_1997 %>%
  mutate(year = 1997)

tidy_EP_immigrant_2004 <- tidy_EP_immigrant_2004 %>%
  mutate(year = 2004)

tidy_EP_immigrant_2011 <- tidy_EP_immigrant_2011 %>%
  mutate(year = 2011)

tidy_EP_immigrant_2017 <- tidy_EP_immigrant_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_immigrant_1922_salient <- tidy_EP_immigrant_1922[tidy_EP_immigrant_1922$immigrant > 1.65, ]
tidy_EP_immigrant_1927_salient <- tidy_EP_immigrant_1927[tidy_EP_immigrant_1927$immigrant > 1.65, ]
tidy_EP_immigrant_1931_salient <- tidy_EP_immigrant_1931[tidy_EP_immigrant_1931$immigrant > 1.65, ]
tidy_EP_immigrant_1934_salient <- tidy_EP_immigrant_1934[tidy_EP_immigrant_1934$immigrant > 1.65, ]
tidy_EP_immigrant_1937_salient <- tidy_EP_immigrant_1937[tidy_EP_immigrant_1937$immigrant > 1.65, ]
tidy_EP_immigrant_1940_salient <- tidy_EP_immigrant_1940[tidy_EP_immigrant_1940$immigrant > 1.65, ]
tidy_EP_immigrant_1942_salient <- tidy_EP_immigrant_1942[tidy_EP_immigrant_1942$immigrant > 1.65, ]
tidy_EP_immigrant_1945_salient <- tidy_EP_immigrant_1945[tidy_EP_immigrant_1945$immigrant > 1.65, ]
tidy_EP_immigrant_1950_salient <- tidy_EP_immigrant_1950[tidy_EP_immigrant_1950$immigrant > 1.65, ]
tidy_EP_immigrant_1956_salient <- tidy_EP_immigrant_1956[tidy_EP_immigrant_1956$immigrant > 1.65, ]
tidy_EP_immigrant_1960_salient <- tidy_EP_immigrant_1960[tidy_EP_immigrant_1960$immigrant > 1.65, ]
tidy_EP_immigrant_1965_salient <- tidy_EP_immigrant_1965[tidy_EP_immigrant_1965$immigrant > 1.65, ]
tidy_EP_immigrant_1969_salient <- tidy_EP_immigrant_1969[tidy_EP_immigrant_1969$immigrant > 1.65, ]
tidy_EP_immigrant_1975_salient <- tidy_EP_immigrant_1975[tidy_EP_immigrant_1975$immigrant > 1.65, ]
tidy_EP_immigrant_1984_salient <- tidy_EP_immigrant_1984[tidy_EP_immigrant_1984$immigrant > 1.65, ]
tidy_EP_immigrant_1992_salient <- tidy_EP_immigrant_1992[tidy_EP_immigrant_1992$immigrant > 1.65, ]
tidy_EP_immigrant_1997_salient <- tidy_EP_immigrant_1997[tidy_EP_immigrant_1997$immigrant > 1.65, ]
tidy_EP_immigrant_2004_salient <- tidy_EP_immigrant_2004[tidy_EP_immigrant_2004$immigrant > 1.65, ]
tidy_EP_immigrant_2011_salient <- tidy_EP_immigrant_2011[tidy_EP_immigrant_2011$immigrant > 1.65, ]
tidy_EP_immigrant_2017_salient <- tidy_EP_immigrant_2017[tidy_EP_immigrant_2017$immigrant > 1.65, ]



# list of salient
immigrant_chunk_1922 <- print(tidy_EP_immigrant_1922_salient$doc_id)
immigrant_chunk_1927 <- print(tidy_EP_immigrant_1927_salient$doc_id)
immigrant_chunk_1931 <- print(tidy_EP_immigrant_1931_salient$doc_id)
immigrant_chunk_1934 <- print(tidy_EP_immigrant_1934_salient$doc_id)
immigrant_chunk_1937 <- print(tidy_EP_immigrant_1937_salient$doc_id)
immigrant_chunk_1940 <- print(tidy_EP_immigrant_1940_salient$doc_id)
immigrant_chunk_1942 <- print(tidy_EP_immigrant_1942_salient$doc_id)
immigrant_chunk_1945 <- print(tidy_EP_immigrant_1945_salient$doc_id)
immigrant_chunk_1950 <- print(tidy_EP_immigrant_1950_salient$doc_id)
immigrant_chunk_1956 <- print(tidy_EP_immigrant_1956_salient$doc_id)
immigrant_chunk_1960 <- print(tidy_EP_immigrant_1960_salient$doc_id)
immigrant_chunk_1965 <- print(tidy_EP_immigrant_1965_salient$doc_id)
immigrant_chunk_1969 <- print(tidy_EP_immigrant_1969_salient$doc_id)
immigrant_chunk_1975 <- print(tidy_EP_immigrant_1975_salient$doc_id)
immigrant_chunk_1984 <- print(tidy_EP_immigrant_1984_salient$doc_id)
immigrant_chunk_1992 <- print(tidy_EP_immigrant_1992_salient$doc_id)
immigrant_chunk_1997 <- print(tidy_EP_immigrant_1997_salient$doc_id)
immigrant_chunk_2004 <- print(tidy_EP_immigrant_2004_salient$doc_id)
immigrant_chunk_2011 <- print(tidy_EP_immigrant_2011_salient$doc_id)
immigrant_chunk_2017 <- print(tidy_EP_immigrant_2017_salient$doc_id)



# select if in chunk
tidy_EP_immigrant_1922_select <- filter(tidy_EP_1922, chunk %in% immigrant_chunk_1922)
tidy_EP_immigrant_1927_select <- filter(tidy_EP_1927, chunk %in% immigrant_chunk_1927)
tidy_EP_immigrant_1931_select <- filter(tidy_EP_1931, chunk %in% immigrant_chunk_1931)
tidy_EP_immigrant_1934_select <- filter(tidy_EP_1934, chunk %in% immigrant_chunk_1934)
tidy_EP_immigrant_1937_select <- filter(tidy_EP_1937, chunk %in% immigrant_chunk_1937)
tidy_EP_immigrant_1940_select <- filter(tidy_EP_1940, chunk %in% immigrant_chunk_1940)
tidy_EP_immigrant_1942_select <- filter(tidy_EP_1942, chunk %in% immigrant_chunk_1942)
tidy_EP_immigrant_1945_select <- filter(tidy_EP_1945, chunk %in% immigrant_chunk_1945)
tidy_EP_immigrant_1950_select <- filter(tidy_EP_1950, chunk %in% immigrant_chunk_1950)
tidy_EP_immigrant_1956_select <- filter(tidy_EP_1956, chunk %in% immigrant_chunk_1956)
tidy_EP_immigrant_1960_select <- filter(tidy_EP_1960, chunk %in% immigrant_chunk_1960)
tidy_EP_immigrant_1965_select <- filter(tidy_EP_1965, chunk %in% immigrant_chunk_1965)
tidy_EP_immigrant_1969_select <- filter(tidy_EP_1969, chunk %in% immigrant_chunk_1969)
tidy_EP_immigrant_1975_select <- filter(tidy_EP_1975, chunk %in% immigrant_chunk_1975)
tidy_EP_immigrant_1984_select <- filter(tidy_EP_1984, chunk %in% immigrant_chunk_1984)
tidy_EP_immigrant_1992_select <- filter(tidy_EP_1992, chunk %in% immigrant_chunk_1992)
tidy_EP_immigrant_1997_select <- filter(tidy_EP_1997, chunk %in% immigrant_chunk_1997)
tidy_EP_immigrant_2004_select <- filter(tidy_EP_2004, chunk %in% immigrant_chunk_2004)
tidy_EP_immigrant_2011_select <- filter(tidy_EP_2011, chunk %in% immigrant_chunk_2011)
tidy_EP_immigrant_2017_select <- filter(tidy_EP_2017, chunk %in% immigrant_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_immigrant <- full_join(tidy_EP_immigrant_1922_select, tidy_EP_immigrant_1927_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1931_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1934_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1937_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1940_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1942_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1945_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1950_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1956_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1960_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1965_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1969_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1975_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1984_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1992_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_1997_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_2004_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_2011_select)
tidy_EP_chunks_immigrant <- full_join(tidy_EP_chunks_immigrant, tidy_EP_immigrant_2017_select)



#   build cmdist for antonym pair, immigrant

immigrant_closeness <- tidy_EP_chunks_immigrant %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )










#    morality


tidy_EP_morality_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1920)

tidy_EP_morality_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1920)

tidy_EP_morality_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1930)

tidy_EP_morality_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1930)

tidy_EP_morality_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1930)

tidy_EP_morality_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1940)

tidy_EP_morality_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1940)

tidy_EP_morality_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1940)

tidy_EP_morality_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1950)

tidy_EP_morality_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1950)

tidy_EP_morality_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1960)

tidy_EP_morality_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1960)

tidy_EP_morality_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1960)


tidy_EP_morality_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1970)

tidy_EP_morality_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1980)

tidy_EP_morality_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1990)


tidy_EP_morality_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=ngram_1990)

tidy_EP_morality_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=fastTM)

tidy_EP_morality_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=fastTM)

tidy_EP_morality_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("morality"), wv=fastTM)


# combine, need to add "year"; also add unique

tidy_EP_morality_1922 <- tidy_EP_morality_1922 %>%
  mutate(year = 1922) 

tidy_EP_morality_1927 <- tidy_EP_morality_1927 %>%
  mutate(year = 1927)

tidy_EP_morality_1931 <- tidy_EP_morality_1931 %>%
  mutate(year = 1931)

tidy_EP_morality_1934 <- tidy_EP_morality_1934 %>%
  mutate(year = 1934)

tidy_EP_morality_1937 <- tidy_EP_morality_1937 %>%
  mutate(year = 1937)

tidy_EP_morality_1940 <- tidy_EP_morality_1940 %>%
  mutate(year = 1940)

tidy_EP_morality_1942 <- tidy_EP_morality_1942 %>%
  mutate(year = 1942)

tidy_EP_morality_1945 <- tidy_EP_morality_1945 %>%
  mutate(year = 1945)

tidy_EP_morality_1950 <- tidy_EP_morality_1950 %>%
  mutate(year = 1950)

tidy_EP_morality_1956 <- tidy_EP_morality_1956 %>%
  mutate(year = 1956)

tidy_EP_morality_1960 <- tidy_EP_morality_1960 %>%
  mutate(year = 1960)

tidy_EP_morality_1965 <- tidy_EP_morality_1965 %>%
  mutate(year = 1965)


tidy_EP_morality_1969 <- tidy_EP_morality_1969 %>%
  mutate(year = 1969)

tidy_EP_morality_1975 <- tidy_EP_morality_1975 %>%
  mutate(year = 1975)

tidy_EP_morality_1984 <- tidy_EP_morality_1984 %>%
  mutate(year = 1984)

tidy_EP_morality_1992 <- tidy_EP_morality_1992 %>%
  mutate(year = 1992)

tidy_EP_morality_1997 <- tidy_EP_morality_1997 %>%
  mutate(year = 1997)

tidy_EP_morality_2004 <- tidy_EP_morality_2004 %>%
  mutate(year = 2004)

tidy_EP_morality_2011 <- tidy_EP_morality_2011 %>%
  mutate(year = 2011)

tidy_EP_morality_2017 <- tidy_EP_morality_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_morality_1922_salient <- tidy_EP_morality_1922[tidy_EP_morality_1922$morality > 1.65, ]
tidy_EP_morality_1927_salient <- tidy_EP_morality_1927[tidy_EP_morality_1927$morality > 1.65, ]
tidy_EP_morality_1931_salient <- tidy_EP_morality_1931[tidy_EP_morality_1931$morality > 1.65, ]
tidy_EP_morality_1934_salient <- tidy_EP_morality_1934[tidy_EP_morality_1934$morality > 1.65, ]
tidy_EP_morality_1937_salient <- tidy_EP_morality_1937[tidy_EP_morality_1937$morality > 1.65, ]
tidy_EP_morality_1940_salient <- tidy_EP_morality_1940[tidy_EP_morality_1940$morality > 1.65, ]
tidy_EP_morality_1942_salient <- tidy_EP_morality_1942[tidy_EP_morality_1942$morality > 1.65, ]
tidy_EP_morality_1945_salient <- tidy_EP_morality_1945[tidy_EP_morality_1945$morality > 1.65, ]
tidy_EP_morality_1950_salient <- tidy_EP_morality_1950[tidy_EP_morality_1950$morality > 1.65, ]
tidy_EP_morality_1956_salient <- tidy_EP_morality_1956[tidy_EP_morality_1956$morality > 1.65, ]
tidy_EP_morality_1960_salient <- tidy_EP_morality_1960[tidy_EP_morality_1960$morality > 1.65, ]
tidy_EP_morality_1965_salient <- tidy_EP_morality_1965[tidy_EP_morality_1965$morality > 1.65, ]
tidy_EP_morality_1969_salient <- tidy_EP_morality_1969[tidy_EP_morality_1969$morality > 1.65, ]
tidy_EP_morality_1975_salient <- tidy_EP_morality_1975[tidy_EP_morality_1975$morality > 1.65, ]
tidy_EP_morality_1984_salient <- tidy_EP_morality_1984[tidy_EP_morality_1984$morality > 1.65, ]
tidy_EP_morality_1992_salient <- tidy_EP_morality_1992[tidy_EP_morality_1992$morality > 1.65, ]
tidy_EP_morality_1997_salient <- tidy_EP_morality_1997[tidy_EP_morality_1997$morality > 1.65, ]
tidy_EP_morality_2004_salient <- tidy_EP_morality_2004[tidy_EP_morality_2004$morality > 1.65, ]
tidy_EP_morality_2011_salient <- tidy_EP_morality_2011[tidy_EP_morality_2011$morality > 1.65, ]
tidy_EP_morality_2017_salient <- tidy_EP_morality_2017[tidy_EP_morality_2017$morality > 1.65, ]



# list of salient
morality_chunk_1922 <- print(tidy_EP_morality_1922_salient$doc_id)
morality_chunk_1927 <- print(tidy_EP_morality_1927_salient$doc_id)
morality_chunk_1931 <- print(tidy_EP_morality_1931_salient$doc_id)
morality_chunk_1934 <- print(tidy_EP_morality_1934_salient$doc_id)
morality_chunk_1937 <- print(tidy_EP_morality_1937_salient$doc_id)
morality_chunk_1940 <- print(tidy_EP_morality_1940_salient$doc_id)
morality_chunk_1942 <- print(tidy_EP_morality_1942_salient$doc_id)
morality_chunk_1945 <- print(tidy_EP_morality_1945_salient$doc_id)
morality_chunk_1950 <- print(tidy_EP_morality_1950_salient$doc_id)
morality_chunk_1956 <- print(tidy_EP_morality_1956_salient$doc_id)
morality_chunk_1960 <- print(tidy_EP_morality_1960_salient$doc_id)
morality_chunk_1965 <- print(tidy_EP_morality_1965_salient$doc_id)
morality_chunk_1969 <- print(tidy_EP_morality_1969_salient$doc_id)
morality_chunk_1975 <- print(tidy_EP_morality_1975_salient$doc_id)
morality_chunk_1984 <- print(tidy_EP_morality_1984_salient$doc_id)
morality_chunk_1992 <- print(tidy_EP_morality_1992_salient$doc_id)
morality_chunk_1997 <- print(tidy_EP_morality_1997_salient$doc_id)
morality_chunk_2004 <- print(tidy_EP_morality_2004_salient$doc_id)
morality_chunk_2011 <- print(tidy_EP_morality_2011_salient$doc_id)
morality_chunk_2017 <- print(tidy_EP_morality_2017_salient$doc_id)



# select if in chunk
tidy_EP_morality_1922_select <- filter(tidy_EP_1922, chunk %in% morality_chunk_1922)
tidy_EP_morality_1927_select <- filter(tidy_EP_1927, chunk %in% morality_chunk_1927)
tidy_EP_morality_1931_select <- filter(tidy_EP_1931, chunk %in% morality_chunk_1931)
tidy_EP_morality_1934_select <- filter(tidy_EP_1934, chunk %in% morality_chunk_1934)
tidy_EP_morality_1937_select <- filter(tidy_EP_1937, chunk %in% morality_chunk_1937)
tidy_EP_morality_1940_select <- filter(tidy_EP_1940, chunk %in% morality_chunk_1940)
tidy_EP_morality_1942_select <- filter(tidy_EP_1942, chunk %in% morality_chunk_1942)
tidy_EP_morality_1945_select <- filter(tidy_EP_1945, chunk %in% morality_chunk_1945)
tidy_EP_morality_1950_select <- filter(tidy_EP_1950, chunk %in% morality_chunk_1950)
tidy_EP_morality_1956_select <- filter(tidy_EP_1956, chunk %in% morality_chunk_1956)
tidy_EP_morality_1960_select <- filter(tidy_EP_1960, chunk %in% morality_chunk_1960)
tidy_EP_morality_1965_select <- filter(tidy_EP_1965, chunk %in% morality_chunk_1965)
tidy_EP_morality_1969_select <- filter(tidy_EP_1969, chunk %in% morality_chunk_1969)
tidy_EP_morality_1975_select <- filter(tidy_EP_1975, chunk %in% morality_chunk_1975)
tidy_EP_morality_1984_select <- filter(tidy_EP_1984, chunk %in% morality_chunk_1984)
tidy_EP_morality_1992_select <- filter(tidy_EP_1992, chunk %in% morality_chunk_1992)
tidy_EP_morality_1997_select <- filter(tidy_EP_1997, chunk %in% morality_chunk_1997)
tidy_EP_morality_2004_select <- filter(tidy_EP_2004, chunk %in% morality_chunk_2004)
tidy_EP_morality_2011_select <- filter(tidy_EP_2011, chunk %in% morality_chunk_2011)
tidy_EP_morality_2017_select <- filter(tidy_EP_2017, chunk %in% morality_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_morality <- full_join(tidy_EP_morality_1922_select, tidy_EP_morality_1927_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1931_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1934_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1937_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1940_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1942_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1945_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1950_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1956_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1960_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1965_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1969_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1975_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1984_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1992_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_1997_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_2004_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_2011_select)
tidy_EP_chunks_morality <- full_join(tidy_EP_chunks_morality, tidy_EP_morality_2017_select)



#   build cmdist for antonym pair, morality

morality_closeness <- tidy_EP_chunks_morality %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )




















#  women



tidy_EP_women_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1920)

tidy_EP_women_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1920)

tidy_EP_women_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1930)

tidy_EP_women_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1930)

tidy_EP_women_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1930)

tidy_EP_women_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1940)

tidy_EP_women_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1940)

tidy_EP_women_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1940)

tidy_EP_women_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1950)

tidy_EP_women_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1950)

tidy_EP_women_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1960)

tidy_EP_women_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1960)

tidy_EP_women_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1960)


tidy_EP_women_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1970)

tidy_EP_women_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1980)

tidy_EP_women_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1990)


tidy_EP_women_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=ngram_1990)

tidy_EP_women_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=fastTM)

tidy_EP_women_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=fastTM)

tidy_EP_women_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("women"), wv=fastTM)


# combine, need to add "year"; also add unique

tidy_EP_women_1922 <- tidy_EP_women_1922 %>%
  mutate(year = 1922) 

tidy_EP_women_1927 <- tidy_EP_women_1927 %>%
  mutate(year = 1927)

tidy_EP_women_1931 <- tidy_EP_women_1931 %>%
  mutate(year = 1931)

tidy_EP_women_1934 <- tidy_EP_women_1934 %>%
  mutate(year = 1934)

tidy_EP_women_1937 <- tidy_EP_women_1937 %>%
  mutate(year = 1937)

tidy_EP_women_1940 <- tidy_EP_women_1940 %>%
  mutate(year = 1940)

tidy_EP_women_1942 <- tidy_EP_women_1942 %>%
  mutate(year = 1942)

tidy_EP_women_1945 <- tidy_EP_women_1945 %>%
  mutate(year = 1945)

tidy_EP_women_1950 <- tidy_EP_women_1950 %>%
  mutate(year = 1950)

tidy_EP_women_1956 <- tidy_EP_women_1956 %>%
  mutate(year = 1956)

tidy_EP_women_1960 <- tidy_EP_women_1960 %>%
  mutate(year = 1960)

tidy_EP_women_1965 <- tidy_EP_women_1965 %>%
  mutate(year = 1965)


tidy_EP_women_1969 <- tidy_EP_women_1969 %>%
  mutate(year = 1969)

tidy_EP_women_1975 <- tidy_EP_women_1975 %>%
  mutate(year = 1975)

tidy_EP_women_1984 <- tidy_EP_women_1984 %>%
  mutate(year = 1984)

tidy_EP_women_1992 <- tidy_EP_women_1992 %>%
  mutate(year = 1992)

tidy_EP_women_1997 <- tidy_EP_women_1997 %>%
  mutate(year = 1997)

tidy_EP_women_2004 <- tidy_EP_women_2004 %>%
  mutate(year = 2004)

tidy_EP_women_2011 <- tidy_EP_women_2011 %>%
  mutate(year = 2011)

tidy_EP_women_2017 <- tidy_EP_women_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_women_1922_salient <- tidy_EP_women_1922[tidy_EP_women_1922$women > 1.65, ]
tidy_EP_women_1927_salient <- tidy_EP_women_1927[tidy_EP_women_1927$women > 1.65, ]
tidy_EP_women_1931_salient <- tidy_EP_women_1931[tidy_EP_women_1931$women > 1.65, ]
tidy_EP_women_1934_salient <- tidy_EP_women_1934[tidy_EP_women_1934$women > 1.65, ]
tidy_EP_women_1937_salient <- tidy_EP_women_1937[tidy_EP_women_1937$women > 1.65, ]
tidy_EP_women_1940_salient <- tidy_EP_women_1940[tidy_EP_women_1940$women > 1.65, ]
tidy_EP_women_1942_salient <- tidy_EP_women_1942[tidy_EP_women_1942$women > 1.65, ]
tidy_EP_women_1945_salient <- tidy_EP_women_1945[tidy_EP_women_1945$women > 1.65, ]
tidy_EP_women_1950_salient <- tidy_EP_women_1950[tidy_EP_women_1950$women > 1.65, ]
tidy_EP_women_1956_salient <- tidy_EP_women_1956[tidy_EP_women_1956$women > 1.65, ]
tidy_EP_women_1960_salient <- tidy_EP_women_1960[tidy_EP_women_1960$women > 1.65, ]
tidy_EP_women_1965_salient <- tidy_EP_women_1965[tidy_EP_women_1965$women > 1.65, ]
tidy_EP_women_1969_salient <- tidy_EP_women_1969[tidy_EP_women_1969$women > 1.65, ]
tidy_EP_women_1975_salient <- tidy_EP_women_1975[tidy_EP_women_1975$women > 1.65, ]
tidy_EP_women_1984_salient <- tidy_EP_women_1984[tidy_EP_women_1984$women > 1.65, ]
tidy_EP_women_1992_salient <- tidy_EP_women_1992[tidy_EP_women_1992$women > 1.65, ]
tidy_EP_women_1997_salient <- tidy_EP_women_1997[tidy_EP_women_1997$women > 1.65, ]
tidy_EP_women_2004_salient <- tidy_EP_women_2004[tidy_EP_women_2004$women > 1.65, ]
tidy_EP_women_2011_salient <- tidy_EP_women_2011[tidy_EP_women_2011$women > 1.65, ]
tidy_EP_women_2017_salient <- tidy_EP_women_2017[tidy_EP_women_2017$women > 1.65, ]



# list of salient
women_chunk_1922 <- print(tidy_EP_women_1922_salient$doc_id)
women_chunk_1927 <- print(tidy_EP_women_1927_salient$doc_id)
women_chunk_1931 <- print(tidy_EP_women_1931_salient$doc_id)
women_chunk_1934 <- print(tidy_EP_women_1934_salient$doc_id)
women_chunk_1937 <- print(tidy_EP_women_1937_salient$doc_id)
women_chunk_1940 <- print(tidy_EP_women_1940_salient$doc_id)
women_chunk_1942 <- print(tidy_EP_women_1942_salient$doc_id)
women_chunk_1945 <- print(tidy_EP_women_1945_salient$doc_id)
women_chunk_1950 <- print(tidy_EP_women_1950_salient$doc_id)
women_chunk_1956 <- print(tidy_EP_women_1956_salient$doc_id)
women_chunk_1960 <- print(tidy_EP_women_1960_salient$doc_id)
women_chunk_1965 <- print(tidy_EP_women_1965_salient$doc_id)
women_chunk_1969 <- print(tidy_EP_women_1969_salient$doc_id)
women_chunk_1975 <- print(tidy_EP_women_1975_salient$doc_id)
women_chunk_1984 <- print(tidy_EP_women_1984_salient$doc_id)
women_chunk_1992 <- print(tidy_EP_women_1992_salient$doc_id)
women_chunk_1997 <- print(tidy_EP_women_1997_salient$doc_id)
women_chunk_2004 <- print(tidy_EP_women_2004_salient$doc_id)
women_chunk_2011 <- print(tidy_EP_women_2011_salient$doc_id)
women_chunk_2017 <- print(tidy_EP_women_2017_salient$doc_id)



# select if in chunk
tidy_EP_women_1922_select <- filter(tidy_EP_1922, chunk %in% women_chunk_1922)
tidy_EP_women_1927_select <- filter(tidy_EP_1927, chunk %in% women_chunk_1927)
tidy_EP_women_1931_select <- filter(tidy_EP_1931, chunk %in% women_chunk_1931)
tidy_EP_women_1934_select <- filter(tidy_EP_1934, chunk %in% women_chunk_1934)
tidy_EP_women_1937_select <- filter(tidy_EP_1937, chunk %in% women_chunk_1937)
tidy_EP_women_1940_select <- filter(tidy_EP_1940, chunk %in% women_chunk_1940)
tidy_EP_women_1942_select <- filter(tidy_EP_1942, chunk %in% women_chunk_1942)
tidy_EP_women_1945_select <- filter(tidy_EP_1945, chunk %in% women_chunk_1945)
tidy_EP_women_1950_select <- filter(tidy_EP_1950, chunk %in% women_chunk_1950)
tidy_EP_women_1956_select <- filter(tidy_EP_1956, chunk %in% women_chunk_1956)
tidy_EP_women_1960_select <- filter(tidy_EP_1960, chunk %in% women_chunk_1960)
tidy_EP_women_1965_select <- filter(tidy_EP_1965, chunk %in% women_chunk_1965)
tidy_EP_women_1969_select <- filter(tidy_EP_1969, chunk %in% women_chunk_1969)
tidy_EP_women_1975_select <- filter(tidy_EP_1975, chunk %in% women_chunk_1975)
tidy_EP_women_1984_select <- filter(tidy_EP_1984, chunk %in% women_chunk_1984)
tidy_EP_women_1992_select <- filter(tidy_EP_1992, chunk %in% women_chunk_1992)
tidy_EP_women_1997_select <- filter(tidy_EP_1997, chunk %in% women_chunk_1997)
tidy_EP_women_2004_select <- filter(tidy_EP_2004, chunk %in% women_chunk_2004)
tidy_EP_women_2011_select <- filter(tidy_EP_2011, chunk %in% women_chunk_2011)
tidy_EP_women_2017_select <- filter(tidy_EP_2017, chunk %in% women_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_women <- full_join(tidy_EP_women_1922_select, tidy_EP_women_1927_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1931_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1934_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1937_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1940_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1942_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1945_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1950_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1956_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1960_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1965_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1969_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1975_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1984_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1992_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_1997_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_2004_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_2011_select)
tidy_EP_chunks_women <- full_join(tidy_EP_chunks_women, tidy_EP_women_2017_select)



#   build cmdist for antonym pair, women

women_closeness <- tidy_EP_chunks_women %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )








#  Make CSV, rename first

table_closeness <- mutate(table_closeness,  table.normal  = normal_pole)
friend_closeness  <- mutate(friend_closeness,   friend.normal   = normal_pole)
book_closeness   <- mutate(book_closeness,    book.normal    = normal_pole)
rain_closeness<- mutate(rain_closeness, rain.normal = normal_pole)
immigrant_closeness <- mutate(immigrant_closeness,  immigrant.normal  = normal_pole)
morality_closeness <- mutate(morality_closeness,  morality.normal  = normal_pole)
women_closeness  <- mutate(women_closeness,   women.normal   = normal_pole)


table_closeness  <- select(table_closeness,  -(normal_pole))
friend_closeness   <- select(friend_closeness,   -(normal_pole))
book_closeness    <- select(book_closeness,    -(normal_pole))
rain_closeness <- select(rain_closeness, -(normal_pole))
immigrant_closeness  <- select(immigrant_closeness,  -(normal_pole))
morality_closeness  <- select(morality_closeness,  -(normal_pole))
women_closeness   <- select(women_closeness,   -(normal_pole))


immigrant_closeness <- full_join(table_closeness,   friend_closeness)
immigrant_closeness <- full_join(immigrant_closeness, book_closeness)
immigrant_closeness <- full_join(immigrant_closeness, rain_closeness)
immigrant_closeness <- full_join(immigrant_closeness, immigrant_closeness)
immigrant_closeness <- full_join(immigrant_closeness, morality_closeness)
immigrant_closeness <- full_join(immigrant_closeness, women_closeness)


















# 10 combine closeness and standardize scores across all pseudo-documents
tidy_EP_chunks_table <-   mutate(tidy_EP_chunks_table,   chunk_unique = year+.1)
tidy_EP_chunks_friend <-    mutate(tidy_EP_chunks_friend,    chunk_unique = year+.2)
tidy_EP_chunks_book <-     mutate(tidy_EP_chunks_book,     chunk_unique = year+.3)
tidy_EP_chunks_rain <-  mutate(tidy_EP_chunks_rain,  chunk_unique = year+.4)
tidy_EP_chunks_immigrant <-   mutate(tidy_EP_chunks_immigrant,   chunk_unique = year+.5)
tidy_EP_chunks_morality <-   mutate(tidy_EP_chunks_morality,   chunk_unique = year+.6)
tidy_EP_chunks_women <-    mutate(tidy_EP_chunks_women,    chunk_unique = year+.8)



tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_table, tidy_EP_chunks_friend)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_book)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_rain)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_immigrant)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_morality)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_women)


#combine full set


tidy_EP_chunks_combined_closeness <- tidy_EP_chunks_combined %>% 
  cast_dtm(term = word, 
           document = chunk_unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )

tidy_EP_chunks_combined_closeness <- mutate(tidy_EP_chunks_combined_closeness, year = substr(doc_id, 1, 4))
tidy_EP_chunks_combined_closeness <- mutate(tidy_EP_chunks_combined_closeness, group = -100 * (as.numeric(year)-as.numeric(doc_id)))





#    11 determine sentiment scores


#Create dataframe with sentiment scores

sent_scores <- data.frame("year" = c(1922, 1927, 1931, 1934, 
                                     1937, 1940, 1942, 1945, 
                                     1950, 1956, 1960, 1965, 
                                     1969, 1975, 1984, 1992, 
                                     1997, 2004, 2011, 2017),
"rain_group" = 
  c(mean(tidy_EP_rain_1922_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment), 
    mean(tidy_EP_rain_1927_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1931_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1934_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1937_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1940_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1942_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1945_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1950_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1956_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1960_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1965_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1969_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1975_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1984_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1992_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_1997_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_2004_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_2011_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment),
    mean(tidy_EP_rain_2017_select$mean_sentiment- tidy_EP_chunks_rain$mean_sentiment)),
"morality_group" =  
  c(mean(tidy_EP_morality_1922_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ), 
    mean(tidy_EP_morality_1927_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1931_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1934_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1937_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1940_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1942_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1945_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1950_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1956_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1960_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1965_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1969_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1975_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1984_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1992_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_1997_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_2004_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_2011_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment ),
    mean(tidy_EP_morality_2017_select$mean_sentiment - tidy_EP_chunks_morality$mean_sentiment )),
"book_group" =    
 c(mean(tidy_EP_book_1922_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ), 
   mean(tidy_EP_book_1927_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1931_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1934_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1937_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1940_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1942_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1945_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1950_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1956_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1960_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1965_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1969_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1975_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1984_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1992_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_1997_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_2004_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_2011_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   ),
   mean(tidy_EP_book_2017_select$mean_sentiment   - tidy_EP_chunks_book$mean_sentiment   )),
"table_group" =  
  c(mean(tidy_EP_table_1922_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ), 
    mean(tidy_EP_table_1927_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1931_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1934_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1937_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1940_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1942_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1945_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1950_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1956_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1960_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1965_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1969_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1975_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1984_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1992_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_1997_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_2004_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_2011_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment ),
    mean(tidy_EP_table_2017_select$mean_sentiment - tidy_EP_chunks_table$mean_sentiment )),
"friend_group" =   
  c(mean(tidy_EP_friend_1922_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ), 
    mean(tidy_EP_friend_1927_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1931_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1934_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1937_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1940_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1942_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1945_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1950_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1956_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1960_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1965_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1969_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1975_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1984_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1992_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_1997_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_2004_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_2011_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  ),
    mean(tidy_EP_friend_2017_select$mean_sentiment  - tidy_EP_chunks_friend$mean_sentiment  )),
"immigrant_group" =  
  c(mean(tidy_EP_immigrant_1922_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ), 
    mean(tidy_EP_immigrant_1927_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1931_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1934_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1937_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1940_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1942_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1945_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1950_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1956_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1960_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1965_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1969_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1975_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1984_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1992_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_1997_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_2004_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_2011_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment ),
    mean(tidy_EP_immigrant_2017_select$mean_sentiment - tidy_EP_chunks_immigrant$mean_sentiment )),
"women_group" =   
  c(mean(tidy_EP_women_1922_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ), 
    mean(tidy_EP_women_1927_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1931_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1934_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1937_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1940_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1942_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1945_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1950_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1956_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1960_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1965_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1969_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1975_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1984_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1992_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_1997_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_2004_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_2011_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  ),
    mean(tidy_EP_women_2017_select$mean_sentiment  - tidy_EP_chunks_women$mean_sentiment  )),
"rain_combined" = 
  c(mean(tidy_EP_rain_1922_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment), 
    mean(tidy_EP_rain_1927_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1931_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1934_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1937_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1940_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1942_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1945_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1950_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1956_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1960_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1965_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1969_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1975_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1984_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1992_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_1997_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_2004_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_2011_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment),
    mean(tidy_EP_rain_2017_select$mean_sentiment- tidy_EP_chunks_combined$mean_sentiment)),
"morality_combined" =  
  c(mean(tidy_EP_morality_1922_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ), 
    mean(tidy_EP_morality_1927_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1931_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1934_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1937_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1940_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1942_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1945_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1950_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1956_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1960_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1965_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1969_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1975_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1984_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1992_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_1997_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_2004_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_2011_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_morality_2017_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment )),
"book_combined" =    
  c(mean(tidy_EP_book_1922_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ), 
    mean(tidy_EP_book_1927_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1931_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1934_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1937_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1940_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1942_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1945_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1950_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1956_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1960_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1965_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1969_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1975_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1984_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1992_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_1997_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_2004_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_2011_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   ),
    mean(tidy_EP_book_2017_select$mean_sentiment   - tidy_EP_chunks_combined$mean_sentiment   )),
"table_combined" =  
  c(mean(tidy_EP_table_1922_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ), 
    mean(tidy_EP_table_1927_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1931_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1934_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1937_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1940_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1942_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1945_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1950_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1956_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1960_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1965_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1969_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1975_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1984_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1992_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_1997_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_2004_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_2011_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_table_2017_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment )),
"friend_combined" =   
  c(mean(tidy_EP_friend_1922_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ), 
    mean(tidy_EP_friend_1927_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1931_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1934_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1937_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1940_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1942_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1945_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1950_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1956_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1960_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1965_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1969_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1975_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1984_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1992_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_1997_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_2004_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_2011_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_friend_2017_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  )),
"immigrant_combined" =  
  c(mean(tidy_EP_immigrant_1922_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ), 
    mean(tidy_EP_immigrant_1927_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1931_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1934_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1937_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1940_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1942_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1945_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1950_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1956_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1960_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1965_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1969_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1975_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1984_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1992_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_1997_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_2004_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_2011_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment ),
    mean(tidy_EP_immigrant_2017_select$mean_sentiment - tidy_EP_chunks_combined$mean_sentiment )),
"women_combined" =   
  c(mean(tidy_EP_women_1922_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ), 
    mean(tidy_EP_women_1927_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1931_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1934_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1937_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1940_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1942_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1945_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1950_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1956_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1960_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1965_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1969_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1975_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1984_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1992_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_1997_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_2004_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_2011_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  ),
    mean(tidy_EP_women_2017_select$mean_sentiment  - tidy_EP_chunks_combined$mean_sentiment  )))

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

sent_scores[is.nan(sent_scores)] <- NA

# create tidy data frame for plotting




sent_scores_group  <- select(sent_scores,  "year", 
                             "rain_group", "book_group", 
                             "table_group", "friend_group", 
                             "morality_group",
                             "immigrant_group", "women_group")


sent_scores_tidy_group <- reshape2::melt(sent_scores, id.var = "year")



sent_scores_combined  <- select(sent_scores,  "year", 
                                "rain_combined", "book_combined", 
                                "table_combined", "friend_combined", 
                                "morality_combined", 
                                "immigrant_combined", "women_combined")


sent_scores_tidy_combined <- reshape2::melt(sent_scores_combined, id.var = "year")


# groups
sent_scores_tidy_rain <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="rain_group"),]
sent_scores_tidy_morality <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="morality_group"),]
sent_scores_tidy_book <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="book_group"),]
sent_scores_tidy_table <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="table_group"),] 
sent_scores_tidy_friend <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="friend_group"),]
sent_scores_tidy_immigrant <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="immigrant_group"),]
sent_scores_tidy_women <- sent_scores_tidy_group [c(sent_scores_tidy_group$variable =="women_group"),] 

























#14 create plot of sentiment scores

#combined
sent_scores_plot_combined <- sent_scores_tidy_combined %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
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



sent_scores_plot_table <- sent_scores_tidy_table %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  ggtitle("table") +  
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




sent_scores_plot_friend <- sent_scores_tidy_friend %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  ggtitle("friend") +  
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




sent_scores_plot_book <- sent_scores_tidy_book %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +  
  ggtitle("book") +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))



sent_scores_plot_rain <- sent_scores_tidy_rain %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  ggtitle("rain") +  
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




sent_scores_plot_immigrant <- sent_scores_tidy_immigrant %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  ggtitle("immigrant") + 
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




sent_scores_plot_morality <- sent_scores_tidy_morality %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +
  geom_smooth(color="grey50", se = TRUE ) + 
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Combined Sentiment, Mean Centered') +
  ggtitle("morality") +  
  scale_color_gradient2(low = "grey20", mid="grey50", high = "grey80" ,                      
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm")) +
  theme_classic() +
  geom_hline(aes(yintercept=0))





sent_scores_plot_women <- sent_scores_tidy_women %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  ggtitle("women") +  
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

print (sent_scores_plot_table)
print (sent_scores_plot_friend)
print (sent_scores_plot_book)
print (sent_scores_plot_rain)
print (sent_scores_plot_immigrant)
print (sent_scores_plot_morality)
print (sent_scores_plot_women)


# combine plots into groups


plot_group1_sent <- ggarrange(sent_scores_plot_table, 
                              sent_scores_plot_friend,  
                              sent_scores_plot_rain,  
                              sent_scores_plot_book, 
                         nrow = 2, ncol = 2,
                         common.legend = TRUE,
                         legend = "bottom")


print(plot_group1_sent)

## Group 2: curban, women, morality, immigrant 
plot_group2_sent <- ggarrange(sent_scores_plot_women,  
                         sent_scores_plot_morality,  
                         sent_scores_plot_immigrant, 
                         nrow = 2, ncol = 2,
                         common.legend = TRUE,
                         legend = "bottom")


print(plot_group2_sent)






############ Table and Figure summary ############


# Grouped Sentiment

print(plot_group1_sent)
print(plot_group2_sent)





############

figure_D1  <- plot_group1_sent
figure_D2 <- plot_group2_sent

save(figure_D1, file = "figure_D1.Rdata")
save(figure_D2, file = "figure_D2.Rdata")


