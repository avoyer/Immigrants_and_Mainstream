
##  EP(Emily Post): CMDist analysis
# File Name: EP-IMG
# Date:     2021_3_1
# Who:      Andrea Voyer, Zachary D. Kline, Madison Danton, Tanja Volkova

##  To do before running this script in RStudio:
#   1) copy txt from git-hub directory
#     Note 1: Text comes from 2020-12-3. All changes to the corpus made
#                 after this date are excluded from analysis.

#     1  Set 2021_2_28 Directory
#UPDATE WITH YOUR DIRECTORY!
setwd("C:/Users/zdk15001/Desktop/University_of_Connecticut/Emily_Post/Work/Analysis/")
wd <- "C:/Users/zdk15001/Desktop/University_of_Connecticut/Emily_Post/Work/Analysis/"

#     2  Install and Load Packages (if not using remote server, you may only need to do this once)
#           Some of these packages are often updated, creating conflicts. 
#                 Errors may be the result of commands that are 
#                 defined differently in multiple packages

#install.packages("tidytext")
#install.packages("tidyverse")
#install Rtools from https://cran.r-project.org/bin/windows/Rtools/
# Run function CMDist from Stoltz(2019_11_11)Concept_movers_distance_git
#install.packages("pacman")
#install.packages("devtools")
#install.packages('BiocManager')
#install.packages('stringr')
#install.packages('backports')
#install.packages("utils")
#devtools::install_github("UrbanInstitute/urbnthemes")
#devtools::install_github("statsmaths/fasttextM")           
#devtools::install_github("lionel-/ggstance")
#devtools::install_github("quanteda/quanteda.corpora")
#devtools::install_github("dustinstoltz/CMDist")
#devtools::install_github("mukul13/rword2vec")


#   Load in packages
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
library(CMDist)
library(text2vec)##
library(fasttextM)
library(urbnthemes)##
library(utils)

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


# only need to run once per directory; gathers word embeddings from fasttextM
#ft_download_model("en", mb = 3000, location = wd)

fastTM <- readRDS("en.Rds")

# load word embeddings from fasttexM
ft_load_model("en", location = wd)

#     3  Load in and combine text into "source_EP"

# Folder where editions.txt are stored (Change as updated)
path2file <- "txt_2020_12_3"

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

#   4 Create tidy data set

tidy_EP <- source_EP %>% 
  mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = replace_white(text)) %>%
  mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
  mutate(text = replace_number(text))  %>%
  unnest_tokens(word, text, to_lower = TRUE) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]+") ) 

#   5 Rename filename 

tidy_EP <- mutate(tidy_EP, filename =  
           ifelse(grepl("txt_2020_12_3/1922EditionFull.txt", filename), "1922",
           ifelse(grepl("txt_2020_12_3/1927EditionFull.txt", filename), "1927",
           ifelse(grepl("txt_2020_12_3/1931EditionFull.txt", filename), "1931",
           ifelse(grepl("txt_2020_12_3/1934EditionFull.txt", filename), "1934",
           ifelse(grepl("txt_2020_12_3/1937EditionFull.txt", filename), "1937",
           ifelse(grepl("txt_2020_12_3/1940EditionFull.txt", filename), "1940",
           ifelse(grepl("txt_2020_12_3/1942EditionFull.txt", filename), "1942",
           ifelse(grepl("txt_2020_12_3/1945EditionFull.txt", filename), "1945",
           ifelse(grepl("txt_2020_12_3/1950EditionFull.txt", filename), "1950",
           ifelse(grepl("txt_2020_12_3/1956EditionFull.txt", filename), "1956",
           ifelse(grepl("txt_2020_12_3/1960EditionFull.txt", filename), "1960",
           ifelse(grepl("txt_2020_12_3/1965EditionFull.txt", filename), "1965",
           ifelse(grepl("txt_2020_12_3/1969EditionFull.txt", filename), "1969",
           ifelse(grepl("txt_2020_12_3/1975EditionFull.txt", filename), "1975",
           ifelse(grepl("txt_2020_12_3/1984EditionFull.txt", filename), "1984",
           ifelse(grepl("txt_2020_12_3/1992EditionFull.txt", filename), "1992",
           ifelse(grepl("txt_2020_12_3/1997EditionFull.txt", filename), "1997",
           ifelse(grepl("txt_2020_12_3/2004EditionFull.txt", filename), "2004",
           ifelse(grepl("txt_2020_12_3/2011EditionFull.txt", filename), "2011",
           ifelse(grepl("txt_2020_12_3/2017EditionFull.txt", filename), "2017","x")))))))))))))))))))))


#   6 150 word chunks

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


#   7 Create ID variables for each row

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

























#   9 create immigrant direction (strange vs normal)


additions <- c("normal")
subtracts  <- c("strange")
pairs <- cbind(additions, subtracts)
immigrant.sd <- get_direction(pairs, fastTM)


#   10 Italian


tidy_EP_italian_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)


tidy_EP_italian_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)


tidy_EP_italian_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)

tidy_EP_italian_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("italian"), wv=fastTM)


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



# select on CMD > > 2 for each edition, gen unique

tidy_EP_italian_1922_salient <- tidy_EP_italian_1922[tidy_EP_italian_1922$cmd.italian > 3, ]
tidy_EP_italian_1927_salient <- tidy_EP_italian_1927[tidy_EP_italian_1927$cmd.italian > 3, ]
tidy_EP_italian_1931_salient <- tidy_EP_italian_1931[tidy_EP_italian_1931$cmd.italian > 3, ]
tidy_EP_italian_1934_salient <- tidy_EP_italian_1934[tidy_EP_italian_1934$cmd.italian > 3, ]
tidy_EP_italian_1937_salient <- tidy_EP_italian_1937[tidy_EP_italian_1937$cmd.italian > 3, ]
tidy_EP_italian_1940_salient <- tidy_EP_italian_1940[tidy_EP_italian_1940$cmd.italian > 3, ]
tidy_EP_italian_1942_salient <- tidy_EP_italian_1942[tidy_EP_italian_1942$cmd.italian > 3, ]
tidy_EP_italian_1945_salient <- tidy_EP_italian_1945[tidy_EP_italian_1945$cmd.italian > 3, ]
tidy_EP_italian_1950_salient <- tidy_EP_italian_1950[tidy_EP_italian_1950$cmd.italian > 3, ]
tidy_EP_italian_1956_salient <- tidy_EP_italian_1956[tidy_EP_italian_1956$cmd.italian > 3, ]
tidy_EP_italian_1960_salient <- tidy_EP_italian_1960[tidy_EP_italian_1960$cmd.italian > 3, ]
tidy_EP_italian_1965_salient <- tidy_EP_italian_1965[tidy_EP_italian_1965$cmd.italian > 3, ]
tidy_EP_italian_1969_salient <- tidy_EP_italian_1969[tidy_EP_italian_1969$cmd.italian > 3, ]
tidy_EP_italian_1975_salient <- tidy_EP_italian_1975[tidy_EP_italian_1975$cmd.italian > 3, ]
tidy_EP_italian_1984_salient <- tidy_EP_italian_1984[tidy_EP_italian_1984$cmd.italian > 3, ]
tidy_EP_italian_1992_salient <- tidy_EP_italian_1992[tidy_EP_italian_1992$cmd.italian > 3, ]
tidy_EP_italian_1997_salient <- tidy_EP_italian_1997[tidy_EP_italian_1997$cmd.italian > 3, ]
tidy_EP_italian_2004_salient <- tidy_EP_italian_2004[tidy_EP_italian_2004$cmd.italian > 3, ]
tidy_EP_italian_2011_salient <- tidy_EP_italian_2011[tidy_EP_italian_2011$cmd.italian > 3, ]
tidy_EP_italian_2017_salient <- tidy_EP_italian_2017[tidy_EP_italian_2017$cmd.italian > 3, ]



# list of salient
italian_chunk_1922 <- print(tidy_EP_italian_1922_salient$docs)
italian_chunk_1927 <- print(tidy_EP_italian_1927_salient$docs)
italian_chunk_1931 <- print(tidy_EP_italian_1931_salient$docs)
italian_chunk_1934 <- print(tidy_EP_italian_1934_salient$docs)
italian_chunk_1937 <- print(tidy_EP_italian_1937_salient$docs)
italian_chunk_1940 <- print(tidy_EP_italian_1940_salient$docs)
italian_chunk_1942 <- print(tidy_EP_italian_1942_salient$docs)
italian_chunk_1945 <- print(tidy_EP_italian_1945_salient$docs)
italian_chunk_1950 <- print(tidy_EP_italian_1950_salient$docs)
italian_chunk_1956 <- print(tidy_EP_italian_1956_salient$docs)
italian_chunk_1960 <- print(tidy_EP_italian_1960_salient$docs)
italian_chunk_1965 <- print(tidy_EP_italian_1965_salient$docs)
italian_chunk_1969 <- print(tidy_EP_italian_1969_salient$docs)
italian_chunk_1975 <- print(tidy_EP_italian_1975_salient$docs)
italian_chunk_1984 <- print(tidy_EP_italian_1984_salient$docs)
italian_chunk_1992 <- print(tidy_EP_italian_1992_salient$docs)
italian_chunk_1997 <- print(tidy_EP_italian_1997_salient$docs)
italian_chunk_2004 <- print(tidy_EP_italian_2004_salient$docs)
italian_chunk_2011 <- print(tidy_EP_italian_2011_salient$docs)
italian_chunk_2017 <- print(tidy_EP_italian_2017_salient$docs)



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
  CMDist(cv = immigrant.sd, wv=fastTM )




#   12 jewish


tidy_EP_jewish_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)


tidy_EP_jewish_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)


tidy_EP_jewish_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)

tidy_EP_jewish_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("jewish"), wv=fastTM)


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

tidy_EP_jewish_1922_salient <- tidy_EP_jewish_1922[tidy_EP_jewish_1922$cmd.jewish > 3, ]
tidy_EP_jewish_1927_salient <- tidy_EP_jewish_1927[tidy_EP_jewish_1927$cmd.jewish > 3, ]
tidy_EP_jewish_1931_salient <- tidy_EP_jewish_1931[tidy_EP_jewish_1931$cmd.jewish > 3, ]
tidy_EP_jewish_1934_salient <- tidy_EP_jewish_1934[tidy_EP_jewish_1934$cmd.jewish > 3, ]
tidy_EP_jewish_1937_salient <- tidy_EP_jewish_1937[tidy_EP_jewish_1937$cmd.jewish > 3, ]
tidy_EP_jewish_1940_salient <- tidy_EP_jewish_1940[tidy_EP_jewish_1940$cmd.jewish > 3, ]
tidy_EP_jewish_1942_salient <- tidy_EP_jewish_1942[tidy_EP_jewish_1942$cmd.jewish > 3, ]
tidy_EP_jewish_1945_salient <- tidy_EP_jewish_1945[tidy_EP_jewish_1945$cmd.jewish > 3, ]
tidy_EP_jewish_1950_salient <- tidy_EP_jewish_1950[tidy_EP_jewish_1950$cmd.jewish > 3, ]
tidy_EP_jewish_1956_salient <- tidy_EP_jewish_1956[tidy_EP_jewish_1956$cmd.jewish > 3, ]
tidy_EP_jewish_1960_salient <- tidy_EP_jewish_1960[tidy_EP_jewish_1960$cmd.jewish > 3, ]
tidy_EP_jewish_1965_salient <- tidy_EP_jewish_1965[tidy_EP_jewish_1965$cmd.jewish > 3, ]
tidy_EP_jewish_1969_salient <- tidy_EP_jewish_1969[tidy_EP_jewish_1969$cmd.jewish > 3, ]
tidy_EP_jewish_1975_salient <- tidy_EP_jewish_1975[tidy_EP_jewish_1975$cmd.jewish > 3, ]
tidy_EP_jewish_1984_salient <- tidy_EP_jewish_1984[tidy_EP_jewish_1984$cmd.jewish > 3, ]
tidy_EP_jewish_1992_salient <- tidy_EP_jewish_1992[tidy_EP_jewish_1992$cmd.jewish > 3, ]
tidy_EP_jewish_1997_salient <- tidy_EP_jewish_1997[tidy_EP_jewish_1997$cmd.jewish > 3, ]
tidy_EP_jewish_2004_salient <- tidy_EP_jewish_2004[tidy_EP_jewish_2004$cmd.jewish > 3, ]
tidy_EP_jewish_2011_salient <- tidy_EP_jewish_2011[tidy_EP_jewish_2011$cmd.jewish > 3, ]
tidy_EP_jewish_2017_salient <- tidy_EP_jewish_2017[tidy_EP_jewish_2017$cmd.jewish > 3, ]


# list of salient
jewish_chunk_1922 <- print(tidy_EP_jewish_1922_salient$docs)
jewish_chunk_1927 <- print(tidy_EP_jewish_1927_salient$docs)
jewish_chunk_1931 <- print(tidy_EP_jewish_1931_salient$docs)
jewish_chunk_1934 <- print(tidy_EP_jewish_1934_salient$docs)
jewish_chunk_1937 <- print(tidy_EP_jewish_1937_salient$docs)
jewish_chunk_1940 <- print(tidy_EP_jewish_1940_salient$docs)
jewish_chunk_1942 <- print(tidy_EP_jewish_1942_salient$docs)
jewish_chunk_1945 <- print(tidy_EP_jewish_1945_salient$docs)
jewish_chunk_1950 <- print(tidy_EP_jewish_1950_salient$docs)
jewish_chunk_1956 <- print(tidy_EP_jewish_1956_salient$docs)
jewish_chunk_1960 <- print(tidy_EP_jewish_1960_salient$docs)
jewish_chunk_1965 <- print(tidy_EP_jewish_1965_salient$docs)
jewish_chunk_1969 <- print(tidy_EP_jewish_1969_salient$docs)
jewish_chunk_1975 <- print(tidy_EP_jewish_1975_salient$docs)
jewish_chunk_1984 <- print(tidy_EP_jewish_1984_salient$docs)
jewish_chunk_1992 <- print(tidy_EP_jewish_1992_salient$docs)
jewish_chunk_1997 <- print(tidy_EP_jewish_1997_salient$docs)
jewish_chunk_2004 <- print(tidy_EP_jewish_2004_salient$docs)
jewish_chunk_2011 <- print(tidy_EP_jewish_2011_salient$docs)
jewish_chunk_2017 <- print(tidy_EP_jewish_2017_salient$docs)



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



#   build cmdist for antonym pair

jewish_closeness <- tidy_EP_chunks_jewish %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )












#   13 irish


tidy_EP_irish_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)


tidy_EP_irish_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)


tidy_EP_irish_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)

tidy_EP_irish_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("irish"), wv=fastTM)


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

tidy_EP_irish_1922_salient <- tidy_EP_irish_1922[tidy_EP_irish_1922$cmd.irish > 3, ]
tidy_EP_irish_1927_salient <- tidy_EP_irish_1927[tidy_EP_irish_1927$cmd.irish > 3, ]
tidy_EP_irish_1931_salient <- tidy_EP_irish_1931[tidy_EP_irish_1931$cmd.irish > 3, ]
tidy_EP_irish_1934_salient <- tidy_EP_irish_1934[tidy_EP_irish_1934$cmd.irish > 3, ]
tidy_EP_irish_1937_salient <- tidy_EP_irish_1937[tidy_EP_irish_1937$cmd.irish > 3, ]
tidy_EP_irish_1940_salient <- tidy_EP_irish_1940[tidy_EP_irish_1940$cmd.irish > 3, ]
tidy_EP_irish_1942_salient <- tidy_EP_irish_1942[tidy_EP_irish_1942$cmd.irish > 3, ]
tidy_EP_irish_1945_salient <- tidy_EP_irish_1945[tidy_EP_irish_1945$cmd.irish > 3, ]
tidy_EP_irish_1950_salient <- tidy_EP_irish_1950[tidy_EP_irish_1950$cmd.irish > 3, ]
tidy_EP_irish_1956_salient <- tidy_EP_irish_1956[tidy_EP_irish_1956$cmd.irish > 3, ]
tidy_EP_irish_1960_salient <- tidy_EP_irish_1960[tidy_EP_irish_1960$cmd.irish > 3, ]
tidy_EP_irish_1965_salient <- tidy_EP_irish_1965[tidy_EP_irish_1965$cmd.irish > 3, ]
tidy_EP_irish_1969_salient <- tidy_EP_irish_1969[tidy_EP_irish_1969$cmd.irish > 3, ]
tidy_EP_irish_1975_salient <- tidy_EP_irish_1975[tidy_EP_irish_1975$cmd.irish > 3, ]
tidy_EP_irish_1984_salient <- tidy_EP_irish_1984[tidy_EP_irish_1984$cmd.irish > 3, ]
tidy_EP_irish_1992_salient <- tidy_EP_irish_1992[tidy_EP_irish_1992$cmd.irish > 3, ]
tidy_EP_irish_1997_salient <- tidy_EP_irish_1997[tidy_EP_irish_1997$cmd.irish > 3, ]
tidy_EP_irish_2004_salient <- tidy_EP_irish_2004[tidy_EP_irish_2004$cmd.irish > 3, ]
tidy_EP_irish_2011_salient <- tidy_EP_irish_2011[tidy_EP_irish_2011$cmd.irish > 3, ]
tidy_EP_irish_2017_salient <- tidy_EP_irish_2017[tidy_EP_irish_2017$cmd.irish > 3, ]


# list of salient
irish_chunk_1922 <- print(tidy_EP_irish_1922_salient$docs)
irish_chunk_1927 <- print(tidy_EP_irish_1927_salient$docs)
irish_chunk_1931 <- print(tidy_EP_irish_1931_salient$docs)
irish_chunk_1934 <- print(tidy_EP_irish_1934_salient$docs)
irish_chunk_1937 <- print(tidy_EP_irish_1937_salient$docs)
irish_chunk_1940 <- print(tidy_EP_irish_1940_salient$docs)
irish_chunk_1942 <- print(tidy_EP_irish_1942_salient$docs)
irish_chunk_1945 <- print(tidy_EP_irish_1945_salient$docs)
irish_chunk_1950 <- print(tidy_EP_irish_1950_salient$docs)
irish_chunk_1956 <- print(tidy_EP_irish_1956_salient$docs)
irish_chunk_1960 <- print(tidy_EP_irish_1960_salient$docs)
irish_chunk_1965 <- print(tidy_EP_irish_1965_salient$docs)
irish_chunk_1969 <- print(tidy_EP_irish_1969_salient$docs)
irish_chunk_1975 <- print(tidy_EP_irish_1975_salient$docs)
irish_chunk_1984 <- print(tidy_EP_irish_1984_salient$docs)
irish_chunk_1992 <- print(tidy_EP_irish_1992_salient$docs)
irish_chunk_1997 <- print(tidy_EP_irish_1997_salient$docs)
irish_chunk_2004 <- print(tidy_EP_irish_2004_salient$docs)
irish_chunk_2011 <- print(tidy_EP_irish_2011_salient$docs)
irish_chunk_2017 <- print(tidy_EP_irish_2017_salient$docs)



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



#   build cmdist for antonym pair

irish_closeness <- tidy_EP_chunks_irish %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )



















#   14 catholic


tidy_EP_catholic_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)


tidy_EP_catholic_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)


tidy_EP_catholic_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)

tidy_EP_catholic_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("catholic"), wv=fastTM)


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

tidy_EP_catholic_1922_salient <- tidy_EP_catholic_1922[tidy_EP_catholic_1922$cmd.catholic > 3, ]
tidy_EP_catholic_1927_salient <- tidy_EP_catholic_1927[tidy_EP_catholic_1927$cmd.catholic > 3, ]
tidy_EP_catholic_1931_salient <- tidy_EP_catholic_1931[tidy_EP_catholic_1931$cmd.catholic > 3, ]
tidy_EP_catholic_1934_salient <- tidy_EP_catholic_1934[tidy_EP_catholic_1934$cmd.catholic > 3, ]
tidy_EP_catholic_1937_salient <- tidy_EP_catholic_1937[tidy_EP_catholic_1937$cmd.catholic > 3, ]
tidy_EP_catholic_1940_salient <- tidy_EP_catholic_1940[tidy_EP_catholic_1940$cmd.catholic > 3, ]
tidy_EP_catholic_1942_salient <- tidy_EP_catholic_1942[tidy_EP_catholic_1942$cmd.catholic > 3, ]
tidy_EP_catholic_1945_salient <- tidy_EP_catholic_1945[tidy_EP_catholic_1945$cmd.catholic > 3, ]
tidy_EP_catholic_1950_salient <- tidy_EP_catholic_1950[tidy_EP_catholic_1950$cmd.catholic > 3, ]
tidy_EP_catholic_1956_salient <- tidy_EP_catholic_1956[tidy_EP_catholic_1956$cmd.catholic > 3, ]
tidy_EP_catholic_1960_salient <- tidy_EP_catholic_1960[tidy_EP_catholic_1960$cmd.catholic > 3, ]
tidy_EP_catholic_1965_salient <- tidy_EP_catholic_1965[tidy_EP_catholic_1965$cmd.catholic > 3, ]
tidy_EP_catholic_1969_salient <- tidy_EP_catholic_1969[tidy_EP_catholic_1969$cmd.catholic > 3, ]
tidy_EP_catholic_1975_salient <- tidy_EP_catholic_1975[tidy_EP_catholic_1975$cmd.catholic > 3, ]
tidy_EP_catholic_1984_salient <- tidy_EP_catholic_1984[tidy_EP_catholic_1984$cmd.catholic > 3, ]
tidy_EP_catholic_1992_salient <- tidy_EP_catholic_1992[tidy_EP_catholic_1992$cmd.catholic > 3, ]
tidy_EP_catholic_1997_salient <- tidy_EP_catholic_1997[tidy_EP_catholic_1997$cmd.catholic > 3, ]
tidy_EP_catholic_2004_salient <- tidy_EP_catholic_2004[tidy_EP_catholic_2004$cmd.catholic > 3, ]
tidy_EP_catholic_2011_salient <- tidy_EP_catholic_2011[tidy_EP_catholic_2011$cmd.catholic > 3, ]
tidy_EP_catholic_2017_salient <- tidy_EP_catholic_2017[tidy_EP_catholic_2017$cmd.catholic > 3, ]



# list of salient
catholic_chunk_1922 <- print(tidy_EP_catholic_1922_salient$docs)
catholic_chunk_1927 <- print(tidy_EP_catholic_1927_salient$docs)
catholic_chunk_1931 <- print(tidy_EP_catholic_1931_salient$docs)
catholic_chunk_1934 <- print(tidy_EP_catholic_1934_salient$docs)
catholic_chunk_1937 <- print(tidy_EP_catholic_1937_salient$docs)
catholic_chunk_1940 <- print(tidy_EP_catholic_1940_salient$docs)
catholic_chunk_1942 <- print(tidy_EP_catholic_1942_salient$docs)
catholic_chunk_1945 <- print(tidy_EP_catholic_1945_salient$docs)
catholic_chunk_1950 <- print(tidy_EP_catholic_1950_salient$docs)
catholic_chunk_1956 <- print(tidy_EP_catholic_1956_salient$docs)
catholic_chunk_1960 <- print(tidy_EP_catholic_1960_salient$docs)
catholic_chunk_1965 <- print(tidy_EP_catholic_1965_salient$docs)
catholic_chunk_1969 <- print(tidy_EP_catholic_1969_salient$docs)
catholic_chunk_1975 <- print(tidy_EP_catholic_1975_salient$docs)
catholic_chunk_1984 <- print(tidy_EP_catholic_1984_salient$docs)
catholic_chunk_1992 <- print(tidy_EP_catholic_1992_salient$docs)
catholic_chunk_1997 <- print(tidy_EP_catholic_1997_salient$docs)
catholic_chunk_2004 <- print(tidy_EP_catholic_2004_salient$docs)
catholic_chunk_2011 <- print(tidy_EP_catholic_2011_salient$docs)
catholic_chunk_2017 <- print(tidy_EP_catholic_2017_salient$docs)



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



#   build cmdist for antonym pair

catholic_closeness <- tidy_EP_chunks_catholic %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )














#   15 mexican


tidy_EP_mexican_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)


tidy_EP_mexican_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)


tidy_EP_mexican_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)

tidy_EP_mexican_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("mexican"), wv=fastTM)


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

tidy_EP_mexican_1922_salient <- tidy_EP_mexican_1922[tidy_EP_mexican_1922$cmd.mexican > 3, ]
tidy_EP_mexican_1927_salient <- tidy_EP_mexican_1927[tidy_EP_mexican_1927$cmd.mexican > 3, ]
tidy_EP_mexican_1931_salient <- tidy_EP_mexican_1931[tidy_EP_mexican_1931$cmd.mexican > 3, ]
tidy_EP_mexican_1934_salient <- tidy_EP_mexican_1934[tidy_EP_mexican_1934$cmd.mexican > 3, ]
tidy_EP_mexican_1937_salient <- tidy_EP_mexican_1937[tidy_EP_mexican_1937$cmd.mexican > 3, ]
tidy_EP_mexican_1940_salient <- tidy_EP_mexican_1940[tidy_EP_mexican_1940$cmd.mexican > 3, ]
tidy_EP_mexican_1942_salient <- tidy_EP_mexican_1942[tidy_EP_mexican_1942$cmd.mexican > 3, ]
tidy_EP_mexican_1945_salient <- tidy_EP_mexican_1945[tidy_EP_mexican_1945$cmd.mexican > 3, ]
tidy_EP_mexican_1950_salient <- tidy_EP_mexican_1950[tidy_EP_mexican_1950$cmd.mexican > 3, ]
tidy_EP_mexican_1956_salient <- tidy_EP_mexican_1956[tidy_EP_mexican_1956$cmd.mexican > 3, ]
tidy_EP_mexican_1960_salient <- tidy_EP_mexican_1960[tidy_EP_mexican_1960$cmd.mexican > 3, ]
tidy_EP_mexican_1965_salient <- tidy_EP_mexican_1965[tidy_EP_mexican_1965$cmd.mexican > 3, ]
tidy_EP_mexican_1969_salient <- tidy_EP_mexican_1969[tidy_EP_mexican_1969$cmd.mexican > 3, ]
tidy_EP_mexican_1975_salient <- tidy_EP_mexican_1975[tidy_EP_mexican_1975$cmd.mexican > 3, ]
tidy_EP_mexican_1984_salient <- tidy_EP_mexican_1984[tidy_EP_mexican_1984$cmd.mexican > 3, ]
tidy_EP_mexican_1992_salient <- tidy_EP_mexican_1992[tidy_EP_mexican_1992$cmd.mexican > 3, ]
tidy_EP_mexican_1997_salient <- tidy_EP_mexican_1997[tidy_EP_mexican_1997$cmd.mexican > 3, ]
tidy_EP_mexican_2004_salient <- tidy_EP_mexican_2004[tidy_EP_mexican_2004$cmd.mexican > 3, ]
tidy_EP_mexican_2011_salient <- tidy_EP_mexican_2011[tidy_EP_mexican_2011$cmd.mexican > 3, ]
tidy_EP_mexican_2017_salient <- tidy_EP_mexican_2017[tidy_EP_mexican_2017$cmd.mexican > 3, ]


# list of salient
mexican_chunk_1922 <- print(tidy_EP_mexican_1922_salient$docs)
mexican_chunk_1927 <- print(tidy_EP_mexican_1927_salient$docs)
mexican_chunk_1931 <- print(tidy_EP_mexican_1931_salient$docs)
mexican_chunk_1934 <- print(tidy_EP_mexican_1934_salient$docs)
mexican_chunk_1937 <- print(tidy_EP_mexican_1937_salient$docs)
mexican_chunk_1940 <- print(tidy_EP_mexican_1940_salient$docs)
mexican_chunk_1942 <- print(tidy_EP_mexican_1942_salient$docs)
mexican_chunk_1945 <- print(tidy_EP_mexican_1945_salient$docs)
mexican_chunk_1950 <- print(tidy_EP_mexican_1950_salient$docs)
mexican_chunk_1956 <- print(tidy_EP_mexican_1956_salient$docs)
mexican_chunk_1960 <- print(tidy_EP_mexican_1960_salient$docs)
mexican_chunk_1965 <- print(tidy_EP_mexican_1965_salient$docs)
mexican_chunk_1969 <- print(tidy_EP_mexican_1969_salient$docs)
mexican_chunk_1975 <- print(tidy_EP_mexican_1975_salient$docs)
mexican_chunk_1984 <- print(tidy_EP_mexican_1984_salient$docs)
mexican_chunk_1992 <- print(tidy_EP_mexican_1992_salient$docs)
mexican_chunk_1997 <- print(tidy_EP_mexican_1997_salient$docs)
mexican_chunk_2004 <- print(tidy_EP_mexican_2004_salient$docs)
mexican_chunk_2011 <- print(tidy_EP_mexican_2011_salient$docs)
mexican_chunk_2017 <- print(tidy_EP_mexican_2017_salient$docs)



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



#   build cmdist for antonym pair

mexican_closeness <- tidy_EP_chunks_mexican %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )











#   16 chinese


tidy_EP_chinese_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)


tidy_EP_chinese_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)


tidy_EP_chinese_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)

tidy_EP_chinese_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("chinese"), wv=fastTM)


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

tidy_EP_chinese_1922_salient <- tidy_EP_chinese_1922[tidy_EP_chinese_1922$cmd.chinese > 3, ]
tidy_EP_chinese_1927_salient <- tidy_EP_chinese_1927[tidy_EP_chinese_1927$cmd.chinese > 3, ]
tidy_EP_chinese_1931_salient <- tidy_EP_chinese_1931[tidy_EP_chinese_1931$cmd.chinese > 3, ]
tidy_EP_chinese_1934_salient <- tidy_EP_chinese_1934[tidy_EP_chinese_1934$cmd.chinese > 3, ]
tidy_EP_chinese_1937_salient <- tidy_EP_chinese_1937[tidy_EP_chinese_1937$cmd.chinese > 3, ]
tidy_EP_chinese_1940_salient <- tidy_EP_chinese_1940[tidy_EP_chinese_1940$cmd.chinese > 3, ]
tidy_EP_chinese_1942_salient <- tidy_EP_chinese_1942[tidy_EP_chinese_1942$cmd.chinese > 3, ]
tidy_EP_chinese_1945_salient <- tidy_EP_chinese_1945[tidy_EP_chinese_1945$cmd.chinese > 3, ]
tidy_EP_chinese_1950_salient <- tidy_EP_chinese_1950[tidy_EP_chinese_1950$cmd.chinese > 3, ]
tidy_EP_chinese_1956_salient <- tidy_EP_chinese_1956[tidy_EP_chinese_1956$cmd.chinese > 3, ]
tidy_EP_chinese_1960_salient <- tidy_EP_chinese_1960[tidy_EP_chinese_1960$cmd.chinese > 3, ]
tidy_EP_chinese_1965_salient <- tidy_EP_chinese_1965[tidy_EP_chinese_1965$cmd.chinese > 3, ]
tidy_EP_chinese_1969_salient <- tidy_EP_chinese_1969[tidy_EP_chinese_1969$cmd.chinese > 3, ]
tidy_EP_chinese_1975_salient <- tidy_EP_chinese_1975[tidy_EP_chinese_1975$cmd.chinese > 3, ]
tidy_EP_chinese_1984_salient <- tidy_EP_chinese_1984[tidy_EP_chinese_1984$cmd.chinese > 3, ]
tidy_EP_chinese_1992_salient <- tidy_EP_chinese_1992[tidy_EP_chinese_1992$cmd.chinese > 3, ]
tidy_EP_chinese_1997_salient <- tidy_EP_chinese_1997[tidy_EP_chinese_1997$cmd.chinese > 3, ]
tidy_EP_chinese_2004_salient <- tidy_EP_chinese_2004[tidy_EP_chinese_2004$cmd.chinese > 3, ]
tidy_EP_chinese_2011_salient <- tidy_EP_chinese_2011[tidy_EP_chinese_2011$cmd.chinese > 3, ]
tidy_EP_chinese_2017_salient <- tidy_EP_chinese_2017[tidy_EP_chinese_2017$cmd.chinese > 3, ]



# list of salient
chinese_chunk_1922 <- print(tidy_EP_chinese_1922_salient$docs)
chinese_chunk_1927 <- print(tidy_EP_chinese_1927_salient$docs)
chinese_chunk_1931 <- print(tidy_EP_chinese_1931_salient$docs)
chinese_chunk_1934 <- print(tidy_EP_chinese_1934_salient$docs)
chinese_chunk_1937 <- print(tidy_EP_chinese_1937_salient$docs)
chinese_chunk_1940 <- print(tidy_EP_chinese_1940_salient$docs)
chinese_chunk_1942 <- print(tidy_EP_chinese_1942_salient$docs)
chinese_chunk_1945 <- print(tidy_EP_chinese_1945_salient$docs)
chinese_chunk_1950 <- print(tidy_EP_chinese_1950_salient$docs)
chinese_chunk_1956 <- print(tidy_EP_chinese_1956_salient$docs)
chinese_chunk_1960 <- print(tidy_EP_chinese_1960_salient$docs)
chinese_chunk_1965 <- print(tidy_EP_chinese_1965_salient$docs)
chinese_chunk_1969 <- print(tidy_EP_chinese_1969_salient$docs)
chinese_chunk_1975 <- print(tidy_EP_chinese_1975_salient$docs)
chinese_chunk_1984 <- print(tidy_EP_chinese_1984_salient$docs)
chinese_chunk_1992 <- print(tidy_EP_chinese_1992_salient$docs)
chinese_chunk_1997 <- print(tidy_EP_chinese_1997_salient$docs)
chinese_chunk_2004 <- print(tidy_EP_chinese_2004_salient$docs)
chinese_chunk_2011 <- print(tidy_EP_chinese_2011_salient$docs)
chinese_chunk_2017 <- print(tidy_EP_chinese_2017_salient$docs)



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


#   build cmdist for antonym pair

chinese_closeness <- tidy_EP_chunks_chinese %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )
















#   17 cuban


tidy_EP_cuban_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)


tidy_EP_cuban_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)


tidy_EP_cuban_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)

tidy_EP_cuban_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("cuban"), wv=fastTM)


# combine, need to add "year"; also add unique

tidy_EP_cuban_1922 <- tidy_EP_cuban_1922 %>%
  mutate(year = 1922) 

tidy_EP_cuban_1927 <- tidy_EP_cuban_1927 %>%
  mutate(year = 1927)

tidy_EP_cuban_1931 <- tidy_EP_cuban_1931 %>%
  mutate(year = 1931)

tidy_EP_cuban_1934 <- tidy_EP_cuban_1934 %>%
  mutate(year = 1934)

tidy_EP_cuban_1937 <- tidy_EP_cuban_1937 %>%
  mutate(year = 1937)

tidy_EP_cuban_1940 <- tidy_EP_cuban_1940 %>%
  mutate(year = 1940)

tidy_EP_cuban_1942 <- tidy_EP_cuban_1942 %>%
  mutate(year = 1942)

tidy_EP_cuban_1945 <- tidy_EP_cuban_1945 %>%
  mutate(year = 1945)

tidy_EP_cuban_1950 <- tidy_EP_cuban_1950 %>%
  mutate(year = 1950)

tidy_EP_cuban_1956 <- tidy_EP_cuban_1956 %>%
  mutate(year = 1956)

tidy_EP_cuban_1960 <- tidy_EP_cuban_1960 %>%
  mutate(year = 1960)

tidy_EP_cuban_1965 <- tidy_EP_cuban_1965 %>%
  mutate(year = 1965)


tidy_EP_cuban_1969 <- tidy_EP_cuban_1969 %>%
  mutate(year = 1969)

tidy_EP_cuban_1975 <- tidy_EP_cuban_1975 %>%
  mutate(year = 1975)

tidy_EP_cuban_1984 <- tidy_EP_cuban_1984 %>%
  mutate(year = 1984)

tidy_EP_cuban_1992 <- tidy_EP_cuban_1992 %>%
  mutate(year = 1992)

tidy_EP_cuban_1997 <- tidy_EP_cuban_1997 %>%
  mutate(year = 1997)

tidy_EP_cuban_2004 <- tidy_EP_cuban_2004 %>%
  mutate(year = 2004)

tidy_EP_cuban_2011 <- tidy_EP_cuban_2011 %>%
  mutate(year = 2011)

tidy_EP_cuban_2017 <- tidy_EP_cuban_2017 %>%
  mutate(year = 2017)



# select on CMD > > 2 for each edition, gen unique

tidy_EP_cuban_1922_salient <- tidy_EP_cuban_1922[tidy_EP_cuban_1922$cmd.cuban > 3, ]
tidy_EP_cuban_1927_salient <- tidy_EP_cuban_1927[tidy_EP_cuban_1927$cmd.cuban > 3, ]
tidy_EP_cuban_1931_salient <- tidy_EP_cuban_1931[tidy_EP_cuban_1931$cmd.cuban > 3, ]
tidy_EP_cuban_1934_salient <- tidy_EP_cuban_1934[tidy_EP_cuban_1934$cmd.cuban > 3, ]
tidy_EP_cuban_1937_salient <- tidy_EP_cuban_1937[tidy_EP_cuban_1937$cmd.cuban > 3, ]
tidy_EP_cuban_1940_salient <- tidy_EP_cuban_1940[tidy_EP_cuban_1940$cmd.cuban > 3, ]
tidy_EP_cuban_1942_salient <- tidy_EP_cuban_1942[tidy_EP_cuban_1942$cmd.cuban > 3, ]
tidy_EP_cuban_1945_salient <- tidy_EP_cuban_1945[tidy_EP_cuban_1945$cmd.cuban > 3, ]
tidy_EP_cuban_1950_salient <- tidy_EP_cuban_1950[tidy_EP_cuban_1950$cmd.cuban > 3, ]
tidy_EP_cuban_1956_salient <- tidy_EP_cuban_1956[tidy_EP_cuban_1956$cmd.cuban > 3, ]
tidy_EP_cuban_1960_salient <- tidy_EP_cuban_1960[tidy_EP_cuban_1960$cmd.cuban > 3, ]
tidy_EP_cuban_1965_salient <- tidy_EP_cuban_1965[tidy_EP_cuban_1965$cmd.cuban > 3, ]
tidy_EP_cuban_1969_salient <- tidy_EP_cuban_1969[tidy_EP_cuban_1969$cmd.cuban > 3, ]
tidy_EP_cuban_1975_salient <- tidy_EP_cuban_1975[tidy_EP_cuban_1975$cmd.cuban > 3, ]
tidy_EP_cuban_1984_salient <- tidy_EP_cuban_1984[tidy_EP_cuban_1984$cmd.cuban > 3, ]
tidy_EP_cuban_1992_salient <- tidy_EP_cuban_1992[tidy_EP_cuban_1992$cmd.cuban > 3, ]
tidy_EP_cuban_1997_salient <- tidy_EP_cuban_1997[tidy_EP_cuban_1997$cmd.cuban > 3, ]
tidy_EP_cuban_2004_salient <- tidy_EP_cuban_2004[tidy_EP_cuban_2004$cmd.cuban > 3, ]
tidy_EP_cuban_2011_salient <- tidy_EP_cuban_2011[tidy_EP_cuban_2011$cmd.cuban > 3, ]
tidy_EP_cuban_2017_salient <- tidy_EP_cuban_2017[tidy_EP_cuban_2017$cmd.cuban > 3, ]



# list of salient
cuban_chunk_1922 <- print(tidy_EP_cuban_1922_salient$docs)
cuban_chunk_1927 <- print(tidy_EP_cuban_1927_salient$docs)
cuban_chunk_1931 <- print(tidy_EP_cuban_1931_salient$docs)
cuban_chunk_1934 <- print(tidy_EP_cuban_1934_salient$docs)
cuban_chunk_1937 <- print(tidy_EP_cuban_1937_salient$docs)
cuban_chunk_1940 <- print(tidy_EP_cuban_1940_salient$docs)
cuban_chunk_1942 <- print(tidy_EP_cuban_1942_salient$docs)
cuban_chunk_1945 <- print(tidy_EP_cuban_1945_salient$docs)
cuban_chunk_1950 <- print(tidy_EP_cuban_1950_salient$docs)
cuban_chunk_1956 <- print(tidy_EP_cuban_1956_salient$docs)
cuban_chunk_1960 <- print(tidy_EP_cuban_1960_salient$docs)
cuban_chunk_1965 <- print(tidy_EP_cuban_1965_salient$docs)
cuban_chunk_1969 <- print(tidy_EP_cuban_1969_salient$docs)
cuban_chunk_1975 <- print(tidy_EP_cuban_1975_salient$docs)
cuban_chunk_1984 <- print(tidy_EP_cuban_1984_salient$docs)
cuban_chunk_1992 <- print(tidy_EP_cuban_1992_salient$docs)
cuban_chunk_1997 <- print(tidy_EP_cuban_1997_salient$docs)
cuban_chunk_2004 <- print(tidy_EP_cuban_2004_salient$docs)
cuban_chunk_2011 <- print(tidy_EP_cuban_2011_salient$docs)
cuban_chunk_2017 <- print(tidy_EP_cuban_2017_salient$docs)



# select if in chunk
tidy_EP_cuban_1922_select <- filter(tidy_EP_1922, chunk %in% cuban_chunk_1922)
tidy_EP_cuban_1927_select <- filter(tidy_EP_1927, chunk %in% cuban_chunk_1927)
tidy_EP_cuban_1931_select <- filter(tidy_EP_1931, chunk %in% cuban_chunk_1931)
tidy_EP_cuban_1934_select <- filter(tidy_EP_1934, chunk %in% cuban_chunk_1934)
tidy_EP_cuban_1937_select <- filter(tidy_EP_1937, chunk %in% cuban_chunk_1937)
tidy_EP_cuban_1940_select <- filter(tidy_EP_1940, chunk %in% cuban_chunk_1940)
tidy_EP_cuban_1942_select <- filter(tidy_EP_1942, chunk %in% cuban_chunk_1942)
tidy_EP_cuban_1945_select <- filter(tidy_EP_1945, chunk %in% cuban_chunk_1945)
tidy_EP_cuban_1950_select <- filter(tidy_EP_1950, chunk %in% cuban_chunk_1950)
tidy_EP_cuban_1956_select <- filter(tidy_EP_1956, chunk %in% cuban_chunk_1956)
tidy_EP_cuban_1960_select <- filter(tidy_EP_1960, chunk %in% cuban_chunk_1960)
tidy_EP_cuban_1965_select <- filter(tidy_EP_1965, chunk %in% cuban_chunk_1965)
tidy_EP_cuban_1969_select <- filter(tidy_EP_1969, chunk %in% cuban_chunk_1969)
tidy_EP_cuban_1975_select <- filter(tidy_EP_1975, chunk %in% cuban_chunk_1975)
tidy_EP_cuban_1984_select <- filter(tidy_EP_1984, chunk %in% cuban_chunk_1984)
tidy_EP_cuban_1992_select <- filter(tidy_EP_1992, chunk %in% cuban_chunk_1992)
tidy_EP_cuban_1997_select <- filter(tidy_EP_1997, chunk %in% cuban_chunk_1997)
tidy_EP_cuban_2004_select <- filter(tidy_EP_2004, chunk %in% cuban_chunk_2004)
tidy_EP_cuban_2011_select <- filter(tidy_EP_2011, chunk %in% cuban_chunk_2011)
tidy_EP_cuban_2017_select <- filter(tidy_EP_2017, chunk %in% cuban_chunk_2017)

# recombine chunks into edition


tidy_EP_chunks_cuban <- full_join(tidy_EP_cuban_1922_select, tidy_EP_cuban_1927_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1931_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1934_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1937_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1940_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1942_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1945_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1950_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1956_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1960_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1965_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1969_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1975_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1984_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1992_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_1997_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_2004_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_2011_select)
tidy_EP_chunks_cuban <- full_join(tidy_EP_chunks_cuban, tidy_EP_cuban_2017_select)



#   build cmdist for antonym pair

cuban_closeness <- tidy_EP_chunks_cuban %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )















#   18 muslim


tidy_EP_muslim_1922 <- tidy_EP_1922 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1927 <- tidy_EP_1927 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1931  <- tidy_EP_1931 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1934 <- tidy_EP_1934 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1937 <- tidy_EP_1937 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1940 <- tidy_EP_1940 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1942 <- tidy_EP_1942 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1945 <- tidy_EP_1945 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1950 <- tidy_EP_1950 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1956 <- tidy_EP_1956 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1960 <- tidy_EP_1960 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1965 <- tidy_EP_1965 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1969 <- tidy_EP_1969 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)


tidy_EP_muslim_1975 <- tidy_EP_1975 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1984 <- tidy_EP_1984 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_1992 <- tidy_EP_1992 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)


tidy_EP_muslim_1997 <- tidy_EP_1997 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_2004 <- tidy_EP_2004 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_2011 <- tidy_EP_2011 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)

tidy_EP_muslim_2017 <- tidy_EP_2017 %>% 
  cast_dtm(term = word, 
           document = chunk, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cw =c("muslim"), wv=fastTM)


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

tidy_EP_muslim_1922_salient <- tidy_EP_muslim_1922[tidy_EP_muslim_1922$cmd.muslim > 3, ]
tidy_EP_muslim_1927_salient <- tidy_EP_muslim_1927[tidy_EP_muslim_1927$cmd.muslim > 3, ]
tidy_EP_muslim_1931_salient <- tidy_EP_muslim_1931[tidy_EP_muslim_1931$cmd.muslim > 3, ]
tidy_EP_muslim_1934_salient <- tidy_EP_muslim_1934[tidy_EP_muslim_1934$cmd.muslim > 3, ]
tidy_EP_muslim_1937_salient <- tidy_EP_muslim_1937[tidy_EP_muslim_1937$cmd.muslim > 3, ]
tidy_EP_muslim_1940_salient <- tidy_EP_muslim_1940[tidy_EP_muslim_1940$cmd.muslim > 3, ]
tidy_EP_muslim_1942_salient <- tidy_EP_muslim_1942[tidy_EP_muslim_1942$cmd.muslim > 3, ]
tidy_EP_muslim_1945_salient <- tidy_EP_muslim_1945[tidy_EP_muslim_1945$cmd.muslim > 3, ]
tidy_EP_muslim_1950_salient <- tidy_EP_muslim_1950[tidy_EP_muslim_1950$cmd.muslim > 3, ]
tidy_EP_muslim_1956_salient <- tidy_EP_muslim_1956[tidy_EP_muslim_1956$cmd.muslim > 3, ]
tidy_EP_muslim_1960_salient <- tidy_EP_muslim_1960[tidy_EP_muslim_1960$cmd.muslim > 3, ]
tidy_EP_muslim_1965_salient <- tidy_EP_muslim_1965[tidy_EP_muslim_1965$cmd.muslim > 3, ]
tidy_EP_muslim_1969_salient <- tidy_EP_muslim_1969[tidy_EP_muslim_1969$cmd.muslim > 3, ]
tidy_EP_muslim_1975_salient <- tidy_EP_muslim_1975[tidy_EP_muslim_1975$cmd.muslim > 3, ]
tidy_EP_muslim_1984_salient <- tidy_EP_muslim_1984[tidy_EP_muslim_1984$cmd.muslim > 3, ]
tidy_EP_muslim_1992_salient <- tidy_EP_muslim_1992[tidy_EP_muslim_1992$cmd.muslim > 3, ]
tidy_EP_muslim_1997_salient <- tidy_EP_muslim_1997[tidy_EP_muslim_1997$cmd.muslim > 3, ]
tidy_EP_muslim_2004_salient <- tidy_EP_muslim_2004[tidy_EP_muslim_2004$cmd.muslim > 3, ]
tidy_EP_muslim_2011_salient <- tidy_EP_muslim_2011[tidy_EP_muslim_2011$cmd.muslim > 3, ]
tidy_EP_muslim_2017_salient <- tidy_EP_muslim_2017[tidy_EP_muslim_2017$cmd.muslim > 3, ]



# list of salient
muslim_chunk_1922 <- print(tidy_EP_muslim_1922_salient$docs)
muslim_chunk_1927 <- print(tidy_EP_muslim_1927_salient$docs)
muslim_chunk_1931 <- print(tidy_EP_muslim_1931_salient$docs)
muslim_chunk_1934 <- print(tidy_EP_muslim_1934_salient$docs)
muslim_chunk_1937 <- print(tidy_EP_muslim_1937_salient$docs)
muslim_chunk_1940 <- print(tidy_EP_muslim_1940_salient$docs)
muslim_chunk_1942 <- print(tidy_EP_muslim_1942_salient$docs)
muslim_chunk_1945 <- print(tidy_EP_muslim_1945_salient$docs)
muslim_chunk_1950 <- print(tidy_EP_muslim_1950_salient$docs)
muslim_chunk_1956 <- print(tidy_EP_muslim_1956_salient$docs)
muslim_chunk_1960 <- print(tidy_EP_muslim_1960_salient$docs)
muslim_chunk_1965 <- print(tidy_EP_muslim_1965_salient$docs)
muslim_chunk_1969 <- print(tidy_EP_muslim_1969_salient$docs)
muslim_chunk_1975 <- print(tidy_EP_muslim_1975_salient$docs)
muslim_chunk_1984 <- print(tidy_EP_muslim_1984_salient$docs)
muslim_chunk_1992 <- print(tidy_EP_muslim_1992_salient$docs)
muslim_chunk_1997 <- print(tidy_EP_muslim_1997_salient$docs)
muslim_chunk_2004 <- print(tidy_EP_muslim_2004_salient$docs)
muslim_chunk_2011 <- print(tidy_EP_muslim_2011_salient$docs)
muslim_chunk_2017 <- print(tidy_EP_muslim_2017_salient$docs)



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


#   build cmdist for antonym pair

muslim_closeness <- tidy_EP_chunks_muslim %>% 
  cast_dtm(term = word, 
           document = year, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )


# 19  Make CSV, rename first

italian_closeness <- mutate(italian_closeness,  italian.normal  = cmd.normal.pole.1)
jewish_closeness  <- mutate(jewish_closeness,   jewish.normal   = cmd.normal.pole.1)
irish_closeness   <- mutate(irish_closeness,    irish.normal    = cmd.normal.pole.1)
catholic_closeness<- mutate(catholic_closeness, catholic.normal = cmd.normal.pole.1)
mexican_closeness <- mutate(mexican_closeness,  mexican.normal  = cmd.normal.pole.1)
chinese_closeness <- mutate(chinese_closeness,  chinese.normal  = cmd.normal.pole.1)
cuban_closeness   <- mutate(cuban_closeness,    cuban.normal    = cmd.normal.pole.1)
muslim_closeness  <- mutate(muslim_closeness,   muslim.normal   = cmd.normal.pole.1)


italian_closeness  <- select(italian_closeness,  -(cmd.normal.pole.1))
jewish_closeness   <- select(jewish_closeness,   -(cmd.normal.pole.1))
irish_closeness    <- select(irish_closeness,    -(cmd.normal.pole.1))
catholic_closeness <- select(catholic_closeness, -(cmd.normal.pole.1))
mexican_closeness  <- select(mexican_closeness,  -(cmd.normal.pole.1))
chinese_closeness  <- select(chinese_closeness,  -(cmd.normal.pole.1))
cuban_closeness    <- select(cuban_closeness,    -(cmd.normal.pole.1))
muslim_closeness   <- select(muslim_closeness,   -(cmd.normal.pole.1))


immigrant_closeness <- full_join(italian_closeness,   jewish_closeness)
immigrant_closeness <- full_join(immigrant_closeness, irish_closeness)
immigrant_closeness <- full_join(immigrant_closeness, catholic_closeness)
immigrant_closeness <- full_join(immigrant_closeness, mexican_closeness)
immigrant_closeness <- full_join(immigrant_closeness, chinese_closeness)
immigrant_closeness <- full_join(immigrant_closeness, cuban_closeness)
immigrant_closeness <- full_join(immigrant_closeness, muslim_closeness)


write.table(immigrant_closeness, "immigrant_closeness.csv", row.names = FALSE)







#      20 combing closeness and standardize scores across all pseudo-documents
tidy_EP_chunks_italian <-   mutate(tidy_EP_chunks_italian,   chunk_unique = year+.1)
tidy_EP_chunks_jewish <-    mutate(tidy_EP_chunks_jewish,    chunk_unique = year+.2)
tidy_EP_chunks_irish <-     mutate(tidy_EP_chunks_irish,     chunk_unique = year+.3)
tidy_EP_chunks_catholic <-  mutate(tidy_EP_chunks_catholic,  chunk_unique = year+.4)
tidy_EP_chunks_mexican <-   mutate(tidy_EP_chunks_mexican,   chunk_unique = year+.5)
tidy_EP_chunks_chinese <-   mutate(tidy_EP_chunks_chinese,   chunk_unique = year+.6)
tidy_EP_chunks_cuban <-     mutate(tidy_EP_chunks_cuban,     chunk_unique = year+.7)
tidy_EP_chunks_muslim <-    mutate(tidy_EP_chunks_muslim,    chunk_unique = year+.8)



tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_italian, tidy_EP_chunks_jewish)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_irish)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_catholic)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_mexican)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_chinese)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_cuban)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_muslim)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_british)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_russian)
tidy_EP_chunks_combined <- full_join(tidy_EP_chunks_combined, tidy_EP_chunks_german)

#combine full set


tidy_EP_chunks_combined_closeness <- tidy_EP_chunks_combined %>% 
  cast_dtm(term = word, 
           document = chunk_unique, 
           value = n, 
           weighting = tm::weightTf)  %>%
  removeSparseTerms(.999) %>%
  CMDist(cv = immigrant.sd, wv=fastTM )

tidy_EP_chunks_combined_closeness <- mutate(tidy_EP_chunks_combined_closeness, year = substr(docs, 1, 4))
tidy_EP_chunks_combined_closeness <- mutate(tidy_EP_chunks_combined_closeness, group = -100 * (as.numeric(year)-as.numeric(docs)))





#    21 Sentiment preparation






#Create dataframe with sentiment scores

sent_scores <- data.frame("year" = c(1922, 1927, 1931, 1934, 
                                     1937, 1940, 1942, 1945, 
                                     1950, 1956, 1960, 1965, 
                                     1969, 1975, 1984, 1992, 
                                     1997, 2004, 2011, 2017),
"catholic" = c(mean(tidy_EP_catholic_1922_select$mean_sentiment), 
               mean(tidy_EP_catholic_1927_select$mean_sentiment),
               mean(tidy_EP_catholic_1931_select$mean_sentiment),
               mean(tidy_EP_catholic_1934_select$mean_sentiment),
               mean(tidy_EP_catholic_1937_select$mean_sentiment),
               mean(tidy_EP_catholic_1940_select$mean_sentiment),
               mean(tidy_EP_catholic_1942_select$mean_sentiment),
               mean(tidy_EP_catholic_1945_select$mean_sentiment),
               mean(tidy_EP_catholic_1950_select$mean_sentiment),
               mean(tidy_EP_catholic_1956_select$mean_sentiment),
               mean(tidy_EP_catholic_1960_select$mean_sentiment),
               mean(tidy_EP_catholic_1965_select$mean_sentiment),
               mean(tidy_EP_catholic_1969_select$mean_sentiment),
               mean(tidy_EP_catholic_1975_select$mean_sentiment),
               mean(tidy_EP_catholic_1984_select$mean_sentiment),
               mean(tidy_EP_catholic_1992_select$mean_sentiment),
               mean(tidy_EP_catholic_1997_select$mean_sentiment),
               mean(tidy_EP_catholic_2004_select$mean_sentiment),
               mean(tidy_EP_catholic_2011_select$mean_sentiment),
               mean(tidy_EP_catholic_2017_select$mean_sentiment)),
"chinese" =  c(mean(tidy_EP_chinese_1922_select$mean_sentiment), 
               mean(tidy_EP_chinese_1927_select$mean_sentiment),
               mean(tidy_EP_chinese_1931_select$mean_sentiment),
               mean(tidy_EP_chinese_1934_select$mean_sentiment),
               mean(tidy_EP_chinese_1937_select$mean_sentiment),
               mean(tidy_EP_chinese_1940_select$mean_sentiment),
               mean(tidy_EP_chinese_1942_select$mean_sentiment),
               mean(tidy_EP_chinese_1945_select$mean_sentiment),
               mean(tidy_EP_chinese_1950_select$mean_sentiment),
               mean(tidy_EP_chinese_1956_select$mean_sentiment),
               mean(tidy_EP_chinese_1960_select$mean_sentiment),
               mean(tidy_EP_chinese_1965_select$mean_sentiment),
               mean(tidy_EP_chinese_1969_select$mean_sentiment),
               mean(tidy_EP_chinese_1975_select$mean_sentiment),
               mean(tidy_EP_chinese_1984_select$mean_sentiment),
               mean(tidy_EP_chinese_1992_select$mean_sentiment),
               mean(tidy_EP_chinese_1997_select$mean_sentiment),
               mean(tidy_EP_chinese_2004_select$mean_sentiment),
               mean(tidy_EP_chinese_2011_select$mean_sentiment),
               mean(tidy_EP_chinese_2017_select$mean_sentiment)),
"cuban" =    c(mean(tidy_EP_cuban_1922_select$mean_sentiment), 
               mean(tidy_EP_cuban_1927_select$mean_sentiment),
               mean(tidy_EP_cuban_1931_select$mean_sentiment),
               mean(tidy_EP_cuban_1934_select$mean_sentiment),
               mean(tidy_EP_cuban_1937_select$mean_sentiment),
               mean(tidy_EP_cuban_1940_select$mean_sentiment),
               mean(tidy_EP_cuban_1942_select$mean_sentiment),
               mean(tidy_EP_cuban_1945_select$mean_sentiment),
               mean(tidy_EP_cuban_1950_select$mean_sentiment),
               mean(tidy_EP_cuban_1956_select$mean_sentiment),
               mean(tidy_EP_cuban_1960_select$mean_sentiment),
               mean(tidy_EP_cuban_1965_select$mean_sentiment),
               mean(tidy_EP_cuban_1969_select$mean_sentiment),
               mean(tidy_EP_cuban_1975_select$mean_sentiment),
               mean(tidy_EP_cuban_1984_select$mean_sentiment),
               mean(tidy_EP_cuban_1992_select$mean_sentiment),
               mean(tidy_EP_cuban_1997_select$mean_sentiment),
               mean(tidy_EP_cuban_2004_select$mean_sentiment),
               mean(tidy_EP_cuban_2011_select$mean_sentiment),
               mean(tidy_EP_cuban_2017_select$mean_sentiment)),
"irish" = c(mean(tidy_EP_irish_1922_select$mean_sentiment), 
               mean(tidy_EP_irish_1927_select$mean_sentiment),
               mean(tidy_EP_irish_1931_select$mean_sentiment),
               mean(tidy_EP_irish_1934_select$mean_sentiment),
               mean(tidy_EP_irish_1937_select$mean_sentiment),
               mean(tidy_EP_irish_1940_select$mean_sentiment),
               mean(tidy_EP_irish_1942_select$mean_sentiment),
               mean(tidy_EP_irish_1945_select$mean_sentiment),
               mean(tidy_EP_irish_1950_select$mean_sentiment),
               mean(tidy_EP_irish_1956_select$mean_sentiment),
               mean(tidy_EP_irish_1960_select$mean_sentiment),
               mean(tidy_EP_irish_1965_select$mean_sentiment),
               mean(tidy_EP_irish_1969_select$mean_sentiment),
               mean(tidy_EP_irish_1975_select$mean_sentiment),
               mean(tidy_EP_irish_1984_select$mean_sentiment),
               mean(tidy_EP_irish_1992_select$mean_sentiment),
               mean(tidy_EP_irish_1997_select$mean_sentiment),
               mean(tidy_EP_irish_2004_select$mean_sentiment),
               mean(tidy_EP_irish_2011_select$mean_sentiment),
               mean(tidy_EP_irish_2017_select$mean_sentiment)),
"italian" =  c(mean(tidy_EP_italian_1922_select$mean_sentiment), 
               mean(tidy_EP_italian_1927_select$mean_sentiment),
               mean(tidy_EP_italian_1931_select$mean_sentiment),
               mean(tidy_EP_italian_1934_select$mean_sentiment),
               mean(tidy_EP_italian_1937_select$mean_sentiment),
               mean(tidy_EP_italian_1940_select$mean_sentiment),
               mean(tidy_EP_italian_1942_select$mean_sentiment),
               mean(tidy_EP_italian_1945_select$mean_sentiment),
               mean(tidy_EP_italian_1950_select$mean_sentiment),
               mean(tidy_EP_italian_1956_select$mean_sentiment),
               mean(tidy_EP_italian_1960_select$mean_sentiment),
               mean(tidy_EP_italian_1965_select$mean_sentiment),
               mean(tidy_EP_italian_1969_select$mean_sentiment),
               mean(tidy_EP_italian_1975_select$mean_sentiment),
               mean(tidy_EP_italian_1984_select$mean_sentiment),
               mean(tidy_EP_italian_1992_select$mean_sentiment),
               mean(tidy_EP_italian_1997_select$mean_sentiment),
               mean(tidy_EP_italian_2004_select$mean_sentiment),
               mean(tidy_EP_italian_2011_select$mean_sentiment),
               mean(tidy_EP_italian_2017_select$mean_sentiment)),
"jewish" = c(mean(tidy_EP_jewish_1922_select$mean_sentiment), 
               mean(tidy_EP_jewish_1927_select$mean_sentiment),
               mean(tidy_EP_jewish_1931_select$mean_sentiment),
               mean(tidy_EP_jewish_1934_select$mean_sentiment),
               mean(tidy_EP_jewish_1937_select$mean_sentiment),
               mean(tidy_EP_jewish_1940_select$mean_sentiment),
               mean(tidy_EP_jewish_1942_select$mean_sentiment),
               mean(tidy_EP_jewish_1945_select$mean_sentiment),
               mean(tidy_EP_jewish_1950_select$mean_sentiment),
               mean(tidy_EP_jewish_1956_select$mean_sentiment),
               mean(tidy_EP_jewish_1960_select$mean_sentiment),
               mean(tidy_EP_jewish_1965_select$mean_sentiment),
               mean(tidy_EP_jewish_1969_select$mean_sentiment),
               mean(tidy_EP_jewish_1975_select$mean_sentiment),
               mean(tidy_EP_jewish_1984_select$mean_sentiment),
               mean(tidy_EP_jewish_1992_select$mean_sentiment),
               mean(tidy_EP_jewish_1997_select$mean_sentiment),
               mean(tidy_EP_jewish_2004_select$mean_sentiment),
               mean(tidy_EP_jewish_2011_select$mean_sentiment),
               mean(tidy_EP_jewish_2017_select$mean_sentiment)),
"mexican" = c(mean(tidy_EP_mexican_1922_select$mean_sentiment), 
               mean(tidy_EP_mexican_1927_select$mean_sentiment),
               mean(tidy_EP_mexican_1931_select$mean_sentiment),
               mean(tidy_EP_mexican_1934_select$mean_sentiment),
               mean(tidy_EP_mexican_1937_select$mean_sentiment),
               mean(tidy_EP_mexican_1940_select$mean_sentiment),
               mean(tidy_EP_mexican_1942_select$mean_sentiment),
               mean(tidy_EP_mexican_1945_select$mean_sentiment),
               mean(tidy_EP_mexican_1950_select$mean_sentiment),
               mean(tidy_EP_mexican_1956_select$mean_sentiment),
               mean(tidy_EP_mexican_1960_select$mean_sentiment),
               mean(tidy_EP_mexican_1965_select$mean_sentiment),
               mean(tidy_EP_mexican_1969_select$mean_sentiment),
               mean(tidy_EP_mexican_1975_select$mean_sentiment),
               mean(tidy_EP_mexican_1984_select$mean_sentiment),
               mean(tidy_EP_mexican_1992_select$mean_sentiment),
               mean(tidy_EP_mexican_1997_select$mean_sentiment),
               mean(tidy_EP_mexican_2004_select$mean_sentiment),
               mean(tidy_EP_mexican_2011_select$mean_sentiment),
               mean(tidy_EP_mexican_2017_select$mean_sentiment)),
"muslim" = c(mean(tidy_EP_muslim_1922_select$mean_sentiment), 
               mean(tidy_EP_muslim_1927_select$mean_sentiment),
               mean(tidy_EP_muslim_1931_select$mean_sentiment),
               mean(tidy_EP_muslim_1934_select$mean_sentiment),
               mean(tidy_EP_muslim_1937_select$mean_sentiment),
               mean(tidy_EP_muslim_1940_select$mean_sentiment),
               mean(tidy_EP_muslim_1942_select$mean_sentiment),
               mean(tidy_EP_muslim_1945_select$mean_sentiment),
               mean(tidy_EP_muslim_1950_select$mean_sentiment),
               mean(tidy_EP_muslim_1956_select$mean_sentiment),
               mean(tidy_EP_muslim_1960_select$mean_sentiment),
               mean(tidy_EP_muslim_1965_select$mean_sentiment),
               mean(tidy_EP_muslim_1969_select$mean_sentiment),
               mean(tidy_EP_muslim_1975_select$mean_sentiment),
               mean(tidy_EP_muslim_1984_select$mean_sentiment),
               mean(tidy_EP_muslim_1992_select$mean_sentiment),
               mean(tidy_EP_muslim_1997_select$mean_sentiment),
               mean(tidy_EP_muslim_2004_select$mean_sentiment),
               mean(tidy_EP_muslim_2011_select$mean_sentiment),
               mean(tidy_EP_muslim_2017_select$mean_sentiment)),
"corpus" = c(mean(tidy_EP_1922$mean_sentiment), 
               mean(tidy_EP_1927$mean_sentiment),
               mean(tidy_EP_1931$mean_sentiment),
               mean(tidy_EP_1934$mean_sentiment),
               mean(tidy_EP_1937$mean_sentiment),
               mean(tidy_EP_1940$mean_sentiment),
               mean(tidy_EP_1942$mean_sentiment),
               mean(tidy_EP_1945$mean_sentiment),
               mean(tidy_EP_1950$mean_sentiment),
               mean(tidy_EP_1956$mean_sentiment),
               mean(tidy_EP_1960$mean_sentiment),
               mean(tidy_EP_1965$mean_sentiment),
               mean(tidy_EP_1969$mean_sentiment),
               mean(tidy_EP_1975$mean_sentiment),
               mean(tidy_EP_1984$mean_sentiment),
               mean(tidy_EP_1992$mean_sentiment),
               mean(tidy_EP_1997$mean_sentiment),
               mean(tidy_EP_2004$mean_sentiment),
               mean(tidy_EP_2011$mean_sentiment),
               mean(tidy_EP_2017$mean_sentiment)),
"diff_catholic" = c(mean(tidy_EP_catholic_1922_select$mean_sentiment) - mean(tidy_EP_1922$mean_sentiment),
               mean(tidy_EP_catholic_1927_select$mean_sentiment) - mean(tidy_EP_1927$mean_sentiment),
               mean(tidy_EP_catholic_1931_select$mean_sentiment) - mean(tidy_EP_1931$mean_sentiment),
               mean(tidy_EP_catholic_1934_select$mean_sentiment) - mean(tidy_EP_1934$mean_sentiment),
               mean(tidy_EP_catholic_1937_select$mean_sentiment) - mean(tidy_EP_1937$mean_sentiment),
              mean(tidy_EP_catholic_1940_select$mean_sentiment) - mean(tidy_EP_1940$mean_sentiment),
                                              mean(tidy_EP_catholic_1942_select$mean_sentiment) - mean(tidy_EP_1942$mean_sentiment),
                                              mean(tidy_EP_catholic_1945_select$mean_sentiment) - mean(tidy_EP_1945$mean_sentiment),
                                              mean(tidy_EP_catholic_1950_select$mean_sentiment) - mean(tidy_EP_1950$mean_sentiment),
                                              mean(tidy_EP_catholic_1956_select$mean_sentiment) - mean(tidy_EP_1956$mean_sentiment),
                                              mean(tidy_EP_catholic_1960_select$mean_sentiment) - mean(tidy_EP_1960$mean_sentiment),
                                              mean(tidy_EP_catholic_1965_select$mean_sentiment) - mean(tidy_EP_1965$mean_sentiment),
                                              mean(tidy_EP_catholic_1969_select$mean_sentiment) - mean(tidy_EP_1969$mean_sentiment),
                                              mean(tidy_EP_catholic_1975_select$mean_sentiment) - mean(tidy_EP_1975$mean_sentiment),
                                              mean(tidy_EP_catholic_1984_select$mean_sentiment) - mean(tidy_EP_1984$mean_sentiment),
                                              mean(tidy_EP_catholic_1992_select$mean_sentiment) - mean(tidy_EP_1992$mean_sentiment),
                                              mean(tidy_EP_catholic_1997_select$mean_sentiment) - mean(tidy_EP_1997$mean_sentiment),
                                              mean(tidy_EP_catholic_2004_select$mean_sentiment) - mean(tidy_EP_2004$mean_sentiment),
                                              mean(tidy_EP_catholic_2011_select$mean_sentiment) - mean(tidy_EP_2011$mean_sentiment),
                                              mean(tidy_EP_catholic_2017_select$mean_sentiment) - mean(tidy_EP_2017$mean_sentiment)),
                          "diff_chinese" = c(mean(tidy_EP_chinese_1922_select$mean_sentiment) - mean(tidy_EP_1922$mean_sentiment),
                                             mean(tidy_EP_chinese_1927_select$mean_sentiment) - mean(tidy_EP_1927$mean_sentiment),
                                             mean(tidy_EP_chinese_1931_select$mean_sentiment) - mean(tidy_EP_1931$mean_sentiment),
                                             mean(tidy_EP_chinese_1934_select$mean_sentiment) - mean(tidy_EP_1934$mean_sentiment),
                                             mean(tidy_EP_chinese_1937_select$mean_sentiment) - mean(tidy_EP_1937$mean_sentiment),
                                             mean(tidy_EP_chinese_1940_select$mean_sentiment) - mean(tidy_EP_1940$mean_sentiment),
                                             mean(tidy_EP_chinese_1942_select$mean_sentiment) - mean(tidy_EP_1942$mean_sentiment),
                                             mean(tidy_EP_chinese_1945_select$mean_sentiment) - mean(tidy_EP_1945$mean_sentiment),
                                             mean(tidy_EP_chinese_1950_select$mean_sentiment) - mean(tidy_EP_1950$mean_sentiment),
                                             mean(tidy_EP_chinese_1956_select$mean_sentiment) - mean(tidy_EP_1956$mean_sentiment),
                                             mean(tidy_EP_chinese_1960_select$mean_sentiment) - mean(tidy_EP_1960$mean_sentiment),
                                             mean(tidy_EP_chinese_1965_select$mean_sentiment) - mean(tidy_EP_1965$mean_sentiment),
                                             mean(tidy_EP_chinese_1969_select$mean_sentiment) - mean(tidy_EP_1969$mean_sentiment),
                                             mean(tidy_EP_chinese_1975_select$mean_sentiment) - mean(tidy_EP_1975$mean_sentiment),
                                             mean(tidy_EP_chinese_1984_select$mean_sentiment) - mean(tidy_EP_1984$mean_sentiment),
                                             mean(tidy_EP_chinese_1992_select$mean_sentiment) - mean(tidy_EP_1992$mean_sentiment),
                                             mean(tidy_EP_chinese_1997_select$mean_sentiment) - mean(tidy_EP_1997$mean_sentiment),
                                             mean(tidy_EP_chinese_2004_select$mean_sentiment) - mean(tidy_EP_2004$mean_sentiment),
                                             mean(tidy_EP_chinese_2011_select$mean_sentiment) - mean(tidy_EP_2011$mean_sentiment),
                                             mean(tidy_EP_chinese_2017_select$mean_sentiment) - mean(tidy_EP_2017$mean_sentiment)),
                          "diff_cuban" = c(mean(tidy_EP_cuban_1922_select$mean_sentiment) - mean(tidy_EP_1922$mean_sentiment),
                                           mean(tidy_EP_cuban_1927_select$mean_sentiment) - mean(tidy_EP_1927$mean_sentiment),
                                           mean(tidy_EP_cuban_1931_select$mean_sentiment) - mean(tidy_EP_1931$mean_sentiment),
                                           mean(tidy_EP_cuban_1934_select$mean_sentiment) - mean(tidy_EP_1934$mean_sentiment),
                                           mean(tidy_EP_cuban_1937_select$mean_sentiment) - mean(tidy_EP_1937$mean_sentiment),
                                           mean(tidy_EP_cuban_1940_select$mean_sentiment) - mean(tidy_EP_1940$mean_sentiment),
                                           mean(tidy_EP_cuban_1942_select$mean_sentiment) - mean(tidy_EP_1942$mean_sentiment),
                                           mean(tidy_EP_cuban_1945_select$mean_sentiment) - mean(tidy_EP_1945$mean_sentiment),
                                           mean(tidy_EP_cuban_1950_select$mean_sentiment) - mean(tidy_EP_1950$mean_sentiment),
                                           mean(tidy_EP_cuban_1956_select$mean_sentiment) - mean(tidy_EP_1956$mean_sentiment),
                                           mean(tidy_EP_cuban_1960_select$mean_sentiment) - mean(tidy_EP_1960$mean_sentiment),
                                           mean(tidy_EP_cuban_1965_select$mean_sentiment) - mean(tidy_EP_1965$mean_sentiment),
                                           mean(tidy_EP_cuban_1969_select$mean_sentiment) - mean(tidy_EP_1969$mean_sentiment),
                                           mean(tidy_EP_cuban_1975_select$mean_sentiment) - mean(tidy_EP_1975$mean_sentiment),
                                           mean(tidy_EP_cuban_1984_select$mean_sentiment) - mean(tidy_EP_1984$mean_sentiment),
                                           mean(tidy_EP_cuban_1992_select$mean_sentiment) - mean(tidy_EP_1992$mean_sentiment),
                                           mean(tidy_EP_cuban_1997_select$mean_sentiment) - mean(tidy_EP_1997$mean_sentiment),
                                           mean(tidy_EP_cuban_2004_select$mean_sentiment) - mean(tidy_EP_2004$mean_sentiment),
                                           mean(tidy_EP_cuban_2011_select$mean_sentiment) - mean(tidy_EP_2011$mean_sentiment),
                                           mean(tidy_EP_cuban_2017_select$mean_sentiment) - mean(tidy_EP_2017$mean_sentiment)),
                          "diff_irish" = c(mean(tidy_EP_irish_1922_select$mean_sentiment) - mean(tidy_EP_1922$mean_sentiment),
                                           mean(tidy_EP_irish_1927_select$mean_sentiment) - mean(tidy_EP_1927$mean_sentiment),
                                           mean(tidy_EP_irish_1931_select$mean_sentiment) - mean(tidy_EP_1931$mean_sentiment),
                                           mean(tidy_EP_irish_1934_select$mean_sentiment) - mean(tidy_EP_1934$mean_sentiment),
                                           mean(tidy_EP_irish_1937_select$mean_sentiment) - mean(tidy_EP_1937$mean_sentiment),
                                           mean(tidy_EP_irish_1940_select$mean_sentiment) - mean(tidy_EP_1940$mean_sentiment),
                                           mean(tidy_EP_irish_1942_select$mean_sentiment) - mean(tidy_EP_1942$mean_sentiment),
                                           mean(tidy_EP_irish_1945_select$mean_sentiment) - mean(tidy_EP_1945$mean_sentiment),
                                           mean(tidy_EP_irish_1950_select$mean_sentiment) - mean(tidy_EP_1950$mean_sentiment),
                                           mean(tidy_EP_irish_1956_select$mean_sentiment) - mean(tidy_EP_1956$mean_sentiment),
                                           mean(tidy_EP_irish_1960_select$mean_sentiment) - mean(tidy_EP_1960$mean_sentiment),
                                           mean(tidy_EP_irish_1965_select$mean_sentiment) - mean(tidy_EP_1965$mean_sentiment),
                                           mean(tidy_EP_irish_1969_select$mean_sentiment) - mean(tidy_EP_1969$mean_sentiment),
                                           mean(tidy_EP_irish_1975_select$mean_sentiment) - mean(tidy_EP_1975$mean_sentiment),
                                           mean(tidy_EP_irish_1984_select$mean_sentiment) - mean(tidy_EP_1984$mean_sentiment),
                                           mean(tidy_EP_irish_1992_select$mean_sentiment) - mean(tidy_EP_1992$mean_sentiment),
                                           mean(tidy_EP_irish_1997_select$mean_sentiment) - mean(tidy_EP_1997$mean_sentiment),
                                           mean(tidy_EP_irish_2004_select$mean_sentiment) - mean(tidy_EP_2004$mean_sentiment),
                                           mean(tidy_EP_irish_2011_select$mean_sentiment) - mean(tidy_EP_2011$mean_sentiment),
                                           mean(tidy_EP_irish_2017_select$mean_sentiment) - mean(tidy_EP_2017$mean_sentiment)),
                          "diff_italian" = c(mean(tidy_EP_italian_1922_select$mean_sentiment) - mean(tidy_EP_1922$mean_sentiment),
                                             mean(tidy_EP_italian_1927_select$mean_sentiment) - mean(tidy_EP_1927$mean_sentiment),
                                             mean(tidy_EP_italian_1931_select$mean_sentiment) - mean(tidy_EP_1931$mean_sentiment),
                                             mean(tidy_EP_italian_1934_select$mean_sentiment) - mean(tidy_EP_1934$mean_sentiment),
                                             mean(tidy_EP_italian_1937_select$mean_sentiment) - mean(tidy_EP_1937$mean_sentiment),
                                             mean(tidy_EP_italian_1940_select$mean_sentiment) - mean(tidy_EP_1940$mean_sentiment),
                                             mean(tidy_EP_italian_1942_select$mean_sentiment) - mean(tidy_EP_1942$mean_sentiment),
                                             mean(tidy_EP_italian_1945_select$mean_sentiment) - mean(tidy_EP_1945$mean_sentiment),
                                             mean(tidy_EP_italian_1950_select$mean_sentiment) - mean(tidy_EP_1950$mean_sentiment),
                                             mean(tidy_EP_italian_1956_select$mean_sentiment) - mean(tidy_EP_1956$mean_sentiment),
                                             mean(tidy_EP_italian_1960_select$mean_sentiment) - mean(tidy_EP_1960$mean_sentiment),
                                             mean(tidy_EP_italian_1965_select$mean_sentiment) - mean(tidy_EP_1965$mean_sentiment),
                                             mean(tidy_EP_italian_1969_select$mean_sentiment) - mean(tidy_EP_1969$mean_sentiment),
                                             mean(tidy_EP_italian_1975_select$mean_sentiment) - mean(tidy_EP_1975$mean_sentiment),
                                             mean(tidy_EP_italian_1984_select$mean_sentiment) - mean(tidy_EP_1984$mean_sentiment),
                                             mean(tidy_EP_italian_1992_select$mean_sentiment) - mean(tidy_EP_1992$mean_sentiment),
                                             mean(tidy_EP_italian_1997_select$mean_sentiment) - mean(tidy_EP_1997$mean_sentiment),
                                             mean(tidy_EP_italian_2004_select$mean_sentiment) - mean(tidy_EP_2004$mean_sentiment),
                                             mean(tidy_EP_italian_2011_select$mean_sentiment) - mean(tidy_EP_2011$mean_sentiment),
                                             mean(tidy_EP_italian_2017_select$mean_sentiment) - mean(tidy_EP_2017$mean_sentiment)),
                          "diff_jewish" = c(mean(tidy_EP_jewish_1922_select$mean_sentiment) - mean(tidy_EP_1922$mean_sentiment),
                                            mean(tidy_EP_jewish_1927_select$mean_sentiment) - mean(tidy_EP_1927$mean_sentiment),
                                            mean(tidy_EP_jewish_1931_select$mean_sentiment) - mean(tidy_EP_1931$mean_sentiment),
                                            mean(tidy_EP_jewish_1934_select$mean_sentiment) - mean(tidy_EP_1934$mean_sentiment),
                                            mean(tidy_EP_jewish_1937_select$mean_sentiment) - mean(tidy_EP_1937$mean_sentiment),
                                            mean(tidy_EP_jewish_1940_select$mean_sentiment) - mean(tidy_EP_1940$mean_sentiment),
                                            mean(tidy_EP_jewish_1942_select$mean_sentiment) - mean(tidy_EP_1942$mean_sentiment),
                                            mean(tidy_EP_jewish_1945_select$mean_sentiment) - mean(tidy_EP_1945$mean_sentiment),
                                            mean(tidy_EP_jewish_1950_select$mean_sentiment) - mean(tidy_EP_1950$mean_sentiment),
                                            mean(tidy_EP_jewish_1956_select$mean_sentiment) - mean(tidy_EP_1956$mean_sentiment),
                                            mean(tidy_EP_jewish_1960_select$mean_sentiment) - mean(tidy_EP_1960$mean_sentiment),
                                            mean(tidy_EP_jewish_1965_select$mean_sentiment) - mean(tidy_EP_1965$mean_sentiment),
                                            mean(tidy_EP_jewish_1969_select$mean_sentiment) - mean(tidy_EP_1969$mean_sentiment),
                                            mean(tidy_EP_jewish_1975_select$mean_sentiment) - mean(tidy_EP_1975$mean_sentiment),
                                            mean(tidy_EP_jewish_1984_select$mean_sentiment) - mean(tidy_EP_1984$mean_sentiment),
                                            mean(tidy_EP_jewish_1992_select$mean_sentiment) - mean(tidy_EP_1992$mean_sentiment),
                                            mean(tidy_EP_jewish_1997_select$mean_sentiment) - mean(tidy_EP_1997$mean_sentiment),
                                            mean(tidy_EP_jewish_2004_select$mean_sentiment) - mean(tidy_EP_2004$mean_sentiment),
                                            mean(tidy_EP_jewish_2011_select$mean_sentiment) - mean(tidy_EP_2011$mean_sentiment),
                                            mean(tidy_EP_jewish_2017_select$mean_sentiment) - mean(tidy_EP_2017$mean_sentiment)),
                          "diff_mexican" = c(mean(tidy_EP_mexican_1922_select$mean_sentiment) - mean(tidy_EP_1922$mean_sentiment),
                                             mean(tidy_EP_mexican_1927_select$mean_sentiment) - mean(tidy_EP_1927$mean_sentiment),
                                             mean(tidy_EP_mexican_1931_select$mean_sentiment) - mean(tidy_EP_1931$mean_sentiment),
                                             mean(tidy_EP_mexican_1934_select$mean_sentiment) - mean(tidy_EP_1934$mean_sentiment),
                                             mean(tidy_EP_mexican_1937_select$mean_sentiment) - mean(tidy_EP_1937$mean_sentiment),
                                             mean(tidy_EP_mexican_1940_select$mean_sentiment) - mean(tidy_EP_1940$mean_sentiment),
                                             mean(tidy_EP_mexican_1942_select$mean_sentiment) - mean(tidy_EP_1942$mean_sentiment),
                                             mean(tidy_EP_mexican_1945_select$mean_sentiment) - mean(tidy_EP_1945$mean_sentiment),
                                             mean(tidy_EP_mexican_1950_select$mean_sentiment) - mean(tidy_EP_1950$mean_sentiment),
                                             mean(tidy_EP_mexican_1956_select$mean_sentiment) - mean(tidy_EP_1956$mean_sentiment),
                                             mean(tidy_EP_mexican_1960_select$mean_sentiment) - mean(tidy_EP_1960$mean_sentiment),
                                             mean(tidy_EP_mexican_1965_select$mean_sentiment) - mean(tidy_EP_1965$mean_sentiment),
                                             mean(tidy_EP_mexican_1969_select$mean_sentiment) - mean(tidy_EP_1969$mean_sentiment),
                                             mean(tidy_EP_mexican_1975_select$mean_sentiment) - mean(tidy_EP_1975$mean_sentiment),
                                             mean(tidy_EP_mexican_1984_select$mean_sentiment) - mean(tidy_EP_1984$mean_sentiment),
                                             mean(tidy_EP_mexican_1992_select$mean_sentiment) - mean(tidy_EP_1992$mean_sentiment),
                                             mean(tidy_EP_mexican_1997_select$mean_sentiment) - mean(tidy_EP_1997$mean_sentiment),
                                             mean(tidy_EP_mexican_2004_select$mean_sentiment) - mean(tidy_EP_2004$mean_sentiment),
                                             mean(tidy_EP_mexican_2011_select$mean_sentiment) - mean(tidy_EP_2011$mean_sentiment),
                                             mean(tidy_EP_mexican_2017_select$mean_sentiment) - mean(tidy_EP_2017$mean_sentiment)),
                          "diff_muslim" = c(mean(tidy_EP_muslim_1922_select$mean_sentiment) - mean(tidy_EP_1922$mean_sentiment),
                                            mean(tidy_EP_muslim_1927_select$mean_sentiment) - mean(tidy_EP_1927$mean_sentiment),
                                            mean(tidy_EP_muslim_1931_select$mean_sentiment) - mean(tidy_EP_1931$mean_sentiment),
                                            mean(tidy_EP_muslim_1934_select$mean_sentiment) - mean(tidy_EP_1934$mean_sentiment),
                                            mean(tidy_EP_muslim_1937_select$mean_sentiment) - mean(tidy_EP_1937$mean_sentiment),
                                            mean(tidy_EP_muslim_1940_select$mean_sentiment) - mean(tidy_EP_1940$mean_sentiment),
                                            mean(tidy_EP_muslim_1942_select$mean_sentiment) - mean(tidy_EP_1942$mean_sentiment),
                                            mean(tidy_EP_muslim_1945_select$mean_sentiment) - mean(tidy_EP_1945$mean_sentiment),
                                            mean(tidy_EP_muslim_1950_select$mean_sentiment) - mean(tidy_EP_1950$mean_sentiment),
                                            mean(tidy_EP_muslim_1956_select$mean_sentiment) - mean(tidy_EP_1956$mean_sentiment),
                                            mean(tidy_EP_muslim_1960_select$mean_sentiment) - mean(tidy_EP_1960$mean_sentiment),
                                            mean(tidy_EP_muslim_1965_select$mean_sentiment) - mean(tidy_EP_1965$mean_sentiment),
                                            mean(tidy_EP_muslim_1969_select$mean_sentiment) - mean(tidy_EP_1969$mean_sentiment),
                                            mean(tidy_EP_muslim_1975_select$mean_sentiment) - mean(tidy_EP_1975$mean_sentiment),
                                            mean(tidy_EP_muslim_1984_select$mean_sentiment) - mean(tidy_EP_1984$mean_sentiment),
                                            mean(tidy_EP_muslim_1992_select$mean_sentiment) - mean(tidy_EP_1992$mean_sentiment),
                                            mean(tidy_EP_muslim_1997_select$mean_sentiment) - mean(tidy_EP_1997$mean_sentiment),
                                            mean(tidy_EP_muslim_2004_select$mean_sentiment) - mean(tidy_EP_2004$mean_sentiment),
                                            mean(tidy_EP_muslim_2011_select$mean_sentiment) - mean(tidy_EP_2011$mean_sentiment),
                                            mean(tidy_EP_muslim_2017_select$mean_sentiment) - mean(tidy_EP_2017$mean_sentiment)))
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

sent_scores[is.nan(sent_scores)] <- NA

# create tidy data frame for plotting


sent_scores_diff <- sent_scores[, 
      c("year", "diff_catholic", "diff_irish", "diff_italian", "diff_jewish", 
                "diff_chinese", "diff_cuban", "diff_mexican", "diff_muslim")]

sent_scores_tidy_diff <- reshape2::melt(sent_scores_diff, id.var = "year")


# groups
sent_scores_tidy_catholic <- sent_scores_tidy_diff [c(sent_scores_tidy_diff$variable =="diff_catholic"),]
sent_scores_tidy_chinese <- sent_scores_tidy_diff [c(sent_scores_tidy_diff$variable =="diff_chinese"),]
sent_scores_tidy_cuban <- sent_scores_tidy_diff [c(sent_scores_tidy_diff$variable =="diff_cuban"),]
sent_scores_tidy_irish <- sent_scores_tidy_diff [c(sent_scores_tidy_diff$variable =="diff_irish"),]
sent_scores_tidy_italian <- sent_scores_tidy_diff [c(sent_scores_tidy_diff$variable =="diff_italian"),] 
sent_scores_tidy_jewish <- sent_scores_tidy_diff [c(sent_scores_tidy_diff$variable =="diff_jewish"),]
sent_scores_tidy_mexican <- sent_scores_tidy_diff [c(sent_scores_tidy_diff$variable =="diff_mexican"),]
sent_scores_tidy_muslim <- sent_scores_tidy_diff [c(sent_scores_tidy_diff$variable =="diff_muslim"),] 




## 22 Document similarity score: TABLE 1



similarity <- source_EP %>% 
  mutate(text = str_replace_all(text, "[^[:ascii:]]", " ")) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = replace_white(text)) %>%
  mutate(text = strip(text, apostrophe.remove=TRUE)) %>%
  mutate(text = replace_number(text))  %>%
  filter(!str_detect(text, "[0-9]+") ) 


similarity$text_clean = similarity$text







similarity <- mutate(similarity, filename =  
          ifelse(grepl("txt_2020_12_3/1922EditionFull.txt", filename), "1922",
          ifelse(grepl("txt_2020_12_3/1927EditionFull.txt", filename), "1927",
          ifelse(grepl("txt_2020_12_3/1931EditionFull.txt", filename), "1931",
          ifelse(grepl("txt_2020_12_3/1934EditionFull.txt", filename), "1934",
          ifelse(grepl("txt_2020_12_3/1937EditionFull.txt", filename), "1937",
          ifelse(grepl("txt_2020_12_3/1940EditionFull.txt", filename), "1940",
          ifelse(grepl("txt_2020_12_3/1942EditionFull.txt", filename), "1942",
          ifelse(grepl("txt_2020_12_3/1945EditionFull.txt", filename), "1945",
          ifelse(grepl("txt_2020_12_3/1950EditionFull.txt", filename), "1950",
          ifelse(grepl("txt_2020_12_3/1956EditionFull.txt", filename), "1956",
          ifelse(grepl("txt_2020_12_3/1960EditionFull.txt", filename), "1960",
          ifelse(grepl("txt_2020_12_3/1965EditionFull.txt", filename), "1965",
          ifelse(grepl("txt_2020_12_3/1969EditionFull.txt", filename), "1969",
          ifelse(grepl("txt_2020_12_3/1975EditionFull.txt", filename), "1975",
          ifelse(grepl("txt_2020_12_3/1984EditionFull.txt", filename), "1984",
          ifelse(grepl("txt_2020_12_3/1992EditionFull.txt", filename), "1992",
          ifelse(grepl("txt_2020_12_3/1997EditionFull.txt", filename), "1997",
          ifelse(grepl("txt_2020_12_3/2004EditionFull.txt", filename), "2004",
          ifelse(grepl("txt_2020_12_3/2011EditionFull.txt", filename), "2011",
          ifelse(grepl("txt_2020_12_3/2017EditionFull.txt", filename), "2017","x")))))))))))))))))))))

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

# put in excel: USED TO CREATE TABLE
tfidf_EP_cos_sim_df <- tfidf_EP_cos_sim %>%
  as.matrix %>% as.data.frame
write.csv(tfidf_EP_cos_sim_df, "tfidf_cos_sim.csv")

# 23 Plot all scores together (shows standardization)



tidy_EP_chunks_combined_closeness_plot <- tidy_EP_chunks_combined_closeness %>% 
  ggplot(aes(x = as.numeric(year), y = as.numeric(cmd.normal.pole.1))) +
  geom_point(aes(color = as.numeric(cmd.normal.pole.1)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 
print(tidy_EP_chunks_combined_closeness_plot)


# print groups
pdf("kline(W)EP-closeness_combined_plot.pdf")
tidy_EP_chunks_combined_closeness_plot
dev.off()


write.table(tidy_EP_chunks_combined_closeness, "closeness_combined.csv", row.names = FALSE)





#24 Grouped graphs, separate

plot_italian_normal <- italian_closeness %>% 
  ggplot(aes(x = as.numeric(docs), y = as.numeric(italian.normal))) +
  geom_point(aes(color = as.numeric(italian.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') + 
  ggtitle("Italian") +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 



plot_jewish_normal <- jewish_closeness %>% 
  ggplot(aes(x = as.numeric(docs), y = as.numeric(jewish.normal))) +
  geom_point(aes(color = as.numeric(jewish.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  ggtitle("Jewish") +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 



plot_irish_normal <- irish_closeness %>% 
  ggplot(aes(x = as.numeric(docs), y = as.numeric(irish.normal))) +
  geom_point(aes(color = as.numeric(irish.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  ggtitle("Irish") +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 


plot_catholic_normal <- catholic_closeness %>% 
  ggplot(aes(x = as.numeric(docs), y = as.numeric(catholic.normal))) +
  geom_point(aes(color = as.numeric(catholic.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  ggtitle("Catholic") +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 


plot_mexican_normal <- mexican_closeness %>% 
  ggplot(aes(x = as.numeric(docs), y = as.numeric(mexican.normal))) +
  geom_point(aes(color = as.numeric(mexican.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  ggtitle("Mexican") +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 


plot_chinese_normal <- chinese_closeness %>% 
  ggplot(aes(x = as.numeric(docs), y = as.numeric(chinese.normal))) +
  geom_point(aes(color = as.numeric(chinese.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  ggtitle("Chinese") +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 



plot_cuban_normal <- cuban_closeness %>% 
  ggplot(aes(x = as.numeric(docs), y = as.numeric(cuban.normal))) +
  geom_point(aes(color = as.numeric(cuban.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  ggtitle("Cuban") +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 


plot_muslim_normal <- muslim_closeness %>% 
  ggplot(aes(x = as.numeric(docs), y = as.numeric(muslim.normal))) +
  geom_point(aes(color = as.numeric(muslim.normal)), size = 2) +
  ylim(-3,3) +
  xlab("Year") +
  scale_x_continuous(breaks=seq(1920,2020,20)) +
  ylab('Engagement with "Normal" Pole') +
  ggtitle("Muslim") +
  scale_color_gradient2(low = "#8B1A1A", mid="#B2BECC", high = "#003366",
                        name = "", 
                        limits = c(-3,3),
                        breaks = c(-3,3), labels = c("Strange", "Normal")) +
  theme(legend.position = "bottom",
        legend.key.size = unit(5, "mm")) 



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
plot_group2_meaning <- ggarrange(plot_cuban_normal, 
                         plot_muslim_normal,  
                         plot_chinese_normal,  
                         plot_mexican_normal, 
                         nrow = 2, ncol = 2,
                         common.legend = TRUE,
                         legend = "bottom")


print(plot_group2_meaning)





# print groups
pdf("kline(2021_2_28)EP-closeness_groups_meaning.pdf")
plot_group1_meaning
plot_group2_meaning
dev.off()








#25 create plot of sentiment scores

#combined
sent_scores_plot_com <- sent_scores_tidy_diff %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  scale_color_gradient2(low = "#8B1A1A", 
                        mid = "#B2BECC", 
                        high = "#003366",                        
                        name ="",                        
  limits = c(-.4, .4),                        
  breaks = c(-.4, .4), 
  labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
  legend.key.size = unit(5, "mm"))




#groups



sent_scores_plot_italian <- sent_scores_tidy_italian %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  ggtitle("Italian") +  
  scale_color_gradient2(low = "#8B1A1A", mid = "#B2BECC", high = "#003366",    
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "negative")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm"))




sent_scores_plot_jewish <- sent_scores_tidy_jewish %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  ggtitle("Jewish") +  
  scale_color_gradient2(low = "#8B1A1A", mid = "#B2BECC", high = "#003366",    
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm"))




sent_scores_plot_irish <- sent_scores_tidy_irish %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  ggtitle("Irish") +  
  scale_color_gradient2(low = "#8B1A1A", mid = "#B2BECC", high = "#003366",    
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm"))




sent_scores_plot_catholic <- sent_scores_tidy_catholic %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  ggtitle("Catholic") +  
  scale_color_gradient2(low = "#8B1A1A", mid = "#B2BECC", high = "#003366",    
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm"))




sent_scores_plot_mexican <- sent_scores_tidy_mexican %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  ggtitle("Mexican") +  
  scale_color_gradient2(low = "#8B1A1A", mid = "#B2BECC", high = "#003366",    
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm"))





sent_scores_plot_chinese <- sent_scores_tidy_chinese %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  ggtitle("Chinese") +  
  scale_color_gradient2(low = "#8B1A1A", mid = "#B2BECC", high = "#003366",    
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm"))




sent_scores_plot_cuban <- sent_scores_tidy_cuban %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  ggtitle("Cuban") +  
  scale_color_gradient2(low = "#8B1A1A", mid = "#B2BECC", high = "#003366",    
                        name ="",                        
                        limits = c(-.4, .4),                        
                        breaks = c(-.4, .4), 
                        labels = c("Negative", "Positive")) +  
  theme(legend.position = "bottom",        
        legend.key.size = unit(5, "mm"))



sent_scores_plot_muslim <- sent_scores_tidy_muslim %>%  
  ggplot(aes(x = as.numeric(year), y = as.numeric(value))) +  
  geom_point(aes(color = as.numeric(value)), size = 2) +  
  ylim(-.4, .4) +  xlab("Year") +  
  scale_x_continuous(breaks=seq(1920,2020,20)) +  
  ylab('Mean Sentiment, Relative to Corpus') +  
  ggtitle("Muslim") +  
  scale_color_gradient2(low = "#8B1A1A", mid = "#B2BECC", high = "#003366",    
    name ="",                        
    limits = c(-.4, .4),                        
    breaks = c(-.4, .4), 
    labels = c("Negative", "Positive")) +  
    theme(legend.position = "bottom",        
    legend.key.size = unit(5, "mm"))

# print all

print(sent_scores_plot_com)

print (sent_scores_plot_italian)
print (sent_scores_plot_jewish)
print (sent_scores_plot_irish)
print (sent_scores_plot_catholic)
print (sent_scores_plot_mexican)
print (sent_scores_plot_chinese)
print (sent_scores_plot_cuban)
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
plot_group2_sent <- ggarrange(sent_scores_plot_cuban, 
                              sent_scores_plot_muslim,  
                         sent_scores_plot_chinese,  
                         sent_scores_plot_mexican, 
                         nrow = 2, ncol = 2,
                         common.legend = TRUE,
                         legend = "bottom")


print(plot_group2_sent)





# print groups
pdf("kline(2021_2_28)EP-closeness_groups_sent.pdf")
sent_scores_plot_com
plot_group1_sent
plot_group2_sent
dev.off()






















#26 List of all chunks

print(italian_chunk_1922)
print(italian_chunk_1927)
print(italian_chunk_1931)
print(italian_chunk_1934)
print(italian_chunk_1937)
print(italian_chunk_1940)
print(italian_chunk_1942)
print(italian_chunk_1945)
print(italian_chunk_1950)
print(italian_chunk_1956)
print(italian_chunk_1960)
print(italian_chunk_1965)
print(italian_chunk_1969)
print(italian_chunk_1975)
print(italian_chunk_1984)
print(italian_chunk_1992)
print(italian_chunk_1997)
print(italian_chunk_2004)
print(italian_chunk_2011)
print(italian_chunk_2017)



print(jewish_chunk_1922)
print(jewish_chunk_1927)
print(jewish_chunk_1931)
print(jewish_chunk_1934)
print(jewish_chunk_1937)
print(jewish_chunk_1940)
print(jewish_chunk_1942)
print(jewish_chunk_1945)
print(jewish_chunk_1950)
print(jewish_chunk_1956)
print(jewish_chunk_1960)
print(jewish_chunk_1965)
print(jewish_chunk_1969)
print(jewish_chunk_1975)
print(jewish_chunk_1984)
print(jewish_chunk_1992)
print(jewish_chunk_1997)
print(jewish_chunk_2004)
print(jewish_chunk_2011)
print(jewish_chunk_2017)



print(irish_chunk_1922)
print(irish_chunk_1927)
print(irish_chunk_1931)
print(irish_chunk_1934)
print(irish_chunk_1937)
print(irish_chunk_1940)
print(irish_chunk_1942)
print(irish_chunk_1945)
print(irish_chunk_1950)
print(irish_chunk_1956)
print(irish_chunk_1960)
print(irish_chunk_1965)
print(irish_chunk_1969)
print(irish_chunk_1975)
print(irish_chunk_1984)
print(irish_chunk_1992)
print(irish_chunk_1997)
print(irish_chunk_2004)
print(irish_chunk_2011)
print(irish_chunk_2017)




print(catholic_chunk_1922)
print(catholic_chunk_1927)
print(catholic_chunk_1931)
print(catholic_chunk_1934)
print(catholic_chunk_1937)
print(catholic_chunk_1940)
print(catholic_chunk_1942)
print(catholic_chunk_1945)
print(catholic_chunk_1950)
print(catholic_chunk_1956)
print(catholic_chunk_1960)
print(catholic_chunk_1965)
print(catholic_chunk_1969)
print(catholic_chunk_1975)
print(catholic_chunk_1984)
print(catholic_chunk_1992)
print(catholic_chunk_1997)
print(catholic_chunk_2004)
print(catholic_chunk_2011)
print(catholic_chunk_2017)







print(mexican_chunk_1922)
print(mexican_chunk_1927)
print(mexican_chunk_1931)
print(mexican_chunk_1934)
print(mexican_chunk_1937)
print(mexican_chunk_1940)
print(mexican_chunk_1942)
print(mexican_chunk_1945)
print(mexican_chunk_1950)
print(mexican_chunk_1956)
print(mexican_chunk_1960)
print(mexican_chunk_1965)
print(mexican_chunk_1969)
print(mexican_chunk_1975)
print(mexican_chunk_1984)
print(mexican_chunk_1992)
print(mexican_chunk_1997)
print(mexican_chunk_2004)
print(mexican_chunk_2011)
print(mexican_chunk_2017)




print(chinese_chunk_1922)
print(chinese_chunk_1927)
print(chinese_chunk_1931)
print(chinese_chunk_1934)
print(chinese_chunk_1937)
print(chinese_chunk_1940)
print(chinese_chunk_1942)
print(chinese_chunk_1945)
print(chinese_chunk_1950)
print(chinese_chunk_1956)
print(chinese_chunk_1960)
print(chinese_chunk_1965)
print(chinese_chunk_1969)
print(chinese_chunk_1975)
print(chinese_chunk_1984)
print(chinese_chunk_1992)
print(chinese_chunk_1997)
print(chinese_chunk_2004)
print(chinese_chunk_2011)
print(chinese_chunk_2017)






print(cuban_chunk_1922)
print(cuban_chunk_1927)
print(cuban_chunk_1931)
print(cuban_chunk_1934)
print(cuban_chunk_1937)
print(cuban_chunk_1940)
print(cuban_chunk_1942)
print(cuban_chunk_1945)
print(cuban_chunk_1950)
print(cuban_chunk_1956)
print(cuban_chunk_1960)
print(cuban_chunk_1965)
print(cuban_chunk_1969)
print(cuban_chunk_1975)
print(cuban_chunk_1984)
print(cuban_chunk_1992)
print(cuban_chunk_1997)
print(cuban_chunk_2004)
print(cuban_chunk_2011)
print(cuban_chunk_2017)






#17 editions with relevant parts


EP_1922 <- tidy_EP %>%
  filter(filename == "1922")

EP_1922 <- EP_1922 %>%
  mutate(id = as.numeric(rownames(EP_1922))) %>%
  mutate(chunkx = as.numeric(id)/c1922 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1922_italian   <- filter(EP_1922, chunk %in% italian_chunk_1922)
EP_1922_jewish    <- filter(EP_1922, chunk %in% jewish_chunk_1922)
EP_1922_irish     <- filter(EP_1922, chunk %in% irish_chunk_1922)
EP_1922_catholic  <- filter(EP_1922, chunk %in% catholic_chunk_1922)
EP_1922_mexican   <- filter(EP_1922, chunk %in% mexican_chunk_1922)
EP_1922_chinese   <- filter(EP_1922, chunk %in% chinese_chunk_1922)
EP_1922_cuban     <- filter(EP_1922, chunk %in% cuban_chunk_1922)


write.table(EP_1922_italian, "EP_1922_italian.csv", row.names = FALSE)
write.table(EP_1922_jewish, "EP_1922_jewish.csv", row.names = FALSE)
write.table(EP_1922_irish, "EP_1922_irish.csv", row.names = FALSE)
write.table(EP_1922_catholic, "EP_1922_catholic.csv", row.names = FALSE)
write.table(EP_1922_mexican, "EP_1922_mexican.csv", row.names = FALSE)
write.table(EP_1922_chinese, "EP_1922_chinese.csv", row.names = FALSE)
write.table(EP_1922_cuban, "EP_1922_cuban.csv", row.names = FALSE)

# 1927


EP_1927 <- tidy_EP %>%
  filter(filename == "1927")

EP_1927 <- EP_1927 %>%
  mutate(id = as.numeric(rownames(EP_1927))) %>%
  mutate(chunkx = as.numeric(id)/c1927 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1927_italian   <- filter(EP_1927, chunk %in% italian_chunk_1927)
EP_1927_jewish    <- filter(EP_1927, chunk %in% jewish_chunk_1927)
EP_1927_irish     <- filter(EP_1927, chunk %in% irish_chunk_1927)
EP_1927_catholic  <- filter(EP_1927, chunk %in% catholic_chunk_1927)
EP_1927_mexican   <- filter(EP_1927, chunk %in% mexican_chunk_1927)
EP_1927_chinese   <- filter(EP_1927, chunk %in% chinese_chunk_1927)
EP_1927_cuban     <- filter(EP_1927, chunk %in% cuban_chunk_1927)


write.table(EP_1927_italian, "EP_1927_italian.csv", row.names = FALSE)
write.table(EP_1927_jewish, "EP_1927_jewish.csv", row.names = FALSE)
write.table(EP_1927_irish, "EP_1927_irish.csv", row.names = FALSE)
write.table(EP_1927_catholic, "EP_1927_catholic.csv", row.names = FALSE)
write.table(EP_1927_mexican, "EP_1927_mexican.csv", row.names = FALSE)
write.table(EP_1927_chinese, "EP_1927_chinese.csv", row.names = FALSE)
write.table(EP_1927_cuban, "EP_1927_cuban.csv", row.names = FALSE)


#1931

EP_1931 <- tidy_EP %>%
  filter(filename == "1931")

EP_1931 <- EP_1931 %>%
  mutate(id = as.numeric(rownames(EP_1931))) %>%
  mutate(chunkx = as.numeric(id)/c1931 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1931_italian   <- filter(EP_1931, chunk %in% italian_chunk_1931)
EP_1931_jewish    <- filter(EP_1931, chunk %in% jewish_chunk_1931)
EP_1931_irish     <- filter(EP_1931, chunk %in% irish_chunk_1931)
EP_1931_catholic  <- filter(EP_1931, chunk %in% catholic_chunk_1931)
EP_1931_mexican   <- filter(EP_1931, chunk %in% mexican_chunk_1931)
EP_1931_chinese   <- filter(EP_1931, chunk %in% chinese_chunk_1931)
EP_1931_cuban     <- filter(EP_1931, chunk %in% cuban_chunk_1931)


write.table(EP_1931_italian, "EP_1931_italian.csv", row.names = FALSE)
write.table(EP_1931_jewish, "EP_1931_jewish.csv", row.names = FALSE)
write.table(EP_1931_irish, "EP_1931_irish.csv", row.names = FALSE)
write.table(EP_1931_catholic, "EP_1931_catholic.csv", row.names = FALSE)
write.table(EP_1931_mexican, "EP_1931_mexican.csv", row.names = FALSE)
write.table(EP_1931_chinese, "EP_1931_chinese.csv", row.names = FALSE)
write.table(EP_1931_cuban, "EP_1931_cuban.csv", row.names = FALSE)

#1934

EP_1934 <- tidy_EP %>%
  filter(filename == "1934")

EP_1934 <- EP_1934 %>%
  mutate(id = as.numeric(rownames(EP_1934))) %>%
  mutate(chunkx = as.numeric(id)/c1934 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1934_italian   <- filter(EP_1934, chunk %in% italian_chunk_1934)
EP_1934_jewish    <- filter(EP_1934, chunk %in% jewish_chunk_1934)
EP_1934_irish     <- filter(EP_1934, chunk %in% irish_chunk_1934)
EP_1934_catholic  <- filter(EP_1934, chunk %in% catholic_chunk_1934)
EP_1934_mexican   <- filter(EP_1934, chunk %in% mexican_chunk_1934)
EP_1934_chinese   <- filter(EP_1934, chunk %in% chinese_chunk_1934)
EP_1934_cuban     <- filter(EP_1934, chunk %in% cuban_chunk_1934)


write.table(EP_1934_italian, "EP_1934_italian.csv", row.names = FALSE)
write.table(EP_1934_jewish, "EP_1934_jewish.csv", row.names = FALSE)
write.table(EP_1934_irish, "EP_1934_irish.csv", row.names = FALSE)
write.table(EP_1934_catholic, "EP_1934_catholic.csv", row.names = FALSE)
write.table(EP_1934_mexican, "EP_1934_mexican.csv", row.names = FALSE)
write.table(EP_1934_chinese, "EP_1934_chinese.csv", row.names = FALSE)
write.table(EP_1934_cuban, "EP_1934_cuban.csv", row.names = FALSE)

#1937

EP_1937 <- tidy_EP %>%
  filter(filename == "1937")

EP_1937 <- EP_1937 %>%
  mutate(id = as.numeric(rownames(EP_1937))) %>%
  mutate(chunkx = as.numeric(id)/c1937 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1937_italian   <- filter(EP_1937, chunk %in% italian_chunk_1937)
EP_1937_jewish    <- filter(EP_1937, chunk %in% jewish_chunk_1937)
EP_1937_irish     <- filter(EP_1937, chunk %in% irish_chunk_1937)
EP_1937_catholic  <- filter(EP_1937, chunk %in% catholic_chunk_1937)
EP_1937_mexican   <- filter(EP_1937, chunk %in% mexican_chunk_1937)
EP_1937_chinese   <- filter(EP_1937, chunk %in% chinese_chunk_1937)
EP_1937_cuban     <- filter(EP_1937, chunk %in% cuban_chunk_1937)


write.table(EP_1937_italian, "EP_1937_italian.csv", row.names = FALSE)
write.table(EP_1937_jewish, "EP_1937_jewish.csv", row.names = FALSE)
write.table(EP_1937_irish, "EP_1937_irish.csv", row.names = FALSE)
write.table(EP_1937_catholic, "EP_1937_catholic.csv", row.names = FALSE)
write.table(EP_1937_mexican, "EP_1937_mexican.csv", row.names = FALSE)
write.table(EP_1937_chinese, "EP_1937_chinese.csv", row.names = FALSE)
write.table(EP_1937_cuban, "EP_1937_cuban.csv", row.names = FALSE)


#1940

EP_1940 <- tidy_EP %>%
  filter(filename == "1940")

EP_1940 <- EP_1940 %>%
  mutate(id = as.numeric(rownames(EP_1940))) %>%
  mutate(chunkx = as.numeric(id)/c1940 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1940_italian   <- filter(EP_1940, chunk %in% italian_chunk_1940)
EP_1940_jewish    <- filter(EP_1940, chunk %in% jewish_chunk_1940)
EP_1940_irish     <- filter(EP_1940, chunk %in% irish_chunk_1940)
EP_1940_catholic  <- filter(EP_1940, chunk %in% catholic_chunk_1940)
EP_1940_mexican   <- filter(EP_1940, chunk %in% mexican_chunk_1940)
EP_1940_chinese   <- filter(EP_1940, chunk %in% chinese_chunk_1940)
EP_1940_cuban     <- filter(EP_1940, chunk %in% cuban_chunk_1940)


write.table(EP_1940_italian, "EP_1940_italian.csv", row.names = FALSE)
write.table(EP_1940_jewish, "EP_1940_jewish.csv", row.names = FALSE)
write.table(EP_1940_irish, "EP_1940_irish.csv", row.names = FALSE)
write.table(EP_1940_catholic, "EP_1940_catholic.csv", row.names = FALSE)
write.table(EP_1940_mexican, "EP_1940_mexican.csv", row.names = FALSE)
write.table(EP_1940_chinese, "EP_1940_chinese.csv", row.names = FALSE)
write.table(EP_1940_cuban, "EP_1940_cuban.csv", row.names = FALSE)

#1942

EP_1942 <- tidy_EP %>%
  filter(filename == "1942")

EP_1942 <- EP_1942 %>%
  mutate(id = as.numeric(rownames(EP_1942))) %>%
  mutate(chunkx = as.numeric(id)/c1942 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1942_italian   <- filter(EP_1942, chunk %in% italian_chunk_1942)
EP_1942_jewish    <- filter(EP_1942, chunk %in% jewish_chunk_1942)
EP_1942_irish     <- filter(EP_1942, chunk %in% irish_chunk_1942)
EP_1942_catholic  <- filter(EP_1942, chunk %in% catholic_chunk_1942)
EP_1942_mexican   <- filter(EP_1942, chunk %in% mexican_chunk_1942)
EP_1942_chinese   <- filter(EP_1942, chunk %in% chinese_chunk_1942)
EP_1942_cuban     <- filter(EP_1942, chunk %in% cuban_chunk_1942)


write.table(EP_1942_italian, "EP_1942_italian.csv", row.names = FALSE)
write.table(EP_1942_jewish, "EP_1942_jewish.csv", row.names = FALSE)
write.table(EP_1942_irish, "EP_1942_irish.csv", row.names = FALSE)
write.table(EP_1942_catholic, "EP_1942_catholic.csv", row.names = FALSE)
write.table(EP_1942_mexican, "EP_1942_mexican.csv", row.names = FALSE)
write.table(EP_1942_chinese, "EP_1942_chinese.csv", row.names = FALSE)
write.table(EP_1942_cuban, "EP_1942_cuban.csv", row.names = FALSE)


#1945

EP_1945 <- tidy_EP %>%
  filter(filename == "1945")

EP_1945 <- EP_1945 %>%
  mutate(id = as.numeric(rownames(EP_1945))) %>%
  mutate(chunkx = as.numeric(id)/c1945 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1945_italian   <- filter(EP_1945, chunk %in% italian_chunk_1945)
EP_1945_jewish    <- filter(EP_1945, chunk %in% jewish_chunk_1945)
EP_1945_irish     <- filter(EP_1945, chunk %in% irish_chunk_1945)
EP_1945_catholic  <- filter(EP_1945, chunk %in% catholic_chunk_1945)
EP_1945_mexican   <- filter(EP_1945, chunk %in% mexican_chunk_1945)
EP_1945_chinese   <- filter(EP_1945, chunk %in% chinese_chunk_1945)
EP_1945_cuban     <- filter(EP_1945, chunk %in% cuban_chunk_1945)


write.table(EP_1945_italian, "EP_1945_italian.csv", row.names = FALSE)
write.table(EP_1945_jewish, "EP_1945_jewish.csv", row.names = FALSE)
write.table(EP_1945_irish, "EP_1945_irish.csv", row.names = FALSE)
write.table(EP_1945_catholic, "EP_1945_catholic.csv", row.names = FALSE)
write.table(EP_1945_mexican, "EP_1945_mexican.csv", row.names = FALSE)
write.table(EP_1945_chinese, "EP_1945_chinese.csv", row.names = FALSE)
write.table(EP_1945_cuban, "EP_1945_cuban.csv", row.names = FALSE)

#1950

EP_1950 <- tidy_EP %>%
  filter(filename == "1950")

EP_1950 <- EP_1950 %>%
  mutate(id = as.numeric(rownames(EP_1950))) %>%
  mutate(chunkx = as.numeric(id)/c1950 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1950_italian   <- filter(EP_1950, chunk %in% italian_chunk_1950)
EP_1950_jewish    <- filter(EP_1950, chunk %in% jewish_chunk_1950)
EP_1950_irish     <- filter(EP_1950, chunk %in% irish_chunk_1950)
EP_1950_catholic  <- filter(EP_1950, chunk %in% catholic_chunk_1950)
EP_1950_mexican   <- filter(EP_1950, chunk %in% mexican_chunk_1950)
EP_1950_chinese   <- filter(EP_1950, chunk %in% chinese_chunk_1950)
EP_1950_cuban     <- filter(EP_1950, chunk %in% cuban_chunk_1950)


write.table(EP_1950_italian, "EP_1950_italian.csv", row.names = FALSE)
write.table(EP_1950_jewish, "EP_1950_jewish.csv", row.names = FALSE)
write.table(EP_1950_irish, "EP_1950_irish.csv", row.names = FALSE)
write.table(EP_1950_catholic, "EP_1950_catholic.csv", row.names = FALSE)
write.table(EP_1950_mexican, "EP_1950_mexican.csv", row.names = FALSE)
write.table(EP_1950_chinese, "EP_1950_chinese.csv", row.names = FALSE)
write.table(EP_1950_cuban, "EP_1950_cuban.csv", row.names = FALSE)

#1956

EP_1956 <- tidy_EP %>%
  filter(filename == "1956")

EP_1956 <- EP_1956 %>%
  mutate(id = as.numeric(rownames(EP_1956))) %>%
  mutate(chunkx = as.numeric(id)/c1956 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1956_italian   <- filter(EP_1956, chunk %in% italian_chunk_1956)
EP_1956_jewish    <- filter(EP_1956, chunk %in% jewish_chunk_1956)
EP_1956_irish     <- filter(EP_1956, chunk %in% irish_chunk_1956)
EP_1956_catholic  <- filter(EP_1956, chunk %in% catholic_chunk_1956)
EP_1956_mexican   <- filter(EP_1956, chunk %in% mexican_chunk_1956)
EP_1956_chinese   <- filter(EP_1956, chunk %in% chinese_chunk_1956)
EP_1956_cuban     <- filter(EP_1956, chunk %in% cuban_chunk_1956)


write.table(EP_1956_italian, "EP_1956_italian.csv", row.names = FALSE)
write.table(EP_1956_jewish, "EP_1956_jewish.csv", row.names = FALSE)
write.table(EP_1956_irish, "EP_1956_irish.csv", row.names = FALSE)
write.table(EP_1956_catholic, "EP_1956_catholic.csv", row.names = FALSE)
write.table(EP_1956_mexican, "EP_1956_mexican.csv", row.names = FALSE)
write.table(EP_1956_chinese, "EP_1956_chinese.csv", row.names = FALSE)
write.table(EP_1956_cuban, "EP_1956_cuban.csv", row.names = FALSE)

#1960

EP_1960 <- tidy_EP %>%
  filter(filename == "1960")

EP_1960 <- EP_1960 %>%
  mutate(id = as.numeric(rownames(EP_1960))) %>%
  mutate(chunkx = as.numeric(id)/c1960 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1960_italian   <- filter(EP_1960, chunk %in% italian_chunk_1960)
EP_1960_jewish    <- filter(EP_1960, chunk %in% jewish_chunk_1960)
EP_1960_irish     <- filter(EP_1960, chunk %in% irish_chunk_1960)
EP_1960_catholic  <- filter(EP_1960, chunk %in% catholic_chunk_1960)
EP_1960_mexican   <- filter(EP_1960, chunk %in% mexican_chunk_1960)
EP_1960_chinese   <- filter(EP_1960, chunk %in% chinese_chunk_1960)
EP_1960_cuban     <- filter(EP_1960, chunk %in% cuban_chunk_1960)


write.table(EP_1960_italian, "EP_1960_italian.csv", row.names = FALSE)
write.table(EP_1960_jewish, "EP_1960_jewish.csv", row.names = FALSE)
write.table(EP_1960_irish, "EP_1960_irish.csv", row.names = FALSE)
write.table(EP_1960_catholic, "EP_1960_catholic.csv", row.names = FALSE)
write.table(EP_1960_mexican, "EP_1960_mexican.csv", row.names = FALSE)
write.table(EP_1960_chinese, "EP_1960_chinese.csv", row.names = FALSE)
write.table(EP_1960_cuban, "EP_1960_cuban.csv", row.names = FALSE)

#1965

EP_1965 <- tidy_EP %>%
  filter(filename == "1965")

EP_1965 <- EP_1965 %>%
  mutate(id = as.numeric(rownames(EP_1965))) %>%
  mutate(chunkx = as.numeric(id)/c1965 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1965_italian   <- filter(EP_1965, chunk %in% italian_chunk_1965)
EP_1965_jewish    <- filter(EP_1965, chunk %in% jewish_chunk_1965)
EP_1965_irish     <- filter(EP_1965, chunk %in% irish_chunk_1965)
EP_1965_catholic  <- filter(EP_1965, chunk %in% catholic_chunk_1965)
EP_1965_mexican   <- filter(EP_1965, chunk %in% mexican_chunk_1965)
EP_1965_chinese   <- filter(EP_1965, chunk %in% chinese_chunk_1965)
EP_1965_cuban     <- filter(EP_1965, chunk %in% cuban_chunk_1965)


write.table(EP_1965_italian, "EP_1965_italian.csv", row.names = FALSE)
write.table(EP_1965_jewish, "EP_1965_jewish.csv", row.names = FALSE)
write.table(EP_1965_irish, "EP_1965_irish.csv", row.names = FALSE)
write.table(EP_1965_catholic, "EP_1965_catholic.csv", row.names = FALSE)
write.table(EP_1965_mexican, "EP_1965_mexican.csv", row.names = FALSE)
write.table(EP_1965_chinese, "EP_1965_chinese.csv", row.names = FALSE)
write.table(EP_1965_cuban, "EP_1965_cuban.csv", row.names = FALSE)


#1969

EP_1969 <- tidy_EP %>%
  filter(filename == "1969")

EP_1969 <- EP_1969 %>%
  mutate(id = as.numeric(rownames(EP_1969))) %>%
  mutate(chunkx = as.numeric(id)/c1969 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1969_italian   <- filter(EP_1969, chunk %in% italian_chunk_1969)
EP_1969_jewish    <- filter(EP_1969, chunk %in% jewish_chunk_1969)
EP_1969_irish     <- filter(EP_1969, chunk %in% irish_chunk_1969)
EP_1969_catholic  <- filter(EP_1969, chunk %in% catholic_chunk_1969)
EP_1969_mexican   <- filter(EP_1969, chunk %in% mexican_chunk_1969)
EP_1969_chinese   <- filter(EP_1969, chunk %in% chinese_chunk_1969)
EP_1969_cuban     <- filter(EP_1969, chunk %in% cuban_chunk_1969)


write.table(EP_1969_italian, "EP_1969_italian.csv", row.names = FALSE)
write.table(EP_1969_jewish, "EP_1969_jewish.csv", row.names = FALSE)
write.table(EP_1969_irish, "EP_1969_irish.csv", row.names = FALSE)
write.table(EP_1969_catholic, "EP_1969_catholic.csv", row.names = FALSE)
write.table(EP_1969_mexican, "EP_1969_mexican.csv", row.names = FALSE)
write.table(EP_1969_chinese, "EP_1969_chinese.csv", row.names = FALSE)
write.table(EP_1969_cuban, "EP_1969_cuban.csv", row.names = FALSE)

#1975

EP_1975 <- tidy_EP %>%
  filter(filename == "1975")

EP_1975 <- EP_1975 %>%
  mutate(id = as.numeric(rownames(EP_1975))) %>%
  mutate(chunkx = as.numeric(id)/c1975 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1975_italian   <- filter(EP_1975, chunk %in% italian_chunk_1975)
EP_1975_jewish    <- filter(EP_1975, chunk %in% jewish_chunk_1975)
EP_1975_irish     <- filter(EP_1975, chunk %in% irish_chunk_1975)
EP_1975_catholic  <- filter(EP_1975, chunk %in% catholic_chunk_1975)
EP_1975_mexican   <- filter(EP_1975, chunk %in% mexican_chunk_1975)
EP_1975_chinese   <- filter(EP_1975, chunk %in% chinese_chunk_1975)
EP_1975_cuban     <- filter(EP_1975, chunk %in% cuban_chunk_1975)


write.table(EP_1975_italian, "EP_1975_italian.csv", row.names = FALSE)
write.table(EP_1975_jewish, "EP_1975_jewish.csv", row.names = FALSE)
write.table(EP_1975_irish, "EP_1975_irish.csv", row.names = FALSE)
write.table(EP_1975_catholic, "EP_1975_catholic.csv", row.names = FALSE)
write.table(EP_1975_mexican, "EP_1975_mexican.csv", row.names = FALSE)
write.table(EP_1975_chinese, "EP_1975_chinese.csv", row.names = FALSE)
write.table(EP_1975_cuban, "EP_1975_cuban.csv", row.names = FALSE)

#1984

EP_1984 <- tidy_EP %>%
  filter(filename == "1984")

EP_1984 <- EP_1984 %>%
  mutate(id = as.numeric(rownames(EP_1984))) %>%
  mutate(chunkx = as.numeric(id)/c1984 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1984_italian   <- filter(EP_1984, chunk %in% italian_chunk_1984)
EP_1984_jewish    <- filter(EP_1984, chunk %in% jewish_chunk_1984)
EP_1984_irish     <- filter(EP_1984, chunk %in% irish_chunk_1984)
EP_1984_catholic  <- filter(EP_1984, chunk %in% catholic_chunk_1984)
EP_1984_mexican   <- filter(EP_1984, chunk %in% mexican_chunk_1984)
EP_1984_chinese   <- filter(EP_1984, chunk %in% chinese_chunk_1984)
EP_1984_cuban     <- filter(EP_1984, chunk %in% cuban_chunk_1984)


write.table(EP_1984_italian, "EP_1984_italian.csv", row.names = FALSE)
write.table(EP_1984_jewish, "EP_1984_jewish.csv", row.names = FALSE)
write.table(EP_1984_irish, "EP_1984_irish.csv", row.names = FALSE)
write.table(EP_1984_catholic, "EP_1984_catholic.csv", row.names = FALSE)
write.table(EP_1984_mexican, "EP_1984_mexican.csv", row.names = FALSE)
write.table(EP_1984_chinese, "EP_1984_chinese.csv", row.names = FALSE)
write.table(EP_1984_cuban, "EP_1984_cuban.csv", row.names = FALSE)


#1992

EP_1992 <- tidy_EP %>%
  filter(filename == "1992")

EP_1992 <- EP_1992 %>%
  mutate(id = as.numeric(rownames(EP_1992))) %>%
  mutate(chunkx = as.numeric(id)/c1992 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1992_italian   <- filter(EP_1992, chunk %in% italian_chunk_1992)
EP_1992_jewish    <- filter(EP_1992, chunk %in% jewish_chunk_1992)
EP_1992_irish     <- filter(EP_1992, chunk %in% irish_chunk_1992)
EP_1992_catholic  <- filter(EP_1992, chunk %in% catholic_chunk_1992)
EP_1992_mexican   <- filter(EP_1992, chunk %in% mexican_chunk_1992)
EP_1992_chinese   <- filter(EP_1992, chunk %in% chinese_chunk_1992)
EP_1992_cuban     <- filter(EP_1992, chunk %in% cuban_chunk_1992)


write.table(EP_1992_italian, "EP_1992_italian.csv", row.names = FALSE)
write.table(EP_1992_jewish, "EP_1992_jewish.csv", row.names = FALSE)
write.table(EP_1992_irish, "EP_1992_irish.csv", row.names = FALSE)
write.table(EP_1992_catholic, "EP_1992_catholic.csv", row.names = FALSE)
write.table(EP_1992_mexican, "EP_1992_mexican.csv", row.names = FALSE)
write.table(EP_1992_chinese, "EP_1992_chinese.csv", row.names = FALSE)
write.table(EP_1992_cuban, "EP_1992_cuban.csv", row.names = FALSE)


#1997

EP_1997 <- tidy_EP %>%
  filter(filename == "1997")

EP_1997 <- EP_1997 %>%
  mutate(id = as.numeric(rownames(EP_1997))) %>%
  mutate(chunkx = as.numeric(id)/c1997 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_1997_italian   <- filter(EP_1997, chunk %in% italian_chunk_1997)
EP_1997_jewish    <- filter(EP_1997, chunk %in% jewish_chunk_1997)
EP_1997_irish     <- filter(EP_1997, chunk %in% irish_chunk_1997)
EP_1997_catholic  <- filter(EP_1997, chunk %in% catholic_chunk_1997)
EP_1997_mexican   <- filter(EP_1997, chunk %in% mexican_chunk_1997)
EP_1997_chinese   <- filter(EP_1997, chunk %in% chinese_chunk_1997)
EP_1997_cuban     <- filter(EP_1997, chunk %in% cuban_chunk_1997)


write.table(EP_1997_italian, "EP_1997_italian.csv", row.names = FALSE)
write.table(EP_1997_jewish, "EP_1997_jewish.csv", row.names = FALSE)
write.table(EP_1997_irish, "EP_1997_irish.csv", row.names = FALSE)
write.table(EP_1997_catholic, "EP_1997_catholic.csv", row.names = FALSE)
write.table(EP_1997_mexican, "EP_1997_mexican.csv", row.names = FALSE)
write.table(EP_1997_chinese, "EP_1997_chinese.csv", row.names = FALSE)
write.table(EP_1997_cuban, "EP_1997_cuban.csv", row.names = FALSE)

#2004

EP_2004 <- tidy_EP %>%
  filter(filename == "2004")

EP_2004 <- EP_2004 %>%
  mutate(id = as.numeric(rownames(EP_2004))) %>%
  mutate(chunkx = as.numeric(id)/c2004 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_2004_italian   <- filter(EP_2004, chunk %in% italian_chunk_2004)
EP_2004_jewish    <- filter(EP_2004, chunk %in% jewish_chunk_2004)
EP_2004_irish     <- filter(EP_2004, chunk %in% irish_chunk_2004)
EP_2004_catholic  <- filter(EP_2004, chunk %in% catholic_chunk_2004)
EP_2004_mexican   <- filter(EP_2004, chunk %in% mexican_chunk_2004)
EP_2004_chinese   <- filter(EP_2004, chunk %in% chinese_chunk_2004)
EP_2004_cuban     <- filter(EP_2004, chunk %in% cuban_chunk_2004)


write.table(EP_2004_italian, "EP_2004_italian.csv", row.names = FALSE)
write.table(EP_2004_jewish, "EP_2004_jewish.csv", row.names = FALSE)
write.table(EP_2004_irish, "EP_2004_irish.csv", row.names = FALSE)
write.table(EP_2004_catholic, "EP_2004_catholic.csv", row.names = FALSE)
write.table(EP_2004_mexican, "EP_2004_mexican.csv", row.names = FALSE)
write.table(EP_2004_chinese, "EP_2004_chinese.csv", row.names = FALSE)
write.table(EP_2004_cuban, "EP_2004_cuban.csv", row.names = FALSE)


#2011

EP_2011 <- tidy_EP %>%
  filter(filename == "2011")

EP_2011 <- EP_2011 %>%
  mutate(id = as.numeric(rownames(EP_2011))) %>%
  mutate(chunkx = as.numeric(id)/c2011 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_2011_italian   <- filter(EP_2011, chunk %in% italian_chunk_2011)
EP_2011_jewish    <- filter(EP_2011, chunk %in% jewish_chunk_2011)
EP_2011_irish     <- filter(EP_2011, chunk %in% irish_chunk_2011)
EP_2011_catholic  <- filter(EP_2011, chunk %in% catholic_chunk_2011)
EP_2011_mexican   <- filter(EP_2011, chunk %in% mexican_chunk_2011)
EP_2011_chinese   <- filter(EP_2011, chunk %in% chinese_chunk_2011)
EP_2011_cuban     <- filter(EP_2011, chunk %in% cuban_chunk_2011)


write.table(EP_2011_italian, "EP_2011_italian.csv", row.names = FALSE)
write.table(EP_2011_jewish, "EP_2011_jewish.csv", row.names = FALSE)
write.table(EP_2011_irish, "EP_2011_irish.csv", row.names = FALSE)
write.table(EP_2011_catholic, "EP_2011_catholic.csv", row.names = FALSE)
write.table(EP_2011_mexican, "EP_2011_mexican.csv", row.names = FALSE)
write.table(EP_2011_chinese, "EP_2011_chinese.csv", row.names = FALSE)
write.table(EP_2011_cuban, "EP_2011_cuban.csv", row.names = FALSE)

#2017

EP_2017 <- tidy_EP %>%
  filter(filename == "2017")

EP_2017 <- EP_2017 %>%
  mutate(id = as.numeric(rownames(EP_2017))) %>%
  mutate(chunkx = as.numeric(id)/c2017 + 1) %>%
  mutate(chunk = round(as.numeric(chunkx), digits = 0)) 

EP_2017_italian   <- filter(EP_2017, chunk %in% italian_chunk_2017)
EP_2017_jewish    <- filter(EP_2017, chunk %in% jewish_chunk_2017)
EP_2017_irish     <- filter(EP_2017, chunk %in% irish_chunk_2017)
EP_2017_catholic  <- filter(EP_2017, chunk %in% catholic_chunk_2017)
EP_2017_mexican   <- filter(EP_2017, chunk %in% mexican_chunk_2017)
EP_2017_chinese   <- filter(EP_2017, chunk %in% chinese_chunk_2017)
EP_2017_cuban     <- filter(EP_2017, chunk %in% cuban_chunk_2017)


write.table(EP_2017_italian, "EP_2017_italian.csv", row.names = FALSE)
write.table(EP_2017_jewish, "EP_2017_jewish.csv", row.names = FALSE)
write.table(EP_2017_irish, "EP_2017_irish.csv", row.names = FALSE)
write.table(EP_2017_catholic, "EP_2017_catholic.csv", row.names = FALSE)
write.table(EP_2017_mexican, "EP_2017_mexican.csv", row.names = FALSE)
write.table(EP_2017_chinese, "EP_2017_chinese.csv", row.names = FALSE)
write.table(EP_2017_cuban, "EP_2017_cuban.csv", row.names = FALSE)


#18 Count, Proportion data



# Proportion of selected text table 2

max(tidy_EP_1922$chunk)
max(tidy_EP_1927$chunk)
max(tidy_EP_1931$chunk)
max(tidy_EP_1934$chunk)
max(tidy_EP_1937$chunk)
max(tidy_EP_1940$chunk)
max(tidy_EP_1942$chunk)
max(tidy_EP_1945$chunk)
max(tidy_EP_1950$chunk)
max(tidy_EP_1956$chunk)
max(tidy_EP_1960$chunk)
max(tidy_EP_1965$chunk)
max(tidy_EP_1975$chunk)
max(tidy_EP_1984$chunk)
max(tidy_EP_1992$chunk)
max(tidy_EP_1997$chunk)
max(tidy_EP_2004$chunk)
max(tidy_EP_2011$chunk)
max(tidy_EP_2017$chunk)

#italian

count_italian_1922 <- length(italian_chunk_1922)
count_italian_1927 <- length(italian_chunk_1927)
count_italian_1931 <- length(italian_chunk_1931)
count_italian_1934 <- length(italian_chunk_1934)
count_italian_1937 <- length(italian_chunk_1937)
count_italian_1940 <- length(italian_chunk_1940)
count_italian_1942 <- length(italian_chunk_1942)
count_italian_1945 <- length(italian_chunk_1945)
count_italian_1950 <- length(italian_chunk_1950)
count_italian_1956 <- length(italian_chunk_1956)
count_italian_1960 <- length(italian_chunk_1960)
count_italian_1965 <- length(italian_chunk_1965)
count_italian_1969 <- length(italian_chunk_1969)
count_italian_1975 <- length(italian_chunk_1975)
count_italian_1984 <- length(italian_chunk_1984)
count_italian_1992 <- length(italian_chunk_1992)
count_italian_1997 <- length(italian_chunk_1997)
count_italian_2004 <- length(italian_chunk_2004)
count_italian_2011 <- length(italian_chunk_2011)
count_italian_2017 <- length(italian_chunk_2017)


count_jewish_1922 <- length(jewish_chunk_1922)
count_jewish_1927 <- length(jewish_chunk_1927)
count_jewish_1931 <- length(jewish_chunk_1931)
count_jewish_1934 <- length(jewish_chunk_1934)
count_jewish_1937 <- length(jewish_chunk_1937)
count_jewish_1940 <- length(jewish_chunk_1940)
count_jewish_1942 <- length(jewish_chunk_1942)
count_jewish_1945 <- length(jewish_chunk_1945)
count_jewish_1950 <- length(jewish_chunk_1950)
count_jewish_1956 <- length(jewish_chunk_1956)
count_jewish_1960 <- length(jewish_chunk_1960)
count_jewish_1965 <- length(jewish_chunk_1965)
count_jewish_1969 <- length(jewish_chunk_1969)
count_jewish_1975 <- length(jewish_chunk_1975)
count_jewish_1984 <- length(jewish_chunk_1984)
count_jewish_1992 <- length(jewish_chunk_1992)
count_jewish_1997 <- length(jewish_chunk_1997)
count_jewish_2004 <- length(jewish_chunk_2004)
count_jewish_2011 <- length(jewish_chunk_2011)
count_jewish_2017 <- length(jewish_chunk_2017)

count_irish_1922 <- length(irish_chunk_1922)
count_irish_1927 <- length(irish_chunk_1927)
count_irish_1931 <- length(irish_chunk_1931)
count_irish_1934 <- length(irish_chunk_1934)
count_irish_1937 <- length(irish_chunk_1937)
count_irish_1940 <- length(irish_chunk_1940)
count_irish_1942 <- length(irish_chunk_1942)
count_irish_1945 <- length(irish_chunk_1945)
count_irish_1950 <- length(irish_chunk_1950)
count_irish_1956 <- length(irish_chunk_1956)
count_irish_1960 <- length(irish_chunk_1960)
count_irish_1965 <- length(irish_chunk_1965)
count_irish_1969 <- length(irish_chunk_1969)
count_irish_1975 <- length(irish_chunk_1975)
count_irish_1984 <- length(irish_chunk_1984)
count_irish_1992 <- length(irish_chunk_1992)
count_irish_1997 <- length(irish_chunk_1997)
count_irish_2004 <- length(irish_chunk_2004)
count_irish_2011 <- length(irish_chunk_2011)
count_irish_2017 <- length(irish_chunk_2017)

count_catholic_1922 <- length(catholic_chunk_1922)
count_catholic_1927 <- length(catholic_chunk_1927)
count_catholic_1931 <- length(catholic_chunk_1931)
count_catholic_1934 <- length(catholic_chunk_1934)
count_catholic_1937 <- length(catholic_chunk_1937)
count_catholic_1940 <- length(catholic_chunk_1940)
count_catholic_1942 <- length(catholic_chunk_1942)
count_catholic_1945 <- length(catholic_chunk_1945)
count_catholic_1950 <- length(catholic_chunk_1950)
count_catholic_1956 <- length(catholic_chunk_1956)
count_catholic_1960 <- length(catholic_chunk_1960)
count_catholic_1965 <- length(catholic_chunk_1965)
count_catholic_1969 <- length(catholic_chunk_1969)
count_catholic_1975 <- length(catholic_chunk_1975)
count_catholic_1984 <- length(catholic_chunk_1984)
count_catholic_1992 <- length(catholic_chunk_1992)
count_catholic_1997 <- length(catholic_chunk_1997)
count_catholic_2004 <- length(catholic_chunk_2004)
count_catholic_2011 <- length(catholic_chunk_2011)
count_catholic_2017 <- length(catholic_chunk_2017)

count_mexican_1922 <- length(mexican_chunk_1922)
count_mexican_1927 <- length(mexican_chunk_1927)
count_mexican_1931 <- length(mexican_chunk_1931)
count_mexican_1934 <- length(mexican_chunk_1934)
count_mexican_1937 <- length(mexican_chunk_1937)
count_mexican_1940 <- length(mexican_chunk_1940)
count_mexican_1942 <- length(mexican_chunk_1942)
count_mexican_1945 <- length(mexican_chunk_1945)
count_mexican_1950 <- length(mexican_chunk_1950)
count_mexican_1956 <- length(mexican_chunk_1956)
count_mexican_1960 <- length(mexican_chunk_1960)
count_mexican_1965 <- length(mexican_chunk_1965)
count_mexican_1969 <- length(mexican_chunk_1969)
count_mexican_1975 <- length(mexican_chunk_1975)
count_mexican_1984 <- length(mexican_chunk_1984)
count_mexican_1992 <- length(mexican_chunk_1992)
count_mexican_1997 <- length(mexican_chunk_1997)
count_mexican_2004 <- length(mexican_chunk_2004)
count_mexican_2011 <- length(mexican_chunk_2011)
count_mexican_2017 <- length(mexican_chunk_2017)

count_chinese_1922 <- length(chinese_chunk_1922)
count_chinese_1927 <- length(chinese_chunk_1927)
count_chinese_1931 <- length(chinese_chunk_1931)
count_chinese_1934 <- length(chinese_chunk_1934)
count_chinese_1937 <- length(chinese_chunk_1937)
count_chinese_1940 <- length(chinese_chunk_1940)
count_chinese_1942 <- length(chinese_chunk_1942)
count_chinese_1945 <- length(chinese_chunk_1945)
count_chinese_1950 <- length(chinese_chunk_1950)
count_chinese_1956 <- length(chinese_chunk_1956)
count_chinese_1960 <- length(chinese_chunk_1960)
count_chinese_1965 <- length(chinese_chunk_1965)
count_chinese_1969 <- length(chinese_chunk_1969)
count_chinese_1975 <- length(chinese_chunk_1975)
count_chinese_1984 <- length(chinese_chunk_1984)
count_chinese_1992 <- length(chinese_chunk_1992)
count_chinese_1997 <- length(chinese_chunk_1997)
count_chinese_2004 <- length(chinese_chunk_2004)
count_chinese_2011 <- length(chinese_chunk_2011)
count_chinese_2017 <- length(chinese_chunk_2017)

count_cuban_1922 <- length(cuban_chunk_1922)
count_cuban_1927 <- length(cuban_chunk_1927)
count_cuban_1931 <- length(cuban_chunk_1931)
count_cuban_1934 <- length(cuban_chunk_1934)
count_cuban_1937 <- length(cuban_chunk_1937)
count_cuban_1940 <- length(cuban_chunk_1940)
count_cuban_1942 <- length(cuban_chunk_1942)
count_cuban_1945 <- length(cuban_chunk_1945)
count_cuban_1950 <- length(cuban_chunk_1950)
count_cuban_1956 <- length(cuban_chunk_1956)
count_cuban_1960 <- length(cuban_chunk_1960)
count_cuban_1965 <- length(cuban_chunk_1965)
count_cuban_1969 <- length(cuban_chunk_1969)
count_cuban_1975 <- length(cuban_chunk_1975)
count_cuban_1984 <- length(cuban_chunk_1984)
count_cuban_1992 <- length(cuban_chunk_1992)
count_cuban_1997 <- length(cuban_chunk_1997)
count_cuban_2004 <- length(cuban_chunk_2004)
count_cuban_2011 <- length(cuban_chunk_2011)
count_cuban_2017 <- length(cuban_chunk_2017)

count_muslim_1922 <- length(muslim_chunk_1922)
count_muslim_1927 <- length(muslim_chunk_1927)
count_muslim_1931 <- length(muslim_chunk_1931)
count_muslim_1934 <- length(muslim_chunk_1934)
count_muslim_1937 <- length(muslim_chunk_1937)
count_muslim_1940 <- length(muslim_chunk_1940)
count_muslim_1942 <- length(muslim_chunk_1942)
count_muslim_1945 <- length(muslim_chunk_1945)
count_muslim_1950 <- length(muslim_chunk_1950)
count_muslim_1956 <- length(muslim_chunk_1956)
count_muslim_1960 <- length(muslim_chunk_1960)
count_muslim_1965 <- length(muslim_chunk_1965)
count_muslim_1969 <- length(muslim_chunk_1969)
count_muslim_1975 <- length(muslim_chunk_1975)
count_muslim_1984 <- length(muslim_chunk_1984)
count_muslim_1992 <- length(muslim_chunk_1992)
count_muslim_1997 <- length(muslim_chunk_1997)
count_muslim_2004 <- length(muslim_chunk_2004)
count_muslim_2011 <- length(muslim_chunk_2011)
count_muslim_2017 <- length(muslim_chunk_2017)


# proportion
prop_italian_1922 <- length(italian_chunk_1922) / max(tidy_EP_1922$chunk)
prop_italian_1927 <- length(italian_chunk_1927) / max(tidy_EP_1927$chunk)
prop_italian_1931 <- length(italian_chunk_1931) / max(tidy_EP_1931$chunk)
prop_italian_1934 <- length(italian_chunk_1934) / max(tidy_EP_1934$chunk)
prop_italian_1937 <- length(italian_chunk_1937) / max(tidy_EP_1937$chunk)
prop_italian_1940 <- length(italian_chunk_1940) / max(tidy_EP_1940$chunk)
prop_italian_1942 <- length(italian_chunk_1942) / max(tidy_EP_1942$chunk)
prop_italian_1945 <- length(italian_chunk_1945) / max(tidy_EP_1945$chunk)
prop_italian_1950 <- length(italian_chunk_1950) / max(tidy_EP_1950$chunk)
prop_italian_1956 <- length(italian_chunk_1956) / max(tidy_EP_1956$chunk)
prop_italian_1960 <- length(italian_chunk_1960) / max(tidy_EP_1960$chunk)
prop_italian_1965 <- length(italian_chunk_1965) / max(tidy_EP_1965$chunk)
prop_italian_1969 <- length(italian_chunk_1969) / max(tidy_EP_1969$chunk)
prop_italian_1975 <- length(italian_chunk_1975) / max(tidy_EP_1975$chunk)
prop_italian_1984 <- length(italian_chunk_1984) / max(tidy_EP_1984$chunk)
prop_italian_1992 <- length(italian_chunk_1992) / max(tidy_EP_1992$chunk)
prop_italian_1997 <- length(italian_chunk_1997) / max(tidy_EP_1997$chunk)
prop_italian_2004 <- length(italian_chunk_2004) / max(tidy_EP_2004$chunk)
prop_italian_2011 <- length(italian_chunk_2011) / max(tidy_EP_2011$chunk)
prop_italian_2017 <- length(italian_chunk_2017) / max(tidy_EP_2017$chunk)

# proportion
prop_jewish_1922 <- length(jewish_chunk_1922) / max(tidy_EP_1922$chunk)
prop_jewish_1927 <- length(jewish_chunk_1927) / max(tidy_EP_1927$chunk)
prop_jewish_1931 <- length(jewish_chunk_1931) / max(tidy_EP_1931$chunk)
prop_jewish_1934 <- length(jewish_chunk_1934) / max(tidy_EP_1934$chunk)
prop_jewish_1937 <- length(jewish_chunk_1937) / max(tidy_EP_1937$chunk)
prop_jewish_1940 <- length(jewish_chunk_1940) / max(tidy_EP_1940$chunk)
prop_jewish_1942 <- length(jewish_chunk_1942) / max(tidy_EP_1942$chunk)
prop_jewish_1945 <- length(jewish_chunk_1945) / max(tidy_EP_1945$chunk)
prop_jewish_1950 <- length(jewish_chunk_1950) / max(tidy_EP_1950$chunk)
prop_jewish_1956 <- length(jewish_chunk_1956) / max(tidy_EP_1956$chunk)
prop_jewish_1960 <- length(jewish_chunk_1960) / max(tidy_EP_1960$chunk)
prop_jewish_1965 <- length(jewish_chunk_1965) / max(tidy_EP_1965$chunk)
prop_jewish_1969 <- length(jewish_chunk_1969) / max(tidy_EP_1969$chunk)
prop_jewish_1975 <- length(jewish_chunk_1975) / max(tidy_EP_1975$chunk)
prop_jewish_1984 <- length(jewish_chunk_1984) / max(tidy_EP_1984$chunk)
prop_jewish_1992 <- length(jewish_chunk_1992) / max(tidy_EP_1992$chunk)
prop_jewish_1997 <- length(jewish_chunk_1997) / max(tidy_EP_1997$chunk)
prop_jewish_2004 <- length(jewish_chunk_2004) / max(tidy_EP_2004$chunk)
prop_jewish_2011 <- length(jewish_chunk_2011) / max(tidy_EP_2011$chunk)
prop_jewish_2017 <- length(jewish_chunk_2017) / max(tidy_EP_2017$chunk)

# proportion
prop_irish_1922 <- length(irish_chunk_1922) / max(tidy_EP_1922$chunk)
prop_irish_1927 <- length(irish_chunk_1927) / max(tidy_EP_1927$chunk)
prop_irish_1931 <- length(irish_chunk_1931) / max(tidy_EP_1931$chunk)
prop_irish_1934 <- length(irish_chunk_1934) / max(tidy_EP_1934$chunk)
prop_irish_1937 <- length(irish_chunk_1937) / max(tidy_EP_1937$chunk)
prop_irish_1940 <- length(irish_chunk_1940) / max(tidy_EP_1940$chunk)
prop_irish_1942 <- length(irish_chunk_1942) / max(tidy_EP_1942$chunk)
prop_irish_1945 <- length(irish_chunk_1945) / max(tidy_EP_1945$chunk)
prop_irish_1950 <- length(irish_chunk_1950) / max(tidy_EP_1950$chunk)
prop_irish_1956 <- length(irish_chunk_1956) / max(tidy_EP_1956$chunk)
prop_irish_1960 <- length(irish_chunk_1960) / max(tidy_EP_1960$chunk)
prop_irish_1965 <- length(irish_chunk_1965) / max(tidy_EP_1965$chunk)
prop_irish_1969 <- length(irish_chunk_1969) / max(tidy_EP_1969$chunk)
prop_irish_1975 <- length(irish_chunk_1975) / max(tidy_EP_1975$chunk)
prop_irish_1984 <- length(irish_chunk_1984) / max(tidy_EP_1984$chunk)
prop_irish_1992 <- length(irish_chunk_1992) / max(tidy_EP_1992$chunk)
prop_irish_1997 <- length(irish_chunk_1997) / max(tidy_EP_1997$chunk)
prop_irish_2004 <- length(irish_chunk_2004) / max(tidy_EP_2004$chunk)
prop_irish_2011 <- length(irish_chunk_2011) / max(tidy_EP_2011$chunk)
prop_irish_2017 <- length(irish_chunk_2017) / max(tidy_EP_2017$chunk)

# proportion
prop_catholic_1922 <- length(catholic_chunk_1922) / max(tidy_EP_1922$chunk)
prop_catholic_1927 <- length(catholic_chunk_1927) / max(tidy_EP_1927$chunk)
prop_catholic_1931 <- length(catholic_chunk_1931) / max(tidy_EP_1931$chunk)
prop_catholic_1934 <- length(catholic_chunk_1934) / max(tidy_EP_1934$chunk)
prop_catholic_1937 <- length(catholic_chunk_1937) / max(tidy_EP_1937$chunk)
prop_catholic_1940 <- length(catholic_chunk_1940) / max(tidy_EP_1940$chunk)
prop_catholic_1942 <- length(catholic_chunk_1942) / max(tidy_EP_1942$chunk)
prop_catholic_1945 <- length(catholic_chunk_1945) / max(tidy_EP_1945$chunk)
prop_catholic_1950 <- length(catholic_chunk_1950) / max(tidy_EP_1950$chunk)
prop_catholic_1956 <- length(catholic_chunk_1956) / max(tidy_EP_1956$chunk)
prop_catholic_1960 <- length(catholic_chunk_1960) / max(tidy_EP_1960$chunk)
prop_catholic_1965 <- length(catholic_chunk_1965) / max(tidy_EP_1965$chunk)
prop_catholic_1969 <- length(catholic_chunk_1969) / max(tidy_EP_1969$chunk)
prop_catholic_1975 <- length(catholic_chunk_1975) / max(tidy_EP_1975$chunk)
prop_catholic_1984 <- length(catholic_chunk_1984) / max(tidy_EP_1984$chunk)
prop_catholic_1992 <- length(catholic_chunk_1992) / max(tidy_EP_1992$chunk)
prop_catholic_1997 <- length(catholic_chunk_1997) / max(tidy_EP_1997$chunk)
prop_catholic_2004 <- length(catholic_chunk_2004) / max(tidy_EP_2004$chunk)
prop_catholic_2011 <- length(catholic_chunk_2011) / max(tidy_EP_2011$chunk)
prop_catholic_2017 <- length(catholic_chunk_2017) / max(tidy_EP_2017$chunk)


# proportion
prop_mexican_1922 <- length(mexican_chunk_1922) / max(tidy_EP_1922$chunk)
prop_mexican_1927 <- length(mexican_chunk_1927) / max(tidy_EP_1927$chunk)
prop_mexican_1931 <- length(mexican_chunk_1931) / max(tidy_EP_1931$chunk)
prop_mexican_1934 <- length(mexican_chunk_1934) / max(tidy_EP_1934$chunk)
prop_mexican_1937 <- length(mexican_chunk_1937) / max(tidy_EP_1937$chunk)
prop_mexican_1940 <- length(mexican_chunk_1940) / max(tidy_EP_1940$chunk)
prop_mexican_1942 <- length(mexican_chunk_1942) / max(tidy_EP_1942$chunk)
prop_mexican_1945 <- length(mexican_chunk_1945) / max(tidy_EP_1945$chunk)
prop_mexican_1950 <- length(mexican_chunk_1950) / max(tidy_EP_1950$chunk)
prop_mexican_1956 <- length(mexican_chunk_1956) / max(tidy_EP_1956$chunk)
prop_mexican_1960 <- length(mexican_chunk_1960) / max(tidy_EP_1960$chunk)
prop_mexican_1965 <- length(mexican_chunk_1965) / max(tidy_EP_1965$chunk)
prop_mexican_1969 <- length(mexican_chunk_1969) / max(tidy_EP_1969$chunk)
prop_mexican_1975 <- length(mexican_chunk_1975) / max(tidy_EP_1975$chunk)
prop_mexican_1984 <- length(mexican_chunk_1984) / max(tidy_EP_1984$chunk)
prop_mexican_1992 <- length(mexican_chunk_1992) / max(tidy_EP_1992$chunk)
prop_mexican_1997 <- length(mexican_chunk_1997) / max(tidy_EP_1997$chunk)
prop_mexican_2004 <- length(mexican_chunk_2004) / max(tidy_EP_2004$chunk)
prop_mexican_2011 <- length(mexican_chunk_2011) / max(tidy_EP_2011$chunk)
prop_mexican_2017 <- length(mexican_chunk_2017) / max(tidy_EP_2017$chunk)

# proportion
prop_chinese_1922 <- length(chinese_chunk_1922) / max(tidy_EP_1922$chunk)
prop_chinese_1927 <- length(chinese_chunk_1927) / max(tidy_EP_1927$chunk)
prop_chinese_1931 <- length(chinese_chunk_1931) / max(tidy_EP_1931$chunk)
prop_chinese_1934 <- length(chinese_chunk_1934) / max(tidy_EP_1934$chunk)
prop_chinese_1937 <- length(chinese_chunk_1937) / max(tidy_EP_1937$chunk)
prop_chinese_1940 <- length(chinese_chunk_1940) / max(tidy_EP_1940$chunk)
prop_chinese_1942 <- length(chinese_chunk_1942) / max(tidy_EP_1942$chunk)
prop_chinese_1945 <- length(chinese_chunk_1945) / max(tidy_EP_1945$chunk)
prop_chinese_1950 <- length(chinese_chunk_1950) / max(tidy_EP_1950$chunk)
prop_chinese_1956 <- length(chinese_chunk_1956) / max(tidy_EP_1956$chunk)
prop_chinese_1960 <- length(chinese_chunk_1960) / max(tidy_EP_1960$chunk)
prop_chinese_1965 <- length(chinese_chunk_1965) / max(tidy_EP_1965$chunk)
prop_chinese_1969 <- length(chinese_chunk_1969) / max(tidy_EP_1969$chunk)
prop_chinese_1975 <- length(chinese_chunk_1975) / max(tidy_EP_1975$chunk)
prop_chinese_1984 <- length(chinese_chunk_1984) / max(tidy_EP_1984$chunk)
prop_chinese_1992 <- length(chinese_chunk_1992) / max(tidy_EP_1992$chunk)
prop_chinese_1997 <- length(chinese_chunk_1997) / max(tidy_EP_1997$chunk)
prop_chinese_2004 <- length(chinese_chunk_2004) / max(tidy_EP_2004$chunk)
prop_chinese_2011 <- length(chinese_chunk_2011) / max(tidy_EP_2011$chunk)
prop_chinese_2017 <- length(chinese_chunk_2017) / max(tidy_EP_2017$chunk)

# proportion
prop_cuban_1922 <- length(cuban_chunk_1922) / max(tidy_EP_1922$chunk)
prop_cuban_1927 <- length(cuban_chunk_1927) / max(tidy_EP_1927$chunk)
prop_cuban_1931 <- length(cuban_chunk_1931) / max(tidy_EP_1931$chunk)
prop_cuban_1934 <- length(cuban_chunk_1934) / max(tidy_EP_1934$chunk)
prop_cuban_1937 <- length(cuban_chunk_1937) / max(tidy_EP_1937$chunk)
prop_cuban_1940 <- length(cuban_chunk_1940) / max(tidy_EP_1940$chunk)
prop_cuban_1942 <- length(cuban_chunk_1942) / max(tidy_EP_1942$chunk)
prop_cuban_1945 <- length(cuban_chunk_1945) / max(tidy_EP_1945$chunk)
prop_cuban_1950 <- length(cuban_chunk_1950) / max(tidy_EP_1950$chunk)
prop_cuban_1956 <- length(cuban_chunk_1956) / max(tidy_EP_1956$chunk)
prop_cuban_1960 <- length(cuban_chunk_1960) / max(tidy_EP_1960$chunk)
prop_cuban_1965 <- length(cuban_chunk_1965) / max(tidy_EP_1965$chunk)
prop_cuban_1969 <- length(cuban_chunk_1969) / max(tidy_EP_1969$chunk)
prop_cuban_1975 <- length(cuban_chunk_1975) / max(tidy_EP_1975$chunk)
prop_cuban_1984 <- length(cuban_chunk_1984) / max(tidy_EP_1984$chunk)
prop_cuban_1992 <- length(cuban_chunk_1992) / max(tidy_EP_1992$chunk)
prop_cuban_1997 <- length(cuban_chunk_1997) / max(tidy_EP_1997$chunk)
prop_cuban_2004 <- length(cuban_chunk_2004) / max(tidy_EP_2004$chunk)
prop_cuban_2011 <- length(cuban_chunk_2011) / max(tidy_EP_2011$chunk)
prop_cuban_2017 <- length(cuban_chunk_2017) / max(tidy_EP_2017$chunk)

# proportion
prop_muslim_1922 <- length(muslim_chunk_1922) / max(tidy_EP_1922$chunk)
prop_muslim_1927 <- length(muslim_chunk_1927) / max(tidy_EP_1927$chunk)
prop_muslim_1931 <- length(muslim_chunk_1931) / max(tidy_EP_1931$chunk)
prop_muslim_1934 <- length(muslim_chunk_1934) / max(tidy_EP_1934$chunk)
prop_muslim_1937 <- length(muslim_chunk_1937) / max(tidy_EP_1937$chunk)
prop_muslim_1940 <- length(muslim_chunk_1940) / max(tidy_EP_1940$chunk)
prop_muslim_1942 <- length(muslim_chunk_1942) / max(tidy_EP_1942$chunk)
prop_muslim_1945 <- length(muslim_chunk_1945) / max(tidy_EP_1945$chunk)
prop_muslim_1950 <- length(muslim_chunk_1950) / max(tidy_EP_1950$chunk)
prop_muslim_1956 <- length(muslim_chunk_1956) / max(tidy_EP_1956$chunk)
prop_muslim_1960 <- length(muslim_chunk_1960) / max(tidy_EP_1960$chunk)
prop_muslim_1965 <- length(muslim_chunk_1965) / max(tidy_EP_1965$chunk)
prop_muslim_1969 <- length(muslim_chunk_1969) / max(tidy_EP_1969$chunk)
prop_muslim_1975 <- length(muslim_chunk_1975) / max(tidy_EP_1975$chunk)
prop_muslim_1984 <- length(muslim_chunk_1984) / max(tidy_EP_1984$chunk)
prop_muslim_1992 <- length(muslim_chunk_1992) / max(tidy_EP_1992$chunk)
prop_muslim_1997 <- length(muslim_chunk_1997) / max(tidy_EP_1997$chunk)
prop_muslim_2004 <- length(muslim_chunk_2004) / max(tidy_EP_2004$chunk)
prop_muslim_2011 <- length(muslim_chunk_2011) / max(tidy_EP_2011$chunk)
prop_muslim_2017 <- length(muslim_chunk_2017) / max(tidy_EP_2017$chunk)

# merge all? or export all?

count_prop_list <- data.frame( 
  edition       = c("1922", "1927", "1934", "1937", "1940", "1942",
                    "1945", "1950", "1956", "1960", "1965", "1969", 
                    "1975", "1984", "1992", "1997", "2004", "2011", "2017"),
  italian_count = 
    c(count_italian_1922, count_italian_1927, count_italian_1934,
      count_italian_1937, count_italian_1940, count_italian_1942,
      count_italian_1945, count_italian_1950, count_italian_1956,
      count_italian_1960, count_italian_1965, count_italian_1969,
      count_italian_1975, count_italian_1984, count_italian_1992,
      count_italian_1997, count_italian_2004, count_italian_2011,
      count_italian_2017),
  italian_prop =
    c(prop_italian_1922, prop_italian_1927, prop_italian_1934,
      prop_italian_1937, prop_italian_1940, prop_italian_1942,
      prop_italian_1945, prop_italian_1950, prop_italian_1956,
      prop_italian_1960, prop_italian_1965, prop_italian_1969,
      prop_italian_1975, prop_italian_1984, prop_italian_1992,
      prop_italian_1997, prop_italian_2004, prop_italian_2011,
      prop_italian_2017),
  jewish_count = 
    c(count_jewish_1922, count_jewish_1927, count_jewish_1934,
      count_jewish_1937, count_jewish_1940, count_jewish_1942,
      count_jewish_1945, count_jewish_1950, count_jewish_1956,
      count_jewish_1960, count_jewish_1965, count_jewish_1969,
      count_jewish_1975, count_jewish_1984, count_jewish_1992,
      count_jewish_1997, count_jewish_2004, count_jewish_2011,
      count_jewish_2017),
  jewish_prop =
    c(prop_jewish_1922, prop_jewish_1927, prop_jewish_1934,
      prop_jewish_1937, prop_jewish_1940, prop_jewish_1942,
      prop_jewish_1945, prop_jewish_1950, prop_jewish_1956,
      prop_jewish_1960, prop_jewish_1965, prop_jewish_1969,
      prop_jewish_1975, prop_jewish_1984, prop_jewish_1992,
      prop_jewish_1997, prop_jewish_2004, prop_jewish_2011,
      prop_jewish_2017),
  irish_count = 
    c(count_irish_1922, count_irish_1927, count_irish_1934,
      count_irish_1937, count_irish_1940, count_irish_1942,
      count_irish_1945, count_irish_1950, count_irish_1956,
      count_irish_1960, count_irish_1965, count_irish_1969,
      count_irish_1975, count_irish_1984, count_irish_1992,
      count_irish_1997, count_irish_2004, count_irish_2011,
      count_irish_2017) ,
  irish_prop =
    c(prop_irish_1922, prop_irish_1927, prop_irish_1934,
      prop_irish_1937, prop_irish_1940, prop_irish_1942,
      prop_irish_1945, prop_irish_1950, prop_irish_1956,
      prop_irish_1960, prop_irish_1965, prop_irish_1969,
      prop_irish_1975, prop_irish_1984, prop_irish_1992,
      prop_irish_1997, prop_irish_2004, prop_irish_2011,
      prop_irish_2017),
  catholic_count = 
    c(count_catholic_1922, count_catholic_1927, count_catholic_1934,
      count_catholic_1937, count_catholic_1940, count_catholic_1942,
      count_catholic_1945, count_catholic_1950, count_catholic_1956,
      count_catholic_1960, count_catholic_1965, count_catholic_1969,
      count_catholic_1975, count_catholic_1984, count_catholic_1992,
      count_catholic_1997, count_catholic_2004, count_catholic_2011,
      count_catholic_2017) ,
  catholic_prop =
    c(prop_catholic_1922, prop_catholic_1927, prop_catholic_1934,
      prop_catholic_1937, prop_catholic_1940, prop_catholic_1942,
      prop_catholic_1945, prop_catholic_1950, prop_catholic_1956,
      prop_catholic_1960, prop_catholic_1965, prop_catholic_1969,
      prop_catholic_1975, prop_catholic_1984, prop_catholic_1992,
      prop_catholic_1997, prop_catholic_2004, prop_catholic_2011,
      prop_catholic_2017),
  mexican_count = 
    c(count_mexican_1922, count_mexican_1927, count_mexican_1934,
      count_mexican_1937, count_mexican_1940, count_mexican_1942,
      count_mexican_1945, count_mexican_1950, count_mexican_1956,
      count_mexican_1960, count_mexican_1965, count_mexican_1969,
      count_mexican_1975, count_mexican_1984, count_mexican_1992,
      count_mexican_1997, count_mexican_2004, count_mexican_2011,
      count_mexican_2017) ,
  mexican_prop =
    c(prop_mexican_1922, prop_mexican_1927, prop_mexican_1934,
      prop_mexican_1937, prop_mexican_1940, prop_mexican_1942,
      prop_mexican_1945, prop_mexican_1950, prop_mexican_1956,
      prop_mexican_1960, prop_mexican_1965, prop_mexican_1969,
      prop_mexican_1975, prop_mexican_1984, prop_mexican_1992,
      prop_mexican_1997, prop_mexican_2004, prop_mexican_2011,
      prop_mexican_2017),
  chinese_count = 
    c(count_chinese_1922, count_chinese_1927, count_chinese_1934,
      count_chinese_1937, count_chinese_1940, count_chinese_1942,
      count_chinese_1945, count_chinese_1950, count_chinese_1956,
      count_chinese_1960, count_chinese_1965, count_chinese_1969,
      count_chinese_1975, count_chinese_1984, count_chinese_1992,
      count_chinese_1997, count_chinese_2004, count_chinese_2011,
      count_chinese_2017) ,
  chinese_prop =
    c(prop_chinese_1922, prop_chinese_1927, prop_chinese_1934,
      prop_chinese_1937, prop_chinese_1940, prop_chinese_1942,
      prop_chinese_1945, prop_chinese_1950, prop_chinese_1956,
      prop_chinese_1960, prop_chinese_1965, prop_chinese_1969,
      prop_chinese_1975, prop_chinese_1984, prop_chinese_1992,
      prop_chinese_1997, prop_chinese_2004, prop_chinese_2011,
      prop_chinese_2017),
  cuban_count = 
    c(count_cuban_1922, count_cuban_1927, count_cuban_1934,
      count_cuban_1937, count_cuban_1940, count_cuban_1942,
      count_cuban_1945, count_cuban_1950, count_cuban_1956,
      count_cuban_1960, count_cuban_1965, count_cuban_1969,
      count_cuban_1975, count_cuban_1984, count_cuban_1992,
      count_cuban_1997, count_cuban_2004, count_cuban_2011,
      count_cuban_2017) ,
  cuban_prop =
    c(prop_cuban_1922, prop_cuban_1927, prop_cuban_1934,
      prop_cuban_1937, prop_cuban_1940, prop_cuban_1942,
      prop_cuban_1945, prop_cuban_1950, prop_cuban_1956,
      prop_cuban_1960, prop_cuban_1965, prop_cuban_1969,
      prop_cuban_1975, prop_cuban_1984, prop_cuban_1992,
      prop_cuban_1997, prop_cuban_2004, prop_cuban_2011,
      prop_cuban_2017),
  muslim_count = 
    c(count_muslim_1922, count_muslim_1927, count_muslim_1934,
      count_muslim_1937, count_muslim_1940, count_muslim_1942,
      count_muslim_1945, count_muslim_1950, count_muslim_1956,
      count_muslim_1960, count_muslim_1965, count_muslim_1969,
      count_muslim_1975, count_muslim_1984, count_muslim_1992,
      count_muslim_1997, count_muslim_2004, count_muslim_2011,
      count_muslim_2017) ,
  muslim_prop =
    c(prop_muslim_1922, prop_muslim_1927, prop_muslim_1934,
      prop_muslim_1937, prop_muslim_1940, prop_muslim_1942,
      prop_muslim_1945, prop_muslim_1950, prop_muslim_1956,
      prop_muslim_1960, prop_muslim_1965, prop_muslim_1969,
      prop_muslim_1975, prop_muslim_1984, prop_muslim_1992,
      prop_muslim_1997, prop_muslim_2004, prop_muslim_2011,
      prop_muslim_2017)
)


write.csv(count_prop_list,"count_prop_list.csv")





############ Table and Figure summary ############

# Table 1 reported in tfidf_cos_sim.csv
#     Prepared in Excel and exported to word

# Table 2 reported in count_prop_list.csv
#     Prepared in Excel and exported to word

# Combined sentiments
print(sent_scores_plot_com)
# Grouped Sentiments 

print(plot_group1_sent)
print(plot_group2_sent)

# Combined Meaning
print(tidy_EP_chunks_combined_closeness_plot)

# Grouped Meaning
print(plot_group1_meaning)
print(plot_group2_meaning)


# print all figures to PDF
pdf("kline(2021_3_1)EP-integration.pdf")
print(sent_scores_plot_com)
plot_group1_sent
plot_group2_sent
tidy_EP_chunks_combined_closeness_plot
plot_group1_meaning
plot_group2_meaning
dev.off()


############
















