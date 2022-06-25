# R-code
Coding Script Copied from RStudio:
##### Introduction #####
#This is an R script. I am trying to make my analysis clear and reproducible.
#I will structure it in subsections (#####)
##### Setup #####
#First I need to load the data and the required packages
#If everything goes well, the data should load automatically from my dropbox
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets wd to location of script
data <- openxlsx::read.xlsx("data.xlsx")
keywords <- tolower(scan("keywords.txt", what = "", sep = "\n")) #read in the keywords in lower case
keywords_open_science <- tolower(scan("keywords_open_science.txt", what = "", sep = "\n")) #read in the keywords in lower case
library (janitor)
library (tidyverse)
library (tidytext)
##### Data manipulation #####
#there will be slight manipulations necessary due to small errors when loading the data
### remove non-articles 
data <- data [!is.na(data$Abstract),] #removes the articles that do not have an abstract
### remove articles from 2022
data <- subset (data, Publication.Year != 2022)
### manipulation of variables
#convert stuff to lower
data$Source.Title <- tolower(data$Source.Title) #converts all journal names to lower case
data$Abstract <- tolower(data$Abstract)
### creating variables
data$Number.of.Pages <- as.numeric(data$End.Page) - as.numeric(data$Start.Page) #create number of pages variable
tabyl(data$Number.of.Pages)
#TODO check whether I should remove the short pieces
##### export file for manual analysis ######
data_export <- data %>%
  select (Article.Title, Source.Title, Abstract, Publication.Year, Number.of.Pages, DOI)
data_export$method_description <- NA
data_export$supervised <- NA
data_export$unsupvervised <- NA
data_export$dictionary <- NA
data_export$software <- NA
data_export$keywords <- NA  
openxlsx::write.xlsx(data_export, "coding_file.xlsx")
##### Descriptives #####
#I can use some descriptive operations to answer easy questions about our data
#How many articles are there per journal?
tabyl(data$Source.Title) #frequency table of articles per journal
#How many articles are there per year?
tabyl(data$Publication.Year) #frequency table of publications by year
##### dictionary analysis #####
### check which abstracts contain the keywords
#https://stackoverflow.com/questions/65227565/r-create-category-column-reflecting-match-between-a-dictionary-and-column-in-df 
data <- data %>%
  mutate (contain_keyword = 1L*map_lgl(Abstract, ~any(str_detect(.x, keywords)))) #creates extra column that indicates whether the abstract contains one of the keywords
data <- data %>%
  mutate (contain_osf_keyword = 1L*map_lgl(Abstract, ~any(str_detect(.x, keywords_open_science)))) #creates extra column that indicates whether the abstract contains one of the osf keywords
#how many articles contain keywords and how many not?
table(data$contain_keyword)
table(data$contain_osf_keyword)
#TODO IDEA! Let's review the articles manually whether they are about a computational method or not and develop keywords based on that. Then get an accurate match between dictionary analysis and handish analysis.
##### text analysis #####
### initial word count ###
token_abstracts <- data %>% #by unnesting the tokens I create so called unigrams meaning single words
  unnest_tokens(word, Abstract)
token_abstracts %>% #shows the 10 most commonly used words
  count(word, sort = TRUE) %>% #counting
  head (10) #only showing the 10 most frequent words
#we see that there are a lot of stopwords like "the" and "of". Let's remove them
stopwords <- get_stopwords()
table(stopwords$lexicon)
#I can see that I can remove 175 stopwords from the snowball library
#now recount without stopwords
token_abstracts %>%
  anti_join(stopwords) %>%
  count(word, sort = TRUE) %>%
  head (10)
###### notes #####
data[is.na(data$Publication.Year),] %>% select(DOI)
data[is.na(data$Publication.Year),] %>% select(Article.Title)
data[is.na(data$Number.of.Pages),] %>% select(DOI)
##### reimport data file #####
data_coded <- openxlsx::read.xlsx("coding_file_manual.xlsx") #loading in new data
data_coded <- data_coded [complete.cases(data_coded),] #remove the two red rows
str(data_coded) #shows the structure of it
data_coded$computational <- data_coded$supervised + data_coded$unsupvervised + data_coded$dictionary
tabyl(data_coded$computational)
computational <- subset (data_coded, software != 0)

#TODO Get the coding of the software straight (data_coded$software)
##### descriptive by years and journal #####
table (data_coded$software, data_coded$Source.Title)
table (data_coded$Source.Title)
table (computational$Source.Title)
table (computational$Source.Title) / table(data_coded$Source.Title) * 100 #proportion of computational methods published per article in a journal in percent
table (computational$Publication.Year)
##### text analysis based on keywords #####
keywords_vec <- unlist(str_split(computational$keywords, "\n")) #create vector of keywords that are seperated by \n (new line)
wordcloud::wordcloud(keywords_vec)

![image](https://user-images.githubusercontent.com/108176526/175768298-afd7cdb3-6f1f-4479-9d14-34a0e26958ac.png)
