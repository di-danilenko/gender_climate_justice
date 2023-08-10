library("stm")
library("quanteda")
library("stminsights")
library("tidyverse")
library("readxl")
library("dplyr")
library("ggplot2")
library("tidytext")
library("gutenbergr")
library("reshape2")
library("gridExtra")
library("forestplot")
library("wesanderson")
library("tidyr")

### LOAD DATA ###
setwd('/Users/dianadanilenko/Desktop/methods/R_scripts')
data1 <- read.csv('/Users/dianadanilenko/Desktop/methods/python_scripts/data/df_first_1804.csv', header = TRUE) 
data2 <- read.csv('/Users/dianadanilenko/Desktop/methods/python_scripts/data/df_last_1804.csv', header = TRUE)
data3 <- read.csv('/Users/dianadanilenko/Desktop/methods/python_scripts/data/df_maj_1804.csv', header = TRUE)

data1$text <- "text"
data1$gii_quartile <- ifelse(is.na(data1$gii_quartile), 0, data1$gii_quartile)
data1$impact_quartile = as.numeric(data1$impact_quartile)
data1$impact_quartile <- ifelse(is.na(data1$impact_quartile), 0, data1$impact_quartile)
data2$text <- "text"
data2$gii_quartile <- ifelse(is.na(data2$gii_quartile), 0, data2$gii_quartile)
data2$impact_quartile = as.numeric(data2$impact_quartile)
data2$impact_quartile <- ifelse(is.na(data2$impact_quartile), 0, data2$impact_quartile)
data3$text <- "text"
data3$gii_quartile <- ifelse(is.na(data3$gii_quartile), 0, data3$gii_quartile)
data3$impact_quartile = as.numeric(data3$impact_quartile)
data3$impact_quartile <- ifelse(is.na(data3$impact_quartile), 0, data3$impact_quartile)

### MAKE DESCRIPTIVE STATISTICS GRAPHS ###

# plot cumulative yearly publications (a line)
data2_year <- subset(data2, year > 0)
data2_year_counts <- data2_year %>% 
  group_by(year, id) %>% 
  summarise(count = n()) %>% 
  ungroup()
year_counts <- data2_year_counts %>% 
  group_by(year) %>% 
  summarise(papers_published = sum(count))

cumulative_papers <- year_counts %>%
  mutate(cumulative_sum = cumsum(papers_published))
write_csv(cumulative_papers,"yearly_cumulative_papers2.csv")

### DATA PROCESSING ###
# building a corpus using quanteda
# combine title, abstract and keywords in a column called text
data1$text <- paste(data1$title, data1$abstract, data1$keywords)
data2$text <- paste(data2$title, data2$abstract, data2$keywords)
data3$text <- paste(data3$title, data3$abstract, data3$keywords)

# get rid of the copyright information in our corpus
data1 <- data1 %>% separate(text, c("text","copyright"), sep = "\\(c\\)\\s*\\d+", extra="merge", remove = TRUE) #removes (c) followed by numbers - this covers most copyright messages
data1$text<-gsub("all rights reserved"," ",as.character(data1$text))
data2 <- data2 %>% separate(text, c("text","copyright"), sep = "\\(c\\)\\s*\\d+", extra="merge", remove = TRUE) #removes (c) followed by numbers - this covers most copyright messages
data2$text<-gsub("all rights reserved"," ",as.character(data2$text))
data3 <- data3 %>% separate(text, c("text","copyright"), sep = "\\(c\\)\\s*\\d+", extra="merge", remove = TRUE) #removes (c) followed by numbers - this covers most copyright messages
data3$text<-gsub("all rights reserved"," ",as.character(data3$text))

# create the actual CORPUS 
corp1 <- corpus(data1, text_field = "text")
corp2 <- corpus(data2, text_field = "text")
corp3 <- corpus(data3, text_field = "text")
summary(corp1, 3) 

# the function automatically assumes all other columns contain document variables
head(docvars(corp3)) 

# remove documents with a missing gender value
corp1 <- corpus_subset(corp1, first_author_female != 'NA') 
ndoc(corp1)
corp2 <- corpus_subset(corp2, last_author_female != 'NA') 
ndoc(corp2)
corp3 <- corpus_subset(corp3, majority_female_binary != 'NA') 
ndoc(corp3)

# remove documents with a missing year
corp1 <- corpus_subset(corp1, year > 0) 
ndoc(corp1)
corp2 <- corpus_subset(corp2, year > 0) 
ndoc(corp2)
corp3 <- corpus_subset(corp3, year > 0) 
ndoc(corp3)

# remove documents with a missing gii_quartile
corp1 <- corpus_subset(corp1, gii_quartile > 0) 
ndoc(corp1)
corp2 <- corpus_subset(corp2, gii_quartile > 0) 
ndoc(corp2)
corp3 <- corpus_subset(corp3, gii_quartile > 0) 
ndoc(corp3)

# remove documents with a missing impact_factor
corp1 <- corpus_subset(corp1, impact_quartile > 0) 
ndoc(corp1)
corp2 <- corpus_subset(corp2, impact_quartile > 0) 
ndoc(corp2)
corp3 <- corpus_subset(corp3, impact_quartile > 0) 
ndoc(corp3)

# remove documents with a missing abstract
corp1 <- corpus_subset(corp1, abstract != "") 
ndoc(corp1)
corp2 <- corpus_subset(corp2, abstract != "") 
ndoc(corp2)
corp3 <- corpus_subset(corp3, abstract != "") 
ndoc(corp3)

# create TOKENS from this - this basically just means cutting up the text into individual words
toks1 <- tokens(corp1)
toks2 <- tokens(corp2)
toks3 <- tokens(corp3)

#the below is a fairly standard list for pre-processing
toks1 <- tokens(corp1, remove_punct = TRUE, remove_symbols = TRUE) %>% #create tokens w/o punctuation or symbols
  tokens_tolower() %>% #lowercase -- note the pipe means this function doesn't need any input
  tokens_remove(pattern = stopwords("en"), #remove stopwords
                min_nchar = 2) %>% #remove short words. AMR is three letters, so best keep it to 2
  tokens_wordstem(language = "en") #Using snowball stemmer
toks2 <- tokens(corp2, remove_punct = TRUE, remove_symbols = TRUE) %>% #create tokens w/o punctuation or symbols
  tokens_tolower() %>% #lowercase -- note the pipe means this function doesn't need any input
  tokens_remove(pattern = stopwords("en"), #remove stopwords
                min_nchar = 2) %>% #remove short words. AMR is three letters, so best keep it to 2
  tokens_wordstem(language = "en") #Using snowball stemmer
toks3 <- tokens(corp3, remove_punct = TRUE, remove_symbols = TRUE) %>% #create tokens w/o punctuation or symbols
  tokens_tolower() %>% #lowercase -- note the pipe means this function doesn't need any input
  tokens_remove(pattern = stopwords("en"), #remove stopwords
                min_nchar = 2) %>% #remove short words. AMR is three letters, so best keep it to 2
  tokens_wordstem(language = "en") #Using snowball stemmer

# create two dfms: one with unigrams and one with bi-grams
dfm_single1 = dfm(toks1)%>%
  dfm_trim(min_termfreq = 100, termfreq_type = "count", max_docfreq=0.95, docfreq_type = "prop")
dfm_single2 = dfm(toks2)%>%
  dfm_trim(min_termfreq = 100, termfreq_type = "count", max_docfreq=0.95, docfreq_type = "prop")
dfm_single3 = dfm(toks3)%>%
  dfm_trim(min_termfreq = 100, termfreq_type = "count", max_docfreq=0.95, docfreq_type = "prop")

# we only want to keep frequent bigrams as otherwise it's mostly just duplicating information
# hence, we set a higher min term frequency
toks_bigram1 <- tokens_ngrams(toks1, n = 2, skip = 1:2)
dfm_bigram1 <- dfm(toks_bigram1) %>%
  dfm_trim(min_termfreq = 1000, termfreq_type = "count", max_docfreq=0.95, docfreq_type = "prop")
toks_bigram2 <- tokens_ngrams(toks2, n = 2, skip = 1:2)
dfm_bigram2 <- dfm(toks_bigram2) %>%
  dfm_trim(min_termfreq = 1000, termfreq_type = "count", max_docfreq=0.95, docfreq_type = "prop")
toks_bigram3 <- tokens_ngrams(toks3, n = 2, skip = 1:2)
dfm_bigram3 <- dfm(toks_bigram3) %>%
  dfm_trim(min_termfreq = 1000, termfreq_type = "count", max_docfreq=0.95, docfreq_type = "prop")

# combine
dfm1 = cbind(dfm_bigram1, dfm_single1)
dfm2 = cbind(dfm_bigram2, dfm_single2)
dfm3 = cbind(dfm_bigram3, dfm_single3)
# show most-frequent tokens:
topfeatures(dfm3, 15) # should include some bi-grams now

### TOPIC MODELLING ###
# prevalence co-variates
# year (continuous), gii_quartile and impact_quartile as controls and first_author_female as predictor
model1_90 <- stm(dfm1, K=90, seed=53,
             prevalence =~s(year) + gii_quartile + impact_quartile + first_author_female,
             max.em.its = 75)
             #control = list(alpha = 0.5, Lower than default of 50/k to allow documents to have more topics -> makes small topics more likely to show up
                           #eta = 0.1))  Higher than default of 0.01 to create topics composed of more words -> more clarity for complex topics
model2_90 <- stm(dfm2, K=90, seed=53,
              prevalence =~s(year) + gii_quartile + impact_quartile + last_author_female,
              max.em.its = 75) 
# this one will probably need even smaller K
model3_90 <- stm(dfm3, K=90, seed=53,
              prevalence =~s(year) + gii_quartile + impact_quartile + majority_female_binary,
              max.em.its = 75)
# export the preferred model
saveRDS(model1_90, file = "180423model1_90.Rds")
saveRDS(model2_90, file = "180423model2_90.Rds")
saveRDS(model3_90, file = "180423model3_90.Rds")

# reload the saved model
model3_90 <- readRDS(file = "180423model3_90.Rds")
model2_1 <- readRDS(file = "180423model2_1.Rds")
model1_90 <- readRDS(file = "180423model1_90.Rds")

#to see the topic frequency with the top n keywords:
plot.STM(model1_90, type="summary", n = 5, xlim=c(0,.12), topics = climatejusticetopics1)
plot.STM(model2_1, type="summary", n = 5, xlim=c(0,.12), topics = climatejusticetopics2)
plot.STM(model3_90, type="summary", n = 5, xlim=c(0,.12), topics = climatejusticetopics3)

# get topic frequencies for the topics i am considering in the further analysis
topic_proportions <- as.data.frame(thetaPosterior(model1_90, topics = climatejusticetopics1))

# Print the resulting dataframe
print(topic_proportions)

# assign topic names to all topics
# specify climate justice topics and others

# specify climate justice topics here
# for K = 90
# in order of expected topic proportions in the sample
climatejusticetopics1 <- c(48,33,71,35,80,51,64)
other1 <- c(setdiff(1:90, climatejusticetopics1))
climatejusticetopics2 <- c(95,33,72,91,83,51,93,64)
other2 <- c(setdiff(1:100, climatejusticetopics2))
climatejusticetopics3 <- c(79,71,50,13,20,65,86,43)
other3 <- c(setdiff(1:90, climatejusticetopics3))

for (x in climatejusticetopics1) {
  cloud(model1_90, topic=x)}

#create a dataframe of highest probability words for selected topics (or all here)
topics1 <- data.frame(labelTopics(model1_90, c(1:model1_90$settings$dim$K), n=10)$prob)
topics1$vocab <- "text"
topics1$vocab <- paste(topics1$X1,topics1$X2,topics1$X3,topics1$X4,topics1$X5,topics1$X6,topics1$X7, topics1$X8, topics1$X9, topics1$X10)
topics1 = subset(topics1, select = -c(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10))
topics1 = subset(topics1,rownames(topics1) %in% climatejusticetopics1)

topics2 <- data.frame(labelTopics(model2_1, c(1:model2_1$settings$dim$K), n=10)$prob)
topics2$vocab <- "text"
topics2$vocab <- paste(topics2$X1,topics2$X2,topics2$X3,topics2$X4,topics2$X5,topics2$X6,topics2$X7, topics2$X8, topics2$X9, topics2$X10)
topics2 = subset(topics2, select = -c(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10))
topics2 = subset(topics2,rownames(topics2) %in% climatejusticetopics2)

topics3 <- data.frame(labelTopics(model3_90, c(1:model3_90$settings$dim$K), n=10)$prob)
topics3$vocab <- "text"
topics3$vocab <- paste(topics3$X1,topics3$X2,topics3$X3,topics3$X4,topics3$X5,topics3$X6,topics3$X7, topics3$X8, topics3$X9, topics3$X10)
topics3 = subset(topics3, select = -c(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10))
topics3 = subset(topics3,rownames(topics3) %in% climatejusticetopics3)

write.csv(topics1,"topics1.csv")
write.csv(topics2,"topics2.csv")
write.csv(topics3,"topics3.csv")

# name all the other topics also
topicnames1 <- c('Pathways to Equity','Socio-Economic Vulnerability', 'Local Communities','Food Security','Gender','Island Territories','Developing Countries')
# displacement and migration captured partly in Gender and partly in Island territories
topicnames2 <- c('Pathways to Equity', 'Socio-Economic Vulnerability', 'Local Communities', 'Gender', 'Displacement and Mobility', 'Food Security', 'Developing Countries','Island Territories')
topicnames3 <- c('Pathways to Equity','Socio-Economic Vulnerability','Local Communities','Health and Gender','Food Security','Displacement and Mobility','Developing Countries','Island Territories')

### ANALYSIS ###
# load the workspace 
load("180423_stm.RData")

# specify the formula for effect estimation
effect1 <- estimateEffect(~s(year) + first_author_female + gii_quartile + impact_quartile 
                          + impact_quartile*first_author_female + first_author_female*gii_quartile, 
                              model1_90, metadata = docvars(dfm1))
effect2 <- estimateEffect(~s(year) + last_author_female + gii_quartile + impact_quartile 
                          + impact_quartile*last_author_female + last_author_female*gii_quartile, 
                          model2_1, metadata = docvars(dfm2))
effect3 <- estimateEffect(~s(year) + majority_female_binary + gii_quartile + impact_quartile 
                          + impact_quartile*majority_female_binary + majority_female_binary*gii_quartile, 
                          model3_90, metadata = docvars(dfm3))

### EXTRACT AND EXPLORE EFFECTS ### 
### GENDER ###

effects1 <- get_effects(estimates = effect1,
                       variable = 'first_author_female',
                       type = 'difference',
                       cov_val1=1,cov_val2=0)
effects2 <- get_effects(estimates = effect2,
                        variable = 'last_author_female',
                        type = 'difference',
                        cov_val1=1,cov_val2=0)
effects3 <- get_effects(estimates = effect3,
                        variable = 'majority_female_binary',
                        type = 'difference',
                        cov_val1=1,cov_val2=0)
### YEAR ###
# not sure how to do that with a spline variable
effects1_year <- get_effects(estimates = effect1,
                            variable = 'year',
                            type = 'difference',
                            cov_val1=2023,cov_val2=1900)
effects2_year <- get_effects(estimates = effect2,
                            variable = 'year',
                            type = 'difference',
                            cov_val1=2023,cov_val2=1900)
effects3_year <- get_effects(estimates = effect3,
                            variable = 'year',
                            type = 'difference',
                            cov_val1=2023,cov_val2=1900)

### GII ###
effects1_gii <- get_effects(estimates = effect1,
                        variable = 'gii_quartile',
                        type = 'difference',
                        cov_val1=1,cov_val2=4)
effects2_gii <- get_effects(estimates = effect2,
                        variable = 'gii_quartile',
                        type = 'difference',
                        cov_val1=1,cov_val2=4)
effects3_gii <- get_effects(estimates = effect3,
                        variable = 'gii_quartile',
                        type = 'difference',
                        cov_val1=1,cov_val2=4)
### IMPACT ###
effects1_impact <- get_effects(estimates = effect1,
                            variable = 'impact_quartile',
                            type = 'difference',
                            cov_val1=1,cov_val2=4)
effects2_impact <- get_effects(estimates = effect2,
                            variable = 'impact_quartile',
                            type = 'difference',
                            cov_val1=1,cov_val2=4)
effects3_impact <- get_effects(estimates = effect3,
                            variable = 'impact_quartile',
                            type = 'difference',
                            cov_val1=1,cov_val2=4)

### INTERACTIONS WITH GII ###
# here, we use the effects from the interaction with gii_quartile
effects1_gii_1 <- get_effects(estimates = effect1,
                        variable = 'first_author_female',
                        cov_val1=1,
                        cov_val2=0,
                        type = 'difference',
                        moderator = 'gii_quartile',
                        # interaction effect with the GII quartile = 1
                        modval = 1)

effects1_gii_2 <- get_effects(estimates = effect1,
                        variable = 'first_author_female',
                        cov_val1=1,
                        cov_val2=0,
                        type = 'difference',
                        moderator = 'gii_quartile',
                        # interaction effect with the GII quartile = 2
                        modval = 2)

effects1_gii_3 <- get_effects(estimates = effect1,
                        variable = 'first_author_female',
                        cov_val1=1,
                        cov_val2=0,
                        type = 'difference',
                        moderator = 'gii_quartile',
                        # interaction effect with the GII quartile = 3
                        modval = 3)

effects1_gii_4 <- get_effects(estimates = effect1,
                       variable = 'first_author_female',
                       cov_val1=1,
                       cov_val2=0,
                       type = 'difference',
                       moderator = 'gii_quartile',
                       # interaction effect with the GII quartile = 4
                       modval = 4)

### INTERACTIONS WITH IMPACT ###
# and here, we use the effects from the interaction with impact_quartile
effects1_impact_1 <- get_effects(estimates = effect1,
                            variable = 'first_author_female',
                            cov_val1=1,
                            cov_val2=0,
                            type = 'difference',
                            moderator = 'impact_quartile',
                            # interaction effect with the impact quartile = 1
                            modval = 1)

effects1_impact_2 <- get_effects(estimates = effect1,
                            variable = 'first_author_female',
                            cov_val1=1,
                            cov_val2=0,
                            type = 'difference',
                            moderator = 'impact_quartile',
                            # interaction effect with the impact quartile = 2
                            modval = 2)

effects1_impact_3 <- get_effects(estimates = effect1,
                            variable = 'first_author_female',
                            cov_val1=1,
                            cov_val2=0,
                            type = 'difference',
                            moderator = 'impact_quartile',
                            # interaction effect with the impact quartile = 3
                            modval = 3)

effects1_impact_4 <- get_effects(estimates = effect1,
                            variable = 'first_author_female',
                            cov_val1=1,
                            cov_val2=0,
                            type = 'difference',
                            moderator = 'impact_quartile',
                            # interaction effect with the impact quartile = 4
                            modval = 4)

### INTERACTIONS WITH GII ###
# here, we use the effects from the interaction with gii_quartile
effects2_gii_1 <- get_effects(estimates = effect2,
                              variable = 'last_author_female',
                              cov_val1=1,
                              cov_val2=0,
                              type = 'difference',
                              moderator = 'gii_quartile',
                              # interaction effect with the GII quartile = 1
                              modval = 1)

effects2_gii_2 <- get_effects(estimates = effect2,
                              variable = 'last_author_female',
                              cov_val1=1,
                              cov_val2=0,
                              type = 'difference',
                              moderator = 'gii_quartile',
                              # interaction effect with the GII quartile = 2
                              modval = 2)

effects2_gii_3 <- get_effects(estimates = effect2,
                              variable = 'last_author_female',
                              cov_val1=1,
                              cov_val2=0,
                              type = 'difference',
                              moderator = 'gii_quartile',
                              # interaction effect with the GII quartile = 3
                              modval = 3)

effects2_gii_4 <- get_effects(estimates = effect2,
                              variable = 'last_author_female',
                              cov_val1=1,
                              cov_val2=0,
                              type = 'difference',
                              moderator = 'gii_quartile',
                              # interaction effect with the GII quartile = 4
                              modval = 4)

### INTERACTIONS WITH IMPACT ###
# and here, we use the effects from the interaction with impact_quartile
effects2_impact_1 <- get_effects(estimates = effect2,
                                 variable = 'last_author_female',
                                 cov_val1=1,
                                 cov_val2=0,
                                 type = 'difference',
                                 moderator = 'impact_quartile',
                                 # interaction effect with the impact quartile = 1
                                 modval = 1)

effects2_impact_2 <- get_effects(estimates = effect2,
                                 variable = 'last_author_female',
                                 cov_val1=1,
                                 cov_val2=0,
                                 type = 'difference',
                                 moderator = 'impact_quartile',
                                 # interaction effect with the impact quartile = 2
                                 modval = 2)

effects2_impact_3 <- get_effects(estimates = effect2,
                                 variable = 'last_author_female',
                                 cov_val1=1,
                                 cov_val2=0,
                                 type = 'difference',
                                 moderator = 'impact_quartile',
                                 # interaction effect with the impact quartile = 3
                                 modval = 3)

effects2_impact_4 <- get_effects(estimates = effect2,
                                 variable = 'last_author_female',
                                 cov_val1=1,
                                 cov_val2=0,
                                 type = 'difference',
                                 moderator = 'impact_quartile',
                                 # interaction effect with the impact quartile = 4
                                 modval = 4)
### INTERACTIONS WITH GII ###
# here, we use the effects from the interaction with gii_quartile
effects3_gii_1 <- get_effects(estimates = effect3,
                              variable = 'majority_female_binary',
                              cov_val1=1,
                              cov_val2=0,
                              type = 'difference',
                              moderator = 'gii_quartile',
                              # interaction effect with the GII quartile = 1
                              modval = 1)

effects3_gii_2 <- get_effects(estimates = effect3,
                              variable = 'majority_female_binary',
                              cov_val1=1,
                              cov_val2=0,
                              type = 'difference',
                              moderator = 'gii_quartile',
                              # interaction effect with the GII quartile = 2
                              modval = 2)

effects3_gii_3 <- get_effects(estimates = effect3,
                              variable = 'majority_female_binary',
                              cov_val1=1,
                              cov_val2=0,
                              type = 'difference',
                              moderator = 'gii_quartile',
                              # interaction effect with the GII quartile = 3
                              modval = 3)

effects3_gii_4 <- get_effects(estimates = effect3,
                              variable = 'majority_female_binary',
                              cov_val1=1,
                              cov_val2=0,
                              type = 'difference',
                              moderator = 'gii_quartile',
                              # interaction effect with the GII quartile = 4
                              modval = 4)

### INTERACTIONS WITH IMPACT ###
# and here, we use the effects from the interaction with impact_quartile
effects3_impact_1 <- get_effects(estimates = effect3,
                                 variable = 'majority_female_binary',
                                 cov_val1=1,
                                 cov_val2=0,
                                 type = 'difference',
                                 moderator = 'impact_quartile',
                                 # interaction effect with the impact quartile = 1
                                 modval = 1)

effects3_impact_2 <- get_effects(estimates = effect3,
                                 variable = 'majority_female_binary',
                                 cov_val1=1,
                                 cov_val2=0,
                                 type = 'difference',
                                 moderator = 'impact_quartile',
                                 # interaction effect with the impact quartile = 2
                                 modval = 2)

effects3_impact_3 <- get_effects(estimates = effect3,
                                 variable = 'majority_female_binary',
                                 cov_val1=1,
                                 cov_val2=0,
                                 type = 'difference',
                                 moderator = 'impact_quartile',
                                 # interaction effect with the impact quartile = 3
                                 modval = 3)

effects3_impact_4 <- get_effects(estimates = effect3,
                                 variable = 'majority_female_binary',
                                 cov_val1=1,
                                 cov_val2=0,
                                 type = 'difference',
                                 moderator = 'impact_quartile',
                                 # interaction effect with the impact quartile = 4
                                 modval = 4)
# explore the effects controls, predictors and interactions
# print summary
summary(effects1_year, topics = climatejusticetopics1)
summary(effects2_year, topics = climatejusticetopics2)
summary(effects3_year, topics = climatejusticetopics3)

### TIDY UP THE RESULTS AND MAKE GRAPHS ###
# forest plots gender x control x topic (topics ordered by prevalence)

# filter to climate justice topics
effects1_f <- filter(effects1, topic %in% climatejusticetopics1)
effects1_f$order <- match(effects1_f$topic, climatejusticetopics1)
effects1_f <- effects1_f[order(effects1_f$order),]
effects1_f$topicname <- topicnames1

effects2_f <- filter(effects2, topic %in% climatejusticetopics2)
effects2_f$order <- match(effects2_f$topic, climatejusticetopics2)
effects2_f <- effects2_f[order(effects2_f$order),]
effects2_f$topicname <- topicnames2

effects3_f <- filter(effects3, topic %in% climatejusticetopics3)
effects3_f$order <- match(effects3_f$topic, climatejusticetopics3)
effects3_f <- effects3_f[order(effects3_f$order),]
effects3_f$topicname <- topicnames3

# order by topic prevalence
ordered_data1 <- data.frame(
  topicname = factor(effects1_f$topicname, levels = rev(unique(effects1_f$topicname))),
  difference = effects1_f$difference,
  lower = effects1_f$lower,
  upper = effects1_f$upper
)
ordered_data2 <- data.frame(
  topicname = factor(effects2_f$topicname, levels = rev(unique(effects2_f$topicname))),
  difference = effects2_f$difference,
  lower = effects2_f$lower,
  upper = effects2_f$upper
)
ordered_data3 <- data.frame(
  topicname = factor(effects3_f$topicname, levels = rev(unique(effects3_f$topicname))),
  difference = effects3_f$difference,
  lower = effects3_f$lower,
  upper = effects3_f$upper
)

### VISUALISATION ###
### FOREST PLOTS ###
# save graphs in width = 800

### FIRST AUTHOR DIFFERENCE ###
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0025, 0.01))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### FIRST AUTHOR YEAR ###
fp <- ggplot(data=ordered_data1, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0025, 0.01))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab('Publication Year') +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### FIRST AUTHOR GII ###
effects1_giiF <- filter(effects1_gii, topic %in% climatejusticetopics1)
effects1_giiF$order <- match(effects1_giiF$topic, climatejusticetopics1)
effects1_giiF <- effects1_giiF[order(effects1_giiF$order),]
effects1_giiF$topicname <- topicnames1
ordered_data1_gii <- data.frame(
  topicname = factor(effects1_giiF$topicname, levels = rev(unique(effects1_giiF$topicname))),
  difference = effects1_giiF$difference,
  lower = effects1_giiF$lower,
  upper = effects1_giiF$upper
)
subt4 <- c("← Higher Gender Inequality Index           Lower Gender Inequality Index →")
fp <- ggplot(data=ordered_data1_gii, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of GII",
       subtitle = "Difference in topic proportion") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.02, 0.025))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt4) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### FIRST AUTHOR IMPACT ###
effects1_impactF <- filter(effects1_impact, topic %in% climatejusticetopics1)
effects1_impactF$order <- match(effects1_impactF$topic, climatejusticetopics1)
effects1_impactF <- effects1_impactF[order(effects1_impactF$order),]
effects1_impactF$topicname <- topicnames1
ordered_data1_impact <- data.frame(
  topicname = factor(effects1_impactF$topicname, levels = rev(unique(effects1_impactF$topicname))),
  difference = effects1_impactF$difference,
  lower = effects1_impactF$lower,
  upper = effects1_impactF$upper
)
subt5 <- c("← Lower  Impact Factor          Higher Impact Factor →")
fp <- ggplot(data=ordered_data1_impact, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of Journal Impact Factor",
       subtitle = "Difference in topic proportion") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0075, 0.0075))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt5) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### FIRST AUTHOR x GII 1 ###
effects1_gii_1F <- filter(effects1_gii_1, topic %in% climatejusticetopics1)
effects1_gii_1F$order <- match(effects1_gii_1F$topic, climatejusticetopics1)
effects1_gii_1F <- effects1_gii_1F[order(effects1_gii_1F$order),]
effects1_gii_1F$topicname <- topicnames1
ordered_data1_gii_1 <- data.frame(
  topicname = factor(effects1_gii_1F$topicname, levels = rev(unique(effects1_gii_1F$topicname))),
  difference = effects1_gii_1F$difference,
  lower = effects1_gii_1F$lower,
  upper = effects1_gii_1F$upper
)
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1_gii_1, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion (interaction with GII quartile = 1)") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0025, 0.01))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### FIRST AUTHOR X GII 2 ###
effects1_gii_2F <- filter(effects1_gii_2, topic %in% climatejusticetopics1)
effects1_gii_2F$order <- match(effects1_gii_2F$topic, climatejusticetopics1)
effects1_gii_2F <- effects1_gii_2F[order(effects1_gii_2F$order),]
effects1_gii_2F$topicname <- topicnames1
ordered_data1_gii_2 <- data.frame(
  topicname = factor(effects1_gii_2F$topicname, levels = rev(unique(effects1_gii_2F$topicname))),
  difference = effects1_gii_2F$difference,
  lower = effects1_gii_2F$lower,
  upper = effects1_gii_2F$upper
)
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1_gii_2, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion (interaction with GII quartile = 2)") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0025, 0.01))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### FIRST AUTHOR X GII 3 ###
effects1_gii_3F <- filter(effects1_gii_3, topic %in% climatejusticetopics1)
effects1_gii_3F$order <- match(effects1_gii_3F$topic, climatejusticetopics1)
effects1_gii_3F <- effects1_gii_3F[order(effects1_gii_3F$order),]
effects1_gii_3F$topicname <- topicnames1
ordered_data1_gii_3 <- data.frame(
  topicname = factor(effects1_gii_3F$topicname, levels = rev(unique(effects1_gii_3F$topicname))),
  difference = effects1_gii_3F$difference,
  lower = effects1_gii_3F$lower,
  upper = effects1_gii_3F$upper
)
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1_gii_3, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion (interaction with GII quartile = 3)") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.005, 0.0075))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### FIRST AUTHOR x GII 4 ###
effects1_gii_4F <- filter(effects1_gii_4, topic %in% climatejusticetopics1)
effects1_gii_4F$order <- match(effects1_gii_4F$topic, climatejusticetopics1)
effects1_gii_4F <- effects1_gii_4F[order(effects1_gii_4F$order),]
effects1_gii_4F$topicname <- topicnames1
ordered_data1_gii_4 <- data.frame(
  topicname = factor(effects1_gii_4F$topicname, levels = rev(unique(effects1_gii_4F$topicname))),
  difference = effects1_gii_4F$difference,
  lower = effects1_gii_4F$lower,
  upper = effects1_gii_4F$upper
)
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1_gii_4, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion (interaction with GII quartile = 4)") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0075, 0.01))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)
### FIRST AUTHOR x IMPACT 1 ###
effects1_impact_1F <- filter(effects1_impact_1, topic %in% climatejusticetopics1)
effects1_impact_1F$order <- match(effects1_impact_1F$topic, climatejusticetopics1)
effects1_impact_1F <- effects1_impact_1F[order(effects1_impact_1F$order),]
effects1_impact_1F$topicname <- topicnames1
ordered_data1_impact_1 <- data.frame(
  topicname = factor(effects1_impact_1F$topicname, levels = rev(unique(effects1_impact_1F$topicname))),
  difference = effects1_impact_1F$difference,
  lower = effects1_impact_1F$lower,
  upper = effects1_impact_1F$upper
)
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1_impact_1, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion (interaction with impact quartile = 1)") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0025, 0.01))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)
### FIRST AUTHOR x IMPACT 2 ###
effects1_impact_2F <- filter(effects1_impact_2, topic %in% climatejusticetopics1)
effects1_impact_2F$order <- match(effects1_impact_2F$topic, climatejusticetopics1)
effects1_impact_2F <- effects1_impact_2F[order(effects1_impact_2F$order),]
effects1_impact_2F$topicname <- topicnames1
ordered_data1_impact_2 <- data.frame(
  topicname = factor(effects1_impact_2F$topicname, levels = rev(unique(effects1_impact_2F$topicname))),
  difference = effects1_impact_2F$difference,
  lower = effects1_impact_2F$lower,
  upper = effects1_impact_2F$upper
)
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1_impact_2, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion (interaction with impact quartile = 2)") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0025, 0.01))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)
### FIRST AUTHOR x IMPACT 3 ###
effects1_impact_3F <- filter(effects1_impact_3, topic %in% climatejusticetopics1)
effects1_impact_3F$order <- match(effects1_impact_3F$topic, climatejusticetopics1)
effects1_impact_3F <- effects1_impact_3F[order(effects1_impact_3F$order),]
effects1_impact_3F$topicname <- topicnames1
ordered_data1_impact_3 <- data.frame(
  topicname = factor(effects1_impact_3F$topicname, levels = rev(unique(effects1_impact_3F$topicname))),
  difference = effects1_impact_3F$difference,
  lower = effects1_impact_3F$lower,
  upper = effects1_impact_3F$upper
)
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1_impact_3, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion (interaction with impact = 3)") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.005, 0.015))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)
### FIRST AUTHOR x IMPACT 1 ###
effects1_impact_4F <- filter(effects1_impact_4, topic %in% climatejusticetopics1)
effects1_impact_4F$order <- match(effects1_impact_4F$topic, climatejusticetopics1)
effects1_impact_4F <- effects1_impact_4F[order(effects1_impact_4F$order),]
effects1_impact_4F$topicname <- topicnames1
ordered_data1_impact_4 <- data.frame(
  topicname = factor(effects1_impact_4F$topicname, levels = rev(unique(effects1_impact_4F$topicname))),
  difference = effects1_impact_4F$difference,
  lower = effects1_impact_4F$lower,
  upper = effects1_impact_4F$upper
)
subt <- c("← First author predicted male            First author predicted female →")
fp <- ggplot(data=ordered_data1_impact_4, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of first author gender",
       subtitle = "Difference in topic proportion (interaction with impact = 4)") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[5], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.005, 0.015))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### LAST AUTHOR DIFFERENCE ###
subt2 <- c("← Last author predicted male            Last author predicted female →")
fp <- ggplot(data=ordered_data2, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of last author gender",
       subtitle = "Difference in topic proportion") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[1], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0025, 0.0175))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt2) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.3, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### LAST AUTHOR IMPACT ###
effects2_impactF <- filter(effects2_impact, topic %in% climatejusticetopics2)
effects2_impactF$order <- match(effects2_impactF$topic, climatejusticetopics2)
effects2_impactF <- effects2_impactF[order(effects2_impactF$order),]
effects2_impactF$topicname <- topicnames2
ordered_data2_impact <- data.frame(
  topicname = factor(effects2_impactF$topicname, levels = rev(unique(effects2_impactF$topicname))),
  difference = effects2_impactF$difference,
  lower = effects2_impactF$lower,
  upper = effects2_impactF$upper
)
subt5 <- c("← Lower  Impact Factor          Higher Impact Factor →")
fp <- ggplot(data=ordered_data2_impact, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of Journal Impact Factor",
       subtitle = "Difference in topic proportion") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[1], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0075, 0.0075))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt5) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.25, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### MAJORITY AUTHOR DIFFERENCE ###
subt3 <- c("← Majority author predicted male            Majority author predicted female →")
fp <- ggplot(data=ordered_data3, aes(x=topicname, y=difference, ymin=lower, ymax=upper)) +
  labs(title = "Mean effect of majority author gender",
       subtitle = "Difference in topic proportion") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[2], shape=18, size = 1.25) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.0025, 0.02))  +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  xlab('Topics ordered by topic prevalence in the dataset') + ylab(subt3) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(difference, 3), y = difference), 
             size = 3, nudge_x = 0.3, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp)

### T-SNE VISUALISATIONS ###
# convert DTM to topic proportions
# filter data1, data2 and data3 for missing year, gii, impact
data1 <- subset(data1, first_author_female != '')
data1 <- subset(data1, year > 0)
data1 <- subset(data1, gii_quartile > 0)
data1 <- subset(data1, impact_quartile > 0)
DTM1 <- make.dt(model1_90, meta = data1$id)
data2 <- subset(data2, last_author_female != '')
data2 <- subset(data2, year > 0)
data2 <- subset(data2, gii_quartile > 0)
data2 <- subset(data2, impact_quartile > 0)
DTM2 <- make.dt(model2_1, meta = data2$id)
data3 <- subset(data3, majority_female_binary != '')
data3 <- subset(data3, year > 0)
data3 <- subset(data3, gii_quartile > 0)
data3 <- subset(data3, impact_quartile > 0)
DTM3 <- make.dt(model3_90, meta = data3$id)
# append female_author_gender by 'id'
# append 
write.csv(DTM1,"DTM1.csv")
write.csv(DTM2,"DTM2.csv")
write.csv(DTM3,"DTM3.csv")

# run t-SNE dimension conversion (python) -> load here
tsne1 = read.csv("tsne1.csv")
tsne1$first_author_gender <- factor(ifelse(tsne1$first_author_female != 1,"male","female"))
tsne2 = read.csv("tsne2.csv")
tsne2$last_author_gender <- factor(ifelse(tsne2$last_author_female != 1,"male","female"))
tsne3 = read.csv("tsne3.csv")
tsne3$majority_female_binary <- factor(ifelse(tsne3$majority_female_binary != 1,"male","female"))
#tsne1 = subset(tsne1, first_author_gender != "")

save.image("180423_stm.RData")

### KEYWORDS ASSOCIATED WITH TOPICS ###
mycolours <- cbind(wes_palette("Darjeeling1"), wes_palette("Darjeeling2"))
mycolours1 <- c(wes_palette("Darjeeling1")[1],wes_palette("Darjeeling1")[3],wes_palette("Darjeeling1")[4],wes_palette("Darjeeling1")[5], wes_palette("Darjeeling2"))

### FIRST AUTHOR ###
td_beta1 <- tidy(model1_90)
td_beta1 <- filter(td_beta1,topic %in% climatejusticetopics1)
topicnames1 <- data.frame(topic_name = topicnames1, topic_number = climatejusticetopics1)
td_beta1 <- td_beta1 %>% 
  left_join(topicnames1, by = c("topic" = "topic_number"))
td_beta1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = topic_name,
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.9, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_fill_manual(values = mycolours1) + 
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic on climate justice",
       subtitle = "Topic modelling output for the first author gender dataset at K = 90") +
  theme(aspect.ratio = 1,
        plot.title = element_text (margin = margin(b = 5),size = 14),
        plot.subtitle = element_text(margin = margin(b = 15)))
### LAST AUTHOR ###
td_beta2 <- tidy(model2_1)
td_beta2 <- filter(td_beta2,topic %in% climatejusticetopics2)
topicnames2 <- data.frame(topic_name = topicnames2, topic_number = climatejusticetopics2)
td_beta2 <- td_beta2 %>% 
  left_join(topicnames2, by = c("topic" = "topic_number"))
td_beta2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = topic_name,
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.9, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_fill_manual(values = mycolours) + 
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic on climate justice",
       subtitle = "Topic modelling output for the last author gender dataset at K = 100") +
  theme(aspect.ratio = 1,
        plot.title = element_text (margin = margin(b = 5),size = 14),
        plot.subtitle = element_text(margin = margin(b = 15)))
### MAJORITY AUTHOR ###
td_beta3 <- tidy(model3_90)
td_beta3 <- filter(td_beta3,topic %in% climatejusticetopics3)
topicnames3 <- data.frame(topic_name = topicnames3, topic_number = climatejusticetopics3)
td_beta3 <- td_beta3 %>% 
  left_join(topicnames3, by = c("topic" = "topic_number"))
td_beta3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = topic_name,
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.9, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_fill_manual(values = mycolours) + 
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic on climate justice",
       subtitle = "Topic modelling output for the majority author gender dataset at K = 90") +
  theme(aspect.ratio = 1,
        plot.title = element_text (margin = margin(b = 5),size = 14),
        plot.subtitle = element_text(margin = margin(b = 15)))


