set.seed(1405)

p1 <-  estimateEffect(c(cj_topics) ~first_author_female + s(year) + 
                          subfield +
                           X1_gii_quartile + impact + 
                           subfield*first_author_female +
                           impact*first_author_female +
                           X1_gii_quartile*first_author_female,
                         model120_1,
                         metadata = docvars(dfm), nsims=50)

e1 <- get_effects(estimates = p1,
                        variable = 'first_author_female',
                        type = 'difference',
                        cov_val1="1.0",cov_val2="0.0")

e1 <- merge(e1, transformed_df, by = "topic")
e1$relativedifference <- e1$difference/e1$mean_proportion
e1$relativelower <- e1$lower/e1$mean_proportion
e1$relativeupper <- e1$upper/e1$mean_proportion
e1 <- merge(e1, topic_names[c("topic", "topicname")], by="topic",all.x=TRUE)

data2 <- read.csv("Desktop/gender_climate_justice/code/data/data.csv", header = TRUE)
data2 <- subset(data2, id %in% data$id)
data2 <- data2[ !duplicated(data2$id), ]
data2 <- data2 %>%
  rename(impact = impact_quartile)

data2 <- data2 %>%
  select(c("id", "abstract","title","keywords", "majority_female_binary", "first_author_female","last_author_female","subfield","impact","X1_gii_quartile", "X2_gii_quartile", "year"))

data2$text <- "text"

# prepare the variables for stm
data2 <- data2 %>%
  mutate(impact = if_else(is.na(impact) | impact == "None", "unknown", impact))
data2$subfield <- as.factor(data2$subfield)
data2$first_author_female <- as.factor(data2$first_author_female)
data2$last_author_female <- as.factor(data2$last_author_female)
data2$majority_female_binary <- as.factor(data2$majority_female_binary)
data2$impact <- as.factor(data2$impact)
data2$X1_gii_quartile <- as.factor(data2$X1_gii_quartile)
data2$X2_gii_quartile <- as.factor(data2$X2_gii_quartile)

data2$text <- paste(data2$title, data2$abstract, data2$keywords)

# get rid of the copyright information in our corpus
data2 <- data2 %>% separate(text, c("text","copyright"), sep = "\\(c\\)\\s*\\d+", extra="merge", remove = TRUE) #removes (c) followed by numbers - this covers most copyright messages
data2$text <- gsub("all rights reserved"," ",as.character(data2$text))

# create the actual CORPUS 
corp <- corpus(data2, text_field = "text")
summary(corp, 3)

# the function automatically assumes all other columns contain document variables
head(docvars(corp)) 

# create TOKENS from this - this basically just means cutting up the text into individual words
toks <- tokens(corp)
#the below is a fairly standard list for pre-processing
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE) %>% #create tokens w/o punctuation or symbols
  tokens_tolower() %>% #lowercase -- note the pipe means this function doesn't need any input
  tokens_remove(pattern = stopwords("en"), #remove stopwords
                min_nchar = 2) %>% #remove short words. AMR is three letters, so best keep it to 2
  tokens_wordstem(language = "en") #Using snowball stemmer

# create two dfms: one with unigrams and one with bi-grams
dfm_single = dfm(toks)%>%
  dfm_trim(min_termfreq = 100, termfreq_type = "count", max_docfreq=0.95, docfreq_type = "prop")

toks_bigram <- tokens_ngrams(toks, n = 2, skip = 1:2)
dfm_bigram <- dfm(toks_bigram) %>%
  dfm_trim(min_termfreq = 100, termfreq_type = "count", max_docfreq=0.95, docfreq_type = "prop")

# combine
dfm = cbind(dfm_bigram, dfm_single)

# show most-frequent tokens:
topfeatures(dfm, 15)

# cross check the data for a couple of papers across files - the metadata are the same
# the model is the same
# the equation is the same
# the topic numbers are correct - so we are looking at the right topics - check the metadata


# create the other graphs
# check different versions of the code

# we are getting a lot of negative numbers here  now
# dfm - checked that it is exaclty the same list of variables as before and as used in the stm config

# the issue is the same across variables

# 1. try to figure this out and get consistent results
# 2. focus on other methods / findings - where will we take a numeric estimate from ? 

subt2 <- c("← Last author predicted male            Last author predicted female →")
fp2 <- ggplot(data=effects2_f, aes(x=topicname, y=relativedifference, ymin=relativelower, ymax=relativeupper)) +
  labs(title = "Mean effect of last author gender",
       subtitle = "Relative difference in topic proportion") +
  geom_pointrange(colour = wes_palette("Darjeeling1")[1], shape=18, size = 1) + 
  geom_hline(yintercept=0, lty=2, colour ='darkgrey') +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  scale_y_continuous(limits = c(-0.5, 1.25))  +
  scale_x_discrete(expand = c(0, 0)) +
  xlab('') + 
  ylab(subt2) +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 10)) +
  geom_label(aes(label = round(relativedifference, 3), y = relativedifference), 
             size = 2.75, nudge_x = 0.5, 
             label.padding = unit(0.2, "lines"),
             label.r = unit(0,'lines'),
             fill = "white")
print(fp2)
ggsave("graphs/2_genderxcolour.png", fp2, width = 6.9, height = 6.9, dpi = 1000)