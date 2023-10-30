tsne <- readRDS("/Users/dianadanilenko/Desktop/gender_climate_justice/code/climateadaptation_justice/data/tsne.rds")

# generate cluster based on input$clusters from ui.R
kmeans_model <- kmeans(tsne[c("comp.1","comp.2")], centers = 75)

# add the cluster labels to your original dataframe
tsne$cluster <- as.factor(kmeans_model$cluster)
most_common_topics <- tsne %>% 
  group_by(cluster, topic) %>% 
  summarise(count = n()) %>% 
  group_by(cluster) %>% 
  arrange(desc(count)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(cluster, topic)

first_share_women <- tsne %>%
  group_by(cluster, first_author_female) %>%
  summarise(count_gender = n()) %>%
  group_by(cluster) %>%
  mutate(percent_female = sum(count_gender[first_author_female == "1.0"]) / sum(count_gender)) %>%
  slice(1) %>% 
  ungroup() %>% 
  select(cluster, percent_female)

first_share_women$percent_female <- round(as.numeric(first_share_women$percent_female), 3)

tsne <- left_join(tsne, most_common_topics, by = "cluster")
tsne <- left_join(tsne, first_share_women, by = "cluster")

first_point_per_cluster <- tsne %>% 
  group_by(cluster) %>% 
  slice(1) %>% 
  ungroup()

# get rid of duplicates in labels
# add a zoom in feature 
# add hover/click on the papers
# how do you make this accessible to people online ?? 