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
library("Rtsne")

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
path = "/Users/dianadanilenko/Desktop/methods/R_scripts/"

tsne1 = read.csv("/Users/dianadanilenko/Desktop/methods/R_scripts/tsne1.csv")
tsne1$first_author_gender <- factor(ifelse(tsne1$first_author_female != 1,"male","female"))
tsne2 = read.csv("/Users/dianadanilenko/Desktop/methods/R_scripts/tsne2.csv")
tsne2$last_author_gender <- factor(ifelse(tsne2$last_author_female != 1,"male","female"))
tsne3 = read.csv("/Users/dianadanilenko/Desktop/methods/R_scripts/tsne3.csv")
tsne3$majority_female_binary <- factor(ifelse(tsne3$majority_female_binary != 1,"male","female"))


### ADD CLUSTERS ###
tsne1$topic <- gsub("Topic", "", tsne1$topic)  # Remove the "Topic" part
# Load the CSV file into a new dataframe
topic_names1 <- read.csv("/Users/dianadanilenko/Desktop/methods/R_scripts/topics1.csv", sep = ";") %>% 
  select(number, topicname)
topic_names1 <- topic_names1 %>% rename(topic.y = number)
topic_names1 <- topic_names1 %>% filter(topicname != 0)
# First, combine the comp.1 and comp.2 columns into a matrix
data_matrix1 <- cbind(tsne1$comp.1, tsne1$comp.2)
# Use the kmeans function to cluster the data into 30 clusters
kmeans_model1 <- kmeans(data_matrix1, centers = 60)
# Add the cluster labels to your original dataframe
tsne1$cluster <- as.factor(kmeans_model1$cluster)
### ADD LABELS PER CLUSTER ### 
# Group the data by cluster and topic, count the number of occurrences of each topic within each cluster, and choose the most common topic for each cluster
most_common_topics1 <- tsne1 %>% 
  group_by(cluster, topic) %>% 
  summarise(count = n()) %>% 
  group_by(cluster) %>% 
  arrange(desc(count)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(cluster, topic)
# Join the most common topic for each cluster to your original dataframe
tsne1 <- left_join(tsne1, most_common_topics1, by = "cluster")
first_point_per_cluster1 <- tsne1 %>% 
  group_by(cluster) %>% 
  slice(1) %>% 
  ungroup()
first_point_per_cluster1 <- merge(first_point_per_cluster1, topic_names1, by = "topic.y")
first_point_per_cluster1 <- first_point_per_cluster1 %>% 
  distinct(topicname, .keep_all = TRUE)

tsne2$topic <- gsub("Topic", "", tsne2$topic)  # Remove the "Topic" part
# Load the CSV file into a new dataframe
topic_names2 <- read.csv("/Users/dianadanilenko/Desktop/methods/R_scripts/topics2.csv", sep = ";") %>% 
  select(number, topicname)
topic_names2 <- topic_names2 %>% rename(topic.y = number)
topic_names2 <- topic_names2 %>% filter(topicname != 0)
# First, combine the comp.1 and comp.2 columns into a matrix
data_matrix2 <- cbind(tsne2$comp.1, tsne2$comp.2)
# Use the kmeans function to cluster the data into 30 clusters
kmeans_model2 <- kmeans(data_matrix2, centers = 70)
# Add the cluster labels to your original dataframe
tsne2$cluster <- as.factor(kmeans_model2$cluster)
### ADD LABELS PER CLUSTER ### 
# Group the data by cluster and topic, count the number of occurrences of each topic within each cluster, and choose the most common topic for each cluster
most_common_topics2 <- tsne2 %>% 
  group_by(cluster, topic) %>% 
  summarise(count = n()) %>% 
  group_by(cluster) %>% 
  arrange(desc(count)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(cluster, topic)
# Join the most common topic for each cluster to your original dataframe
tsne2 <- left_join(tsne2, most_common_topics2, by = "cluster")
first_point_per_cluster2 <- tsne2 %>% 
  group_by(cluster) %>% 
  slice(1) %>% 
  ungroup()
first_point_per_cluster2 <- merge(first_point_per_cluster2, topic_names2, by = "topic.y")
first_point_per_cluster2 <- first_point_per_cluster2 %>% 
  distinct(topicname, .keep_all = TRUE)

tsne3$topic <- gsub("Topic", "", tsne3$topic)  # Remove the "Topic" part
# Load the CSV file into a new dataframe
topic_names3 <- read.csv("/Users/dianadanilenko/Desktop/methods/R_scripts/topics3.csv", sep = ";") %>% 
  select(number, topicname)
topic_names3 <- topic_names3 %>% rename(topic.y = number)
topic_names3 <- topic_names3 %>% filter(topicname != 0, topicname != 'South America')
# First, combine the comp.1 and comp.2 columns into a matrix
data_matrix3 <- cbind(tsne3$comp.1, tsne3$comp.2)
# Use the kmeans function to cluster the data into 30 clusters
kmeans_model3 <- kmeans(data_matrix3, centers = 60)
# Add the cluster labels to your original dataframe
tsne3$cluster <- as.factor(kmeans_model3$cluster)
### ADD LABELS PER CLUSTER ### 
# Group the data by cluster and topic, count the number of occurrences of each topic within each cluster, and choose the most common topic for each cluster
most_common_topics3 <- tsne3 %>% 
  group_by(cluster, topic) %>% 
  summarise(count = n()) %>% 
  group_by(cluster) %>% 
  arrange(desc(count)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(cluster, topic)
# Join the most common topic for each cluster to your original dataframe
tsne3 <- left_join(tsne3, most_common_topics3, by = "cluster")
first_point_per_cluster3 <- tsne3 %>% 
  group_by(cluster) %>% 
  slice(1) %>% 
  ungroup()
first_point_per_cluster3 <- merge(first_point_per_cluster3, topic_names3, by = "topic.y")
first_point_per_cluster3 <- first_point_per_cluster3 %>% 
  distinct(topicname, .keep_all = TRUE)

# high resolution tiff image
#tiff("tsne.png", units="in", width=12, height=9, res=500)
# plot
tsne1 %>% ggplot(aes(x = comp.1, y = comp.2)) +
  geom_point(aes(colour = first_author_gender, alpha = climatejusticescore ), size = 2,) + 
  geom_label(data = first_point_per_cluster1, aes(label = topicname), size = 3, nudge_y = 0) +
  xlab("t-SNE 1") + ylab("t-SNE 2") + theme_bw() +
  labs(title = "Topical space and gender of the first author",
       subtitle = "a t-SNE visualisation of the structural topic modelling output")  +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=c (wes_palette("Darjeeling1")[4],wes_palette("Darjeeling1")[2]))

tsne2 %>% ggplot(aes(x = comp.1, y = comp.2)) + 
  geom_point(aes(colour = last_author_gender, alpha = climatejusticescore ), size = 2,)  + 
  geom_label(data = first_point_per_cluster2, aes(label = topicname), size = 3, nudge_y = 0, nudge_x = 0) +
  xlab("t-SNE 1") + ylab("t-SNE 2") + theme_bw() +
  labs(title = "Topical space and gender of the last author",
       subtitle = "a t-SNE visualisation of the structural topic modelling output")  +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=c (wes_palette("Darjeeling1")[4],wes_palette("Darjeeling1")[2]))

tsne3 %>% ggplot(aes(x = comp.1, y = comp.2)) + 
  geom_point(aes(colour = majority_female_binary, alpha = climatejusticescore ), size = 2,)  +
  geom_label(data = first_point_per_cluster3, aes(label = topicname), size = 3, nudge_y = 0) +
  xlab("t-SNE 1") + ylab("t-SNE 2") + theme_bw() +
  labs(title = "Topical space and gender of all group",
       subtitle = "a t-SNE visualisation of the structural topic modelling output") +
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_color_manual(values=c (wes_palette("Darjeeling1")[4],wes_palette("Darjeeling1")[2]))

save.image("180423_stm.RData")

