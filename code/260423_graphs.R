library("wesanderson")
library("ggplot2")
library("tidyverse")
library("dplyr")
library("scales")
library("leaflet")

### DESCRPITIVE STATISTICS ###

# make a graph for cumulative yearly publications             
pub_y <- read_csv("/Users/dianadanilenko/Desktop/personal/master_thesis/methods/R_scripts/yearly_cumulative_papers2.csv")

pub_y <- pub_y %>% filter (year != "2023") 

y <- ggplot(pub_y, aes(x = year, y = cumulative_sum)) +
  geom_bar(aes(y = papers_published*10), stat = "identity", width = 0.9, fill = wes_palette("Darjeeling1")[2], alpha = ifelse(pub_y$year == "2022", 0.75, 1)) +
  geom_line(aes(y = cumulative_sum),linewidth =1.5, color = wes_palette("Darjeeling1")[1], alpha = 0.75) +
  scale_y_continuous(name = "Cumulative sum",sec.axis = sec_axis(~./10, name = "Papers published")) +
  scale_x_continuous(breaks = seq(1991, max(pub_y$year), by = 2), labels = seq(1991, max(pub_y$year), by = 2),
                     name = "Year") +
  theme(aspect.ratio=0.75,
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + theme_bw()

ggsave("graphs/yearly_publications.png", y, width = 6.9, height = 3.9, dpi = 1000)

first_country <- subset(data1, select = c(id,country, gender.inequality.index,gii_quartile))
first_country <- filter(first_country,country != "")
# use aggregate() function to count the number of ids for each country
id_counts1 <- aggregate(id ~ country, data = first_country, FUN = length)
gii_count1 <- aggregate(id ~ gii_quartile, data = first_country, FUN = length)
# print the resulting dataframe
id_counts1
gii_values1 <- aggregate(gender.inequality.index ~ country, data = first_country, FUN = function(x) x[1])

# merge the id_counts and gii_values dataframes by country
result1 <- merge(id_counts1, gii_values1, by = "country")
result1

last_country <- subset(data3, select = c(id,country, gii, gii_quartile))
last_country <- filter(last_country,country != "")
# use aggregate() function to count the number of ids for each country
id_counts3 <- aggregate(id ~ country, data = last_country, FUN = length)
gii_count3 <- aggregate(id ~ gii_quartile, data = last_country, FUN = length)
# print the resulting dataframe
id_counts3
gii_values3 <- aggregate(gii ~ country, data = last_country, FUN = function(x) x[1])

# merge the id_counts and gii_values dataframes by country
result3 <- merge(id_counts3, gii_values3, by = "country")
result3

summary(last_country)
write.csv(result1, "first_country.csv", row.names = FALSE)
write.csv(result3, "last_country.csv", row.names = FALSE)

# make category and journal tables

category <- subset(data3, select = c(id,category))
category <- filter(category,category != "")
category<- category[nchar(category$category) > 4, ]
# use aggregate() function to count the number of ids for each category
category_count <- aggregate(id ~ category, data = category, FUN = length)
# print the resulting dataframe
category_count

journal <- subset(data3, select = c(id,journal,impact_factor,impact_quartile))
journal <- filter(journal,journal != "")
journal <- filter(journal,impact_factor != "")
# use aggregate() function to count the number of ids for each journal
journal_count <- aggregate(id ~ journal, data = journal, FUN = length)
impact_count <- aggregate(id ~ impact_quartile, data = journal, FUN = length)

impact_values <- aggregate(impact_factor ~ journal, data = journal, FUN = function(x) x[1])
quartile_values <- aggregate(impact_quartile ~ journal, data = journal, FUN = function(x) x[1])

# merge the id_counts and gii_values dataframes by country
journal <- merge(journal, impact_values, by = "journal")
journal <- merge(journal, quartile_values, by = "journal")

write.csv(impact_count, "impact_count.csv", row.names = FALSE)
write.csv(gii_count1, "gii_count1.csv", row.names = FALSE)
write.csv(gii_count3, "gii_count3.csv", row.names = FALSE)

### RESULTS - GENDER COMPOSITION ###
df1 <- data.frame(
  category = c("male", "female"),
  count = c(21626, 13727),
  percents = c(100*21626/(21626+13727),100*13727/(21626+13727))
)
df2 <- data.frame(
  category = c("male", "female"),
  count = c(24829, 10050),
  percents = c(100*24829/(24829+10050),100*10050/(24829+10050))
)
df3 <- data.frame(
  category = c("male", "female", "equal"),
  count = c(17169, 6338, 4205),
  percents = c(
    100 * 17169 / (17169 + 6338 + 4205),
    100 * 6338 / (17169 + 6338 + 4205),
    100 * 4205 / (17169 + 6338 + 4205)
  )
)
# double check these numbers again because this seems a bit too good

# combine the three data frames
df_all <- rbind(df1, df2, df3)
df_all$percents <- round(df_all$percents, 2)
df_all$position <- c("first author", "first author", "last author", "last author", "majority author", "majority author","majority author")

# create the histogram
ggplot(data = df_all, aes(fill=category, y=percents, x=position)) + 
  geom_bar(position="stack", stat="identity") +
  geom_label(aes(label = paste(percents,"%")), size = 5,
             fill = "white", position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c(wes_palette("Darjeeling1")[3], wes_palette("Darjeeling1")[4], wes_palette("Darjeeling1")[5])) +
  xlab("") + ylab("percentage")+
  theme(aspect.ratio=1,
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5)) + theme(text = element_text(size = 12)) + 
  theme_bw() +
  labs(title = "Composition of authors by gender",
       subtitle="in authorship instances with an unambiguous gender estimate")
ggsave("/Users/dianadanilenko/Desktop/gender_climate_justice/code/graphs/gender_composition.jpeg", width=6.9, height=6.9, dpi=1000)
