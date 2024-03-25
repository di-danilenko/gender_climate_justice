set.seed(1405)

p1 <-  estimateEffect(c(cj_topics) ~first_author_female + s(year) + 
                          subfield +
                           X1_gii_quartile + impact + 
                           subfield*first_author_female +
                           impact*first_author_female +
                           X1_gii_quartile*first_author_female,
                         model120_1,
                         metadata = docvars(dfm))

e1 <- get_effects(estimates = p1,
                        variable = 'first_author_female',
                        type = 'difference',
                        cov_val1="1.0",cov_val2="0.0")

e1 <- merge(e1, transformed_df, by = "topic")
e1$relativedifference <- e1$difference/e1$mean_proportion
e1$relativelower <- e1$lower/e1$mean_proportion
e1$relativeupper <- e1$upper/e1$mean_proportion
e1 <- merge(e1, topic_names[c("topic", "topicname")], by="topic",all.x=TRUE)

# cross check the data for a couple of papers across files
# create the other graphs
# try loading the model into a visuals app and see whats wrong
# let's try the same thing with a specific seed - but if it gives us different results each time - we cannot use thes numbers
# we are getting a lot of negative numbers here too now
# dfm - checked that it is exaclty the same list of variables as before and as used in the stm config
# the issue is the same across variables
# are the results simply not consistent because of that "adding a prior issue" ?? 
# the results do change if we rerun it in exaclty the same environment
# if we exclude subfield - the results are tiny and none of them significant

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