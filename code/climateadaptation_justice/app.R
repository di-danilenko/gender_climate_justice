library(shiny)
library(thematic)

runApp("climateadaptation_justice")

tsne <- readRDS("/Users/dianadanilenko/Desktop/gender_climate_justice/code/climateadaptation_justice/data/tsne.rds")
#data_matrix <- readRDS("tsnemat.rds")

# define UI for application

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  
    # application title
    titlePanel("Climate justice in adaptation policy"),
  # create sidebar with a dropout to select (1) dataset: first_author, last_author, full_group
  # or (2) output: topical space, heatmap, quantitative results
  
    # sidebar with a slider - what could we use this input for?
  # could be number of clusters for tSNE plot labels or something else? 
  
    sidebarLayout(
        sidebarPanel(
            sliderInput("clusters",
                        "Number of clusters:",
                        min = 5,
                        max = 150,
                        value = 75)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("tSNEplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  thematic::thematic_shiny()
  
    output$tSNEplot <- renderPlot({

      # generate cluster based on input$clusters from ui.R
      kmeans_model <- kmeans(tsne[c("comp.1","comp.2")], centers = input$clusters)
      
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
      
      # calculate % women author per cluster
      
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
    
        # draw the tSNE plot with the specified number of clusters 
      # + specified dataset
        # plot code
        tsne %>% ggplot(aes(x = comp.1, y = comp.2)) +
        geom_point(aes(colour = first_author_gender, alpha = climatejusticescore), size = 0.5,) + 
        geom_label(data = first_point_per_cluster, aes(label = topicname), size = 5, nudge_y = 0) +
        xlab("t-SNE 1") + ylab("t-SNE 2") + theme_bw() +
        labs(title = "Topical space and gender of the first author",
             subtitle = "a t-SNE visualisation of the structural topic modelling output")  +
        theme(aspect.ratio=1,
              plot.title = element_text(hjust = 0.5, size = 20),
              plot.subtitle = element_text(hjust = 0.5)) + theme(text = element_text(size = 15)) +
        scale_color_manual(values=c (wes_palette("Darjeeling1")[4],wes_palette("Darjeeling1")[2],"darkgrey")) + guides(color = guide_legend(override.aes = list(size = 2.5))) + guides(alpha = guide_legend(override.aes = list(size = 2.5)))
        #+
        #annotate("rect", xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, 
                 #color = "black", fill = NA, linewidth = 0.5)
    })
}

# i have made the graph simply appear but it isn't interactive or anything

# Run the application 
shinyApp(ui = ui, server = server)
stopApp(returnValue = NULL)
