#----------------------------------------#

# Operationalizing Women Leadership Variable #

# Ryan Buczkowski #

#----------------------------------------#

# Creating URL object
women_url <- 'https://statusofwomendata.org/explore-the-data/political-participation/additional-state-data/political-participation-composite/'

# Scraping web table
read_html(women_url) %>% 
  html_node(xpath = '//*[@id="post-2937"]/table') %>% 
  html_table(fill = TRUE) %>% 
  as_tibble(
    
  ) -> web_table2

# Cleaning web table
web_table2 %>% 
  select(X1, X2) %>% 
  slice(-c(1:3,12,55:57)) %>% 
  rename('state_name'  = X1,
         'women_index' = X2) %>% 
  mutate(women_index = str_replace(women_index, '–', '-'),
         women_index = parse_number(women_index)) -> women_leadership_table

# Visualizing Women Leadership data
women_leadership_table %>% 
  ggplot(aes(x = women_index)) +
  geom_histogram(bins = 7, color = 'white', fill = 'black') +
  geom_vline(xintercept = mean(women_leadership_table$women_index), color = 'red', linetype = 'dashed', size = 1) +
  geom_vline(xintercept = median(women_leadership_table$women_index), color = 'green', linetype = 'dashed', size = 1) +
  theme_minimal() +
  labs(x = 'Women Leadership Score',
       y = 'Frequency of Observations',
       title = 'Distribution of Women Leadership Scores',
       subtitle = 'By State',
       caption  = 'Visualization created using ggplot2 in RStudio \nCreator: Ryan Buczkowski - University of Mississippi - Political Science Department') +
  theme(
    axis.text         = element_text(face  = 'bold.italic',
                                     size  = 9),
    axis.title        = element_text(face  = 'bold',
                                     size  = 14),
    plot.title        = element_text(face  = 'bold',
                                     size  = 18),
    plot.subtitle     = element_text(face  = 'italic',
                                     size  = 9),
    legend.position   = 'bottom',
    legend.background = element_rect(color = 'black'),
    legend.title      = element_text(face  = 'bold'),
    legend.text       = element_text(face  = 'bold.italic'),
    plot.caption      = element_text(face  = 'italic',
                                     size  = 9,
                                     hjust = 0)
  )
  



