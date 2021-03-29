#------------------------------------------------#

# Data Analysis of Partisanship -- 2016 Election #

# Ryan Buczkowski #

#------------------------------------------------#

# Creating URL object for partisanship table
partisanship_url <- 'https://news.gallup.com/poll/203117/gop-maintains-edge-state-party-affiliation-2016.aspx'

# Scraping partisanship table
read_html(partisanship_url) %>% 
  html_node(xpath = '//*[@id="20170126154846"]/table') %>% 
  html_table(fill = TRUE) %>% 
  as_tibble(
    
  ) -> web_table

# Cleaning web table
web_table %>% 
  select(X1, X4) %>% 
  slice(-c(1,2,53)) %>% 
  mutate(X4 = parse_number(X4)) %>% 
  rename('state_name' = X1,
         'dem_adv'    = X4
         
  ) -> partisanship_table

# Visualizing Partisanship
partisanship_table %>% 
  ggplot(aes(x = reorder(state_name, dem_adv), y = dem_adv)) +
  geom_col(aes(fill = dem_adv), color = 'black') +
  labs(x        = 'State',
       y        = 'Democrat Advantage',
       title    = 'Levels of Partisanship',
       subtitle = 'By State',
       caption  = 'Visualization created using ggplot2 in RStudio \nCreator: Ryan Buczkowski - University of Mississippi - Political Science Department') +
  scale_fill_continuous(name = 'Democrat Advantage') +
  theme_minimal() +
  theme(
    axis.text.x       = element_text(face  = 'bold.italic',
                                     angle = 90,
                                     hjust = 1,
                                     size  = 9),
    axis.text.y       = element_text(face  = 'bold.italic',
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
