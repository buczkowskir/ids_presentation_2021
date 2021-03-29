#------------------------------------------------------------#

# Operationalizing Dependent Variable (Election result) #

# Ryan Buczkowski #

#------------------------------------------------------------#

# Importing Election Data
read_csv('https://raw.githubusercontent.com/buczkowskir/POL_251_Spring_2021/master/election_results_2016.csv') -> election_data

# Creating Variable for election result
election_data %>% 
  mutate(clinton_result = ((clinton_votes - trump_votes) / (clinton_votes + trump_votes)) * 100,
         clinton_result = round(clinton_result, 1)) %>% 
  select(state_name, clinton_result
         
         ) -> election_data2

# Joining all the data together
home_values_joined %>% 
  inner_join(partisanship_table) %>% 
  inner_join(women_leadership_table) %>% 
  inner_join(election_data2
             
             ) -> complete_data

# Visualizing complete data set
complete_data %>% 
  ggplot(aes(x = dem_adv, y = clinton_result)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Democrat Advantage',
       y = 'Clinton Share of Vote (%)') +
  theme_minimal()

complete_data %>% 
  ggplot(aes(x = women_index, y = clinton_result)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Women Leadership',
       y = 'Clinton Share of Vote (%)') +
  theme_minimal()

complete_data %>% 
  ggplot(aes(x = housing_mean, y = clinton_result)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(x = 'Home Value',
       y = 'Clinton Share of Vote (%)') +
  theme_minimal() +
  scale_x_continuous(labels = dollar, trans = 'log')

# Creating boxplots
complete_data %>% 
  ggplot(aes(x = state_region, y = clinton_result)) +
  geom_boxplot(aes(fill = state_region)) +
  theme_minimal() +
  labs(x = 'State Region',
       y = 'Percent Share of Vote for Clinton') +
  scale_fill_discrete(guide = FALSE
                      
  ) -> box1

complete_data %>% 
  ggplot(aes(x = state_region, y = dem_adv)) +
  geom_boxplot(aes(fill = state_region)) +
  theme_minimal() +
  labs(x = 'State Region',
       y = 'Democrat Advantage') +
  scale_fill_discrete(guide = FALSE
                      
  ) -> box2

complete_data %>% 
  ggplot(aes(x = state_region, y = log(housing_mean))) +
  geom_boxplot(aes(fill = state_region)) +
  theme_minimal() +
  labs(x = 'State Region',
       y = 'Home Value') +
  scale_fill_discrete(guide = FALSE
                      
  ) -> box3

complete_data %>% 
  ggplot(aes(x = state_region, y = women_index)) +
  geom_boxplot(aes(fill = state_region)) +
  theme_minimal() +
  labs(x = 'State Region',
       y = 'Women Leadership Score') +
  scale_fill_discrete(guide = FALSE
                      
  ) -> box4


# Putting Plots together
cowplot::plot_grid(box1, box2, box3, box4)




