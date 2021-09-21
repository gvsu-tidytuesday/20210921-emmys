# load packages
library(tidyverse)

# load data
tuesdata <- tidytuesdayR::tt_load('2021-09-21')
nominees <- tuesdata$nominees

# column exploration
skimr::skim(nominees)

# parse out unique info from category
# going to need a lot of hand-coding specifics - skipping for now
nominees %>% 
  select(category) %>% 
  mutate(category = str_to_lower(category),
         category = str_remove(category, "outstanding ")) %>%
  separate(category,
           into = c("category", "year"),
           sep = " - ") %>% 
  separate(category, 
           into = c("category", "subcategory"),
           sep = "( for a | for an | in a | from | in | for )") %>% 
  mutate(subcategory = case_when(
    str_detect(category, "multi-camera") ~ "multi-camera",
    str_detect(category, "single-camera") ~ "single-camera",
    str_detect(category, "short form") ~ "single-camera"
  )) %>%
  filter(is.na(subcategory)) %>% View()

# categories with more than 30 nominees (over all time)
nominees %>% 
  mutate(category = str_to_lower(category),
         category = fct_lump(category, n = 30),
         category = str_remove(category, " - .*"),
         category = str_replace(category, "for a", "-"),
         category = str_remove(category, "outstanding ")) %>%
  filter(category != "Other") %>% 
  ggplot(aes(y = fct_infreq(str_wrap(category, width = 40)))) +
  geom_bar() +
  # facet_wrap(~ year) +
  labs(y = "") +
  theme_minimal()
  
