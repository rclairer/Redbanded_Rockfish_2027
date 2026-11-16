library(tidyverse)
library(pacfintools)
library(nwfscSurvey)
library(r4ss)

wcgop_path <- here::here("data/data_processed/wcgop/recommended")

###load data

#length
wcgop_length_comps <- read.csv(file = paste0(wcgop_path,'/wcgop_discard_lengths.csv')) %>% 
  mutate(fleet_nb=case_when(fleet=='trawl-coastwide'~1,
                                     fleet=="fixed-gear-coastwide"~2)) %>% 
  mutate(fleet=case_when(fleet=='trawl-coastwide'~'TWL',
                         fleet=="fixed-gear-coastwide"~'NONTWL'))  

write.csv(wcgop_length_comps,row.names = FALSE,
          file = here::here("data/data_processed/wcgop/length_comp_wcgop.csv"))


#sample size
wcgop_sample_size <- read.csv(file = paste0(wcgop_path,'/wcgop_discard_sample_sizes_length.csv')) %>% 
  mutate(fleet_nb = case_when(fleet=='trawl-coastwide'~ 1,
                              fleet=="fixed-gear-coastwide"~ 2)) %>% 
  mutate(fleet=case_when(fleet=='trawl-coastwide'~'TWL',
                         fleet=="fixed-gear-coastwide"~'NONTWL')) 

write.csv(wcgop_sample_size,row.names = FALSE,
          file = here::here("data/data_processed/wcgop/length_comp_sample_sizes_wcgop.csv"))


#am I using input n???????
#check input n
#what is the formula used to calculate input_n?

WCGOP_length_sample_effort <- wcgop_sample_size 

WCGOP_length_sample_effort_wide <- WCGOP_length_sample_effort %>% 
  pivot_wider(id_cols = year,names_from = c(fleet),
              values_from = c(trips,fish),
              names_glue="{fleet}_{.value}",values_fn=sum) %>% 
  relocate(year,TWL_trips,TWL_fish,
           NONTWL_trips,NONTWL_fish)

WCGOP_length_sample_effort_wide %>% View()

#write.csv(WCGOP_length_sample_effort_wide,row.names = FALSE,
#          file =paste0(file_path,"WCGOP_length_sampling_effort.csv"))


###age
#no age data yet
