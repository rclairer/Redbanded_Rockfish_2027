library(tidyverse)
library(pacfintools)
library(nwfscSurvey)
library(r4ss)

load(here::here("data/data_raw/PacFIN.RDBD.bds.14.Aug.2026.RData")) 
load(here::here("data/data_raw/PacFIN.RDBD.CompFT.14.Aug.2026.RData")) 

#ss_data <- SS_readdat(file="../../model/2017_yelloweye_model_updated_ss3_exe/yelloweye_data.ss")

#path for storing
save_dir = here::here("data/data_processed/com_length_age_comps")

#Bins for biological data
#some length is in mm, will have to convert
length_bins <- seq(14, 78, by = 2) #min and max of bds_cleaned$lengthcm
age_bins <- seq(0, 67, by = 1) 

used_gears <- c("HKL","MID","NET","POT","TWL", "TWS","TLS")
good_lengths <- "F"
good_samples <- c("","M","C")
good_methods <- "R"
good_age_method <- c("B", "BB","1")

bds_cleaned <- pacfintools::cleanPacFIN(
  Pdata = bds.pacfin,
  keep_gears = used_gears,
  keep_length_type = good_lengths,
  keep_sample_type = good_samples,
  keep_sample_method = good_methods,
  keep_age_method = good_age_method,
  clean = TRUE
  )#|>
  #mutate(geargroup = ifelse(geargroup!='TWL','NONTWL',geargroup)) 

## make the same but keeping al the records so I can see what was removed
bds_cleaned_full <- pacfintools::cleanPacFIN(
  Pdata = bds.pacfin,
  keep_gears = used_gears,
  keep_length_type = good_lengths,
  keep_sample_type = good_samples,
  keep_sample_method = good_methods,
  keep_age_method = good_age_method,
  clean = FALSE
)#|>
#mutate(geargroup = ifelse(geargroup!='TWL','NONTWL',geargroup)) 



nrow(bds.pacfin);nrow(bds_cleaned)
ncol(bds.pacfin);ncol(bds_cleaned)
names(bds_cleaned)

bds_cleaned %>% 
  group_by(geargroup) %>% 
  summarise(n=length(geargroup))

bds_cleaned %>% 
  group_by(geargroup,PACFIN_GEAR_NAME) %>% 
  summarise(n=length(PACFIN_GEAR_NAME))

#plot gear along time
bds_cleaned %>%
  filter(geargroup=='TWL') %>% 
  group_by(year,geargroup,PACFIN_GEAR_NAME) %>% 
  summarise(n=length(SAMPLE_NUMBER)) %>% 
  ggplot()+geom_bar(aes(year,n,fill=PACFIN_GEAR_NAME),position='stack',stat='identity')

bds_cleaned %>%
  filter(!geargroup=='TWL') %>% 
  group_by(year,geargroup,PACFIN_GEAR_NAME) %>% 
  summarise(n=length(SAMPLE_NUMBER)) %>% 
  ggplot()+geom_bar(aes(year,n,fill=PACFIN_GEAR_NAME),position='stack',stat='identity')

bds_cleaned %>%
  group_by(year,PACFIN_GEAR_NAME) %>% 
  filter(PACFIN_GEAR_NAME%in%c('TRAWLS')) %>% 
  summarise(n=length(SAMPLE_NUMBER))

#HAVE TO CHOOSE FLEET STRUCTURE
bds_cleaned %>% pull(FTID) %>% unique() %>% length()

bds_cleaned %>% pull(FTID) %>% is.na() %>% sum()

bds_cleaned %>% pull(SAMPLE_NUMBER) %>% unique() %>% length()
nrow(bds_cleaned)

bds_cleaned %>% 
  group_by(fleet) %>% 
  summarize(FTID_NA=sum(is.na(FTID)),FTID_nNA=sum(!is.na(FTID)),
            FTID_tot=length(FTID)) %>% 
  mutate(verif=FTID_NA+FTID_nNA)

bds_cleaned %>%
  group_by(year,fleet) %>%
  filter(!is.na(FTID)) %>% 
  filter(!is.na(length)) %>% 
  summarise(n_trips=length(unique(FTID)),n_fish=length(length)) 

# df n_fish + n_trip long format
bds_cleaned %>%
  group_by(fleet,year) %>%
  filter(!is.na(length)) %>% 
  summarise(n_fish=length(length),
            n_trips=length(unique(SAMPLE_NUMBER)),
            n_trips2=length(unique(FTID)),
            n_trips3=length((FTID))) %>% 
  mutate(diff=n_trips-n_trips2)

# check 2 ways of counting trips -> they match
bds_cleaned %>% 
  group_by(fleet,year) %>%
  filter(!is.na(FTID)) %>%
  filter(!is.na(length)) %>% 
  # first way of computing trips
  distinct(FTID) %>% 
  summarize(n_trips=length(FTID)) %>%
  ## merge with a second one to compare
  full_join(bds_cleaned %>% 
              group_by(fleet,year) %>%
              filter(!is.na(FTID)) %>%
              filter(!is.na(length)) %>% 
              summarize(n_trips2=length(unique(FTID))),
            by=c('fleet','year')) %>% 
  mutate(isequal=n_trips==n_trips2) %>% summary()

bds_cleaned %>% 
  filter(is.na(FTID),!is.na(Age)) %>% nrow()

#visualizations
plotCleaned(bds_cleaned,savedir = save_dir)


#COMPUTE SAMPLING EFFORT

#Need a number of trips and fish samples per year per fleet

#a row = a fish
#SAMPLE_NUMBER = sample number (sometime a tow sometime not)
#FTID = a fish ticket = proxy of a trip

fleets_names <- tibble(
  fleet_name=c(levels(as.factor(bds_cleaned$fleet))),
  fleet_number=c(1,2,3,4,5,6,7))

#LENGTH SAMPLING EFFORT

### Number of trip and sample

#### Computation
# df n_fish + n_trip 
length_sample_effort <- bds_cleaned %>%
  group_by(year,fleet) %>%
  filter(!is.na(FTID)) %>% 
  filter(!is.na(length)) %>% 
  summarise(n_trips=length(unique(FTID)),n_fish=length(length)) 

# df n_fish + n_trip large format
length_sample_effort_wide <- length_sample_effort %>% 
  pivot_wider(id_cols = year,names_from = c(fleet),
              values_from = c(n_trips,n_fish),
              names_glue="{fleet}_{.value}") %>% 
  relocate(year,TWL_n_trips,TWL_n_fish,
            HKL_n_trips, HKL_n_fish,
           MID_n_trips, MID_n_fish,
           NET_n_trips, NET_n_fish,
           TLS_n_trips, TLS_n_fish,
           TWS_n_trips, TWS_n_fish,
           POT_n_trips, POT_n_fish) #%>% 

#write.csv(length_sample_effort_wide,row.names = FALSE,
#          file =paste0(save_dir,"com_length_sampling_effort.csv"))

minmaxy_l <- c(min(length_sample_effort$year),max(length_sample_effort$year))

length_sample_effort_plt <- length_sample_effort_wide %>% 
  tidyr::pivot_longer(cols=-year) %>% 
  ggplot2::ggplot(aes(x="",y=-year))+ggplot2::geom_tile(aes(fill=value))+
  #ggplot2::geom_text(aes(label=value))+
  ggplot2::facet_wrap(~name,nrow=1)+
  ggplot2::scale_fill_gradient2()+
  ggplot2::scale_y_continuous(breaks=-seq(minmaxy_l[1],minmaxy_l[2],1))+
  labs(x='fleet*(numbers of trips OR fish)',y='year',
       title="Updated length sampling effort")+
  theme(legend.position='bottom') 

length_sample_effort_plt
ggsave(length_sample_effort_plt,filename=
         paste0(save_dir,"/com_length_sampling_effort.png"),
       width=12,height = 8)

##Age sampling effort

### trips and samples 

#### Computation

# df n_fish + n_trip large format
age_sample_effort <- bds_cleaned %>%
  group_by(year,fleet) %>%
  #filter(!is.na(FTID)) %>% #remove NA fish tickets
  filter(!is.na(lengthcm)) %>%  #remove fish w no length, cause we need them to produce caal
  filter(!is.na(Age)) %>%  #remove rows with no age samples
  summarise(n_trips=length(unique(FTID)),n_fish=length(Age)) #compute 


# df n_fish + n_trip large format
age_sample_effort_wide <- age_sample_effort %>% 
  pivot_wider(id_cols = year,names_from = c(fleet),
              values_from = c(n_trips,n_fish),
              names_glue="{fleet}_{.value}") %>% 
  relocate(year,TWL_n_trips,TWL_n_fish,
           HKL_n_trips, HKL_n_fish) #%>% 

#write.csv(age_sample_effort_wide,row.names = FALSE,
#          file = paste0(save_dir,"com_age_sampling_effort.csv"))

minmaxy_age <- c(min(age_sample_effort$year),max(age_sample_effort$year))

#plot these numbers
age_sample_effort_plt <- age_sample_effort %>% 
  pivot_longer(cols=c(n_trips,n_fish),
               values_to = 'value',names_to = 'var') %>% 
  mutate(name=paste0(fleet,'_',var)) %>% 
  ggplot(aes(x="",y=-year))+geom_tile(aes(fill=value))+
  geom_text(aes(label=value))+
  facet_wrap(~name,nrow=1)+
  scale_fill_gradient2()+
  scale_y_continuous(breaks=-seq(minmaxy_age[1],minmaxy_age[2],1))+
  labs(x='fleet*(numbers of trips OR fish)',y='year',
       title="Updated age sampling effort")+
  theme(legend.position='bottom') 

age_sample_effort_plt

ggsave(age_sample_effort_plt,filename=
         paste0(save_dir,"/com_age_sampling_effort.png"),
       width=8,height = 5)

