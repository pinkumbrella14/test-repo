library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

####import data sets
##history is already narrowed down to producers in hql code
history <- read.csv(file = 
      "C:.../well_hist_p_only.csv") 

tag_data <- read.csv(file = 
                       "C:..7.csv")
#View(tag_data)
str(tag_data)
head(tag_data)

meta <- read.csv(file = 
                               "C:...metadata.csv")

####some checks on data
##history table
history %>% group_by(ann_well_data_history_tab.well) %>% summarise(count = n())
  #this shows that there are 1,178 distinct wells in this data set, which stays consistent when dates change

####clean up data sets
##history table work 
producers_list <- history %>%
  select('date' = ann_well_data_history_tab.date_stamp,
         'well' = ann_well_data_history_tab.well,
         'producer_indicator' = ann_well_data_history_tab.well_type) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>%
  #retain the last instance of well in the data set at which it was labeled a producer
  group_by(well) %>%
  slice(which.max(date)) %>%
  ungroup() %>%
  select (-date)

##meta table
meta_final <- meta %>%
  select("tag_name" = alaska_tag_metadata.tag_name,
         "tag_type" = alaska_tag_metadata.tag_type,
         "facility" = alaska_tag_metadata.facility,
         "pad" = alaska_tag_metadata.pad,
         "well" = alaska_tag_metadata.well) %>%
  #keep only IA and OA
  filter(grepl("Inner", tag_type) | grepl("(OA, B)", tag_type)) %>%
  #add short tag type
  mutate(tag_type_short = ifelse(grepl("Inner", tag_type), "IA", "OA")) %>%
  select(-tag_type)
  
##tag data
tag_intermediate <- tag_data %>%
  #this date field has to get adjusted based on dates of the pull!!!!!!!!!
  mutate(date_time = ymd_h(paste(year, month, day, hr, sep= ' '))) %>%
  mutate(date = ymd(paste(year, month, day, sep= ' '))) %>%
  select(tag_name, date, date_time, hourly_avg)

str(tag_intermediate)  
#View(tag_intermediate)

####merge the data
##join tag data with meta data
tag_meta <- inner_join(tag_intermediate, meta_final, by = c('tag_name' = 'tag_name'))
nrow(tag_meta) #n should be less than nrow of tag_intermediate, bc ambari pull also pulled in OOA tags!

##join tag_meta with well history
tag_almost_final <- left_join(tag_meta, producers_list, by = c('well' = 'well'))
nrow(tag_almost_final) #n should still match n of tag_meta

#keep only producer wells
tag_final <- tag_almost_final %>%
  filter(producer_indicator == 'P')
nrow(tag_final) #should most likely reduce, since now limited only to producers

str(tag_final)

###################don't need block below, bc hourly averages computed in ambari
####get hourly averages for the producers from tag_final
#tag_hrly <- tag_final %>% 
  #mutate(hour = hour(date_time)) %>%
  #group_by(tag_name, well, tag_type_short, date, hour) %>%
  #summarize(hr_avg = mean(tag_value)) %>%
  #arrange(well, date, hour) %>%
  #ungroup()

#nrow(tag_hrly) #37,758
##############################################################################

####clean tag_final to get list of tags that didn't match with any well - could mean they are not in the metadata!
not_in_meta <- tag_final %>%
  select(well, tag_name) %>%
  filter(is.na(well)) %>%
  group_by((tag_name)) %>%
  summarise(count = n()) #if result is blank table, that's good!

####separate IA from OA data sets
IA <- tag_final %>% #tag_hrly %>%
  mutate(well = sub('-', '_', well)) %>%
  mutate(well = sub("^", "w", well)) %>%
  filter(tag_type_short == 'IA') %>%
  select(-tag_name) %>%
  arrange(well, date_time) #(well, date, hour)
nrow(IA) 

OA <- tag_final %>% #tag_hrly %>%
  mutate(well = sub('-', '_', well)) %>%
  mutate(well = sub("^", "w", well)) %>%
  filter(tag_type_short == "OA") %>%
  select(-tag_name) %>%
  arrange(well, date_time) #(well, date, hour)
nrow(OA)  #this number and number from ntow(IA) needs to add to total nrow of tag_final!

####reshape data
##IA
IA_wide <- IA %>%
  select(date_time, hourly_avg, well, tag_type_short) %>%
  unite("well_type", c("well", "tag_type_short")) %>%
  spread(well_type, hourly_avg) 

str(IA_wide)

####ignore below two lines - are artifact from when data structure was different
#IA_wide_final <- IA_wide %>%
  #select(date_time, 3:ncol(IA_wide - 1))

write.csv(#IA_wide_final, 
          IA_wide,
          file = "C:...27.csv",
          row.names = FALSE) #this thing is about 20 KB


##OA
OA_wide <- OA %>%
  select(date_time, hourly_avg, well, tag_type_short) %>%
  unite("well_type", c("well", "tag_type_short")) %>%
  spread(well_type, hourly_avg)

####ignore below two lines - are artifact from when data structure was different
#OA_wide_final <- OA_wide %>%
  #select(date_time, 3:ncol(OA_wide - 1))

write.csv(#OA_wide_final,
          OA_wide,
          file = "C:...27.csv")

