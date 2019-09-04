library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(Amelia)
library(Boruta)

####import data sets 

#full history table narrowed down to producers
history <- read.csv(file = 
                      "C:..._history_producers_only.csv")#data set with multiple features for each well

tag_data <- read.csv(file = 
                       "C:..._tags_daily.csv") #daily data pull - combined OA and IA

meta <- read.csv(file = 
                               "C:...metadata.csv")



####clean up data sets
##history table work 
producers_list <- history %>%
  select('date' = ann_well_data_history_tab.date_stamp,
         'well' = ann_well_data_history_tab.well,
         'area' = ann_well_data_history_tab.area,
         'drillsite' = ann_well_data_history_tab.drillsite,
         'well_status' = ann_well_data_history_tab.wi_status,
         #problem, status, planned_action, engineering_note (<= especially this one!),
         'txia_comm' = ann_well_data_history_tab.txia,
         'iaxoa_comm' = ann_well_data_history_tab.iaxoa,
         'producer_indicator' = ann_well_data_history_tab.well_type,
         'type_well' = ann_well_data_history_tab.type_well,
         'ia_oper_pressure' = ann_well_data_history_tab.ia_oper_pressure, #these may be the indicated IA limits for the well
         'oa_oper_pressure' = ann_well_data_history_tab.oa_oper_pressure, #oa indicated pressure
         'gas_lift_line_attached' = ann_well_data_history_tab.gas_lift_line_attached) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) %>%
  #retain the last instance of well in the data set at which it was labeled a producer
  group_by(well) %>%
  slice(which.max(date)) %>%
  ungroup() %>%
  select (-date)

#use wrapper methods to ID relevant features in predicting operatable wells
str(producers_list) #shows that categorical features are factors so don't need to change data type

#treat missing values
##visualize missing values
ggplot_missing <- function(x){
  x %>%
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present", "Missing")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(producers_list)
#missmap(producers_list)

#impute missing values 
##fill in the non-N values for gas_lift_line_attached
producers_list_ <- producers_list %>%
  mutate(gas_lift_line_attached = ifelse(gas_lift_line_attached == 'N', 'No', 'Yes')) %>%
  #add yes to txia_comm and iaxoa_comm
  mutate(txia_comm = ifelse(txia_comm == 'N', 'No', 'Yes')) %>%
  mutate(iaxoa_comm = ifelse(txia_comm == 'N', 'No', 'Yes')) %>%
  #remove columns that you really can't autofill
  select(-type_well) %>%
  #remove rows where there are missing values for ia_oper_pressure and oa_oper_pressure bc can't impute this... just guessing
  filter(!is.na(ia_oper_pressure)) %>%
  filter(!is.na(oa_oper_pressure)) %>%
  filter(!is.na(well))
  
ggplot_missing(producers_list_) #confirm that there aren't any missing values

#ID relevant features w/Boruta
set.seed(111)
boruta.producers_train <- Boruta(well_status~., data = producers_list_, doTrace = 2)
boruta.producers_ <- TentativeRoughFix(boruta.producers_train)
plot(boruta.producers_, xlab = "", xaxt = "n")
lz <- lapply(1:ncol(boruta.producers_$ImpHistory), 
             function(i)boruta.producers_$ImpHistory[is.finite(boruta.producers_$ImpHistory[, i]), i])
names(lz) <- colnames(boruta.producers_$ImpHistory)
Labels <- sort(sapply(lz, median))
axis(side = 3, labels = names(Labels), at = 1:ncol(boruta.producers_$ImpHistory), cex.axis = 0.6, padj = 0, las = 3)

#so txia communication is very important and should try to narrow the wells down by subsetting for the ones that do have it


##meta table
meta_final <- meta %>%
  select("tag_name" = alaska_tag_metadata.tag_name,
         "tag_type" = alaska_tag_metadata.tag_type,
         "facility" = alaska_tag_metadata.facility,
         "pad" = alaska_tag_metadata.pad,
         "well" = alaska_tag_metadata.well) %>%
  #keep only IA and OA
  filter(grepl("(IA, A)", tag_type) | grepl("(OA, B)", tag_type)) %>%
  #add short tag type
  mutate(tag_type_short = ifelse(grepl("Inner", tag_type), "IA", "OA")) %>%
  select(-tag_type)

##tag data
tag_intermediate <- tag_data %>%
  #this date field has to get adjusted based on dates of the pull!!!!!!!!!
  mutate(tag_date = as.POSIXct(tag_date, format="%Y-%m-%d")) %>%
  select(tag_name, tag_date, day_avg)%>%
  arrange(tag_name, tag_date)

str(tag_intermediate)  
#View(tag_intermediate)

#make alterations to producers_list that reflect features developed when made producers_list_
producers_list <- producers_list %>%
  mutate(gas_lift_line_attached = ifelse(gas_lift_line_attached == 'N', 'No', 'Yes')) %>%
  #add yes to txia_comm and iaxoa_comm
  mutate(txia_comm = ifelse(txia_comm == 'N', 'No', 'Yes')) %>%
  mutate(iaxoa_comm = ifelse(txia_comm == 'N', 'No', 'Yes')) %>%
  filter(!is.na(well))

####merge the data
##join tag data with meta data
tag_meta <- inner_join(tag_intermediate, meta_final, by = c('tag_name' = 'tag_name'))
nrow(tag_meta) #n should be less than nrow of tag_intermediate, bc ambari pull also pulled in OOA tags!


##join tag_meta with well history
tag_almost_final <- left_join(tag_meta, producers_list, by = c('well' = 'well'))
nrow(tag_almost_final) #n should still match n of tag_meta


#keep only producer wells
tag_final <- tag_almost_final %>%
  filter(producer_indicator == 'P') %>%
  #filter(txia_comm == 'Yes') %>%
  #limit to wells that have ia_oper_press idicator of 2000
  filter(ia_oper_pressure == 2000) %>%
  filter(gas_lift_line_attached == "No")
nrow(tag_final) #should reduce based on what restrictions are put in the code
#check how many wells that is
tag_final %>% group_by(well) %>% summarize(count = n()) #just 71 wells

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
####separate IA from OA data sets
#reduce the tag_final data frame to only necessary columns
tag_final_reduced <- tag_final %>%
  select(tag_name, tag_date, day_avg, facility, pad, well, tag_type_short)

IA <- tag_final_reduced %>% #tag_hrly %>%
  mutate(well = sub('-', '_', well)) %>%
  mutate(well = sub("^", "w", well)) %>%
  filter(tag_type_short == 'IA') %>% 
  filter(day_avg > 0) %>% #this will need to get checked further.... tags L3:03P25360I.PV and L3:4103P25592AI.PV are supposedly both
        #L3-25 and both IA.... confirmed by the metadata table
  select(-tag_name) %>%
  arrange(well, tag_date) #(well, date)
nrow(IA) 

OA <- tag_final_reduced %>% #tag_hrly %>%
  mutate(well = sub('-', '_', well)) %>%
  mutate(well = sub("^", "w", well)) %>%
  filter(tag_type_short == "OA") %>%
  filter(day_avg > 0) %>%
  select(-tag_name) %>%
  arrange(well, tag_date) #(well, date)
nrow(OA)  #this number and number from ntow(IA) needs to add to total nrow of tag_final!

####reshape data
##IA
IA_wide <- IA %>%
  select(tag_date, day_avg, well, tag_type_short) %>%
  unite("well_type", c("well", "tag_type_short")) %>%
  spread(well_type, day_avg) 

str(IA_wide)

####ignore below two lines - are artifact from when data structure was different
#IA_wide_final <- IA_wide %>%
  #select(date_time, 3:ncol(IA_wide - 1))

write.csv(#IA_wide_final, 
          IA_wide,
          file = "C:..._special1.csv",
          row.names = FALSE) #this thing is about 20 KB


##OA
OA_wide <- OA %>%
  select(tag_date, day_avg, well, tag_type_short) %>%
  unite("well_type", c("well", "tag_type_short")) %>%
  spread(well_type, day_avg)

####ignore below two lines - are artifact from when data structure was different
#OA_wide_final <- OA_wide %>%
  #select(date_time, 3:ncol(OA_wide - 1))

write.csv(#OA_wide_final,
          OA_wide,
          file = "C:..._special1.csv")

#############pull out the SCP know wells - IA and OA separately##########################################################################################
##IA
#pull out known IA SCP wells from tag_meta
tag_meta_ia_scp_wells <- tag_meta %>%
  filter(well %in% c("05-08", "06-08", "07-12", "E-13", "H-14", "L2-20", "P-23", "P2-27", "02-15", "02-34"))
    #check to see how many of these wells we grabbed - nice, got all 10 wells
    tag_meta_ia_scp_wells %>% group_by(well) %>% summarise(n())
    
#join with info from well history
ia_scp_wells <- left_join(tag_meta_ia_scp_wells, producers_list, by = c('well' = 'well'))

#make sure to narrow down to IA sensors only for these wells
ia_scp_wells_final <- ia_scp_wells %>% 
  mutate(well = sub('-', '_', well)) %>%
  mutate(well = sub("^", "w", well)) %>%
  filter(tag_type_short == 'IA') %>% 
  filter(day_avg > 0) %>% #this will need to get checked further.... tags L3:03P25360I.PV and L3:4103P25592AI.PV are supposedly both
  #L3-25 and both IA.... confirmed by the metadata table
  select(-tag_name) %>%
  arrange(well, tag_date) #(well, date)
nrow(ia_scp_wells_final) 

####reshape data
##IA
ia_scp_wells_wide <- ia_scp_wells_final %>%
  select(tag_date, day_avg, well, tag_type_short) %>%
  unite("well_type", c("well", "tag_type_short")) %>%
  spread(well_type, day_avg) 

write.csv(ia_scp_wells_wide,
  file = "C:.../ia_scp_wells_wide_20190101_20190801.csv",
  row.names = FALSE)


#OA STILL NEEDS TO GET DONE