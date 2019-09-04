library(dplyr) #wrangling
library(ggplot2) 
library(gridExtra) #merge plots
library(ggdendro) #dendrograms
library(gplots) #heatmap
library(tseries) #bootstrap
library(TSclust) #cluster TS
library(dtwclust)
library(tidyverse)
library(lubridate)

#import IA well data
df <- read.csv(file = "C:...IA_wide_20190101_20190801.csv")
#import know SCP IA wells
ia_scp_df <- read.csv(file = "C:...ia_scp_wells_wide_20190101_20190801.csv")

#change NAs to 0's so that the time warping will work
df<- as.data.frame(df)
df[is.na(df)] <- 0

ia_scp_df<- as.data.frame(ia_scp_df)
ia_scp_df[is.na(ia_scp_df)] <- 0

#later need to impute the missing values => see stackoverflow post here
# https://stackoverflow.com/questions/13505728/can-dynamic-time-warping-be-used-on-time-series-with-missing-values


#drop the dates
df <- df[, -1] 
ia_scp_df <- ia_scp_df[, -1] 

#create a list of 'known' SCP pattern columns to use for seeding the randomly selected sample
seed <- ia_scp_df %>% select(w06_08_IA, w05_08_IA, wH_14_IA, w07_12_IA)
#create data set wo the seed columns (just in case they are in there)
data_wo_seed <- df %>%
  select(-w06_08_IA, -w05_08_IA, -wH_14_IA, -w07_12_IA)

#random sample of x columns from data that excludes seeding columns
subset_first_x <- data_wo_seed[, sample(ncol(data_wo_seed), 16)]

#combine the randomly selected columns w/the seed columns
ready_first_x <- cbind(subset_first_x, seed) 

dtw_dist <- function(x){dist(x, method="DTW")}

#heat map of the time series
ready_first_x %>%
  as.matrix() %>%
  gplots::heatmap.2(
    #dendrogram control
    distfun = dtw_dist,
    hclustfun = hclust,
    dendrogram = 'column',
    Rowv = FALSE,
    labRow = FALSE
  )

#using tsclust
dist_ts <- TSclust::diss(SERIES = t(ready_first_x), METHOD = "DTWARP")
hc <- stats::hclust(dist_ts, method = "complete")
hclus <- stats::cutree(hc, k = 3) %>%
  as.data.frame(.) %>%
  dplyr::rename(., cluster_group = .) %>%
  tibble::rownames_to_column("type_col")

hcdata <- ggdendro::dendro_data(hc)
names_order <- hcdata$labels$label

p1 <- hcdata %>%
  ggdendro::ggdendrogram(., rotate = TRUE, leaf_labels = FALSE)

p2 <- ready_first_x %>%
  dplyr::mutate(index = 1:213) %>%
  tidyr::gather(key = type_col, value = value, -index) %>%
  dplyr::full_join(., hclus, by = "type_col") %>%
  mutate(type_col = factor(type_col, levels = rev(as.character(names_order)))) %>%
  ggplot(aes(x = index, y = value, colour = cluster_group)) +
  geom_line() +
  facet_wrap(~type_col, ncol = 1, strip.position = "left") +
  guides(color = FALSE) +
  theme_bw() +
  theme(strip.background = element_blank(), strip.text = element_blank())

gp1 <- ggplotGrob(p1)
gp2 <- ggplotGrob(p2)

grid.arrange(gp2, gp1, ncol = 2, widths = c(4, 2))

#stopped here on 8/20/19
#using dtwclust
cluster_dtw_h2 <- dtwclust::tsclust(t(df), 
                                    type = "h", 
                                    k = 3,  
                                    distance = "dtw", 
                                    control = hierarchical_control(method = "complete"),
                                    preproc = NULL, 
                                    args = tsclust_args(dist = list(window.size = 5L)))

hclus <- stats::cutree(cluster_dtw_h2, k = 3) %>% # hclus <- cluster::pam(dist_ts, k = 2)$clustering has a similar result
  as.data.frame(.) %>%
  dplyr::rename(.,cluster_group = .) %>%
  tibble::rownames_to_column("type_col")

hcdata <- ggdendro::dendro_data(cluster_dtw_h2)
names_order <- hcdata$labels$label
# Use the folloing to remove labels from dendogram so not doubling up - but good for checking hcdata$labels$label <- ""

p1 <- hcdata %>%
  ggdendro::ggdendrogram(., rotate=TRUE, leaf_labels=FALSE)

p2 <- df %>%
  dplyr::mutate(index = 1:310) %>%
  tidyr::gather(key = type_col,value = value, -index) %>%
  dplyr::full_join(., hclus, by = "type_col") %>% 
  mutate(type_col = factor(type_col, levels = rev(as.character(names_order)))) %>% 
  ggplot(aes(x = index, y = value, colour = cluster_group)) +
  geom_line() +
  facet_wrap(~type_col, ncol = 1, strip.position="left") + 
  guides(color=FALSE) +
  theme_bw() + 
  theme(strip.background = element_blank(), strip.text = element_blank())

gp1<-ggplotGrob(p1)
gp2<-ggplotGrob(p2) 

grid.arrange(gp2, gp1, ncol=2, widths=c(4,2))




#prophet predictions

#install.packages("naniar")
library(naniar)

values <- read.csv(file = "C:.../tsclust_data.csv")
values <- values[, -1]
values <- values %>% replace_with_na_all(condition = ~.x == 0)

dates <- read.csv(file = "C:.../dates.csv")
dates <- dates[, -1]

for_prophet <- cbind(dates, values)

for_prophet <- for_prophet %>%
  mutate(dates = as.Date(dates))

x = as.character(colnames(for_prophet)[2:ncol(for_prophet)])

###creating multiple traindata
#rm(list)
listforecast <- list()

for (i in 2:ncol(for_prophet))
  
{
  
  tdata <-  for_prophet[,c(1,i)]
  
  names(tdata) = c("ds", "y")
  
  ##applying mprophet model to tank A temperature
  prophetAT = prophet(tdata,daily.seasonality = T)
  
  ##prediction
  future = make_future_dataframe(prophetAT,freq = 365,periods = 7)
  forecast = predict(prophetAT,future)
  forecast$limit <-  ifelse(forecast$yhat >= 1400, 'over limit', 'under_limit')
  plot <- plot(prophetAT,forecast,
               xlab = colnames(for_prophet)[1],
               ylab = colnames(for_prophet)[i])#(you can see)
  #nam <- paste("forecast", i, sep = "")
  #assign (nam, forecast)
  plot_name <- paste("plot", colnames(for_prophet)[i], sep = "")
  assign(plot_name, plot)
  listforecast[[x[i]]] <- forecast
  
}



#listforecast[["A"]]

alert <- list()
for (i in 1:length(listforecast))
{
  forecast_new <- listforecast[[i]]
  forecast_new$limit <-  ifelse(forecast_new$yhat >= 1400, 'over_limit', 'under_limit')
  alert1 <- forecast_new[forecast_new$limit == 'over_limit',]
  if(nrow(alert1) != 0){
    alert1 <- alert1[alert1$ds == min(alert1$ds),]
    alert[[x[i]]] <- alert1[,c('ds','yhat','trend_lower','trend_upper')]
    
  }}

alert[['C']]
listforecast[['C']]


