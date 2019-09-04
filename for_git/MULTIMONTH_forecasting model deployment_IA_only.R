library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
#install.packages("prophet")
library(prophet)
library(tidyr)


####IA NOL work

##download IA data
ia <- read.csv(file = "C:/...27.csv") #import wide data
str(ia)

##all -9999 have been removed in ambari data pull; convert date times to date times
ia_working <- ia %>%
  mutate(date_time = as.POSIXct(date_time, format = '%Y-%m-%d %H:%M:%S', tz = 'UTC')) %>%
  mutate(date_as_date = date(date_time)) 
  
ia_working <- ia_working %>% select(date_as_date, date_time, 2:ncol(ia_working))

str(ia_working[ , c(1:4, 749)])

#check that have 24 hrs per day - have 24 rows and count should equal around the number of days in the df (for each hour)
check <- ia_working %>%
  arrange(date_time) %>%
  group_by(hour(date_time)) %>%
  summarise(count = n())

#remove columns w/>90% NAs
lapply(ia_working, function(x) sum(is.na(x)) / length(x))

ia_working_clean <- ia_working[lapply(ia_working, function(x) sum(is.na(x)) / length(x)) < 0.9]
lapply(ia_working_clean, function(x) sum(is.na(x)) / length(x)) 

ncol(ia_working_clean)

new_x = as.character(colnames(ia_working_clean)[3:ncol(ia_working_clean)])

##deploy model to forecast next 48 hrs
###creating multiple data
#i <- 3
listforecast <- list()

#for (i in 1:ncol(ia_working_clean)-2)
for (i in 1:746) #update to ncol(ia_working_clean - 2)
{
  
  tdata <-  ia_working_clean[,c(2,i+2)]
  
  names(tdata) = c("ds", "y")
 
  ##applying model
  prophetAT = prophet(tdata, daily.seasonality = T)
  
  ##prediction
  future = make_future_dataframe(prophetAT, periods = 48, freq = 60*60)
  forecast = predict(prophetAT,future)
  
  ##plot and changepoints plot
  plot <- plot(prophetAT,forecast,
               xlab = colnames(ia_working_clean)[2],
               ylab = colnames(ia_working_clean)[i+2])
  
  add_changepoints_to_plot <- function(m, threshold = 0.01, 
                                       cp_color = 'red',
                                       cp_linetype = 'dashed',
                                       trend = TRUE, ...) {
    layers <- list()
    if(trend) {
      trend_layer <- ggplot2::geom_line(
        ggplot2::aes_string('ds', 'trend'), color = cp_color, ...)
      layers <- append(layers, trend_layer)
    }
    signif_changepoints <- m$changepoints[abs(m$params$delta) >= threshold]
    cp_layer <- ggplot2::geom_vline(
      xintercept = as.integer(signif_changepoints), color = cp_color,
      linetype = cp_linetype, ...)
    layers <- append(layers, cp_layer)
    return(layers)
  }
  
  changept <- plot(prophetAT,forecast,
                   xlab = colnames(ia_working_clean)[2],
                   ylab = colnames(ia_working_clean)[i+2]) + add_changepoints_to_plot(prophetAT)
  
  #reassign names in a sequence
  #nam <- paste("ia_forecast", i, sep = "")
  #assign (nam, forecast)
  
  #plot_name <- paste("ia_plot", i, sep = "")
  plot_name <- paste("ia_plot_", new_x[[i]], sep = '')
  assign(plot_name, plot)
  
  changept_name <- paste("ia_changept", i, sep = "")
  assign(changept_name, changept)
  
  listforecast[[new_x[i]]] <- forecast
 }


#listforecast[["w01_02_IA"]]

alert <- list()
for (i in 1:length(listforecast))
{
  forecast_new <- listforecast[[i]]
  #!!! this is the IA NOL limit that is being specified!!! 
  forecast_new$limit <-  ifelse(forecast_new$yhat >= 2000, 
                                'over_limit', 'under_limit')
  #!!! this is where the start of the forecast is being specified!!!
  forecast_new$ds_indic <- ifelse(forecast_new$ds >= "2019-06-27 00:00:00",
                                  'yes', 'no') #and specify time#
  alert1 <- forecast_new[with(forecast_new, forecast_new$limit == 'over_limit' & forecast_new$ds_indic == 'yes'),]
  if(nrow(alert1) != 0){
    alert1 <- alert1[alert1$ds == min(alert1$ds),]
    alert[[new_x[i]]] <- alert1[,c('ds','yhat','trend_lower','trend_upper')]
    
  }}

alert[['w01_02_IA']]
listforecast[['w01_02_IA']]

#the stuff below still doesn't work

lapply(alert, cat, "\n", file="test.txt", append=TRUE)
