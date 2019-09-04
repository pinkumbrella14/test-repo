library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
#install.packages("prophet")
library(prophet)
library(tidyr)

##download IA data
ia <- read.csv(file = "C:...01.csv") #download wide format data => date column followed by well columns

oa <- read.csv(file = "C:...01.csv") #download wide format data => date column followed by well columns

#sort by date just to make sure that everything is sorted correctly
ia <- ia %>% arrange(tag_date)
oa <- oa %>% arrange(tag_date)

#remove the first column, which is the index
ia_no_index <- ia[, -1]
oa_no_index <- oa[, -1]

#keep the dates
date_col <- ia[, 1]

#remove date columns from data frames
ia_final <- ia_no_index[, -1]
oa_final <- oa_no_index[, -1]


a = which(adist(names(ia_final), names(oa_final)) == 1, T) #find row/column locations that match by well b/n the two data sets
result = ia_final[, a[,1]] - oa_final[, a[,2]] #computes the differences between cells of columns that match by name 
#(i.e. w01_03 appears in both sets of data, as w01_03_IA and w01_03_OA, and so diff is computed)
result_new_names <- gsub(pattern = "\\_IA$", replacement = "", x = names(result)) #drop IA from name

colnames(result) <- result_new_names

colnames(result) <- result_new_names

#check the number of intersecting columns!
#the lenth of the intersecting colums is the same as the ncol(result)
#remove parts of the column names from the data sets
new_name_ia <- gsub(pattern = "\\_IA$", replacement = "", x = names(ia_final))

new_name_oa <- gsub(pattern = "\\_OA$", replacement = "", x = names(oa_final))

length(intersect(new_name_ia, new_name_oa))
ncol(result)


#add back the date column
final_data <- cbind(date_col, result)

###################################################################################################################################
#fake example to practice subtracting same named columns from two different data sets.
DF1 <- structure(list(w_H_11_XA = 10:11, w_H_13_XA = 12:11, w_H_16_XA = c(1L, 
                                                                       8L), w_13_03_XA = c(8L, 6L), w_13_12_XA = c(12L, 19L)), class = "data.frame",
                 row.names = c(NA, 
                               -2L))

DF2 <- structure(list(w_H_11_BA = 8:9, w_H_16_BA = c(1L, 4L), 
                      w_13_12_BA = 10:9), class = "data.frame", row.names = c(NA, 
                                                                             -2L))
DF1
DF2

#unfortunately strategy below does not actually work for actual data - because grep finds an extra two
##columns in the ia data that have similar names. 

nm1 <- sub("_[A-Z]$", "", names(DF1))
nm2 <- sub("_[A-Z]$", "", names(DF2))
nm3 <- intersect(nm1, nm2)
nm4 <- paste(nm3, collapse="|")
out <- DF1[grep(nm4, names(DF1))] - DF2[grep(nm4, names(DF2))]
names(out) <- sub("_[A-Z]$", "", names(out))
out

#this works!!
a = which(adist(names(DF1), names(DF2)) == 1, T)
result = DF1[, a[,1]] - DF2[, a[,2]]
####################################################################################################################################
