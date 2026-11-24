####Process NOSA output for live imaging- pre and post threat or serotonin####
#1. Calculate normalised deltaf/f0 for pre and post recordings (used to plot traces)
#2. Calculate AUC for post recordings (AUC already calculated for pre in section 1)
#3. Calculate normalised AUC (boxplot quantifications)
#4. Statitics-compare normalised pre and post-AUC (see two_way_anova script for multiple comparisons)


#Clear environment
rm(list=ls())

#Load relevant libraries 
library(DescTools)
library(dplyr)

#Read in data: data = pre recordings, data_post = post recordings
data<-read.delim("file_path_pre.txt",h=T,sep="\t",na.strings = "NA")
attach(data)
data_post<-read.delim("file_path_post.txt",h=T,sep="\t",na.strings = "NA")
attach(data_post)


####### 1. Plot normalised deltaf/f0 for pre and post recordings#######

#a.Processing of  pre data 
#Calculate baseline from the first 1.5 seconds for threat cond. and 10 seconds for sero cond. for each fly   
start_row<-1
end_row<-which(data$Time..s.>=1.5)[1]
column_names<-c()
averages<-c()
for(i in 2:ncol(data)){
  col_name<-names(data)[i]
  avg<-mean(data[[col_name]][start_row:end_row])
  column_names <- c(column_names, col_name)
  averages <- c(averages, avg)
  
}
averages_df <- data.frame(sample = column_names, average = averages)
print(averages_df)

#Calculate the delta f/f0 using the baselines calculated (averages_df) 
deltaf_df_bef <- data.frame(time = data$Time..s.)

for (i in 2:ncol(data)) {
  col_name <- names(data)[i]
  avg <- averages_df$average[averages_df$sample == col_name]
  normalized_values <- ((data[[col_name]]-avg)/avg)*100
  new_col_name <- paste0("normalized_", col_name)
  deltaf_df_bef[[new_col_name]] <- normalized_values
}

#Calculate the AUC of each trace (AUC is calculated between 15-45 seconds for both threat and serotonin pre-AUC)
x<-deltaf_df_bef$time
auc_results_bef <- data.frame(Column = colnames(deltaf_df_bef)[2:ncol(deltaf_df_bef)], AUC = NA)
deltaf_df_filter_bef <- deltaf_df_bef
deltaf_df_filter_bef[, 2:ncol(deltaf_df_bef)] <- 
  apply(deltaf_df_bef[, 2:ncol(deltaf_df_bef)], 2, function(x) ifelse(x < 0, 0, x))

for (i in 2:ncol(deltaf_df_filter_bef)) {
  a <- deltaf_df_filter_bef[[i]]
  auc <- AUC(x, a, from = 15, to = 45, method = "trapezoid", absolutearea = FALSE, subdivisions = 100, na.rm = TRUE)
  auc_results_bef$AUC[i-1] <- auc  
}
print(auc_results_bef)

#Normalise delta df/f0 values by dividing by pre-AUC*100
deltaf_df_divide <- data.frame(time = data$Time..s.)

for (i in 2:ncol(deltaf_df_bef)) {
  col_name <- names(deltaf_df_bef)[i]
  auc <- auc_results_bef$AUC[auc_results_bef$Column == col_name]
  normalized_values <- (deltaf_df_bef[col_name]/auc)*100
  new_col_name <- paste0("normalized_", col_name)
  deltaf_df_divide[new_col_name] <- normalized_values
}

#Calcuate average deltaf/f0 for each time point
row_means<- rowMeans(deltaf_df_divide[, 2:ncol(deltaf_df_divide)])
deltaf_df_divide$av <- row_means

#Calculate SEM values for each time point
sem_values<-c()
for(i in 1:nrow(deltaf_df_divide)){
  row_values<-as.numeric(deltaf_df_divide[i,2:(ncol(deltaf_df_divide)-1)])
  sem_row<-std.error(row_values)
  sem_values<-c(sem_values,sem_row)
}
deltaf_df_divide$SEM<-sem_values

#b.Processing of  post data

#Calculate baseline from the first 1.5 seconds for threat cond. and 10 seconds for sero cond. for each fly 
start_row<-1
end_row<-which(data_post$Time..s.>=1.5)[1]
column_names<-c()
averages<-c()
for(i in 2:ncol(data_post)){
  col_name<-names(data_post)[i]
  avg<-mean(data_post[[col_name]][start_row:end_row])
  
  column_names <- c(column_names, col_name)
  averages <- c(averages, avg)
  
}
averages_df_post <- data.frame(sample = column_names, average = averages)
print(averages_df_post)

#Calculate the delta f/f0 using the baselines calculated (averages_df_post) 
deltaf_df_post <- data.frame(time = data_post$Time..s.)
for (i in 2:ncol(data_post)) {
  col_name <- names(data_post)[i]
  avg <- averages_df_post$average[averages_df_post$sample == col_name]
  normalized_values <- ((data_post[[col_name]]-avg)/avg)*100
  new_col_name <- paste0("normalized_", col_name)
  deltaf_df_post[[new_col_name]] <- normalized_values
}

#Normalise delta df/f0 values by dividing by pre-AUC*100
deltaf_df_divide_post <- data.frame(time = data_post$Time..s.)

for (i in 2:ncol(deltaf_df_post)) {
  auc_index <- i - 1  # because auc_results_bef$AUC starts at row 1 for column 2
  auc <- auc_results_bef[auc_index, 2] 
  col_name <- names(deltaf_df_post)[i]
  normalized_values <- (deltaf_df_post[[i]] / auc) * 100
  new_col_name <- paste0("normalized_", col_name)
  deltaf_df_divide_post[[new_col_name]] <- normalized_values
}

#Calculate the average deltaf/f0 for each time point 
row_means<- rowMeans(deltaf_df_divide_post[, 2:ncol(deltaf_df_divide_post)])
deltaf_df_divide_post$av <- row_means

# Calculate the SEM for each time point
sem_values<-c()
for(i in 1:nrow(deltaf_df_divide_post)){
  row_values<-as.numeric(deltaf_df_divide_post[i,2:(ncol(deltaf_df_divide_post)-1)])
  sem_row<-std.error(row_values)
  sem_values<-c(sem_values,sem_row)
}
deltaf_df_divide_post$SEM<-sem_values

####2. Calculate AUC for post-recordings (AUC is calculated between 2.5-32.5 seconds for post-threat and 15-45 seconds for post-serotonin (post-threat recordings have 2.5 second baseline)) (AUC already calculated for pre in section 1)####
#Calculate post-AUC
z<-deltaf_df_post$time
auc_results_aft <- data.frame(Column = colnames(deltaf_df_post)[2:(ncol(deltaf_df_post))], AUC = NA)
deltaf_df_post_filter <- deltaf_df_post
deltaf_df_post_filter[, 2:(ncol(deltaf_df_post))] <- apply(deltaf_df_post[, 2:(ncol(deltaf_df_post))], 2, function(x) ifelse(x < 0, 0, x))
for (i in 2:(ncol(deltaf_df_post_filter))) {
  d <- deltaf_df_post_filter[[i]]  
  auc <- AUC(z, d, from = 2.5, to = 32.5, method = "trapezoid", absolutearea = FALSE, subdivisions = 100, na.rm = TRUE)
  auc_results_aft$AUC[i-1] <- auc  
}

####3. Calculate normalised AUC####
AUC_prop <- data.frame(Column = rownames(auc_results_bef),prop_pre=NA, prop = NA)
for (i in 1:nrow(auc_results_aft)) {
  col_name <- names(auc_results_aft)[i]
  pre<-auc_results_bef$AUC[i]
  post<- auc_results_aft$AUC[i]
  props <- post/pre
  AUC_prop$prop[i]<- props # Store the result 
  AUC_prop$prop_pre <- 1
}

###4.Statitics- compare normalised pre and post-AUC####
prop<-AUC_prop$prop

#Test normality 
shapiro.test(prop)

#run appropriate test 
t.test(prop, mu = 1)
wilcox.test(prop, mu = 1)

#extract mean, median and quantiles
mean(prop)
median(prop)
quantile(prop)
length(prop)