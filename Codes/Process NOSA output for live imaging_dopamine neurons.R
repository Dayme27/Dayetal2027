####Process NOSA output for live imaging- dopamine neurons####
#1. Calculate deltaf/f0 for each condition 
#2. Calculate AUC for each condition
#3. Statitics-compare AUC of each condition 

#Clear environment
rm(list=ls())

#Load relevant libraries
library(DescTools)
library(dplyr)

##Read in all data for each condition
data_comp<-read.delim("file_path.txt",h=T,sep="\t",na.strings = "NA")
data_female_only<-read.delim("file_path.txt",h=T,sep="\t",na.strings = "NA")
data_melanica<-read.delim("file_path.txt",h=T,sep="\t",na.strings = "NA")
data_male_only<-read.delim("file_path.txt",h=T,sep="\t",na.strings = "NA")
attach(data_comp)
attach(data_female_only)
attach(data_melanica)

####1. Calculate deltaf/f0 for each condition####
#a. COMPETITION DATA processing
#Calculate the baseline from the first 12 seconds   
start_row<-1
end_row<-which(data_comp$Time..s.>=12)[1]
column_names<-c()
averages<-c()
for(i in 2:ncol(data_comp)){
  col_name<-names(data_comp)[i]
  avg<-mean(data_comp[[col_name]][start_row:end_row])
  column_names <- c(column_names, col_name)
  averages <- c(averages, avg)
}
averages_df_comp <- data.frame(sample = column_names, average = averages)
print(averages_df_comp)

#Calculate the delta f/f0 using the baselines calculated (averages_df_comp)
deltaf_df_comp <- data.frame(time = data_comp$Time..s.)
for (i in 2:ncol(data_comp)) {
  col_name <- names(data_comp)[i]
  avg <- averages_df_comp$average[averages_df_comp$sample == col_name]
  normalized_values <- ((data_comp[[col_name]]-avg)/avg)*100
  new_col_name <- paste0("normalized_", col_name)
  deltaf_df_comp[[new_col_name]] <- normalized_values
}
#Calculate the average deltaf/f0 for each time point 
row_means<- rowMeans(deltaf_df_comp[, 2:ncol(deltaf_df_comp)])
deltaf_df_comp$av <- row_means

###Calculate the SEM values for each time point
sem_values<-c()
for(i in 1:nrow(deltaf_df_comp)){
  row_values<-as.numeric(deltaf_df_comp[i,2:(ncol(deltaf_df_comp)-1)])
  sem_row<-std.error(row_values)
  sem_values<-c(sem_values,sem_row)
}
deltaf_df_comp$SEM<-sem_values

#b.FEMALE ONLY DATA processing
#Calculate the baseline from the first 12 seconds
start_row<-1
end_row<-which(data_female_only$Time..s.>=12)[1]
column_names<-c()
averages<-c()
for(i in 2:ncol(data_female_only)){
  col_name<-names(data_female_only)[i]
  avg<-mean(data_female_only[[col_name]][start_row:end_row])
  column_names <- c(column_names, col_name)
  averages <- c(averages, avg)
}
averages_df_female_only <- data.frame(sample = column_names, average = averages)
print(averages_df_female_only)

#Calculate the delta f/f0 using the baselines calculated (averages_df_female_only)
deltaf_df_female_only <- data.frame(time = data_female_only$Time..s.)
for (i in 2:ncol(data_female_only)) {
  col_name <- names(data_female_only)[i]
  avg <- averages_df_female_only$average[averages_df_female_only$sample == col_name]
  normalized_values <- ((data_female_only[[col_name]]-avg)/avg)*100
  # Add the new column to the end of the original data frame
  new_col_name <- paste0("normalized_", col_name)
  deltaf_df_female_only[[new_col_name]] <- normalized_values
}

#Calculate the average deltaf/f0 for each time point 
row_means<- rowMeans(deltaf_df_female_only[, 2:ncol(deltaf_df_female_only)])
deltaf_df_female_only$av <- row_means

###Calculate the SEM values for each time point 
sem_values<-c()
for(i in 1:nrow(deltaf_df_female_only)){
  row_values<-as.numeric(deltaf_df_female_only[i,2:(ncol(deltaf_df_female_only)-1)])
  sem_row<-std.error(row_values)
  sem_values<-c(sem_values,sem_row)
}
deltaf_df_female_only$SEM<-sem_values

#c.MELANICA DATA processing
#Calculate the baseline from the first 12 seconds  
start_row<-1
end_row<-which(data_melanica$Time..s.>=12)[1]
column_names<-c()
averages<-c()
for(i in 2:ncol(data_melanica)){
  col_name<-names(data_melanica)[i]
  avg<-mean(data_melanica[[col_name]][start_row:end_row])
  column_names <- c(column_names, col_name)
  averages <- c(averages, avg)
}
averages_df_melanica <- data.frame(sample = column_names, average = averages)
print(averages_df_melanica)

#Calculate the delta f/f using the baselines calculated (averages_df_melanica) 
deltaf_df_melanica <- data.frame(time = data_melanica$Time..s.)
for (i in 2:ncol(data_melanica)) {
  col_name <- names(data_melanica)[i]
  avg <- averages_df_melanica$average[averages_df_melanica$sample == col_name]
  normalized_values <- ((data_melanica[[col_name]]-avg)/avg)*100
  new_col_name <- paste0("normalized_", col_name)
  deltaf_df_melanica[[new_col_name]] <- normalized_values
}
#Calculate the average deltaf/f0 for each time point 
row_means<- rowMeans(deltaf_df_melanica[, 2:ncol(deltaf_df_melanica)])
deltaf_df_melanica$av <- row_means

###Calculate the SEM values for each time point
sem_values<-c()
for(i in 1:nrow(deltaf_df_melanica)){
  row_values<-as.numeric(deltaf_df_melanica[i,2:(ncol(deltaf_df_melanica)-1)])
  sem_row<-std.error(row_values)
  sem_values<-c(sem_values,sem_row)
}
deltaf_df_melanica$SEM<-sem_values

#d. MALE ONLY processing
#Calculate the baseline from the first 12 seconds   
start_row<-1
end_row<-which(data_male_only$Time..s.>=12)[1]
column_names<-c()
averages<-c()
for(i in 2:ncol(data_male_only)){
  col_name<-names(data_male_only)[i]
  avg<-mean(data_male_only[[col_name]][start_row:end_row])
  column_names <- c(column_names, col_name)
  averages <- c(averages, avg)
}
averages_df_male_only <- data.frame(sample = column_names, average = averages)
print(averages_df_male_only)

#Calculate the delta f/f using the baselines calculated (averages_df_male_only)
deltaf_df_male_only <- data.frame(time = data_male_only$Time..s.)
for (i in 2:ncol(data_male_only)) {
  col_name <- names(data_male_only)[i]
  avg <- averages_df_male_only$average[averages_df_male_only$sample == col_name]
  normalized_values <- ((data_male_only[[col_name]]-avg)/avg)*100
  new_col_name <- paste0("normalized_", col_name)
  deltaf_df_male_only[[new_col_name]] <- normalized_values
}

#Calculate the average delta f/f0 for each time point 
row_means<- rowMeans(deltaf_df_male_only[, 2:ncol(deltaf_df_male_only)])
deltaf_df_male_only$av <- row_means

###Calculate SEM values for each time point
sem_values<-c()
for(i in 1:nrow(deltaf_df_male_only)){
  row_values<-as.numeric(deltaf_df_male_only[i,2:(ncol(deltaf_df_male_only)-1)])
  sem_row<-std.error(row_values)
  sem_values<-c(sem_values,sem_row)
}
deltaf_df_male_only$SEM<-sem_values

####2. Calculate AUC for each condition####
#a.COMP AUC
x<-deltaf_df_comp$time
auc_results_comp <- data.frame(Column = colnames(deltaf_df_comp)[2:(ncol(deltaf_df_comp)-2)], AUC = NA)
deltaf_df_comp_filter <- deltaf_df_comp
deltaf_df_comp_filter[, 2:(ncol(deltaf_df_comp) - 2)] <- 
  apply(deltaf_df_comp[, 2:(ncol(deltaf_df_comp) - 2)], 2, function(x) ifelse(x < 0, 0, x))

for (i in 2:(ncol(deltaf_df_comp_filter)-2)) {
  a <- deltaf_df_comp_filter[[i]]  
  auc <- AUC(x, a, from = 15, to = 75, method = "trapezoid", absolutearea = FALSE, subdivisions = 100, na.rm = TRUE)
  auc_results_comp$AUC[i-1] <- auc  
}
print(auc_results_comp)

#b.FEMALE ONLY AUC
auc_results_female_only <- data.frame(Column = colnames(deltaf_df_female_only)[2:(ncol(deltaf_df_female_only)-2)], AUC = NA)
deltaf_df_female_only_filter <- deltaf_df_female_only
deltaf_df_female_only_filter[, 2:(ncol(deltaf_df_female_only) - 2)] <- 
  apply(deltaf_df_female_only[, 2:(ncol(deltaf_df_female_only) - 2)], 2, function(x) ifelse(x < 0, 0, x))
for (i in 2:(ncol(deltaf_df_female_only_filter)-2)) {
  b <- deltaf_df_female_only_filter[[i]]  
  auc <- AUC(x, b, from = 15, to = 75, method = "trapezoid", absolutearea = FALSE, subdivisions = 100, na.rm = TRUE)
  auc_results_female_only$AUC[i-1] <- auc  
}
print(auc_results_female_only)

#c.MELANICA AUC
y<-deltaf_df_melanica$time
auc_results_melanica <- data.frame(Column = colnames(deltaf_df_melanica)[2:(ncol(deltaf_df_melanica)-2)], AUC = NA)
deltaf_df_melanica_filter <- deltaf_df_melanica
deltaf_df_melanica_filter[, 2:(ncol(deltaf_df_melanica) - 2)] <- 
  apply(deltaf_df_melanica[, 2:(ncol(deltaf_df_melanica) - 2)], 2, function(x) ifelse(x < 0, 0, x))
for (i in 2:(ncol(deltaf_df_melanica_filter)-2)) {
  b <- deltaf_df_melanica_filter[[i]]  
  auc <- AUC(y, b, from = 15, to = 75, method = "trapezoid", absolutearea = FALSE, subdivisions = 100, na.rm = TRUE)
  auc_results_melanica$AUC[i-1] <- auc 
}
print(auc_results_melanica)

#d.MALE ONLY AUC
y<-deltaf_df_male_only$time
auc_results_male_only <- data.frame(Column = colnames(deltaf_df_male_only)[2:(ncol(deltaf_df_male_only)-2)], AUC = NA)
deltaf_df_male_only_filter <- deltaf_df_male_only
deltaf_df_male_only_filter[, 2:(ncol(deltaf_df_male_only) - 2)] <- 
  apply(deltaf_df_male_only[, 2:(ncol(deltaf_df_male_only) - 2)], 2, function(x) ifelse(x < 0, 0, x))
for (i in 2:(ncol(deltaf_df_male_only_filter)-2)) {
  b <- deltaf_df_male_only_filter[[i]]
  auc <- AUC(y, b, from = 15, to = 75, method = "trapezoid", absolutearea = FALSE, subdivisions = 100, na.rm = TRUE)
  auc_results_male_only$AUC[i-1] <- auc
}
print(auc_results_male_only)


####3. Statitics-compare AUC of each condition#### 

#Combine AUC for all conditions into single data frame
auc_results_comp$condition<-"Comp"
auc_results_female_only$condition<-"female_only"
auc_results_melanica$condition<-"melanica"
auc_results_male_only$condition<-"male only"
combined_auc_data <- bind_rows(auc_results_comp,auc_results_female_only,auc_results_melanica,auc_results_male_only)
combined_auc_data$condition <- factor(combined_auc_data$condition)

#Run test on combined data 
anova_model <- aov(AUC ~ condition, data = combined_auc_data)
summary(anova_model)
TukeyHSD(anova_model)

