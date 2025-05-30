library(tidyverse)
# library(plsdepot)  # For PLS analysis
library(ggplot2)
library(readxl)
library(RColorBrewer)
library(reshape2)
library(pls)        # Alternative PLS package

theme_set(theme_minimal(base_size = 25))

folder_path <- "./Connectivity_Matrices"
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
file_id_list<-lapply(str_split(csv_files,"-|_"),"[",c(3,5))
file_id<-unlist(lapply(file_id_list, function(x) paste(x, collapse="_")))
connectivity_matrices <- lapply(csv_files, read.csv)
names(connectivity_matrices)<-file_id

participant_data<-read_xlsx("Dataset.xlsx")
participant_data_clean<-participant_data%>%filter(!is.na(age)&!is.na(condition))%>%filter(SubID!=9)

# 1. Convert connectivity matrices to features
# Extract the upper triangle of each connectivity matrix (excluding diagonal)
extract_upper_tri <- function(matrix) {
  upper_tri <- matrix[upper.tri(matrix)]
  return(upper_tri)
}

# Apply the function to all connectivity matrices
connectivity_features <- t(sapply(connectivity_matrices, extract_upper_tri))
selected_participant<-unlist(map2(rep(sprintf("%03s",participant_data_clean$SubID),each =2), rep(c("01","02"),32), ~ paste(.x, .y, sep = "_")))

valid_ids <- intersect(selected_participant, rownames(connectivity_features))
connectivity_features_selected <- connectivity_features[valid_ids, ]


# Number of features (connections)
n_connections <- ncol(connectivity_features)



behavioral_data1<- participant_data_clean %>%
  # mutate(socialConnectedness_change = socialConnectedness_mean_T2-socialConnectedness_mean_T1, loneliness_change = loneliness_mean_T2-loneliness_mean_T1)%>%
  select(condition,age,Female,contains("mean_T1")) 
colnames(behavioral_data1)<-gsub("_T1","",colnames(behavioral_data1))
behavioral_data1$time<-1

behavioral_data2<- participant_data_clean %>%
  select(condition,age,Female,contains("mean_T2"))
colnames(behavioral_data2)<-gsub("_T2","",colnames(behavioral_data2))
behavioral_data2$time<-2

behavioral_data<-rbind(behavioral_data1,behavioral_data2) %>%
  as.matrix()
  
# Standardize the data
connectivity_features_scaled <- scale(connectivity_features_selected)
behavioral_data_scaled <- scale(behavioral_data)

# Run PLS-C analysis
pls_result <- plsr(behavioral_data_scaled ~ connectivity_features_scaled, 
                   ncomp = min(5, ncol(behavioral_data_scaled)), 
                   validation = "CV", 
                   method = "oscorespls")

# ----- Evaluate and Interpret Results -----

# 1. Display summary of the PLS model
summary(pls_result)

# 2. Plot explained variance by number of components
explained_var <- as.data.frame(R2(pls_result)) %>%
  rownames_to_column("Component") 

ggplot(explained_var, aes(x = comps, y = value, color = response, group = response)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Explained Variance by PLS Component", 
       x = "Number of Components", 
       y = "R-squared")
