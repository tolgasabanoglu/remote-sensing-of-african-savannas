library(readr)
library(data.table)
library(e1071)
library(randomForest)
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(exactextractr)
library(dplyr)
library(tidyverse)
library(rjson)
library(jsonlite)
library(terra)
library(ggplot2)
library(geojsonio)


# 1) Import spectral library files

import_spec_library <- function(file_path) {
  
# Read JSON file and flatten the structure
endmember <- jsonlite::fromJSON(file_path, flatten = TRUE)
  
# Extract the length of profiles
len<-length(endmember[["features"]][["properties.profiles.y"]])
  
# Extract profiles and convert to dataframe
profiles<-endmember[["features"]][["properties.profiles.y"]]
t <- as.data.frame(t(matrix(unlist(profiles), ncol = len)))
  
# Attach class column to the beginning
t <- cbind(class=endmember[["features"]][["properties.name"]],t)
  
# Replace class values with IDs
t$class <- ifelse(t$class == "soil", 1, ifelse(t$class == "grass", 2, ifelse(t$class == "woody", 3, t$class)))
  
  return(t)
}

#file path to the spectral library GeoJSON file
file_path <- "/Users/tolgasabanoglu/Desktop/RSofAfricanSavannas/week 7/data/spec_lib.geojson"

#call the function defined above and import
spec_lib <- import_spec_library(file_path)
 
spec_lib

# 2) Create synthetic training data

synthmix <- function(df, cl_target, cl_background, n_samples=1000,
                     mix_complexity=c(2, 3, 4), p_mix_complexity=c(.7, .2, .1),
                     within_class_mixture=TRUE, include_endmember=TRUE){
  
  "Function to generate synthetic training data mixtures from pure endmember
  spectra.
  
  df:                 (list) Input dataframe. First column must contain the
                      class-IDs in integer format. Remaining columns must 
                      contain the features to be mixed. 
  cl_target:          (int) Target class' integer ID value.
  cl_background:      (int) Background class' integer ID value(s). Vector for 
                      multiple classes, e.g. 'c(2, 3, 4)'.
  n_samples:          (int) Number of synthetic training points to generate.
  mix_complexity:     (int) Vector with desired number of possible mixtures
                      between different classes.
  p_mix_complexity:   (float) Vector containing desired occurence propabilities 
                      associated to the number of possible mixtures 
                      (i.e. mix_complexity). Must be of same length as 
                      'mix_complexity' argument.
  
  returns:            (list) Dataframe with linearily mixed features and 
                      corresponding fraction of target class (i.e. cl_target)
  "
  
  # total number of classes
  all_ems <- c(cl_target, cl_background)
  n_em <- length(all_ems)
  
  # create empty df to store training data
  df_mixture <- setNames(data.frame(matrix(ncol = ncol(df), nrow = 0)), 
                         c(names(df)[2:length(df)], "fraction")) 
  
  # index list of EMs for sampling
  idx_em <- list()
  for (em in all_ems){
    idx_em[[em]] <- which(df[,1] == em)
  }
  
  # vector for fraction calculation
  zero_one <- integer(nrow(df))
  zero_one[idx_em[[cl_target]]] <- 1
  
  # iterator for generating each synthetic mixture 
  for (i in 1:n_samples) {
    
    if (length(p_mix_complexity) == 1){
      complexity = mix_complexity
    } else {
      # sample mixing complexity based on mixing likelihoods
      complexity = sample(as.vector(mix_complexity), 
                          size = 1, 
                          prob = as.vector(p_mix_complexity)) 
    }
    
    # select background EMs which will be included in the mixture
    if (within_class_mixture){
      background <- sample(all_ems, complexity - 1, replace = TRUE)
    } else {
      background <- sample(cl_background, complexity - 1, replace = FALSE)
    }
    
    # sample indices of selected EMs
    response <- c(cl_target, background)      
    drawn_index <- c()
    for (r in response){
      drawn_index <- c(drawn_index, sample(idx_em[[r]], 1))
    }
    drawn_features <- df[drawn_index, 2:length(df)]
    drawn_fraction <- zero_one[drawn_index]
    
    # sample random weights
    drawn_weights <- c()
    for (j in 1:(complexity-1)){
      if (j == 1){
        weight <- runif(1)
      } else {
        weight <- runif(1) * (1. - sum(drawn_weights))
      }
      drawn_weights <- c(drawn_weights, weight)
    }
    drawn_weights <- c(drawn_weights, (1. - sum(drawn_weights)))
    
    # calculate mixtures and associated fractions
    calc_mixtures <- apply(drawn_features * drawn_weights, 2, FUN=sum)
    calc_fraction <- sum(drawn_fraction * drawn_weights)
    
    # append to df
    df_mixture[nrow(df_mixture)+1,] <- c(calc_mixtures, calc_fraction)
  }
  
  if (include_endmember){
    df_endmember <- cbind(df[,2:length(df)], zero_one)
    colnames(df_endmember) <- c(names(df)[2:length(df)], "fraction")
    df_mixture <- rbind(df_mixture, df_endmember)
  }
  
  return(df_mixture)
  
}

spec_lib
# In the next step, we will run the synthmix function on our endmember dataset to create a synthetic training dataset that we can use to train a regression algorithm.

train_soil <- synthmix(df=spec_lib,cl_target = 1,cl_background = c(2,3),n_samples = 1000,mix_complexity = c(2,3),p_mix_complexity = c(.5,.5), within_class_mixture = F)
train_herb <- synthmix(df=spec_lib,cl_target = 2,cl_background = c(1,3),n_samples = 1000,mix_complexity = c(2,3),p_mix_complexity = c(.5,.5),within_class_mixture = F)
train_wood <- synthmix(df=spec_lib,cl_target = 3,cl_background = c(1,2),n_samples = 1000,mix_complexity = c(2,3),p_mix_complexity = c(.5,.5),within_class_mixture = F)
