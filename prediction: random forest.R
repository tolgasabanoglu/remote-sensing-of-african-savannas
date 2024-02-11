# 3) Import raster containing spectral-temporal metrics

# Next, we will import the raster with spectral-temporal metrics on which we will do our prediction.

s2 <- raster::stack(file.path("/Users/tolgasabanoglu/Desktop/RSofAfricanSavannas/week 7/data", "2022_SEN2L_NDV_STM.tif"))
s2
#we name the bands according to the columns of our endmember dataframe
names(s2) <- paste("V", seq(1:13), sep="")


# 4) Apply Random Forest Regression

# Define accuracy from 5-fold cross-validation as optimization measure
cv <- tune.control(cross = 5) 

# Use tune.randomForest to assess the optimal combination of ntree and mtry
tune_soil <- tune.randomForest(fraction~., data = train_soil, ntree=100, mtry=c(2:10), tunecontrol = cv)
tune_herb <- tune.randomForest(fraction~., data = train_herb, ntree=100, mtry=c(2:10), tunecontrol = cv)
tune_wood <- tune.randomForest(fraction~., data = train_wood, ntree=100, mtry=c(2:10), tunecontrol = cv)


# Store the best model in a new object for further use
tuned_soil <- tune_soil$best.model
tuned_herb <- tune_herb$best.model
tuned_wood <- tune_wood$best.model

# Predict each of the soil, herbaceous and grass layer
f_soil <- predict(s2, tuned_soil)
f_herb <- predict(s2, tuned_herb)
f_wood <- predict(s2, tuned_wood)

# Stack the layers
f_s2 <- brick(f_soil, f_herb, f_wood)
f_s2

#Write the predicted fractions to a RGB raster layer
writeRaster(f_s2, "/Users/tolgasabanoglu/Desktop/RSofAfricanSavannas/week 7/data/prediction.tif", overwrite=TRUE)
