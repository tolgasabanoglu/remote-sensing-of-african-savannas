# 5) Validation

# here we will use the predicted woody cover fractions
raster_file=f_wood


# this path goes to the merged validation dataset that we created in the first part of this session
polygon_file="/Users/tolgasabanoglu/Desktop/RSofAfricanSavannas/week 7/data/validation_dataset.gpkg"

# import the dataset as polygons
val_grid <- readOGR(polygon_file)

# convert polygons to dataframe
validation<-as.data.frame(val_grid)

# Read raster file
val_raster <- raster_file
val_raster

# Extract raster values at polygon locations
val_extract <- exact_extract(val_raster, val_grid)
val_extract

# Transform list of data frames to one data frame
val_extract_df <- bind_rows(val_extract)
val_extract_df

# Calculate the mean of every 9 rows to get the average of the 3x3 raster pixels we sample from
predicted<-aggregate(value ~ rep(seq(ceiling(nrow(val_extract_df)/9)), each = 9, length.out = nrow(val_extract_df)), val_extract_df, mean)

#multiply by 100 and round to 1 decimal
predicted$value <- round(predicted$value * 100, 1)

# add the prediction to our observed values
validation$woody_pred<-predicted$value

print(validation)


# 6. Assignment 

# RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((validation$woody_pred - validation$woody)^2))

# MAE (Mean Absolute Error)
mae <- mean(abs(validation$woody_pred - validation$woody))

# R-squared
ss_total <- sum((validation$woody_pred - mean(validation$woody_pred))^2)
ss_residual <- sum((validation$woody - validation$woody_pred)^2)
r_squared <- 1 - (ss_residual / ss_total)


# Bias (Mean Error)
bias <- mean(validation$woody_pred - validation$woody)


# Create a data frame with observed and predicted values
data <- data.frame(Observed = validation$woody, Predicted = validation$woody_pred )

# Create the scatterplot with regression line
plot <- ggplot(data, aes(x = Predicted, y = Observed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Predicted Values") +
  ylab("Observed Woody Cover")

# Display the scatterplot
print(plot)


# Output Evaluation Metrics:

cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")
cat("R-squared:", r_squared, "\n")
cat("Bias:", bias, "\n")


# Load required packages
library(knitr)
library(kableExtra)
library(gridExtra)

# Create a dataframe with your evaluation metrics
results <- data.frame(
  Metrics = c("RMSE", "MAE", "R-squared", "Bias"),
  Values = c(rmse, mae, r_squared, bias)
)

# Create the table
table <- kable(results, align = "c", caption = "Evaluation Metrics") %>%
  kable_styling()

print(table)
