# ----------------------------------------------------------------------------------------
# KUNSKAPSKONTROLL R PROGRAMMERING
# MULTIPLE REGRESSION ANALYSIS OF USED ELECTRIC SUVs
# ----------------------------------------------------------------------------------------

install.packages("readxl")
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("conflicted") 
install.packages("corrplot")
install.packages("leaps")    
install.packages("car") 
install.packages("formattable")
install.packages("Metrics")

library("tidyverse")
library("conflicted")
library("readxl")
library("leaps") 
library("corrplot")
library("car")
library("formattable")
library("Hmisc")
library("Metrics")

conflict_prefer("filter", "dplyr")   
conflict_prefer("lag", "dplyr")  
conflict_prefer("summarize", "dplyr")

library()
search()


# ----------------------------------------------------------------------------------------
# IMPORTING AND INSPECTING THE DATA 
# ---------------------------------------------------------------------------------------

excel_file <- "C:/Users/Bruger/OneDrive/Desktop/Data Science/R programmering/kunskapskontroll/car_data.xlsx"
car_data <- read_excel(excel_file, sheet="Sheet3", col_names = TRUE)

# 467 rows and 6 variables
dim(car_data)          
summary(car_data)      
glimpse(car_data)
# 2 character variables and 4 numeric/dbl
any(is.na(car_data))
# no NA values

# convert dbl to integer and remove spaces
car_data$price <- as.integer(gsub("[^0-9.]", "", car_data$price))
car_data$model_year <- as.integer(gsub("[^0-9.]", "", car_data$model_year))
car_data$mileage <- as.integer(gsub("[^0-9.]", "", car_data$mileage))
car_data$horsepower <- as.integer(gsub("[^0-9.]", "", car_data$horsepower))

# removing potential spaces in the character variables
car_data <- car_data %>%
  mutate(color = gsub(" ", "", car_data$color))
car_data <- car_data %>%
  mutate(brand = gsub(" ", "", car_data$brand))

# checking the data
glimpse(car_data)


# ------------------------------------------------------------------------------------------------
# EXPLORATORY ANALYSIS AND TRANSFORMATION OF VARIABLES  
# ------------------------------------------------------------------------------------------------


# PRICE  VARIABEL 

# range 199.000-1.599.900 and 50% between 429.850 and 589.000
summary(car_data$price)


# Distribution with a right tail with the majority of values centered around 500.000 
ggplot(car_data, aes(x=price)) +
  geom_histogram(fill = "skyblue", color = "black") +
  geom_vline(xintercept = round(mean(car_data$price), digits=0), color = "Blue", linetype = "dashed", linewidth = 0.5) +
  labs(title="Histogram for the Price Variable", x = "Price", y = "Frequency") +
  annotate("text", x=750000, y = 100, label=paste("Mean Price:", comma(round(mean(car_data$price), digits=0), format = "d", decimal.mark = ".")))


#--------------------------------------------------------------------

# MODEL_YEAR VARIABEL 

# table view show very few older cars in the data set 
table(car_data$model_year)   
summary(car_data$model_year)

# somewhat linear relationship between prices and model_year for newer cars
ggplot(car_data, aes(x=model_year,
                     y=price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Relationship between model year and price")


# mean price per model_year shows an increase from 2019 and up
by_year <- car_data %>% 
  group_by(model_year) %>%
  summarize(mean_price=mean(price))
ggplot(by_year, aes(x=model_year,
                    y=mean_price)) +
  geom_line() +
  expand_limits(y=0)

# Grouping the model_year variable into year sections
assign_year_category <- function(model_year) {
  case_when(
    model_year %in% c("2016", "2017", "2018", "2019", "2020") ~ "2020",
    model_year %in% c("2021") ~ "2021",
    model_year %in% c("2022") ~ "2022",
    model_year %in% c("2023") ~ "2023",
    model_year %in% c("2024") ~ "2024",
    TRUE ~ "Other" # Fallback for unmatched colors
  )
}
car_data <- car_data %>%
  mutate(year_category = assign_year_category(model_year))

# No observations labelled as Other
unique(car_data$year_category)   
# most observations for the years 2022 and 2023
table(car_data$year_category)    

# Convert to numerical values to obtain an ordinal order
car_data$year_category <- as.numeric(car_data$year_category)

glimpse(car_data)
str(car_data)

# ------------------------------------------------------------------

# HORSEPOWER VARIABEL 

# range from 136-1020 with 50% of the data falling between 204 and 346 
summary(car_data$horsepower)

#  observations fall around 200 and that there are potential high leverage points
ggplot(car_data, aes(x=horsepower)) +
  geom_histogram(binwidth = 20, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(0, 1200, by = 100)) +
  geom_vline(xintercept = mean(car_data$horsepower), color = "blue", linetype = "dashed") +
  labs(title = "Histogram for the Horsepower Variabel ", x = "Value", y = "Frequency") +
  annotate("text", x=450, y = 100, label=paste("Mean Horsepower:", round(mean(car_data$horsepower))))


# Horsepower vs Price with a linear regression line, shows a lot of variance around the regression line, 
ggplot(car_data, aes(x=horsepower,
                     y=price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Relationship between Horsepower and Price")


# -------------------------------------------------------------------

# MILEAGE VARIABEL 

# range from 50 to 18.900, and 50% falls between 950 and 4246
summary(car_data$mileage)


# The distribution is left skewed with the majority of observations in the lower values
ggplot(car_data, aes(x=mileage)) +
  geom_histogram(fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(0, 15000, by = 2500)) +
  geom_vline(xintercept = mean(car_data$mileage), color = "blue", linetype = "dashed") +
  labs(title = "Histogram for the Mileage Variable", x = "Value", y = "Frequency") +
  annotate("text", x=6000, y = 80, label=paste("Mean Mileage:", round(mean(car_data$mileage))))


# A negative correlation between mileage and price and not a clear linear relationship 
ggplot(car_data, aes(x=mileage,
                     y=price)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Relationship between Mileage and Price")


# ------------------------------------------------------------------

# BRAND VARIABEL 

# A lot of the brands have only few observations which can cause problems
table(car_data$brand)  

# Box plot shows a difference between the brands and the price range
ggplot(car_data, aes(x = brand,
                     y = price)) +
  geom_boxplot()

# divide into into groups with counts above 20
car_data <- car_data %>%
  group_by(brand) %>%
  mutate(
    car_brand = ifelse(n() > 20, as.character(brand), "Other")
  ) %>%
  ungroup()

# New category consists of 10 categories 
unique(car_data$car_brand)
table(car_data$car_brand)

# the box plot with the new brand grouping and price
ggplot(car_data, aes(x=car_brand,
                     y=price)) +
  geom_boxplot() +
  ggtitle("Boxplot with Brand and Price")


# inspecting the brands in the Other category
other_brands <- car_data %>%
  filter(car_brand == "Other")
other_brands

# check price levels of brands in other 
ggplot(other_brands, aes(x = brand,
                         y = price)) +
  geom_boxplot() +
  labs(x = "Brand Category", y = "Price") +
  ggtitle("Box Plot of Price for Other Brand Category")

# convert to factor and change Other to zero
car_data$car_brand <- factor(car_data$car_brand)
car_data$car_brand <- relevel(car_data$car_brand, ref = "Other")

contrasts(car_data$car_brand)
glimpse(car_data)

# ----------------------------------------------------------------

# COLOR VARIABLE 

# Several of the colors has few observations
table(car_data$color)

# bar plot displays that Gray, Black, White colors are in majority
ggplot(car_data, aes(x = color)) +
  geom_bar() +
  labs(x = "Colors", y = "Count", title = "Bar Plot of Colors by Count")

# Little correlation between color and price
ggplot(car_data, aes(x = color,
                     y = price)) +
  geom_boxplot() + 
  labs(x = "Color Category", y = "Price") +
  ggtitle("Price Distribution by Color Category")


# divide into into groups with counts above 20
car_data <- car_data %>%
  group_by(color) %>%
  mutate(
    color_category = ifelse(n() > 20, as.character(color), "Other")
  ) %>%
  ungroup()

# checking the new variable
unique(car_data$color_category)
table(car_data$color_category)

# plotting the new variable
ggplot(car_data, aes(x=color_category,
                     y=price)) +
  geom_boxplot() +
  ggtitle("Boxplot with The Color Category and Price")


# converting to nominal factor variable
car_data$color_category <- factor(car_data$color_category)

glimpse(car_data)


# -----------------------------------------------------------------

# UPDATING THE DATA SET 

car_data <- select(car_data, -brand, -color, -model_year)
# Final data set with both original columns and transformed categorical variables
# in all 9 variables
glimpse(car_data)
str(car_data)


# ASSESSING CORRELATIONS 

# corr plot with numeric variables
correlation_matrix <- cor(car_data[sapply(car_data, is.numeric)])
corrplot(correlation_matrix, 
         method="number", 
         type="upper",
         addgrid.col = "black",
         tl.col = "black")

# scatter plots with variables 
pairs(car_data,
      pch = 16,
      cex = 0.8,
      lower.panel = NULL,
      col = "steelblue",
      main = "Pairs Plot with Variables")


# --------------------------------------------------------------------------------------------
# MODEL TESTING
# --------------------------------------------------------------------------------------------

# Dividing into Test, Val and Test data
spec = c(train = .6, validate = .2, test = .2)

set.seed(1)
g = sample(cut(
  seq(nrow(car_data)), 
  nrow(car_data)*cumsum(c(0,spec)),
  labels = names(spec)
))

data = split(car_data, g)

car_train <- data$train
car_val <- data$validate
car_test <- data$test

# 280x10
dim(car_train)
# 93x10
dim(car_val)
#94x10
dim(car_test)


#--------------------------------------------------------------------

# MODEL 1 

model_1<-lm(price ~ + mileage + horsepower + year_category + car_brand + color_category, data = car_train)
summary(model_1)

vif(model_1)

par(mfrow = c(2, 2))
plot(model_1)

par(mfrow = c(1, 1))
# Cooks, distance plot
plot(model_1, which = 4)
# Cooks distance vs Leverage
plot(model_1, which = 6)


# diagnose influential data points and set threshold 
cooksD <- cooks.distance(model_1)
threshold <- 4/280

# 18 observations had a value above 4/280 
influential <- which(cooksD > threshold)
influential

# Inspecting the influential data points shows no obvious patter between the observations
data_points <- car_train[c(5, 8, 23, 44, 47, 59, 94, 99, 122, 138, 148, 152, 158, 170, 190, 212, 221, 225), ]
data_points


# inspecting the three data points with this highest Cooks distance shows
# three Mercedes-Benz for over 1.4 mio
pp <- car_train[c(44, 99, 212), ]
pp


#--------------------------------------------------------------------

# MODEL 2 - TRANSFORMATION OF Y

# converting the price variable to log
car_train <- car_train %>%
  mutate(price_log = log(price))

str(car_train)

model_2<-lm(price_log ~ + mileage + horsepower + year_category + car_brand + color_category, data = car_train)
summary(model_2)

par(mfrow = c(2, 2))
plot(model_2)

# still high leverage data points
plot(model_2, which = 4)
plot(model_2, which = 6)


#--------------------------------------------------------------------

# MODEL 3 - WITHOUT HIGH LEVERAGE POINTS 

observations <- names(influential)
problem_points <- car_train[observations,]
car_train_trimmed <- car_train %>% anti_join(problem_points)

#262x11
dim(car_train_trimmed)

model_3 <- lm(price ~ + mileage + horsepower + year_category + car_brand + color_category, data = car_train_trimmed)
summary(model_3)

par(mfrow = c(2, 2))
plot(model_3)

# high leverage plots
plot(model_3, which = 4)
plot(model_3, which = 6)


#--------------------------------------------------------------------

# BEST SUBSET SELECTION  

best_subset <- regsubsets(price ~ + mileage + horsepower + car_brand + color_category + year_category,
                      data = car_train_trimmed,
                      nvmax = NULL)

summary_bs <- summary(best_subset)
names(summary_bs)

summary_bs

# best model according to adjr2 has 17 variables
which.max(summary_bs$adjr2)

# Both Cp and BIC plots shows a quick decline 
# min Cp is 20 and min BIC is 12
plot(summary_bs$cp, xlab = "Number of Variables",
       ylab = "Cp", type = "l")
which.min(summary_bs$cp)
points(16, summary_bs$cp[16], col = "red", cex = 2,
         pch = 20)

plot(summary_bs$bic, xlab = "Number of Variables",
       ylab = "BIC", type = "l")
which.min(summary_bs$bic)
points(12, summary_bs$bic[12], col = "red", cex = 2,
         pch = 20)

# which 10 variables are included in the best BIC model
summary_bs$which[10,]


#--------------------------------------------------------------------

# MODEL 4 - WITHOUT THE COLOR CATEGORY 

model_4 <- lm(price ~ + mileage + horsepower + year_category + car_brand, data = car_train_trimmed)
summary(model_4)

par(mfrow = c(2, 2))
plot(model_4)

vif(model_4)


# -------------------------------------------------------------------------------------------
# PREDICTIONS
# -------------------------------------------------------------------------------------------

val_pred_m1 <- predict(model_1, newdata = car_val)
val_pred_m3 <- predict(model_3, newdata = car_val)
val_pred_m4 <- predict(model_4, newdata = car_val)

results <- data.frame(
  Model = c("model_1", "model_3", "model_4"),
  RMSE_val_data = c(rmse(car_val$price, val_pred_m1),
                    rmse(car_val$price, val_pred_m3),
                    rmse(car_val$price, val_pred_m4)),
  Adj_R_squared = c(summary(model_1)$adj.r.squared,
                    summary(model_3)$adj.r.squared,
                    summary(model_4)$adj.r.squared),
  BIC = c(BIC(model_1), BIC(model_3), BIC(model_4))
)

results


# Evaluating model_3 and model_4 on the test set
test_pred_m3 <- predict(model_3, newdata = car_test)
rmse(car_test$price, test_pred_m3)
test_pred_m4 <- predict(model_4, newdata = car_test)
rmse(car_test$price, test_pred_m5)


# -------------------------------------------------------------------------------------------
# INFERENCE 
# -------------------------------------------------------------------------------------------

summary(model_4)

confint(model_4)

new_data <- data.frame(
  mileage = c(5000, 1000),
  horsepower = c(150, 450),
  year_category = c(2022, 2023), 
  car_brand = c("Kia", "Audi")
)

confidence_intervals <- predict(model_4, newdata = new_data, interval = "confidence", level = 0.95)
prediction_intervals <- predict(model_4, newdata = new_data, interval = "prediction", level = 0.95)

confidence_intervals
prediction_intervals

