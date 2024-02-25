# Load the data
data <- read.csv("tv_station.txt")

# Exclude the "Station Call Letters" column
data <- data[,-1]

# Perform multiple linear regression
model <- lm(SalePrice ~ ., data=data)

# Get the regression summary
summary(model)

# To see the coefficients
coefficients(model)

