data <- read.csv("C:\\Users\\brian\\Documents\\CUHK\\SEEM2460\\Asm2\\IceCreamData.csv")
head(data)

# Linear Regression
lm_model <- lm(Revenue ~ Temperature, data = data)
summary(lm_model)

# Plotting
plot(data$Temperature, data$Revenue, main = "Revenue vs Temperature", xlab = "Temperature", ylab = "Revenue")
abline(lm_model, col = "red")

# Show coefficients
coefficients(lm_model)

# Predict
test_temperature <- 26.6
predicted_revenue <- predict(lm_model, newdata = data.frame(Temperature = test_temperature))
predicted_revenue
# OR
predicted_revenue_2 = 21.44363*test_temperature+44.83127
predicted_revenue_2

