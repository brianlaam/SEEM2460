# ========= (b) ========= #
# Create Classes in Matrix
class0 <- matrix(c(1, 0, 2, 2, 3, 1, 4, 3), ncol=2, byrow=TRUE)
class1 <- matrix(c(-5, 5, -5, 2, -2, 5, 0, 0), ncol=2, byrow=TRUE)

# Centroids
(mean0 <- colMeans(class0))
(mean1 <- colMeans(class1))

# Import library
library(ggplot2)

# Create Classes in DF
class0_df <- data.frame(matrix(c(1, 0, 2, 2, 3, 1, 4, 3), ncol=2, byrow=TRUE))
class1_df <- data.frame(matrix(c(-5, 5, -5, 2, -2, 5, 0, 0), ncol=2, byrow=TRUE))
class0_df$class <- '0'
class1_df$class <- '1'
data <- rbind(class0_df, class1_df)

# Covariance matrices for 2 classes
cov0 <- cov(class0)
cov1 <- cov(class1)

# Pooled covariance matrix
pooled_cov <- ((nrow(class0) - 1) * cov0 + (nrow(class1) - 1) * cov1) / (nrow(class0) + nrow(class1) - 2)

# Decision boundary
(decision_boundary <- solve(pooled_cov, mean1 - mean0))

# Function for decision boundary
decision_boundary_func <- function(x) {
  (decision_boundary[1] * x + decision_boundary[2]) / -decision_boundary[2]
}

# Create new data frame for decision boundary
db_data <- data.frame(x = seq(min(data$X1), max(data$X1), length.out = 100))
db_data$y <- decision_boundary_func(db_data$x)

# Plot w/ DB
ggplot(data, aes(x=X1, y=X2, color=class)) +
  geom_point() + geom_line(data=db_data, aes(x=x, y=y), color='blue') +
  labs(x='X', y='Y', title='Data Points and Decision Boundary') +
  xlim(c(-6, 6)) + ylim(c(-1, 6)) + theme_minimal()

# Plot wo/ DB
ggplot(data, aes(x=X1, y=X2, color=class)) +
  geom_point() +
  labs(x='X', y='Y', title='Data Points') +
  xlim(c(-6, 6)) + ylim(c(-1, 6)) + theme_minimal()

# (c)
# Plot w/ DB & New Data
ggplot(data, aes(x=X1, y=X2, color=class)) +
  geom_point() +
  geom_line(data=db_data, aes(x=x, y=y), color='blue') +
  geom_point(x = 0, y = 1, color = 'green', size = 2) +
  labs(x='X', y='Y', title='Data Points and Decision Boundary with New Data') +
  xlim(c(-6, 6)) + ylim(c(-1, 6)) + theme_minimal()
