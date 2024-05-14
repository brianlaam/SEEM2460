## ======================= Import Libaraies ================================= ##
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cluster)
library(corrplot)
library(ggfortify)
library(psych)
library(rpart)
library(rpart.plot)
library(caret)
library(caTools)
library(Boruta)
library(cvms)
library(vip)

## ========================= Import Data ==================================== ##
# Import Data
data <- read.csv("nba_2022-23_all_stats_with_salary.csv")

# Salary Cap Dataframe
SalaryCap <- data.frame(
  Team = c("LAC", "GSW", "MIL", "BOS", "DAL", "PHO", "LAL", "DEN", "BRK", "WAS", "CLE", "CHI", "MIA", "TOR", "PHI", "ATL", "NYK", "OKC", "UTA", "ORL", "MIN", "POR", "SAC", "HOU", "DET", "MEM", "ORL", "CHO", "IND", "SAS"),
  SalaryCap = c(192905421, 192386134, 182930771, 178633307, 177244238, 176042453, 169391473, 162338665, 159566723, 152008934, 151966241, 151964990, 151408266, 150992313, 150496913, 149836313, 148987936, 148856338, 148738241, 148360910, 145793656, 144997250, 139423615, 137579793, 129153570, 127139520, 126107324, 125874047, 125706114, 104545376)
)
data_with_cap <- merge(data, SalaryCap, by = "Team")

## ========================= Data Preprocessing ============================= ##
data$Team <- gsub("/.*", "", data$Team)
data$Position <- gsub("-.*", "", data$Position)
data <- subset(data, select = -1) 

## ========================= Linear Regression ============================== ##
data <- data %>%
  mutate(Salary = as.numeric(str_extract(Salary, "[0-9]+")))

ggplot(data = data %>% drop_na(Salary), aes(x = Salary)) +
  geom_histogram(binwidth = 3000000, fill = "steelblue", color = "white") +
  labs(title = "Frequency Diagram of NBA salary in 2022-23 season",
       x = "Yearly Salary (USD)", y = "Frequency")

numeric_cols <- sapply(data, is.numeric)
numeric_data <- data[, numeric_cols]

cor_Mat <- cor(numeric_data, use = "pairwise.complete.obs")
cor_names <- names(sort(cor_Mat[, "Salary"], decreasing = TRUE))[1:20]
cor_Mat <- cor_Mat[cor_names, cor_names]
corrplot.mixed(cor_Mat, tl.pos = "lt")

ggplotly(ggplot(data %>%
         drop_na(PTS, Salary), aes(x = PTS, y = Salary)) + geom_point(col = "blue") +
         geom_smooth(formula = y ~ x, method = "loess") + 
         labs(title = "points per game vs salary in NBA 2022-23",
         x = "points per game", y = "yearly salary (USD)"))

ggplotly(
  ggplot(data %>% drop_na(VORP, Salary), aes(x = VORP, y = Salary, label = ifelse(VORP <= -0.9 | VORP > 6, Player.Name, ""))) +
    geom_point(col = "blue") +
    geom_smooth(formula = y ~ x, method = "loess") +
    geom_smooth(formula = y ~ x, method = "glm", linetype = "dotted", col = "red", se = FALSE) +
    labs(title = "Value over replacement player vs salary in NBA 2022-23", x = "VORP", y = "yearly salary (USD)") +
    geom_text(size = 3)) %>% style(textposition = "right")

ggplotly(ggplot(data %>%
         drop_na(AST, Salary), aes(x = AST, y = Salary, label = ifelse(AST > 8, Player.Name,""))) + 
         geom_point(col = "blue") + geom_smooth(formula = y ~ x, method = "loess") +
         geom_smooth(formula = y ~ x, method = "glm", linetype = "dotted", col = "red",
         se = FALSE) + labs(title = "Assists per game vs Salary in NBA 2022-23",
         x = "Assists per game", y = "yearly salary (USD)") + geom_text(size = 3)) %>% style(textposition = "right")

ggplotly(ggplot(data %>%
         drop_na(TRB, Salary), aes(x = TRB, y = Salary, label = ifelse(TRB > 10, Player.Name,""))) + 
         geom_point(col = "blue") + geom_smooth(formula = y ~ x, method = "loess") +
         geom_smooth(formula = y ~ x, method = "glm", linetype = "dotted", col = "red",
         se = FALSE) + labs(title = "Total Rebounds per game vs Salary in NBA 2022-23",
         x = "Total Rebounds per game", y = "yearly salary (USD)") + geom_text(size = 3)) %>%style(textposition = "right")

ggplot(data, aes(x = Age, y = Salary)) + geom_point(alpha = 0.7) + theme_minimal() +
  geom_smooth(formula = y ~ x, method = "glm") + labs(title = "Age vs Salary")

## ===================== Clustering Analysis ================================ ##
## Determine the value of k
features <- data[, c("Salary", "Age", "GP", "GS", "MP", "FG", "FGA", "FG.", 
                     "X3P", "X3PA", "X3P.", "X2P", "X2PA", "X2P.", "eFG.",
                     "FT", "FTA", "FT.", "ORB", "DRB", "TRB", "AST", 
                     "STL", "BLK", "TOV", "PF", "PTS", "PER", "TS.",
                     "X3PAr", "FTr", "ORB.", "DRB.", "TRB.", "AST.", 
                     "STL.", "BLK.", "TOV.", "USG.", "OWS", "DWS", "WS",
                     "WS.48", "OBPM", "DBPM", "BPM", "VORP")]
features <- na.omit(features)
scaled_features <- scale(features)

# Function to calculate total within-cluster sum of squares (WCSS)
calculate_total_withinss <- function(data, kmax) {
  wcss <- numeric(kmax)
  for (i in 1:kmax) {
    wcss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  return(wcss)
}

# Determine the optimal number of clusters using the Elbow method
elbow_method <- function(data, kmax) {
  wcss <- calculate_total_withinss(data, kmax)
  df <- data.frame(K = 1:kmax, WCSS = wcss)
  ggplot(df, aes(x = K, y = WCSS)) + 
    geom_line() + 
    geom_point() +
    labs(x = "Number of Clusters (K)", y = "Total Within-Cluster Sum of Squares (WCSS)") +
    theme_minimal()
}

set.seed(123)  # for reproducibility
max_clusters <- 10
elbow_plot <- elbow_method(scaled_features, max_clusters)
print(elbow_plot)

## K-means Analysis
num_clusters <- 5
kmeans_model <- kmeans(scaled_features, centers = num_clusters)
kmeans_model$centers

cluster_assignments <- kmeans_model$cluster
table(cluster_assignments)
merged <- cbind(data, cluster_assignments)

# Cluster Plot
clusplot(scaled_features, cluster_assignments, color = TRUE, shade = TRUE, labels = 2, lines = 0)

# Plot for Single Data Point w/ Player Positions
clustered_data <- data.frame(Cluster = as.factor(cluster_assignments), Position = data$Position, scaled_features)

cluster_plot <- ggplot(clustered_data, aes(x = scaled_features[,1], y = scaled_features[,2], color = Cluster, shape = Position)) +
  geom_point(size = 3) +
  labs(x = "Feature 1", y = "Feature 2", color = "Cluster", shape = "Position") +
  ggtitle("Cluster Plot with Player Positions") +
  theme_minimal()

print(cluster_plot)

# Number of each position in each cluster
cluster_position_counts <- merged %>%
  group_by(cluster_assignments, Position) %>%
  summarise(count = n())

print(cluster_position_counts, n=25)
cluster_position_counts$count

# Plot for cluster w/ position
ggplot(cluster_position_counts, aes(x = as.factor(cluster_assignments), y = cluster_position_counts$count, fill = cluster_position_counts$Position)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cluster Assignments", y = "Count") +
  scale_fill_manual(values = c("C" = "lightgray", "PF" = "lightgreen", "PG" = "lightblue", "SF" = "navyblue", "SG" = "orange")) +
  theme_minimal() +
  theme(legend.title = element_blank())


# Average Salary of each cluster
cluster_avg_salary <- merged %>%
  group_by(cluster_assignments) %>%
  summarise(avg_salary = mean(Salary))

print(cluster_avg_salary)

ggplot(cluster_avg_salary, aes(x = as.factor(cluster_assignments), y = avg_salary)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Cluster", y = "Average Salary") +
  ggtitle("Average Salary of Each Cluster") +
  theme_minimal()   

## ===================== Decision Tree Model w/ Cap ========================= ##
# Best Decision Tree Model
set.seed(123)  # for reproducibility
trainIndex <- createDataPartition(data_with_cap$Salary, p = 0.7, list = FALSE)
training_data <- data_with_cap[trainIndex, ]
testing_data <- data_with_cap[-trainIndex, ]

param_grid <- expand.grid(
  cp = seq(0.001, 0.1, by = 0.001)  # Cost complexity parameter for pruning
)

ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation
dt_model <- train(
  Salary ~ ., 
  data = training_data, 
  method = "rpart", 
  trControl = ctrl, 
  tuneGrid = param_grid,
  tuneLength = 20
)

optimal_model <- dt_model$finalModel
rpart.plot(optimal_model)
summary(optimal_model)

dt_rmse <- sqrt(sum(c(1.068511e+14, 3.395021e+13, 1.641822e+14, 1.161455e+13, 7.730869e+13,
                      3.068864e+13, 1.239471e+14, 7.816028e+12, 1.846962e+13, 2.898146e+13,
                      3.004144e+14, 2.817744e+13, 1.501702e+13, 7.760243e+13, 7.756024e+13,
                      1.28449e+12, 9.553446e+12, 1.986723e+12, 1.08743e+13, 1.451068e+13,
                      3.242964e+13, 6.937313e+13, 3.891695e+13, 4.903989e+13, 3.020402e+13,
                      5.639863e+12, 3.675961e+13, 1.400504e+13, 1.470311e+13))/29)


# Importance
dt_importance <- head(varImp(optimal_model), n=30)

# Importance Plot 1
ggplot(dt_importance, aes(x = rownames(dt_importance), y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(x = "Feature", y = "Importance", title = "Feature Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Importance Plot 2
vip(optimal_model, num_features = 10)

dt_rmse
mean(SalaryCap$SalaryCap)
dt_rmse/mean(SalaryCap$SalaryCap)*100






