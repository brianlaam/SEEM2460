# Load libraries
library(ggplot2)

# Read the CSV file
data <- read.csv("C:/Users/brian/Documents/CUHK/SEEM2460/Asm1/Data/TreeData.csv", header=TRUE, stringsAsFactors=FALSE)
# data_filter <- data[, -1]
# data_filter$Year <- as.factor(data_filter$Year)

# Plot the data #
# Year VS Tree
ggplot(data, aes(x = Year, y = Number.of.trees.sold, fill = Type.of.tree)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Trees Sold by Year and Type", x = "Year", y = "Number of Trees Sold") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Fake Tree", "Real Tree")) +
  theme_bw()

# Year VS Average Tree Price
ggplot(data, aes(x = Year, y = Average.Tree.Price, color = Type.of.tree)) +
  geom_line() +
  labs(title = "Average Tree Price by Year", x = "Year", y = "Average Tree Price", color = "Type of Tree") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Real Tree", "Fake Tree")) +
  theme_bw()

# Year VS Sales
ggplot(data, aes(x = Year, y = Sales, color = Type.of.tree)) +
  geom_line() +
  labs(title = "Sales by Year", x = "Year", y = "Sales", color = "Type of Tree") +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e"), labels = c("Fake Tree", "Real Tree")) +
  theme_bw()

# Correlation between Number of trees sold & Sales
ggplot(data, aes(x = Number.of.trees.sold, y = Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add line of best fit
  labs(title = "Number of Trees Sold vs. Sales Revenue", x = "Number of Trees Sold", y = "Sales Revenue") +
  theme_bw()

# Correlation between Average Tree Price & Sales
ggplot(data, aes(x = Average.Tree.Price, y = Sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add line of best fit
  labs(title = "Average.Tree.Price vs. Sales Revenue", x = "Average.Tree.Price", y = "Sales Revenue") +
  theme_bw()
correlation2 <- cor(data$Average.Tree.Price, data$Sales)

# Pie Chart
tree_subset <- subset(data, Type.of.tree %in% c("Fake tree", "Real tree"))
tree_proportions <- aggregate(Number.of.trees.sold ~ Year + Type.of.tree, tree_subset, sum)
tree_proportions <- transform(tree_proportions, Proportion = Number.of.trees.sold / sum(Number.of.trees.sold))

pie_charts <- lapply(unique(tree_proportions$Year), function(year) {
  subset_data <- subset(tree_proportions, Year == year)
  ggplot(subset_data, aes(x = "", y = Proportion, fill = Type.of.tree)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    labs(title = paste("Proportion of Fake and Real Trees - Year", year),
         fill = "Tree Type") +
    theme_minimal()
})
gridExtra::grid.arrange(grobs = pie_charts, ncol = 3)