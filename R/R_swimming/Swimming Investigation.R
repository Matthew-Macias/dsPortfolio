library(tidyverse)
library(ggpubr)
library(scales)

# Reading in the data
budget_data <- read.csv("", stringsAsFactors = FALSE)
athlete_data <- read.csv("", stringsAsFactors = FALSE)

##### Budget Allocation #####

# Grouping by every four years to see the spending in the olympic period
grouped_data <- budget_data %>%
  mutate(id = row_number()-1,
         group = id%/%4) %>%
  group_by(group) %>%
  summarise(Year = max(Year),
            Revenue = sum(Revenue),
            High_Performance = sum(High.Performance),
            Sport_Development = sum(Sport.Development),
            AIS_Grant = sum(AIS.Grant),
            Gold = sum(Gold),
            Total_Medals = sum(Total)) %>%
  select(-group)

# Checking to see if there is any correlation between variables that might be of interest
data_corr <- cor(grouped_data)
data_corr

# Plotting High Performance Budget and Gold Medal Tally to view the relationship in more depth

ggscatter(grouped_data, x = "High_Performance", y = "Gold", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Performance Team Spending", ylab = "Gold Medals") +
          scale_x_continuous(labels = scales::comma) + 
          ggtitle("How does performance spending influence gold medal outcomes?")


# Plotting High Performance Budget and Total Medal Tally to view the relationship in more depth

ggscatter(grouped_data, x = "High_Performance", y = "Total_Medals", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Performance Team Spending", ylab = "Total Medals") +
          scale_x_continuous(labels = scales::comma) + 
          ggtitle("How does performance spending influence total medal outcomes?")

# Plotting Sport Development Budget and Total Medal Tally to view the relationship in more depth

ggscatter(grouped_data, x = "Sport_Development", y = "Total_Medals", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Sport Development Spending", ylab = "Total Medals") +
  scale_x_continuous(labels = scales::comma) + 
  ggtitle("How does Sport Development spending influence total medal outcomes?")

##### Physiology #####
predictors <- athlete_data %>%
  select(-Medals, -Name, -Gender)

athletes <- athlete_data %>%
  select(Name, Gender)

response <- athlete_data %>%
  select(Medals)

# Checking the means of each predictor variable
apply(predictors, 2, mean)

# Checking the variance of each predictor variable
apply(predictors, 2, var)

# Running PCA
pcas <- prcomp(predictors, scale. = TRUE)

# Viewing the loadings
pcas$rotation

# plotting the first two PC
biplot(pcas, scale = 0)

# Calculating the variance explained by each PC
pca_var <- pcas$sdev^2
pca_var

# Calculating the % variance explained by each PC
pca_var/sum(pca_var)

# Combining the dataset back together for plotting
full_pcas <- cbind(athletes, pcas$x)
final_pcas <- cbind(full_pcas, response) %>%
  mutate(Group = ifelse(Medals > 0, 1, 0))

# plotting for any noticeable clustering
ggplot(data=final_pcas) +
  geom_point(aes(x = PC2, y = PC1, color = Medals), size = 6) + geom_text(aes(x = PC2, y = PC1+0.1, label = Name)) +
  ggtitle("Medals Clustering")

# Plotting against Gender
ggplot(data=final_pcas) +
  geom_point(aes(x = PC2, y = PC1, color = Gender), size = 6) + 
  geom_text(aes(x = PC2, y = PC1+0.1, label = Name)) + 
  ggtitle("Gender Clustering")

# kMeans clustering

kms <- kmeans(predictors, 2, nstart = 20)

# Viewing cluster assignments
kms$cluster

full_kms <- cbind(athletes, kms$cluster)
final_kms <- cbind(full_kms, pcas$x)

# Plotting against Gender
ggplot(data=final_kms) +
  geom_point(aes(x = PC2, y = PC1, color = kms$cluster), size = 6) + 
  geom_text(aes(x = PC2, y = PC1+0.1, label = Name)) + 
  ggtitle("kMeans Clustering")

# Gender Analysis
female_swim <- final_pcas %>%
  filter(Gender == "Female")

ggplot(data=female_swim) +
  geom_point(aes(x = PC2, y = PC1, color = Group), size = 6) + 
  geom_text(aes(x = PC2, y = PC1+0.1, label = Name)) + 
  ggtitle("Female Clustering")

male_swim <- final_pcas %>%
  filter(Gender == "Male")

ggplot(data=male_swim) +
  geom_point(aes(x = PC2, y = PC1, color = Group), size = 6) + 
  geom_text(aes(x = PC2, y = PC1+0.1, label = Name)) + 
  ggtitle("Male Clustering")
