---
title: "Bonheur_Mugisha_101400"
output:
  pdf_document: default
  html_document: default
date: "2025-10-11"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
getwd()
list.files()
```
```{r}
## loading the data into R
CO2_emission_data <- read.csv("CO2_emission.csv", header = TRUE)
world_population_data <- read.csv("world_population.csv", header = TRUE)

##4.2 Exploratory data analysis
##display variable names of the dataset.
names(world_population_data)
```
```{r}
##display variable 5 on head of the dataset.
head(world_population_data,5)
```
```{r}
##display variable 10 on tail of the dataset. 
tail(world_population_data,10)
```
```{r}
##display the data type of the dataset.
str(world_population_data)
```


```{r}
##display  the shape of the dataset.
dim(world_population_data)
```
```{r}
#Drop duplicate rows if any
duplicated(world_population_data)
world_population_data <- world_population_data[!duplicated(world_population_data), ]
```
```{r}
##find the number of missing values in each column
colSums(is.na(world_population_data))
```
```{r}
##use a box plot to check if there are any outliers in the quantitative variables.
# Select numeric columns
numeric_data <- world_population_data[sapply(world_population_data, is.numeric)]
# Boxplot
boxplot(numeric_data, main = "Boxplot of Quantitative Variables", col = "lightblue", las = 2)
```
```{r}
##Use appropriate methods to deal with missing values and outliers
for (c in names(world_population_data)) {
  if (is.numeric(world_population_data[[c]])) {
    world_population_data[[c]][is.na(world_population_data[[c]])] <- mean(world_population_data[[c]], na.rm = TRUE)
  }
}
```

```{r}
library(dplyr)
```
```{r}
## 4.3 Generating newVariable by using World Population Dataset

world_population_data <- world_population_data %>%
  mutate(Population_2030 = X2020.Population * exp(Growth.Rate * (2030-2022)))
head(world_population_data)
```


```{r}
## view data of 2030
world_population_data[, c("Country.Territory", "X2022.Population", "Population_2030")]
head(world_population_data[, c("Country.Territory", "X2022.Population", "Growth.Rate", "Population_2030")])
```
```{r}
world_population_sorted_2030 <- world_population_data %>%
  arrange(desc(Population_2030))
world_population_sorted_2030[, c("Country.Territory", "X2022.Population", "Population_2030")]
```
```{r}
## 4.4 Value extraction and plot
library(ggplot2)
```

```{r}
##Use an appropriate graph to present top 10 most populous counties and their population number during 2022.
world_population_data_2022 <- world_population_data %>%
  arrange(desc(X2022.Population)) %>%
  slice(1:10)
ggplot(world_population_data_2022, aes(x = reorder(Country.Territory, X2022.Population),                        y = X2022.Population)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Top 10 Most Populous Countries in 2022",
       x = "Country",
       y = "Population") +
  theme_minimal()
```
```{r}
##Show the trend in their population number since 1990-2022 by using appropriate graph.
library(tidyr)
top10_countries <- world_population_data %>%
  arrange(desc(X2022.Population)) %>%
  slice(1:10) %>%
  select(Country.Territory, starts_with("X"))
top10_long <- top10_countries %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Population"
  )
top10_long <- top10_long %>%
  mutate(Year = as.integer(gsub("X|\\.Population", "", Year)))
ggplot(top10_long, aes(x = Year, y = Population, color = Country.Territory)) +
  geom_line(size = 1) +
  labs(title = "Population Trend of Top 10 Most Populous Countries (1990-2022)",
       x = "Year",
       y = "Population",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")

```
```{r}
##filling the missing values
for (c in names(CO2_emission_data)) {
  if (is.numeric(CO2_emission_data[[c]])) {
    CO2_emission_data[[c]][is.na(CO2_emission_data[[c]])] <- mean(CO2_emission_data[[c]], na.rm = TRUE)
  }
}
colSums(is.na(CO2_emission_data))
head(CO2_emission_data)
```


```{r}
##Use CO2 Emissions Around the World dataset to extract emission of 10 most populous countries. Use an appropriate graph to show their emission trend since 1990-2019.
# Step 1: Identify top 10 most populous countries
top10_countries <- world_population_data %>%
  arrange(desc(X2022.Population)) %>%
  slice(1:10) %>%
  pull(Country.Territory)

# Step 2: Filter CO2 dataset for these countries
co2_top10 <- CO2_emission_data %>%
  filter(Country.Name %in% top10_countries) %>%
  select(Country.Name, starts_with("X")) %>%   
  select(-X2019.1)                             

# Step 3: Reshape from wide to long format
co2_top10_long <- co2_top10 %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "CO2_Emissions"
  ) %>%
  mutate(
    Year = as.integer(gsub("X", "", Year)),   
    CO2_Emissions = as.numeric(CO2_Emissions) 
  ) %>%
  filter(!is.na(CO2_Emissions))               

# Step 4: Plot CO2 emission trends
ggplot(co2_top10_long, aes(x = Year, y = CO2_Emissions, color = Country.Name, group = Country.Name)) +
  geom_line(size = 1) +
  labs(title = "CO₂ Emissions Trend (1990–2019) for Top 10 Most Populous Countries",
       x = "Year",
       y = "CO₂ Emissions (Metric Tons per Capita)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")
```
```{r}
##4.5 Correlation Analysis
library(reshape2)
colnames(world_population_data)
```
```{r}
## Use the population dataset to find correlations between Area , Density ,Growth rate, and World Population Percentage by using both numerical values and heatmap.

pop_vars <- world_population_data %>%
  select(`Area..km..`, `Density..per.km..`, Growth.Rate, World.Population.Percentage) %>%
  mutate(
    Area = as.numeric(`Area..km..`),
    Density = as.numeric(`Density..per.km..`),
    Growth.Rate = as.numeric(Growth.Rate),
    World.Pop.Percentage = as.numeric(World.Population.Percentage)
  ) %>%
  select(Area, Density, Growth.Rate, World.Pop.Percentage)

# Step 2: Compute correlation matrix
cor_matrix <- cor(pop_vars, use = "complete.obs")  # ignores NAs
print(cor_matrix)
cor_melt <- melt(cor_matrix)

# Step 5: Plot heatmap
ggplot(cor_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name = "Correlation") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  theme_minimal() +
  labs(title = "Correlation Heatmap of Population Variables",
       x = "",
       y = "")
```
```{r}
pop_data <- world_population_data %>%
  select(Country.Territory, X2022.Population)

co2_data <- CO2_emission_data %>%
  select(Country.Name, X2019) %>%
  rename(CO2_2019 = X2019)

# Step 2: Merge datasets on country name
merged_data <- pop_data %>%
  inner_join(co2_data, by = c("Country.Territory" = "Country.Name"))

# Optional: Remove rows with missing data
merged_data <- merged_data %>%
  filter(!is.na(X2022.Population), !is.na(CO2_2019))
summary(merged_data)
cor(merged_data$X2022.Population, merged_data$CO2_2019, use = "complete.obs")
```
```{r}
ggplot(merged_data, aes(x = X2022.Population, y = CO2_2019)) +
  geom_point(color = "blue", alpha = 0.6) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Relationship Between Population and CO₂ Emissions (2019)",
       x = "Population in 2022",
       y = "CO₂ Emissions in 2019") +
  theme_minimal()
```
```{r}
# Linear regression
model <- lm(CO2_2019 ~ X2022.Population, data = merged_data)
summary(model)

# Add regression line to plot
ggplot(merged_data, aes(x = X2022.Population, y = CO2_2019)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Population vs CO₂ Emissions with Regression Line",
       x = "Population in 2022",
       y = "CO₂ Emissions in 2019") +
  theme_minimal()

```


```{r}
```


```{r}
ggplot(merged_data, aes(x = X2022.Population, y = CO2_2019)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Log-Log Plot: Population vs CO₂ Emissions",
       x = "Population in 2022 (log scale)",
       y = "CO₂ Emissions in 2019 (log scale)") +
  theme_minimal()
```


```{r}
##Comparing CO2 Emissions in continents
merged_data2 <- world_population_data %>%
  select(Country.Territory, Continent, X2022.Population) %>%  # keep Continent
  inner_join(CO2_emission_data %>%
               select(Country.Name, X2019) %>%
               rename(CO2_2019 = X2019,
                      Country.Territory = Country.Name),
             by = "Country.Territory")
co2_by_continent <- merged_data2 %>%
  group_by(Continent) %>%
  summarise(Total_CO2_2019 = sum(CO2_2019, na.rm = TRUE)) %>%
  arrange(desc(Total_CO2_2019))
```

```{r}
ggplot(co2_by_continent, aes(x = reorder(Continent, Total_CO2_2019), 
                             y = Total_CO2_2019, fill = Continent)) +
  geom_bar(stat = "identity") +
  coord_flip() +   # Horizontal bars for readability
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Total 2019 CO₂ Emissions by Continent",
       x = "Continent",
       y = "Total CO₂ Emissions (metric tons)") +
  theme_minimal() +
  theme(legend.position = "none")
```
```{r}
# First continent (highest emissions)
first_continent <- co2_by_continent[1, ]

# Third continent
third_continent <- co2_by_continent[3, ]

# Last continent (lowest emissions)
last_continent <- co2_by_continent[nrow(co2_by_continent), ]

# Print results
first_continent
third_continenta
last_continent
```
#Project I: World Population Trend and CO2 Emission
#Abstract
This work is about investigating the world population trend and CO2 emission across countries in their content using data set that found on kaggle.com as World Population Dataset and CO₂ Emissions Around the World Dataset.  On merging population trend and CO2 emission dataset, we seen a pattern of relationships between population number and CO2 emissions,
This study investigates the relationship between population size and CO2 emissions across countries and continents using the World Population Dataset and CO2 Emissions Around the World Dataset. By merging population and emissions data, we explored patterns in CO₂ emissions relative to population size for 2019 and 2022. We working on statistical analysis, correlation and visualizations, like scatter plots, log-log regression, and bar charts. The result show that countries with tend to produce higher total CO2 emission, while population alone does not total explain emission levels due to different in industrialization and energy use. By this result show that influence of industrialization on CO2 emissions more than population size alone and we can conclude that emission control must consider industrial activity.

#Introduction
Climate change is the one of the most global challenges, CO2 emissions is one of the key driver. Population growth has been linked to higher CO2emission cause, in this work we are trying to compare the population trend and CO2 emission in years to if their no other fact that can be considered like industrialization, energy use or technology.

#Methods

#Data Sources
1.World Population Data (2022): Contains 234 countries with variables such as 
2.CO2 Emissions Data (2019): Contains 215 countries with CO2 emissions recorded from 1990 to 2019.

#Data Processing:
1.Selected relevant columns: population for 2022, CO2 emissions for 2019, and continent.
2.Merged datasets by country name.
3.Aggregated CO2 emissions at continental level.

#Results and Findings
#Continent-Level CO₂ Emissions
From the aggregated data:Rank	Continent	Total 2019 CO2 Emissions (metric tons per capital)
1	Europe	Highest
2	Asia	Second highest
3	North America	Third highest
4	South America	Moderate
5	Africa	Lower
6	Oceania	Lowest
#Discussion
Opposing to common global statistics show that Asia leads due to large industrialized nations like China and India, this dataset indicates Europe as the top CO2 emitter. This likely shows missing or mismatched records for major Asian countries during data merging. Nonetheless, the result still underscores a clear trend: continents with higher levels of industrialization and energy consumption emit more CO2 per capita than less industrialized ones.
The weak statistical relationship between population and CO2 emissions suggests that emissions are more influenced by economic structure, technology use, and energy efficiency than by the sheer number of people.

#Conclusion
This dataset analysis demonstrates that in Europe records the highest total CO2 emissions, while Oceania emits the least. The weak correlation between population and CO2 emissions shows the role of industrialization and development level rather than population size in influencing emissions. To successfully address climate change, we should target cleaner industrialization. 
