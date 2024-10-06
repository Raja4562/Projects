healthcare_data <- read.csv("healthcare_dataset.csv")
install.packages("tweenr")
library(dplyr)
library(ggplot2)
library(ggforce)
library(tidyr)

sankey_data <- healthcare_data %>%
  select(Medical.Condition, Insurance.Provider) %>%
  na.omit() %>%
  count(Medical.Condition, Insurance.Provider) %>%
  ungroup() %>%
  mutate(x = Medical.Condition, y = Insurance.Provider, value = n)


ggplot(sankey_data, aes(x = x, y = y, fill = as.factor(y), node_value = value)) +
  geom_sankey() +
  geom_sankey_label(aes(label = ifelse(x == "Start", "", ifelse(x == "End", "", as.character(value))), color = "black", hjust = ifelse(x == "Start", 1, 0)), size = 3) +
  theme_sankey() +
  labs(title = "Sankey Network: Medical Condition and Insurance Provider",
       x = "Medical Condition",
       y = "Insurance Provider")


chooseCRANmirror()
