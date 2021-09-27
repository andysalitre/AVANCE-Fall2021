library(ggplot2)
library(dplyr)
setwd("D:/Class/Fall21/DSCI 435/Data/")

# Import combined Intake dataset
intake <- read.csv("CombinedIntake.csv")

# Create data frame specific for graduation status by year
intake_status <- table(intake$Status, intake$END.YEAR) %>%
  data.frame()

# Reformat data frame for specific variable names and variable types
colnames(intake_status) <- c("Status", "Year", "Count")
intake_status$Status <- as.character(intake_status$Status)
intake_status$Year <- as.character(intake_status$Year)

# Plot histogram of particpant program status by year
ggplot(intake_status) +
  geom_col(aes(reorder(Status, Count), Count, fill = I("red"))) +
  coord_flip() +
  ggtitle("Histogram of Particpant Program Status, by Year") +
  xlab("Status") +
  facet_wrap(~Year)

# Create dataset specific for birth years of parents by graduation status
intake_status2 <- table(intake$Birth.Year, intake$Status) %>%
  data.frame() %>%
  arrange(-Birth.Year)

# Reformat data frame for specific variable names and variable types
colnames(intake_status2) <- c("Birth Year", "Status", "Count")
intake_status2$`Birth Year` <- as.character(intake_status2$`Birth Year`) %>%
  as.numeric()

# Plot histogram of parent particpant age
ggplot(filter(intake_status2, `Birth Year` < 2016)) +
  geom_col(aes(2021 - `Birth Year`, Count, fill = Status)) +
  ggtitle("Histogram of Parent Particpant Age") +
  scale_fill_brewer(direction = -1, palette = "Set3") +
  xlab("Age")