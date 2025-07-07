

data <- read.csv("/Users/berkanakin/Library/CloudStorage/OneDrive-Personal/Studium/Master/UvA Psychology (research) Master/Courses/Thesis/Brute-Force-vs-Comprehension/data/analysis_data/analysis_data_long.csv")

invalids <- data[data$valid==F & !is.na(data$task_level),]

str(invalids)


# Step 1: Subset to experimental condition only
experimental_invalids <- subset(invalids, condition_type == "experimental")

# Step 2: Count invalids by task level
invalid_counts <- table(experimental_invalids$task_level)

# Step 3: Define total trials per task level
total_trials <- c("0" = 268, "1" = 65, "2" = 5)

# Step 4: Calculate percentage of invalids per level
percent_invalids <- (invalid_counts / total_trials[names(invalid_counts)]) * 100

# Step 5: Combine into a data frame for clarity
percent_df <- data.frame(
  task_level = as.integer(names(percent_invalids)),
  invalid_count = as.vector(invalid_counts),
  total_trials = total_trials[names(invalid_counts)],
  percent_invalid = round(percent_invalids, 2)
)

# Step 6: Run a chi-squared test
chisq_test <- chisq.test(
  x = as.integer(invalid_counts),
  p = as.numeric(total_trials[names(invalid_counts)]) / sum(total_trials)
)

print(chisq_test)



### check

check <- data[data$condition_type=="experimental" & !is.na(data$task_level),]
check2 <- data[data$task_level==0 & !is.na(data$task_level),]

# If you have the lme4 package:
library(lme4)


model <- glmer(valid ~ task_level + (1 + task_level | model), data = check, family = binomial)
model2_ri <- glmer(valid ~ condition_type + (1 | model), data = check2, family = binomial)
model2_rs <- glmer(valid ~ condition_type + (1 + condition_type | model), data = check2, family = binomial)
summary(model2_ri)      
anova(model2_ri, model2_rs)









