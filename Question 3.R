library(dplyr) # data manipulation
library(data.table) # data manipulation
library(readr) # input/output
library(MatchIt) # PSM
library(cobalt) # calculate SMD
library(stargazer) # table exporter

# Import data
data <- read_csv('C:/Users/PC-Quang Bien/Desktop/Impact evaluation final/Q3/data_3.csv')

# Get data structure
data <- data %>% mutate_at(2:6,as.factor)
data %>% summary
glimpse(data)

# Remove missing values and change all data types to numeric
data <- na.omit(data)
data <- mutate_all(data, as.numeric)

# Normalize health expenditure
data <- mutate(data, ln_exptot = log(1+exptot_health))

# Perform propensity matching score with caliper 0.1 to avoid unbalance covariate
m.out <- matchit(insurance ~ famsize + income + agehead + educhead + ln_exptot,
                 data = data, method = "nearest", distance = "glm", caliper=0.1, ratio=4)
summary(m.out)

# Plotting SMD
love.plot(bal.tab(m.out,m.threshold=0.1),
          stat="mean.diffs",
          grid=TRUE,
          stars="raw",
          abs=F)

# Rename distance to dist to avoid replicated name when extract matched data in PSM
data <- data %>% 
  rename("dist" = "distance")

# Extract only matched data in PSM
mdata <- match.data(m.out)


# Estimate the treatment effect after matching (PSM)
model <- glm(lexptot ~ famsize + income + agehead + educhead + insurance, data = mdata)
print(summary(model))

# Clear the environment
rm(list = ls())
