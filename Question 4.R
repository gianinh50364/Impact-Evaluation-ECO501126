library(dplyr) # data manipulation
library(readr) # input/output
library(MatchIt) # PSM
library(Matching) # PSM
library(stargazer) # table exporter
library(survey) # subsampling design
library(plm) # regression model
library(ggplot2) # data visualization

# Import data
data <- read_csv('C:/Users/PC-Quang Bien/Desktop/Impact evaluation final/Q4/data_4.csv')

# Get basic information on data
data <- data %>% mutate_at(2:6,as.factor)
data %>% summary
glimpse(data)

# Remove missing values
data <- na.omit(data)
data <- mutate_all(data, as.numeric)

# Data setup
data <- mutate(data, ln_revenue = log(1+revenue)) # normalize revenue
data <- mutate(data, ln_profit = log(1+profit)) # normalize profit
data <- mutate(data, year = ifelse(year == 2011, 0, ifelse(year == 2015, 1, year))) # change 2011 to 0 and 2015 to 1 
data <- mutate(data, training1=ifelse(training == 1 & year == 1, 1, 0)) # create a column that seperate treatment group and control group in both year
data <- group_by(data,firm_id) %>%
  mutate(training2015 = max(training1)) # create a column for treatment group in 2015
data <- filter(data, year == 0)
data$X <- 1:nrow(data)
data$weight <- 1/nrow(data) # Add weight to each observation (assumes equal weights for all data points).

# First Regression (Unbalanced)
des1 <- svydesign(id = ~X,  weights = ~weight, data = data)
prog.lm <- svyglm(training2015 ~ edu + firm_size + age + ln_profit + ln_revenue,
                  design=des1, family = quasibinomial(link = "probit"))
X <- prog.lm$fitted
Tr <- data$training
m.out <- Match(Tr = Tr, X = X, caliper = 0.01)
MatchBalance(training2015 ~ edu + firm_size + age + ln_profit + ln_revenue, data = data, nboots = 1000)
summary(m.out)

#Graph density of propensity scores
fit <- prog.lm$data
fit$fvalues <- prog.lm$fitted.values
fit.control <- filter(fit, training == 0)
fit.treated <- filter(fit, training == 1)
ggplot() + 
      geom_density(aes(x=fit.control$fvalues, linetype = '2'), color = 'blue', size = 1) +
      geom_density(aes(x=fit.treated$fvalues, linetype = '3'), color = 'red', size = 1) +
      xlab("") +
      scale_linetype_discrete(name = "", labels = c("Control", "Treated")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

# Build data frame with propensity score and firm_id, then drop observation that not matched
ps_dropped <- m.out$index.dropped
ps_data <- data.frame(psm = prog.lm$fitted.values)
ps_data$firm_id <- prog.lm$data$firm_id
ps_data <- ps_data[-ps_dropped,]
rownames(ps_data) <- NULL

# Merge to original data frame by firm_id
data <- read_csv('C:/Users/PC-Quang Bien/Desktop/Impact evaluation final/Q4/data_4.csv')
psm_data <- right_join(data, ps_data, by = "firm_id")

# Re-estimate baseline model with matched data set
psm_data <- mutate(psm_data, ln_revenue = log(1+revenue))
psm_data <- mutate(psm_data, ln_profit = log(1+profit))
psm_data <- mutate(psm_data, year = ifelse(year == 2011, 0, ifelse(year == 2015, 1, year)))
psm_data <- mutate(psm_data, training1=ifelse(training == 1 & year == 1, 1, 0))
psm_data <- group_by(psm_data,firm_id) %>%
  mutate(training2015 = max(training1))
psm_data <- mutate(psm_data, trainingyr = training2015*year)
psm_data <- ungroup(psm_data)

# Re-estimate Basic Model
lm1 <- lm(ln_revenue ~ year + training2015 + trainingyr, data = psm_data)
summary(lm1)
stargazer(lm1, type="html", out="C:/Users/PC-Quang Bien/Desktop/Impact evaluation final/Q4/modelbasic1.html")
lm2 <- lm(ln_profit ~ year + training2015 + trainingyr, data = psm_data)
summary(lm2)
stargazer(lm2, type="html", out="C:/Users/PC-Quang Bien/Desktop/Impact evaluation final/Q4/modelbasic2.html")

# Create Analytical Weights
psm_data$a_weight <- 1
psm_data$a_weight <- ifelse(psm_data$training == 0, psm_data$psm/(1-psm_data$psm), 1)

# Re-estimate with analytical weights
lm1_w <- lm(ln_revenue ~ year + training2015 + trainingyr, data = psm_data, weights = a_weight)
summary(lm1_w)
stargazer(lm1_w, type="html", out="C:/Users/PC-Quang Bien/Desktop/Impact evaluation final/Q4/modelweight1.html")

lm2_w <- lm(ln_profit ~ year + training2015 + trainingyr, data = psm_data, weights = a_weight)
summary(lm2_w)
stargazer(lm2_w, type="html", out="C:/Users/PC-Quang Bien/Desktop/Impact evaluation final/Q4/modelweight2.html")

#clear R environment
rm(list = ls())
