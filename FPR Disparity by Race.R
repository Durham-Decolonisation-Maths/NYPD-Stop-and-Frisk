# 1. Load the pre-cleaned CSV extracted from auditblackbox
df <- read.csv("nypd_cpw_stops.csv")

# Ensure categorical predictors are treated as factors
df$precinct <- as.factor(df$precinct)

# 2. Train a simple transparent model (Logistic Regression)
# Predicts whether a weapon was found based on precinct & officer observations
model <- glm(
  found_weapon ~ precinct + stopped_bc_object + stopped_bc_bulge + time_of_day,
  data = df,
  family = binomial(link = "logit")
)

# 3. Audit predictions for False Positive Rate disparity across race
# type = "response" generates predicted probabilities P(y = 1)
df$prob_weapon <- predict(model, newdata = df, type = "response")

# ... students compute metric disparities and test threshold adjustments

# Set a default stop threshold
threshold <- 0.50
df$predicted_stop <- ifelse(df$prob_weapon >= threshold, 1, 0)

# Calculate FPR by race (Stops where NO weapon was found)
# FPR = (Predicted Stop & No Weapon) / Total No Weapon Stops
fpr_by_race <- aggregate(
  (predicted_stop == 1 & found_weapon == 0) ~ race, 
  data = df, 
  FUN = function(x) sum(x)
)

total_no_weapon <- aggregate(
  (found_weapon == 0) ~ race, 
  data = df, 
  FUN = function(x) sum(x)
)

# Combine into a quick summary table
fpr_table <- data.frame(
  Race = fpr_by_race$race,
  FPR = fpr_by_race[[2]] / total_no_weapon[[2]]
)

print(fpr_table)