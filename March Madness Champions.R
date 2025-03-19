
Title: "March Madness Champions"
Author: "Gavin Baskette"
Date: Sys.Date()


# ---- R Libraries ----

library(dplyr)
library(car)
library(lmtest)


# ---- DATA LOADING AND INITIAL PROCESSING ----

March_madness_champions <- read.csv("C:/Users/gavin/OneDrive/Desktop/Sport Models/March Madness Winners Clean.csv")
head(March_madness_champions)


# ---- DATA CLEANING: REMOVE NON-NUMERIC COLUMNS & HANDLE MISSING DATA ----

non_numeric_columns <- c("Team","Ken.Pom.Rank","Top.10.in.either.","Pre.Season.Rank", 
                         "HC.Tournament.Win.pct.","Seed.Prior.Year",
                         "Win.Conference.in.Regular.Season.and.or.Tournament.",
                         "Top.guard.Offensive.Rating","Luck.Rating", "Rank.in.Week.6.AP.Poll",
                         "Made.Conference.Tournament.Semifinals.", "Ken.Pom.AdjO","Kem.Pom.AdjD",
                         "Ken.Pom.AdjO.Rank","Ken.Pom.AdjD.Rank","Top.20.in.both.",
                         "Turnover.Differential","Natti.Winning.HC.","Final.Four.HC.")

numeric_data <- March_madness_champions[, !(names(March_madness_champions) %in% 
                                             non_numeric_columns)]

numeric_data <- numeric_data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

clean_numeric_data <- numeric_data[complete.cases(numeric_data), ]

clean_numeric_data <- clean_numeric_data[, -which(names(clean_numeric_data) == "Season")]


# ---- INITIAL LINEAR MODEL TO CHECK MULTICOLLINEARITY ----

clean_numeric_data$dummy_y <- rnorm(nrow(clean_numeric_data))
lm_model <- lm(dummy_y ~ ., data = clean_numeric_data)

print(names(clean_numeric_data))

predictors <- clean_numeric_data[, !names(clean_numeric_data) %in% "dummy_y"]
target <- clean_numeric_data$dummy_y


# ---- IDENTIFYING AND REMOVING HIGHLY CORRELATED VARIABLES ----

cor_matrix <- cor(predictors)
high_corr <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

variables_to_drop <- c("SRS.Rank", "Point.Differential.Rank", "Rank", "Seeding", "Win.pct..Vs..same.conference", 
                       "HC.Tournament.Wins","HC.Tournament.Experience","Second.half.win..")  # Use high_corr results
predictors_reduced <- predictors[, !(names(predictors) %in% variables_to_drop)]


# ---- UPDATED MODEL AFTER DROPPING HIGHLY CORRELATED VARIABLES ----

final_data <- cbind(predictors_reduced, dummy_y = target)

lm_model_final <- lm(dummy_y ~ ., data = final_data)


# ---- CHECKING FOR ALIASED VARIABLES ----

alias_results_final <- alias(lm_model_final)
print(alias_results_final)

alias(lm_model_final)$Complete

lm_model_final

final_data_filtered <- final_data

na_vars <- names(coef(lm_model_final))[is.na(coef(lm_model_final))]

final_data_filtered <- final_data_filtered[, !names(final_data_filtered) %in% na_vars]

lm_model_final <- lm(dummy_y ~ ., data = final_data_filtered)

summary(lm_model_final)

dim(final_data_filtered)

cor_matrix <- cor(final_data_filtered)
high_corr <- which(abs(cor_matrix) > 0.85 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

vars_to_remove <- unique(rownames(high_corr))

final_data_filtered <- final_data_filtered[, !(names(final_data_filtered) %in% vars_to_remove)]

dim(final_data_filtered)

lm_model_final <- lm(dummy_y ~ ., data = final_data_filtered)
summary(lm_model_final)


# ---- FINAL MODEL SELECTION & VIF ANALYSIS ----

vif_values_final <- vif(lm_model_final)
print(vif_values_final)


# ---- ADJUSTING WEIGHTS BASED ON VIF ----

# Original weights (z-scores from dataset)
weights <- c(0.776895382, 1.152380952, 6.395135437, 0.93448678, 
             1.401493638, 2.970351402, 2.330835808, 7.873799417, 
             5.220682549, 2.769824553, 1.232012387, 1.42128463, 
             1.897428298, 0.890970471, 2.193582686, 3.363281273, 
             4.763474574, 6.018636931, 7.571319083, 3.53667336,
             0.723546909, 0.769056306)

# Actual VIF values from earlier result
vif_values <- c(62.191502, 78.251768, 14.153381, 6.731069, 
                9.431756, 48.139689, 61.579294, 33.036847, 
                39.300123, 3.361933, 15.794122, 4.799045, 35.083294, 
                9.500346, 8.787856, 34.297635, 59.463069, 16.883497, 
                28.864419, 18.860275, 22.057141, 11.748226)

# Calculated adjusted weights based on VIF
adjusted_weights <- weights / sqrt(vif_values)

# Normalized adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Summary table to compare
results <- data.frame(
  Variable = c("Strength.of.Schedule..SOS.", "SOS.Rank", "Wins",
               "Wins.vs..ranked.teams", "Win.pct..Vs..ranked.teams",
               "Offensive.Efficiency.Rating..OER.", "Defensive.Effeciency.Rating..DER.", 
               "OER.Rank","DER.Rank", "Points.For.Rank", "Points.Against.Rank",
               "X..of.all.americans", "X..of.players.drafted",
               "Average.Starting.5.Tournament.Experience", "Wins.prior.year", 
               "Home.win.pct.", "Away.win.pct.", "Win.pct..Vs..other.conference",
               "Highest.rank.for.team", "Conference.Result",
               "Win...in.games.decided.by.5.or.less..or.OT", "Free.Throw.."),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

print(results)


# ---- FINAL SELECTION OF REMAINING VARIABLES & RUNNING VIF ANALYSIS ----

head(March_madness_champions)
head(predictors)

remaining_vars <- c("Simple.Rating.System..SRS.", "SRS.Rank", "Point.Differential", 
                    "Point.Differential.Rank", "Seeding", "Rank", "Second.half.win..", 
                    "HC.Tournament.Experience", "HC.Tournament.Wins", 
                    "Win.pct..Vs..same.conference", "X3.Point..", "Free.Throw.Attempts.per.game", 
                    "Conference.Win..", "Conference.SRS.Rank")

predictors_with_target <- cbind(predictors, dummy_y = target)

final_data_remaining <- predictors_with_target[, c(remaining_vars, "dummy_y")]

lm_remaining <- lm(dummy_y ~ ., data = final_data_remaining)

vif_values_remaining <- vif(lm_remaining)

print(vif_values_remaining)


# ---- ADJUSTING WEIGHTS BASED ON VIF FOR REMAINING VARIABLES ----

# Original weights (z-scores from dataset)
weights <- c(3.80150881, 7.526242723, 3.370403668, 6.969981782, 
             12.67479607, 8.017699912, 3.843683943, 1.261545633, 
             1.179504215, 3.647210013, 1.84845713, 0.905870221, 
             0.770735927, 0.597260628)

# Actual VIF values from earlier result
vif_values <- c(10.964062, 31.398975, 14.993087, 30.419597, 
                35.183976, 26.895914, 12.233242, 388.835364, 
                353.678975, 25.032860, 2.944837, 2.066410, 
                3.001535, 7.509006)


# Calculated adjusted weights based on VIF
adjusted_weights <- weights / sqrt(vif_values)

# Normalized adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Summary table to compare
results <- data.frame(
  Variable = c("Simple.Rating.System..SRS.", "SRS.Rank", 
               "Point.Differential", "Point.Differential.Rank", 
               "Seeding", "Rank", "Second.half.win..",
               "HC.Tournament.Experience", "HC.Tournament.Wins", 
               "Win.pct..Vs..same.conference","X3.Point..",
               "Free.Throw.Attempts.per.game", "Conference.Win..",
               "Conference.SRS.Rank"),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

print(results)



