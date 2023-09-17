remove.packages("survival")
install.packages("survival") 
install.packages("psych")
install.packages('ModelMetrics')
install.packages("survC1")
install.packages('discSurv')
install.packages('mice')
install.packages("gridExtra")
library(survival)
library(psych)
library(survminer)
library(ggplot2)
library(gridExtra)
library(mice)
library(survC1)


################# Loading the dataset ##################################

hf_data <- read.csv("D:/Downloads/Msc Dissertation/Dissertation Data/heart_failure_clinical_records_dataset.csv")
describe(hf_data)
# Creating a survival object and plotting the empirical survival fn

s <- Surv(hf_data$time)

model<- survfit(s~1)
summary(model)
plot(model)


plot(model,
     ylab = "empirical survival function",
     xlab = "time to event (t)",
     conf.int = FALSE,
     col = "blue")








######################### Creating KM Plots #################################


### KM plots for the entire dataset
par(mfrow=c(1,1))
hf.sv <- Surv(hf_data$time, hf_data$DEATH_EVENT)

km_fit <- survfit(hf.sv ~ 1, data = hf_data)
ggsurvplot(km_fit, main = "Kaplan-Meier Survival Curve",
     xlab = "Days", ylab = "Survival Probability", ggtheme = theme_bw())
grid()
# Getting the survival probability of someone on the last recorded day of follow-up
summary(km_fit, times = max(km_fit$time))


### Creating KM plots for the categorical features

par(mfrow=c(3,2))

hf.sv <- Surv(hf_data$time, hf_data$DEATH_EVENT, type = "right")

## KM plot based on sex

hf_data$sex = as.factor(hf_data$sex)

hfSurv <- survfit(hf.sv ~ hf_data$sex, data=hf_data)

summary(hfSurv)

category_counts <- table(hf_data$sex)
legend_labels <- paste(c("Female","Male"), " (n =", category_counts, ")")

# Plot the KM estimate
plot(hfSurv, 
     col =  c("blue", "red"), lwd = 2, 
     ylab = "Survival Probability", 
     xlab = "Days",
     main = "sex")
legend("bottomleft", legend = legend_labels, col = c("blue", "red"), lty = 1)
grid()

# Extract the survival probabilities at the end of the follow-up period
surv_prob_end <- summary(hfSurv)$surv[nrow(summary(hfSurv))]

# Display the survival probabilities
for (i in 1:length(legend_labels)) {
  cat(legend_labels[i], ": ", surv_prob_end[i], "\n")
}

summary(hfSurv, times = 275)

## KM plot based on smoking

hf_data$smoking = as.factor(hf_data$smoking)

hfSurv <- survfit(hf.sv ~ hf_data$smoking, data=hf_data)

category_counts <- table(hf_data$smoking)
legend_labels <- paste(c("Non-Smoker","Smoker"), " (n =", category_counts, ")")

# Plot the KM estimate
plot(hfSurv, 
     col =  c("blue", "red"), lwd = 2, 
     ylab = "Survival Probability", 
     xlab = "Days",
     main = "smoking")
legend("bottomleft", legend = legend_labels, col = c("blue", "red"), lty = 1)
grid()

## Creating KM plot based on diabetes

hf_data$diabetes = as.factor(hf_data$diabetes)

# Creating category_counts
category_counts <- table(hf_data$diabetes)

# Create the legend labels with category names and patient counts
legend_labels <- paste(c("Non-Diabetic", "Diabetic"), " (n =", category_counts, ")")

# Create a survival object
surv_obj <- Surv(time = hf_data$time, event = hf_data$DEATH_EVENT)

# Fit Kaplan-Meier survival curves by diabetes category
km_fit <- survfit(surv_obj ~ hf_data$diabetes)

# Plot Kaplan-Meier survival curves for each category separately
plot(km_fit,lwd=2, col = c("blue", "red"), main = "diabetes",
     xlab = "Days", ylab = "Survival Probability")
legend("bottomleft", legend = legend_labels, col = c("blue", "red"),
       lty = 1)
grid()

ggsurvplot(km_fit, data=hf_data, xlab = "Days", ylab = "Survival Probability", ggtheme = theme_bw(),risk.table.pos = "in",  
           ,palette = c(),legend.title = "",
           title = "Diabetes",
           legend=c(0.1,0.2))

## KM plot based on high_blood_pressure

hf_data$high_blood_pressure = as.factor(hf_data$high_blood_pressure)

hfSurv <- survfit(hf.sv ~ hf_data$high_blood_pressure, data=hf_data)

category_counts <- table(hf_data$high_blood_pressure)
legend_labels <- paste(c("Non high BP","High BP"), " (n =", category_counts, ")")

# Plot the KM estimate
plot(hfSurv, 
     col =  c("blue", "red"), lwd = 2, 
     ylab = "Survival Probability", 
     xlab = "Days",
     main = "high blood pressure")
legend("bottomleft", legend = legend_labels, col = c("blue", "red"), lty = 1)
grid()

## KM plot based on anaemia

hf_data$anaemia = as.factor(hf_data$anaemia)

hfSurv <- survfit(hf.sv ~ hf_data$anaemia, data=hf_data)

category_counts <- table(hf_data$anaemia)
legend_labels <- paste(c("Non-anaemic","Anaemic"), " (n =", category_counts, ")")

# Plot the KM estimate
plot(hfSurv, 
     col =  c("blue", "red"), lwd = 2, 
     ylab = "Survival Probability", 
     xlab = "Days",
     main = "anaemia")
legend("bottomleft", legend = legend_labels, col = c("blue", "red"), )
grid()



##############Creating KM plots for numerical features #####################

par(mfrow=c(3,2))

## Creating KM plot based on age

# Calculate quantile breakpoints
quantile_breaks <- quantile(hf_data$age, probs = c(0, 1/3, 2/3, 1))

# Calculate starting and ending numbers of each interval
interval_labels <- paste0(quantile_breaks[-4], " - ", quantile_breaks[-1])

# Divide dataset into categories based on quantiles with interval labels
age_category <- cut(hf_data$age, breaks = quantile_breaks, labels = interval_labels)

category_counts <- table(age_category)

# Create the legend labels with category names and patient counts
legend_labels <- paste(levels(age_category), " (n =", category_counts, ")")

# Create a survival object
surv_obj <- Surv(time = hf_data$time, event = hf_data$DEATH_EVENT)

# Fit Kaplan-Meier survival curves by age category
km_fit <- survfit(surv_obj ~ age_category)

# Plot Kaplan-Meier survival curves for each category separately
plot(km_fit, col = c("blue", "green", "red"), main = "age",
     xlab = "Days", ylab = "Survival Probability")
legend("bottomleft", legend = legend_labels, col = c("blue", "green", "red"),
       lty = 1)
grid()


## Creating KM plot for creatine phosphokinase

# Calculate quantile breakpoints
quantile_breaks <- quantile(hf_data$creatinine_phosphokinase, probs = c(0, 1/3, 2/3, 1))

quantile_breaks <- round(quantile_breaks, 2)

# Calculate starting and ending numbers of each interval
interval_labels <- paste0(quantile_breaks[-4], " - ", quantile_breaks[-1])

# Divide dataset into categories based on quantiles with interval labels
cpk_category <- cut(hf_data$creatinine_phosphokinase, breaks = quantile_breaks, labels = interval_labels)

category_counts <- table(cpk_category)

# Create the legend labels with category names and patient counts
legend_labels <- paste(levels(cpk_category), " (n =", category_counts, ")")

# Create a survival object
surv_obj <- Surv(time = hf_data$time, event = hf_data$DEATH_EVENT)

km_fit <- survfit(surv_obj ~ cpk_category)

# Plot Kaplan-Meier survival curves for each category separately
plot(km_fit, col = c("blue", "green", "red"), main = "creatine phosphokinase",
     xlab = "Days", ylab = "Survival Probability")
legend("bottomleft", legend = legend_labels, col = c("blue", "green", "red"),
       lty = 1:length(levels(cpk_category)))
grid()

## Creating KM based on ejection fraction


# Calculate quantile breakpoints
quantile_breaks <- quantile(hf_data$ejection_fraction, probs = c(0, 1/3, 2/3, 1))

# Calculate starting and ending numbers of each interval
interval_labels <- paste0(quantile_breaks[-4], " - ", quantile_breaks[-1])

# Divide dataset into categories based on quantiles with interval labels
ef_category <- cut(hf_data$ejection_fraction, breaks = quantile_breaks, labels = interval_labels)

category_counts <- table(ef_category)

# Create the legend labels with category names and patient counts
legend_labels <- paste(levels(ef_category), " (n =", category_counts, ")")

# Create a survival object
surv_obj <- Surv(time = hf_data$time, event = hf_data$DEATH_EVENT)

km_fit <- survfit(surv_obj ~ ef_category)

# Plot Kaplan-Meier survival curves for each category separately
plot(km_fit, col = c("blue", "green", "red"), main = "ejection fraction",
     xlab = "Days", ylab = "Survival Probability")
legend("bottomleft", legend = legend_labels, col = c("blue", "green", "red"),
       lty = 1:length(levels(ef_category)))
grid()


## Creating KM plot for platelets

# Calculate quantile breakpoints
quantile_breaks <- quantile(hf_data$platelets, probs = c(0, 1/3, 2/3, 1))

quantile_breaks <- round(quantile_breaks, 2)

# Calculate starting and ending numbers of each interval
interval_labels <- paste0(quantile_breaks[-4], " - ", quantile_breaks[-1])

# Divide dataset into categories based on quantiles with interval labels
platelets_category <- cut(hf_data$platelets, breaks = quantile_breaks, labels = interval_labels)

category_counts <- table(platelets_category)

# Create the legend labels with category names and patient counts
legend_labels <- paste(levels(platelets_category), " (n =", category_counts, ")")

# Create a survival object
surv_obj <- Surv(time = hf_data$time, event = hf_data$DEATH_EVENT)

# Fit Kaplan-Meier survival curves by age category
km_fit <- survfit(surv_obj ~ platelets_category)

# Plot Kaplan-Meier survival curves for each category separately
plot(km_fit, col = c("blue", "green", "red"), main = "platelets",
     xlab = "Days", ylab = "Survival Probability")
legend("bottomleft", legend = legend_labels, col = c("blue", "green", "red"),
       lty = 1:length(levels(platelets_category)))
grid()


## Creating KM plot for serum creatinine

# Calculate quantile breakpoints
quantile_breaks <- quantile(hf_data$serum_creatinine, probs = c(0, 1/3, 2/3, 1))

# Calculate starting and ending numbers of each interval
interval_labels <- paste0(quantile_breaks[-4], " - ", quantile_breaks[-1])

# Divide dataset into categories based on quantiles with interval labels
sc_category <- cut(hf_data$serum_creatinine, breaks = quantile_breaks, labels = interval_labels)

category_counts <- table(sc_category)

# Create the legend labels with category names and patient counts
legend_labels <- paste(levels(sc_category), " (n =", category_counts, ")")

# Create a survival object
surv_obj <- Surv(time = hf_data$time, event = hf_data$DEATH_EVENT)

km_fit <- survfit(surv_obj ~ sc_category)

# Plot Kaplan-Meier survival curves for each category separately
plot(km_fit, col = c("blue", "green", "red"), main = "serum creatinine",
     xlab = "Days", ylab = "Survival Probability")
legend("bottomleft", legend = legend_labels, col = c("blue", "green", "red"),
       lty = 1:length(levels(sc_category)))

grid()


## Creating KM plot for serum sodium

# Calculate quantile breakpoints
quantile_breaks <- quantile(hf_data$serum_sodium, probs = c(0, 1/3, 2/3, 1))

# Calculate starting and ending numbers of each interval
interval_labels <- paste0(quantile_breaks[-4], " - ", quantile_breaks[-1])

# Divide dataset into categories based on quantiles with interval labels
ss_category <- cut(hf_data$serum_sodium, breaks = quantile_breaks, labels = interval_labels)

category_counts <- table(ss_category)

# Create the legend labels with category names and patient counts
legend_labels <- paste(levels(ss_category), " (n =", category_counts, ")")

# Create a survival object
surv_obj <- Surv(time = hf_data$time, event = hf_data$DEATH_EVENT)

km_fit <- survfit(surv_obj ~ ss_category)

# Plot Kaplan-Meier survival curves for each category separately
plot(km_fit, col = c("blue", "green", "red"), main = "serum sodium",
     xlab = "Days", ylab = "Survival Probability")
legend("bottomleft", legend = legend_labels, col = c("blue", "green", "red"),
       lty = 1:length(levels(ss_category)))
grid()


#####################Performing Log-rank test ######################

#1. Age

result <- survdiff(Surv(hf_data$time, hf_data$DEATH_EVENT) ~ age_category)
print(result)

#2. Anaemia

result <- survdiff(Surv(time, DEATH_EVENT) ~ anaemia, data = hf_data)
print(result)


#3 Creatinine phosphokinase

result <- survdiff(Surv(hf_data$time, hf_data$DEATH_EVENT) ~ cpk_category)
print(result)

#4 Diabetes

result <- survdiff(Surv(time, DEATH_EVENT) ~ diabetes, data = hf_data)
print(result)

#5 Ejection fraction

result <- survdiff(Surv(hf_data$time, hf_data$DEATH_EVENT) ~ ef_category)
print(result)

#6 High blood pressure

result <- survdiff(Surv(time, DEATH_EVENT) ~ high_blood_pressure, data = hf_data)
print(result)

#7 Platelets
result <- survdiff(Surv(hf_data$time, hf_data$DEATH_EVENT) ~ platelets_category)
print(result)

#8 Serum creatinine
result <- survdiff(Surv(hf_data$time, hf_data$DEATH_EVENT) ~ sc_category)
print(result)

#9 Serum sodium
result <- survdiff(Surv(hf_data$time, hf_data$DEATH_EVENT) ~ ss_category)
print(result)

#10 Sex

result <- survdiff(Surv(time, DEATH_EVENT) ~ sex, data = hf_data)
print(result)

#11 Smoking

result <- survdiff(Surv(time, DEATH_EVENT) ~ smoking, data = hf_data)
print(result)



######################### Creating Cox regression model #########################



cox_model <- coxph(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking, data = hf_data)

# Print the Cox model summary
summary(cox_model)

print(concordance(cox_model))

conc(X = test_data$time, D = test_data$DEATH_EVENT, W=1,  R=predicted_risk)

?conc

# Checking the PH assumption
test.ph = cox.zph(cox_model)
test.ph

ggcoxzph(test.ph)

martingale_resid <- residuals(cox_model, type = "martingale")
martingale_resid

plot(hf_data$age, martingale_resid, xlab = "Predictor1", ylab = "Martingale Residuals")


########################## Creating AFT model ###########################################

### 1.Fit the Weibull AFT model
aft_model_weibull <- survreg(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking, dist = "weibull", data = hf_data)

# View the summary of the AFT model
summary(aft_model_weibull)



# 2. Fit the Exponential AFT model
aft_model_exp <- survreg(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking, dist = "exp", data = hf_data)

# View the summary of the AFT model
summary(aft_model_exp)

print(concordance(aft_model_exp))

# 3. Fit the log-normal AFT model
aft_model_logn <- survreg(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking, dist = "logn", data = hf_data)

# View the summary of the AFT model
summary(aft_model_logn)


# 4. Fit the log-logistic AFT model
aft_model_logl <- survreg(Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex + smoking, dist = "logl", data = hf_data)

# View the summary of the AFT model
summary(aft_model_logl)


# View the summary of the AFT model
summary(aft_model_exp)

AIC(aft_model_weibull, aft_model_exp, aft_model_logl, aft_model_logn)

BIC(aft_model_weibull, aft_model_exp, aft_model_logl, aft_model_logn)


anova(aft_model_weibull, aft_model_exp)


# Predicting on the test dataset using the exponential AFT model
predicted_survival <- predict(aft_model_exp, newdata = test_data, type = "response", times=test_data$time)
print(predicted_survival)
length(aft_model_exp$linear.predictors)

survival_probabilities <- pexp(predicted_survival, rate = 1 / exp(aft_model_exp$linear.predictors))
print(survival_probabilities)


actual_survival = Surv(time=test_data$time, event=test_data$DEATH_EVENT)
print(actual_survival)

c_index <- survConcordance(actual_survival, -predicted_survival)
print(paste("Concordance Index:", c_index))


mae <- mean(abs(predicted_survival - test_data$time))

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predicted_survival - test_data$time)^2))

# Print the results
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Create actual and predicted survival objects
actual_surv_obj <- Surv(test_data$time, test_data$DEATH_EVENT)
predicted_surv_obj <- Surv(predicted_survival, rep(1, length(predicted_survival)))

# Combine actual and predicted survival data into a data frame
survival_data <- data.frame(actual = actual_surv_obj, predicted = predicted_surv_obj)

# Fit Kaplan-Meier survival curves
km_actual <- survfit(actual ~ 1, data = survival_data)
km_predicted <- survfit(predicted ~ 1, data = survival_data)


par(mfrow=c(1,1))
# Plot Kaplan-Meier curves
plot(km_actual, col = "blue", main = "Kaplan-Meier Survival Curves")
lines(km_predicted, col = "red")
legend("bottomleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)


################ Nelson-Aalen Estimator #########################


## Nelson Aalen estimator for the whole dataset


nelsonaalen(hf_data, time, DEATH_EVENT)
plot(nelsonaalen(hf_data, time, DEATH_EVENT))

plot(nelsonaalen(hf_data, time, DEATH_EVENT), xlab = "Time", ylab = "Cumulative Hazard", main = "Nelson-Aalen Estimator", col = "blue", type="l")


fit = survfit(Surv(time, DEATH_EVENT) ~ 1, data = hf_data)
ggsurvplot(fit,
        #   conf.int = FALSE,
           risk.table.col = "", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
       #    palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")

print(fit[['cumhaz']])

## Plotting Nelson Aalen Estimator for the categorical features #############

par(mfrow = c(3, 2))
individual_plots <- list()

#1. Gender
fit <- survfit(Surv(time, DEATH_EVENT) ~ sex, data = hf_data)
print(fit)

individual_plots[1] = ggsurvplot(fit,
           conf.int = FALSE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz",
           risk.table.title = "",  # Remove the title above the risk table
           event.size = 0)

#2. Smoking
fit <- survfit(Surv(time, DEATH_EVENT) ~ smoking, data = hf_data)
print(fit)

par(mfrow=c(3,2))
individual_plots[2] = ggsurvplot(fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.title = "",  # Remove the title above the risk table
                                 event.size = 0)


#3. Diabetes
fit <- survfit(Surv(time, DEATH_EVENT) ~ diabetes, data = hf_data)
print(fit)

par(mfrow=c(3,2))
individual_plots[3] = ggsurvplot(fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.title = "",  # Remove the title above the risk table
                                 event.size = 0)


#4. High blood Pressure
fit <- survfit(Surv(time, DEATH_EVENT) ~ high_blood_pressure, data = hf_data)
print(fit$cumhaz)

par(mfrow=c(3,2))
individual_plots[4] = ggsurvplot(fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.pos = "in",  
                                 legend.title = "",
                                 title = "High blood pressure",
                                 legend=c(0.2,0.8))
                         #         censor = FALSE)
print(fit[['cumhaz']])
#5. Anaemia
fit <- survfit(Surv(time, DEATH_EVENT) ~ anaemia, data = hf_data)
print(fit$cumhaz)
individual_plots[5] = ggsurvplot(fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.title = "",  # Remove the title above the risk table
                                 event.size = 0)



grid = individual_plots[[1]] + individual_plots[[2]] + individual_plots[[3]] + individual_plots[[4]] + individual_plots[[5]] + plot_spacer() 
grid


# Plotting the Nelson Aalen estimator for the continous variables


#1. Age


na_fit <- survfit(surv_obj ~ age_category, data = hf_data)
individual_plots[1] = ggsurvplot(na_fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                  #               palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                  risk.table.pos = "in",  
                  legend.title = "",
                  title = "Age",
                  legend=c(0.15,0.8))

#2. Creatine phosphokinase


na_fit <- survfit(surv_obj ~ cpk_category, data = hf_data)
individual_plots[2] = ggsurvplot(na_fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 #               palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.pos = "in",  
                                 legend.title = "",
                                 title = "Creatinine phosphokinase",
                                 legend=c(0.8, 0.2))

# 3. Ejection fraction

na_fit <- survfit(surv_obj ~ ef_category, data = hf_data)
individual_plots[3] = ggsurvplot(na_fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 #               palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.pos = "in",  
                                 legend.title = "",
                                 title = "Ejection Fraction",
                                 legend=c(0.8, 0.175))

# 4. Platelets

na_fit <- survfit(surv_obj ~ platelets_category, data = hf_data)
individual_plots[4] = ggsurvplot(na_fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 #               palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.pos = "in",  
                                 legend.title = "",
                                 title = "Platelets",
                                 legend=c(0.7,0.2))

#5. Serum Creatinine 

na_fit <- survfit(surv_obj ~ sc_category, data = hf_data)
individual_plots[5] = ggsurvplot(na_fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 #               palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.pos = "in",  
                                 legend.title = "",
                                 title = "Serum Creatinine",
                                 legend=c(0.2,0.7))

#6. Serum sodium

na_fit <- survfit(surv_obj ~ ss_category, data = hf_data)
individual_plots[6] = ggsurvplot(na_fit,
                                 conf.int = FALSE,
                                 risk.table.col = "strata", # Change risk table color by groups
                                 ggtheme = theme_bw(), # Change ggplot2 theme
                                 #               palette = c("#E7B800", "#2E9FDF"),
                                 fun = "cumhaz",
                                 risk.table.pos = "in",  
                                 legend.title = "",
                                 title = "Serum Sodium",
                                 legend=c(0.2,0.7))

print(na_fit[['cumhaz']])

grid = individual_plots[[1]] + individual_plots[[2]] + individual_plots[[3]] + individual_plots[[4]] + individual_plots[[5]] + individual_plots[[6]]
grid

grid.arrange(
  individual_plots[[1]],
  individual_plots[[2]],
  individual_plots[[3]],
  individual_plots[[4]],
  individual_plots[[5]],
  individual_plots[[6]],
  ncol = 2  # You can adjust the number of columns as needed
)

?ggsurvplot

par(mfrow = c(3,2))

AIC(cox_model, aft_model_exp)
BIC(cox_model, aft_model_exp)

?concordance

martingale_residuals <- residuals(aft_model_exp, type = "martingale")

# Plot martingale residuals
plot(martingale_residuals, ylab = "Martingale Residuals", xlab = "Observation Order")
abline(h = 0, col = "red")
