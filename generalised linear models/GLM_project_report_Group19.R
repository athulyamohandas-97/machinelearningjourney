rm(list = ls())

library(ggplot2)
library(MASS)
library(dplyr)
library(detectseparation)
library(ROSE)
library(ResourceSelection)
library(patchwork)

set.seed(30)

data <- read.table("data.txt", header = TRUE, quote="\"")

# Making a copy of the data set 
data.eda <- data

# Outcome distribution
data.eda %>% group_by(seen) %>% summarise(n())

# Feature interactions
cor(data.eda[1:4])

# Inspecting the IB behaviour related to W group
summary(data.eda$W)
data.eda$W_group <- cut(data.eda$W,
                        breaks = c(57, 89, 100, 113, 156),
                        include.lowest = TRUE)
W_prop <- data.eda %>% group_by(W_group) %>% summarise(prop_seen=mean(seen), prop_seen_group=n()/nrow(data.eda))
W_prop

ggplot(W_prop, aes(x = W_group)) +
  geom_bar(aes(y = prop_seen_group), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = prop_seen), group = 1, color = "blue",
            linewidth = 2, lineend="round", linejoin="round") +
  labs(x = "W Group", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13))



# Inspecting the IB behaviour related to C group
summary(data.eda$C)
data.eda$C_group <- cut(data.eda$C,
                        breaks = c(46, 65, 73, 84, 112),
                        include.lowest = TRUE)
C_prop <- data.eda %>% group_by(C_group) %>% summarise(prop_seen=mean(seen), prop_seen_group=n()/nrow(data.eda))
C_prop

ggplot(C_prop, aes(x = C_group)) +
  geom_bar(aes(y = prop_seen_group), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = prop_seen), group = 1, color = "blue",
            linewidth = 2, lineend="round", linejoin="round") +
  labs(x = "C Group", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13))

# Inspecting the IB behaviour related to CW group
summary(data.eda$CW)
data.eda$CW_group <- cut(data.eda$CW,
                        breaks = c(28, 42, 48, 53, 67),
                        include.lowest = TRUE)
CW_prop <- data.eda %>% group_by(CW_group) %>% summarise(prop_seen=mean(seen), prop_seen_group=n()/nrow(data.eda))
CW_prop

ggplot(CW_prop, aes(x = CW_group)) +
  geom_bar(aes(y = prop_seen_group), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = prop_seen), group = 1, color = "blue",
            linewidth = 2, lineend="round", linejoin="round") +
  labs(x = "CW Group", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=13),
        axis.title.y=element_text(size=13))

# Scatterplot of the 3 continuous variables
plot(data.eda$W ~ data.eda$C)
lm.multicol <- lm(data.eda$W ~ data.eda$C)
abline(lm.multicol, col="red", lwd=2)

plot(data.eda$W ~ data.eda$CW)
lm.multicol <- lm(data.eda$W ~ data.eda$CW)
abline(lm.multicol, col="red", lwd=2)

plot(data.eda$C ~ data.eda$CW)
lm.multicol <- lm(data.eda$C ~ data.eda$CW)
abline(lm.multicol, col="red", lwd=2)

# Checking for outliers
hist(data.eda$W)
boxplot(data.eda$W)

hist(data.eda$C)
boxplot(data.eda$C)

hist(data.eda$CW)
boxplot(data.eda$CW)

# Model Building

# Detecting Quasi-Complete Separation
detect_qcs <- glm(seen~., data=data, family=binomial, method="detect_separation")
detect_qcs

# Considering Interactions and Higher-Order terms
data.center <- data
data.center$W  <- as.numeric(scale(data$W,  center=TRUE, scale=FALSE))
data.center$C  <- as.numeric(scale(data$C,  center=TRUE, scale=FALSE))
data.center$CW <- as.numeric(scale(data$CW, center=TRUE, scale=FALSE))

lr.full <- glm(seen~W+C+CW+W*C+W*CW+C*CW+I(W^2)+I(C^2)+I(CW^2), data=data.center, family=binomial(link="logit"))
full_selection <- stepAIC(lr.full, direction="both")

# Class imbalance problem
##Following the hierarchical principle, the final model includes both the linear and quadratic terms for W and CW, even though stepwise selection retained only the quadratic effects.
lr.logit <- glm(seen ~ W + I(W^2) + CW + I(CW^2), data=data.center, family=binomial(link="logit"))
preds <- ifelse(predict(lr.logit, type = "response")>0.5,1,0)
table(Predicted=preds, Actual=data$seen)

# Constructing the candidate models
lr.probit <- glm(seen ~ W + I(W^2) + CW + I(CW^2), data=data.center, family=binomial(link="probit"))
lr.cloglog <- glm(seen ~ W + I(W^2) + CW + I(CW^2), data=data.center, family=binomial(link="cloglog"))
# Log-Log custom function
logloga <- function()
{
  linkfun <- function(mu)
    log(-log(mu))
  linkinv <- function(eta)
    exp(-exp(eta))
  mu.eta <- function(eta)
    - exp(-exp(eta)) * exp(eta)
  valideta <- function(eta)
    TRUE
  link <- "logloga"
  structure(
    list(
      linkfun = linkfun,
      linkinv = linkinv,
      mu.eta = mu.eta,
      valideta = valideta,
      name = link
    ),
    class = "link-glm"
  )
}
loglogv <- logloga()
lr.logloglink <- glm(seen ~ W + CW + I(W^2) + I(CW^2), data = data.center, family = binomial(link = loglogv))


# Showing that Hosmer-Lemeshaw test fails
## since n is small, changed g to thew smaller value
hl_trail <- hoslem.test(data.center$seen,lr.cloglog$fitted.values, g=5)
hl_trail
# Model comparison
model_stats <- function(model, data){
  # Cox and Snell R^2, 
  lr.null <- glm(seen~1, data=data, family=binomial)
  n <- nrow(data)
  LL1 <- as.numeric(logLik(model))
  LL0 <- as.numeric(logLik(lr.null))
  cox_snell <- 1 - exp((2/n)*(LL0 - LL1))
  
  # AIC
  aic <- AIC(model)
  
  # Baseline Precision
  preds <- ifelse(predict(model, type="response")>0.5,1,0)
  precision <- sum(preds==1 & data$seen==1)/sum(preds==1)
  
  return(c(Cox_Snell_R2=cox_snell, AIC=aic, Precision=precision))
}

eval_table <- data.frame(
  Model = c("Logit", "Probit", "Cloglog", "Log-Log"),
  R2 = NA,
  AIC = NA,
  Precision = NA
)

eval_table[1, 2:4] <- round(model_stats(lr.logit, data.center), 3)
eval_table[2, 2:4] <- round(model_stats(lr.probit, data.center), 3)
eval_table[3, 2:4] <- round(model_stats(lr.cloglog, data.center), 3)
eval_table[4, 2:4] <- round(model_stats(lr.logloglink, data.center), 3)
eval_table

# Checking for outliers
cooks_d <- cooks.distance(lr.cloglog)
cutoff <- 4 / nrow(data.center)

ggplot(data=data.frame(obs=1:length(cooks_d), cook=cooks_d), aes(x=obs, y=cook)) + 
  geom_point(color="blue") +
  geom_hline(yintercept=4/nrow(data.center), size=1, color="black", linetype="dashed") +
  labs(x="Observation", y="Cook's Distance") +
  theme_minimal()

# Sampling the outliers
data.out <- data.center %>%
  mutate(cooks_distance = round(cooks_d, 3)) %>%
  filter(cooks_distance > cutoff)

data.out$pred_prob <- predict(lr.cloglog, newdata = data.out, type="response")
data.out

# Interpreting the coefficients

summary(lr.cloglog)

mean_CW <- mean(data.center$CW)
mean_W <- mean(data.center$W)

w_seq <- seq(min(data.center$W), max(data.center$W), length.out = 200)
cw_seq <- seq(min(data.center$CW), max(data.center$CW), length.out = 200)

pred_data_W <- data.frame(W = w_seq, CW = mean_CW)
pred_data_CW <- data.frame(W = mean_W, CW = cw_seq)

# lr.cloglog as our final model
pred_data_W$prob <- predict(lr.cloglog, newdata = pred_data_W, type = "response")
pred_data_CW$prob <- predict(lr.cloglog, newdata = pred_data_CW, type = "response")

g1 <- ggplot(pred_data_W, aes(x = W, y = prob)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  labs(
    title = "Probability of Noticing the Gorilla",
    subtitle = paste(
      "Effect of Centered W Score (CW held at mean:",
      round(mean_CW, 2),
      ")"
    ),
    x = "Centered Stroop Word Score (W)",
    y = "Predicted Probability"
  ) +
  theme_minimal()

g2 <- ggplot(pred_data_CW, aes(x = CW, y = prob)) +
  geom_line(color = "darkorange", linewidth = 1.2) +
  labs(
    title = "Probability of Noticing the Gorilla",
    subtitle = paste(
      "Effect of Centered CW Score (W held at mean:",
      round(mean_W, 2),
      ")"
    ),
    x = "Centered Stroop Color-Word Score (CW)",
    y = "Predicted Probability"
  ) +
  theme_minimal()

g1 + g2

