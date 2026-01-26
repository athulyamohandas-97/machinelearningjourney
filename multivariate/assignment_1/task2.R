library(psych)
library(GPArotation)
library(lavaan)

load("Data/fsdata.Rdata")
sum(is.na(fsdata)) # checking for missing values

nrow <- nrow(fsdata)
nrow
ncol <- ncol(fsdata)
ncol

## part a

# scaling data and dropping the "country" column
df_standard <- as.data.frame(scale(fsdata[, 2:ncol], center =
                                     T, scale = T))


# calculating the correlation matrix
cor_df <- cor(df_standard)

# conducting EFA
efa_df <- fa(
  r = cor_df,
  fm = "ml",
  nfactors = 5,
  rotate = "oblimin",
  scores = "regression",
  n.obs = nrow
)
efa_df
efa_df$loadings
data.frame(communality = round(efa_df$communalities, 3),
           uniquenesses = round(efa_df$uniquenesses, 3))


# reproducibility
rep_cor <- efa_df$model
change <- max(abs(cor_df - rep_cor)) 
rms_res <- sqrt(mean((cor_df - rep_cor)[lower.tri(cor_df)]^2)) 
c(change, rms_res)
efa_df$STATISTIC
efa_df$PVAL


## part b

# centering data
df_centered <- as.data.frame(scale(fsdata[, 2:ncol], center =
                                     T, scale = F))

# calculating the covariance matrix
cov_df <- cov(df_centered)
sample_size <- nrow(df_centered)
sample_size

# model parameters
cfa_model_1 <- '
FS=~NA*FS_pay_bills+FS_afford_extras+FS_afford_housing+FS_save_money
FSF=~NA*FSF_pay_bills+FSF_afford_extras+FSF_afford_housing+FSF_save_money
SFJ=~NA*SFJ_no_info+SFJ_no_chance_show+SFJ_no_training+SFJ_no_support_findjob
SDJ=~NA*SDJ_help_people+SDJ_learn_new_things+SDJ_develop_creativity+
SDJ_meet_people+SDJ_feeling_self_worth
HEALTH=~NA*HEALTH_felt_down+HEALTH_limitation
FS ~~ 1*FS
FSF ~~ 1*FSF
SFJ ~~ 1*SFJ
SDJ ~~ 1*SDJ
HEALTH ~~ 1*HEALTH
FS ~~ FSF
FS ~~ SFJ
FS ~~ SDJ
FS ~~ HEALTH
FSF ~~ SFJ
FSF ~~ SDJ
FSF ~~ HEALTH
SFJ ~~ SDJ
SFJ ~~ HEALTH
SDJ ~~ HEALTH
'

# fitting the data and examining the result
cfa_fit_1 <- cfa(model = cfa_model_1,
                 sample.cov = cov_df,
                 sample.nobs = sample_size)
fitmeasures(cfa_fit_1, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "SRMR"))
summary(cfa_fit_1)$pe[, c(1:3, 5)]

# capturing the standard loadings and computing the error variances
std_loadings <- inspect(cfa_fit_1, "std")$lambda
err_var <- 1 - std_loadings^2

# convergence
ave <- function(load){
  load <- load[load != 0] 
  sum(load^2) / length(load)
}
AVE_res <- apply(std_loadings, 2, ave)  
round(AVE_res, 3)

# function for computing composite reliability
comp_rel <- function(loads, errs)
{
  (sum(loads)^2) / ((sum(loads)^2) + sum(errs))
}

# applying the composite reliability function to the 
# standardized loadings and errors
composite_results <-  sapply(1:ncol(std_loadings), function(i) {
  l <- std_loadings[, i][std_loadings[, i] != 0]
  e <- err_var[, i][std_loadings[, i] != 0]
  comp_rel(l, e)
})
composite_results

## part c

# getting the modification indices
mi_fit_1 <- modificationindices(cfa_fit_1)

# sorting based on largest drop in Chi score
mi_order <- order(mi_fit_1$mi, decreasing = T)
mi_fit1_sorted <- mi_fit_1[mi_order, ]
mi_fit1_sorted

# constraining 8 parameters to improve performance
cfa_model_2 <- '
FS=~NA*FS_pay_bills+FS_afford_extras+FS_afford_housing+FS_save_money
FSF=~NA*FSF_pay_bills+FSF_afford_extras+FSF_afford_housing+FSF_save_money
SFJ=~NA*SFJ_no_info+SFJ_no_chance_show+SFJ_no_training+SFJ_no_support_findjob
SDJ=~NA*SDJ_help_people+SDJ_learn_new_things+SDJ_develop_creativity+
SDJ_meet_people+SDJ_feeling_self_worth
HEALTH=~NA*HEALTH_felt_down+HEALTH_limitation
FS ~~ 1*FS
FSF ~~ 1*FSF
SFJ ~~ 1*SFJ
SDJ ~~ 1*SDJ
HEALTH ~~ 1*HEALTH
FS ~~ FSF
FS ~~ SFJ
FS ~~ SDJ
FS ~~ HEALTH
FSF ~~ SFJ
FSF ~~ SDJ
FSF ~~ HEALTH
SFJ ~~ SDJ
SFJ ~~ HEALTH
SDJ ~~ HEALTH
FSF_afford_extras ~~ FSF_save_money
SDJ_help_people ~~ SDJ_meet_people
SDJ_learn_new_things ~~ SDJ_develop_creativity
FS_pay_bills ~~ FSF_pay_bills
FS_afford_housing ~~ FSF_afford_housing
FS_save_money ~~ FSF_save_money
FS_afford_extras ~~ FSF_afford_extras
FS_save_money ~~ FS_afford_extras
'

cfa_fit_2 <- cfa(model = cfa_model_2,
                 sample.cov = cov_df,
                 sample.nobs = sample_size)
summary(cfa_fit_2)
fitmeasures(cfa_fit_2, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "SRMR"))

## part d
df_centered$country <- fsdata$country

model_structural_free <- '
FS=~NA*FS_pay_bills+FS_afford_extras+FS_afford_housing+FS_save_money
FSF=~NA*FSF_pay_bills+FSF_afford_extras+FSF_afford_housing+FSF_save_money
SFJ=~NA*SFJ_no_info+SFJ_no_chance_show+SFJ_no_training+SFJ_no_support_findjob
SDJ=~NA*SDJ_help_people+SDJ_learn_new_things+SDJ_develop_creativity+
SDJ_meet_people+SDJ_feeling_self_worth
HEALTH=~NA*HEALTH_felt_down+HEALTH_limitation

FS ~~ 1*FS
FSF ~~ 1*FSF
SFJ ~~ 1*SFJ
SDJ ~~ 1*SDJ
HEALTH ~~ 1*HEALTH

FSF ~~ SFJ
FSF ~~ SDJ
FSF ~~ HEALTH
SFJ ~~ SDJ
SFJ ~~ HEALTH
SDJ ~~ HEALTH

FSF_afford_extras ~~ FSF_save_money
SDJ_help_people ~~ SDJ_meet_people
SDJ_learn_new_things ~~ SDJ_develop_creativity
FS_pay_bills ~~ FSF_pay_bills
FS_afford_housing ~~ FSF_afford_housing
FS_save_money ~~ FSF_save_money
FS_afford_extras ~~ FSF_afford_extras
FS_save_money ~~ FS_afford_extras

FS ~ FSF + SFJ + SDJ + HEALTH
'

model_structural_constrained <- '
FS=~NA*FS_pay_bills+FS_afford_extras+FS_afford_housing+FS_save_money
FSF=~NA*FSF_pay_bills+FSF_afford_extras+FSF_afford_housing+FSF_save_money
SFJ=~NA*SFJ_no_info+SFJ_no_chance_show+SFJ_no_training+SFJ_no_support_findjob
SDJ=~NA*SDJ_help_people+SDJ_learn_new_things+SDJ_develop_creativity+
SDJ_meet_people+SDJ_feeling_self_worth
HEALTH=~NA*HEALTH_felt_down+HEALTH_limitation

FS ~~ 1*FS
FSF ~~ 1*FSF
SFJ ~~ 1*SFJ
SDJ ~~ 1*SDJ
HEALTH ~~ 1*HEALTH

FSF ~~ SFJ
FSF ~~ SDJ
FSF ~~ HEALTH
SFJ ~~ SDJ
SFJ ~~ HEALTH
SDJ ~~ HEALTH

FSF_afford_extras ~~ FSF_save_money
SDJ_help_people ~~ SDJ_meet_people
SDJ_learn_new_things ~~ SDJ_develop_creativity
FS_pay_bills ~~ FSF_pay_bills
FS_afford_housing ~~ FSF_afford_housing
FS_save_money ~~ FSF_save_money
FS_afford_extras ~~ FSF_afford_extras
FS_save_money ~~ FS_afford_extras

FS ~ b1*FSF + b2*SFJ + b3*SDJ + b4*HEALTH
'

# d.1
config_invariance <- sem(model_structural_free, data = df_centered, group =
                           "country")

summary(config_invariance)


# d.2
config_invariance_equal <- sem(
  model_structural_constrained,
  data = df_centered,
  group = "country",
  group.equal = "regressions"
)

summary(config_invariance_equal)


# d.3
metric_invariance <- sem(
  model_structural_free,
  data = df_centered,
  group = "country",
  group.equal = "loadings"
)

summary(metric_invariance)


# d.4
metric_invariance_equal <- sem(
  model_structural_constrained,
  data = df_centered,
  group = "country",
  group.equal = c("loadings", "regressions")
)

summary(metric_invariance_equal)

# comparing fit measurements
fitmeasures(config_invariance,
            c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
fitmeasures(config_invariance_equal,
            c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
fitmeasures(metric_invariance,
            c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
fitmeasures(metric_invariance_equal,
            c("chisq", "df", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))

# Note: second model is chosen (config_invariance_equal)
# chosen model intercepts
model_intercepts <- parameterEstimates(config_invariance_equal)
subset(model_intercepts, op == "~1") # filtering for intercepts

# regression coefficients
standard_solution <- standardizedSolution(config_invariance_equal)

# filter for regression coefficients
standard_regressions <- subset(standard_solution, op == "~")
standard_regressions
