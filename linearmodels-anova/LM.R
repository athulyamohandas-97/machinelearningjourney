getwd()
setwd("C:/Users/athul/Downloads")

#Loading the data
kidney <- read.table("KindneyFailure.txt", header = FALSE)
names(kidney) <- c("days","treat_dur","weight","id")

#loading required libraries
library(stats)
library(multcomp)
library(gplots)
library(DescTools)
library(car)
library(MASS)

#Add appropriate factors
kidney$treat_dur[kidney$treat_dur==1] <- "short"
kidney$treat_dur[kidney$treat_dur==2] <- "long"
kidney$treat_dur <- as.factor(kidney$treat_dur)

kidney$weight[kidney$weight==1] <- "slight"
kidney$weight[kidney$weight==2] <- "moderate"
kidney$weight[kidney$weight==3] <- "substantial"
kidney$weight <- as.factor(kidney$weight)

#Basic EDA
#summary tables
freq.tab <- table(kidney$treat_dur, kidney$weight)
freq.tab
#balanced study with 10 obs each

#boxplots
boxplot(days~weight,data=kidney,main="Days Hospitalised vs Weight gain", col = "gray")
boxplot(days~treat_dur,data=kidney,main="Days Hospitalised vs Treatment length", col = "gray")

#mean plot
plot.design(days~treat_dur+weight, data = kidney,fun = mean, main="Main effects means plot")

#interaction plot
with(kidney, interaction.plot(x.factor=weight, trace.factor=treat_dur, response=days, fun=mean,
                              type="b", legend=T, ylab="Days Means", main="Interaction Plot", pch=c(1,19)))
#appears to be interaction effect, but only one sample so we need more tests

#ANOVA
#fitting cell means model
kidney.lm <- lm(days~weight*treat_dur, data=kidney)
kidney.fit <- anova(kidney.lm)
kidney.fit
#non-significant interaction, significant main effects for both factors

#Tukey procedure
kidney.aov <- aov(days ~ weight * treat_dur, data = kidney)
weight.tukey <- TukeyHSD(kidney.aov, which = "weight")
weight.tukey
plot(weight.tukey)
#no significant diff between slight and moderate
#significant diff between 
#substantial and moderate: substantial spend 5.55 days longer in hospital on avg
#and substantial and slight: substantial spend 8.60 days longer on avg in hospital

#model without interaction terms
kidney.fit2 <- aov(days~weight+treat_dur, data=kidney)
summary(kidney.fit2)
#same interpretation no change in effects
#but keep interaction to be safer

#table with means
mean.freq <- tapply(kidney$days, list(kidney$treat_dur, kidney$weight), mean)
mean.freq

#Diagnostics - check model assumptions
#Graphical
#analysis of residuals
par(mfrow=c(1,3))

#studentised residuals vs fitted values
plot(fitted.values(kidney.lm), rstandard(kidney.lm),
     xlab="Fitted values",
     ylab="Studentized residuals",
     main="Residuals vs fitted values plot")
abline(h=0,lty="dashed",col="red")
#spread of residuals increases with fitted values meaning variance is not constant

#studentised residuals vs treatment duration levels plot
plot(as.numeric(kidney$treat_dur), rstandard(kidney.lm),
     xlab = "Treatment duration",
     ylab = "Studentized residuals",
     main = "Residuals vs treatment duration", axes=F)
axis(side=1, at = c(1,2), labels = c("Short","Long"))
axis(side=2)
abline(h = 0, lty = "dashed",col="red")
#residuals are more dispersed for long treatment further showing variance not constant

#studentised residuals vs weight levels plot
plot(as.numeric(kidney$weight),rstandard(kidney.lm),
     xlab="Weight levels",
     ylab="Studentized residuals",
     main="Residuals vs weight levels plot",axes=F)
axis(side=1, at = c(1,2,3), labels = c("Slight","Moderate", "Substantial"))
axis(side=2)
abline(h=0,lty="dashed",col="red")
#residuals have different spreads for different weight level gains further proving unequal variance

#Outlier checks
plot(hatvalues(kidney.aov), rstandard(kidney.aov), main="Leverage vs Residuals", xlab="Leverage", ylab="Std Resid")
#X-axis - influence  vs Y-axis - Standardized residuals 
#most points are low leverage (~0.1) and residuals near 0 meaning they are non-influential
#few points with res>2 or res<-2 have low leverage so they are not very problematic
#No points have high leverage and high residuals

plot(cooks.distance(kidney.aov), type="h", main="Cook's distances")
#measure influence of each observation on model coefficients and most have small CDist (<0.05)
#one large value between 25-30 (~28) with max ~0.15.
#nothing too influential as per graphs

#Sequence plot: serial dependence
plot(rstandard(kidney.lm)[-c(1)],rstandard(kidney.lm)[-c(60)],
     xlab="Studentized residuals at 1 lag",
     ylab="Studentized residuals",
     main="Sequence plot")
abline(a=0,b=1,lty="dashed")
#residuals seem evenly distributed around the line so there is no evidence of autocorrelation

#Normal QQ plot: assess normality
qqnorm(residuals(kidney.lm))
qqline(residuals(kidney.lm))
#some deviation at the tails indicating a mild departure from normality

#Formal tests
#Normality
shapiro.test(residuals(kidney.lm))
#p-value<0.05 showing statistical evidence of non-normality in residuals

ks.test(residuals(kidney.lm), "pnorm", mean=mean(residuals(kidney.lm)), sd=sd(residuals(kidney.lm)))
#p-value>0.05 hence null hypothesis is not rejected meaning normality holds

#difference in results because Shapiro–Wilk is more powerful for detecting skewness and departures at tails
#KS test is relatively insensitive for departure at the tails and to larger sample sizes (n>50)
#but, we had to estimate the mean and SD from the data reducing its validity and also there are ties here
#however, the graph shows that most of the deviation is at the tails

#Homoskedasticity
leveneTest(days~weight*treat_dur, data=kidney)
#evidence against homoskedasticity (F=4.766, p=0.001)

#test homogeneity for each factor
leveneTest(days ~ weight, data=kidney)
leveneTest(days ~ treat_dur, data=kidney)
#Both factors show significant heterogeneity of variance

#independence
#Dubin-Watson
durbinWatsonTest(kidney.lm, alternative="two.sided", data=kidney)
#no issue with independence (p=0.254)

#test for outliers: studentised deleted residuals with Bonferroni
nt <- nrow(kidney)
r.kidney <- length(coef(kidney.lm))
pvalue_outliers <- numeric(nt)
stud_res <- rstudent(kidney.lm)
for (i in 1:nt){
  pvalue_outliers[i] <-1 - pt(abs(stud_res[i]), df = nt- r.kidney - 1)}
#bonferroni adjustment: keep only v small p-values
alpha <- 0.05
pvalue_outliers[pvalue_outliers > alpha / nt]<- 1
out.kidney <- data.frame(Stud.Deleted.Res = stud_res,
                         Outlier.p.value = pvalue_outliers)
out.kidney
#no outliers

outlierTest(kidney.lm) 
#number 28 as per the graph as well was the most probable 
#but after applying correction even it does not seem to cause a problem

#remedial measures
#stabilise variance & fix non-normality
#Box-Cox transformation
#shift the response to make it strictly positive
kidney$days_pos <- kidney$days+1
kidney.lm.pos <- lm(days_pos~weight*treat_dur, data=kidney)
kidney.boxcox<-boxcox(kidney.lm.pos,lambda=seq(-2,2,by=0.1))
lambda.max <- kidney.boxcox$x[which.max(kidney.boxcox$y)]
lambda.max
#the MLE of lambda is 0.1 and lambda=0 is in 95% confidence interval
#we select 0 (log transformation) for ease of interpretation

#fit transformed model
kidney.log <- lm(log(days_pos)~weight*treat_dur, data=kidney)
log.fit <- anova(kidney.log)

#check effect on residuals
#heteroskedasticity
#graphical check
par(mfrow=c(1,2))
plot(fitted.values(kidney.lm), rstandard(kidney.lm),
     xlab="Fitted values",
     ylab="Studentized residuals",
     main="Original model")
plot(fitted.values(kidney.log), rstandard(kidney.log),
     xlab="Fitted values",
     ylab="Studentized residuals",
     main="Log Model")
#even spread of residuals
#formal test
leveneTest(log(days_pos)~weight*treat_dur, data=kidney)
#no evidence against homoskedascity (F=0.3217, p=0.898)

#normality
#graphically
par(mfrow=c(1,2))
qqnorm(residuals(kidney.lm), main="Original model")
qqline(residuals(kidney.lm))
qqnorm(residuals(kidney.log), main="Log model")
qqline(residuals(kidney.log))
#formally
shapiro.test(residuals(kidney.log))
#no evidence against normality at 5% significance level (W=0.964,p=0.073)

#interpretation of model
log.fit
log.aov <- aov(log(days_pos)~weight*treat_dur, data=kidney)
#ANOVA on log-transformed days showed a statistically significant main effect of weight 
#statistically significant main effect of treatment duration 
#interaction between weight and treatment duration was not significant 
#consistent with results from untransformed model

log.tukey <- TukeyHSD(log.aov, which = "weight")
log.tukey
plot(log.tukey)
#0 not in any CI, significant difference between all levels of weight

#e^0.63 ≈ 0.53 --> (0.53 − 1)*100 =−47% 
#slight patients stayed about 47% fewer days than moderate patients
#substantial patients stayed about 95% longer than moderate patients
#substantial patients stayed about 3.7 times longer than slight patients (268%)

#alternative methods
#weighted least squares 
#1. estimate cell variances
resid <- residuals(kidney.lm)
cell <- interaction(kidney$weight, kidney$treat_dur, drop = TRUE)
cell_var <- tapply(resid, cell, var)
#3. define weights per cell
w <- 1/cell_var[cell]
#4. refit model with these weights
wls.lm <- lm(days ~ weight * treat_dur,
             data = kidney,
             weights = w)
wls.fit <- anova(wls.lm)
wls.fit
#WLS model accounting for heteroscedasticity indicates a statistically significant main effect of weight 
#main effect of treatment duration was not statistically significant 
#interaction between weight and treatment duration was not significant 

#re-check diagnostics to see if variance is more stable
par(mfrow=c(1,3))
plot(fitted.values(kidney.lm), rstandard(kidney.lm),
     xlab="Fitted values",
     ylab="Studentized residuals",
     main="Original model")
plot(fitted.values(kidney.log), rstandard(kidney.log),
     xlab="Fitted values",
     ylab="Studentized residuals",
     main="Log Model")
wres <- residuals(wls.lm) * sqrt(wls.lm$weights)
plot(fitted(wls.lm), wres,
     xlab = "Fitted values",
     ylab = "Weighted residuals",
     main = "Weighted Residuals vs Fitted Values")

leveneTest(wres ~ treat_dur * weight, data = kidney)
#equal variance assumption is now upheld

shapiro.test(wres)
qqnorm(wres)
qqline(wres, col="red")
#mild non-normality at tails but nothing too significant that could impact WLS findings 

#Count-based Models GLM
# Poisson GLM with 2 x 3 factors
kidney.glm.pois <- glm(days ~ weight * treat_dur,
                       family = poisson(link = "log"),
                       data   = kidney)
summary(kidney.glm.pois)
#patients with moderate weight and long treatment duration stay 3.7 days on avg (e^1.30833≈3.70)
#slight patients stay 41% fewer days than moderate patients -marginally significant
#substantial patients stay twice as long as moderate patients 
#short treatment duration twice as many hospitalization days 
#interaction terms are non-significant

#Check overdispersion
dispersion <- sum(residuals(kidney.glm.pois, type = "pearson")^2) /kidney.glm.pois$df.residual
dispersion
#since there is a strong overdispersion (greater than 1), we cannot use Poisson

library(MASS)
#reference category = moderate weight, long duration
kidney.glm.nb <- glm.nb(days ~ weight * treat_dur, data = kidney)
summary(kidney.glm.nb)
#weightsubstantial differs significantly from the reference and is the only statistically siginficant weight category
#treat_durshort has weak effect
#Interaction terms remain non-significant
#similar coeffs as Poisson but much lower residual deviance thanks to accounting for overdispertion via theta

# Likelihood-ratio test Poisson vs negative binomial
anova(kidney.glm.pois, kidney.glm.nb, test = "LRT")
#as expected due to overdispersion, NB performs better than Poisson

summary(wls.lm)
anova(wls.lm)
#Final Interpretations
#Across models we don't see any interaction between weight and treatment duration 
#Weight is the dominant predictor with the substantial weight group differing the most
#Treatment duration has a weak-to-moderate effect and is less stable than weight
#All models - log transformed + NB + WLS mostly agree on these findings meaning that they are stable 
#variance is highest in substantial and moderate short groups with lower variability in long duration groups
#For interpretability, choose the WLS since it keeps effects as additive

wls.reduced <- lm(days ~ weight + treat_dur,
                  data = kidney,
                  weights = w)

anova(wls.lm,wls.reduced)
#no significant evidence that interaction effects exist
#main effects model is adequate 

library(emmeans)
emm_weight <- emmeans(wls.reduced, ~ weight)
pairs(emm_weight, adjust = "tukey")
#moderate patients stayed ~2.3 days longer than slight (marginally non-significant)
#substantial patients stayed ~4.3 days longer than moderate
#substantial patients stayed ~6.6 days longer than slight patients
#largest difference between slight and substantial weight gain patients
#moderate vs slight shows a trend but is not statistically significant at 0.05
#results averaged over treatment duration levels