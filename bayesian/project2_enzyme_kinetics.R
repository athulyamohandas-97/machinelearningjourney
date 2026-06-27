# Project 2: Enzyme Kinetics

library(datasets)
library(rjags)
library(coda)
library(ggplot2)
library(dplyr)
library(MCMCvis)
library(runjags)

set.seed(2026)
theme_set(theme_minimal(base_size = 14))

data(Puromycin)
df <- Puromycin


# EDA

head(df)
num_rows <- nrow(df)
ncol(df)
col_names <- colnames(df)
print(col_names)
summary(df)
sum(is.na(df)) # no missing values in the data


## concentration (conc) variable
cat("Mean: ", mean(df$conc))
cat("Variance: ", var(df$conc))
cat("Range of Variable:", min(df$conc), "-", max(df$conc))

g1 <- ggplot(df, aes(x = conc)) +
  geom_density(fill = "skyblue", alpha = 0.5) + theme_minimal() +
  labs(title = "Substrate Concentration Levels Distribution", x = "concentration level (ppm)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
g1


## state variable
levels(df$state)  # 1 = treated, 2 = untreated
table(df$state)

df_by_state <- df %>%
  group_by(state) %>%
  summarize(Mean = mean(rate, na.rm = TRUE),
            Var = var(rate, na.rm = TRUE))

df_by_state

g2 <- ggplot(df, aes(x = rate, fill = state, color = state)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Distribution of Rate by State",
    subtitle = "dashed lines indicate group means",
    x = "Rate",
    y = "Density"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  ) +
  geom_vline(
    data = df_by_state,
    aes(xintercept = Mean, colour = state),
    linetype = "dashed",
    linewidth = 1.5,
    show.legend = FALSE
  )
g2

## rate
cat("Mean: ", mean(df$rate))
cat("Variance: ", var(df$rate))
cat("Range of Variable:", min(df$rate), "-", max(df$rate))

g3 <- ggplot(df, aes(x = rate)) +
  geom_density(fill = "skyblue", alpha = 0.5) + theme_minimal() +
  labs(title = "Reaction rates Distribution", x = "Reaction (counts / min)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
g3

g4 <- ggplot(df, aes(x = conc, y = rate, colour = state)) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_colour_manual(values = c(
    "treated" = "steelblue",
    "untreated" = "darkorange"
  )) +
  labs(
    x = "substrate concentration (ppm)",
    y = "Reaction rate (counts/min/min)",
    title = "Treated enzyme consistently reaches higher reaction rates",
    colour = NULL
  ) +
  theme(plot.title = element_text(hjust = 0.5))
g4

# Q1
# model specification

jags_data <- list(
  N     = num_rows,
  rate  = df$rate,
  conc  = df$conc,
  group = as.numeric(df$state)
)


model_string <- "
model {
  for (i in 1:N) {
    rate[i] ~ dnorm(mu[i], tau)
    mu[i] <- Vmax[group[i]] * conc[i] / (K[group[i]] + conc[i])
  }

  for (j in 1:2) {
    Vmax[j] ~ dnorm(0, 0.0001) I(0, )
    K[j]    ~ dunif(0, 2)
  }

  tau ~ dgamma(0.0001, 0.0001)
  sigma <- 1 / sqrt(tau)

  ratio_Vmax <- Vmax[1] / Vmax[2]
  ratio_K    <- K[1] / K[2]
}
"

model_file <- tempfile(fileext = ".txt")
writeLines(model_string, con = model_file)


# Q2
# running the algorithm

init_fn <- function() {
  list(
    Vmax = runif(2, 50, 300),
    K = runif(2, 0.01, 1),
    tau = runif(1, 0.001, 0.01),
    .RNG.name = "base::Wichmann-Hill",
    .RNG.seed = 2026
  )
}

n_chains <- 3
n_burnin <- 5000
n_adapt <- 1000
n_iterations <- 20000
n_thin <- 5



model <- jags.model(
  model_file,
  data = jags_data,
  inits = init_fn,
  n.chains = n_chains,
  n.adapt = n_adapt
)
update(model, n.iter = n_burnin)

params <- c("Vmax", "K", "sigma", "ratio_Vmax", "ratio_K")
posterior_samples <- coda.samples(model,
                                  variable.names = params,
                                  n.iter = n_iterations,
                                  thin = n_thin)


# Gelman-Rubin: R-hat should be < 1.05
gelman.diag(posterior_samples, multivariate = FALSE)
gelman.plot(posterior_samples, ask = FALSE)

# effective sample size
effectiveSize(posterior_samples)

# autocorrelation
autocorr.plot(posterior_samples)


MCMCtrace(
  object = posterior_samples,
  params = c("Vmax", "K", "sigma", "ratio_Vmax", "ratio_K"),
  type = "both",
  ind = TRUE,
  pdf = TRUE,
  filename = "MCMC_Diagnostics"
)


# Q3
# Posterior summaries

print(summary(posterior_samples), digits = 2)

samples_matrix <- as.matrix(posterior_samples)

cat("\n95% Equal-Tail Credibility Intervals:\n")
equal_tail_intervals <- apply(samples_matrix, 2, quantile, probs = c(0.025, 0.975))
print(t(equal_tail_intervals), digits = 2)

cat("\n95% HPD intervals:\n")
print(HPDinterval(as.mcmc(samples_matrix)), digits = 2)


# Q4 Vmax and K ratios
ratios_hpd_intervals <- HPDinterval(as.mcmc(samples_matrix[, c("ratio_Vmax", "ratio_K")]))


ratio_summary <- data.frame(
  ratio     = c("Vmax_treated / Vmax_untreated", "K_treated / K_untreated"),
  mean      = c(mean(samples_matrix[, "ratio_Vmax"]), mean(samples_matrix[, "ratio_K"])),
  median    = c(median(samples_matrix[, "ratio_Vmax"]), median(samples_matrix[, "ratio_K"])),
  sd        = c(sd(samples_matrix[, "ratio_Vmax"]), sd(samples_matrix[, "ratio_K"])),
  
  equal_tail_lower  = c(
    quantile(samples_matrix[, "ratio_Vmax"], 0.025),
    quantile(samples_matrix[, "ratio_K"], 0.025)
  ),
  equal_tail_upper  = c(
    quantile(samples_matrix[, "ratio_Vmax"], 0.975),
    quantile(samples_matrix[, "ratio_K"], 0.975)
  ),
  
  hpd_lower = ratios_hpd_intervals[, "lower"],
  hpd_upper = ratios_hpd_intervals[, "upper"]
)
print(ratio_summary, row.names = FALSE, digits = 2)


# Q5

conc_grid <- seq(0.1, 1, length.out = 200)

fitted_curves <- function(idx, label) {
  v <- samples_matrix[, paste0("Vmax[", idx, "]")]
  k <- samples_matrix[, paste0("K[", idx, "]")]
  mu <- sapply(conc_grid, function(x)
    (v * x) / (k + x))
  
  data.frame(
    conc  = conc_grid,
    mean  = colMeans(mu),
    lower = apply(mu, 2, quantile, 0.025),
    upper = apply(mu, 2, quantile, 0.975),
    group = label
  )
}

fitted <- rbind(fitted_curves(1, "Treated"), fitted_curves(2, "Untreated"))

g5 <- ggplot() +
  geom_ribbon(data = fitted,
              aes(
                x = conc,
                ymin = lower,
                ymax = upper,
                fill = group
              ),
              alpha = 0.18) +
  geom_line(data = fitted,
            aes(x = conc, y = mean, colour = group),
            linewidth = 1)  +
  scale_colour_manual(
    values = c(
      "Treated" = "steelblue",
      "Untreated" = "darkorange",
      "treated" = "steelblue",
      "untreated" = "darkorange"
    ),
    breaks = c("Treated", "Untreated")
  ) +
  scale_fill_manual(
    values = c(
      "Treated" = "steelblue",
      "Untreated" = "darkorange"
    ),
    guide = "none"
  ) +
  labs(
    x = "substrate concentration (ppm)",
    y = "Reaction rate (counts/min/min)",
    title = "Treated enzyme saturates at a higher rate across all concentrations",
    colour = NULL
  ) +
  theme(plot.title = element_text(size = 28))
g5


# Q6 Posterior of reaction rate at x = 0.5

mu05_treated <- (samples_matrix[, "Vmax[1]"] * 0.5) / (samples_matrix[, "K[1]"] + 0.5)
mu05_untreated <- (samples_matrix[, "Vmax[2]"] * 0.5) / (samples_matrix[, "K[2]"] + 0.5)

hpd05_treated <- HPDinterval(as.mcmc(mu05_treated))
hpd05_untreated <- HPDinterval(as.mcmc(mu05_untreated))

cat(
  "\nReaction rate at x=0.5 Treated:\n",
  sprintf(
    "  Mean: %.1f\n  95%% Equal-Tail CI: [%.1f, %.1f]\n  95%% HPD Interval:   [%.1f, %.1f]",
    mean(mu05_treated),
    quantile(mu05_treated, 0.025),
    quantile(mu05_treated, 0.975),
    hpd05_treated[1],
    hpd05_treated[2]
  ),
  "\n\n"
)

cat(
  "Reaction rate at x=0.5 Untreated:\n",
  sprintf(
    "  Mean: %.1f\n  95%% Equal-Tail CI: [%.1f, %.1f]\n  95%% HPD Interval:   [%.1f, %.1f]",
    mean(mu05_untreated),
    quantile(mu05_untreated, 0.025),
    quantile(mu05_untreated, 0.975),
    hpd05_untreated[1],
    hpd05_untreated[2]
  ),
  "\n"
)

dens_df <- data.frame(rate  = c(mu05_treated, mu05_untreated),
                      group = rep(c("Treated", "Untreated"), each = length(mu05_treated)))

g6 <- ggplot(dens_df, aes(x = rate, fill = group, colour = group)) +
  geom_density(alpha = 0.2, linewidth = 0.8) +
  geom_vline(
    xintercept = mean(mu05_treated),
    colour = "steelblue",
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = mean(mu05_untreated),
    colour = "darkorange",
    linetype = "dashed"
  ) +
  scale_fill_manual(values = c(
    "Treated" = "steelblue",
    "Untreated" = "darkorange"
  )) +
  scale_colour_manual(values = c(
    "Treated" = "steelblue",
    "Untreated" = "darkorange"
  )) +
  labs(
    x = "Expected reaction rate (counts/min/min)",
    y = "Posterior density",
    title = "At concentration 0.5, treated enzyme reacts ~30% faster",
    fill = NULL,
    colour = NULL
  ) +
  theme(plot.title = element_text(size = 28))
g6


# Q7 Posterior probabilities for Vmax and K
# computed as proportion of MCMC draws where the condition holds

prob_vmax <- mean(samples_matrix[, "Vmax[1]"] > samples_matrix[, "Vmax[2]"])
prob_k    <- mean(samples_matrix[, "K[1]"]    < samples_matrix[, "K[2]"])

cat(sprintf("\nP(Vmax_treated > Vmax_untreated | data) = %.4f", prob_vmax),
    "\n")
cat(sprintf("P(K_treated < K_untreated | data)       = %.4f", prob_k), "\n")


# Q8 Concentration at 80% of Vmax
# Solving Vmax*x/(K+x) = 0.8*Vmax gives x = 4K

x80_treated <- 4 * samples_matrix[, "K[1]"]
x80_untreated <- 4 * samples_matrix[, "K[2]"]

hpd_x80_treated <- HPDinterval(as.mcmc(x80_treated))
hpd_x80_untreated <- HPDinterval(as.mcmc(x80_untreated))

x80_summary <- data.frame(
  group     = c("Treated", "Untreated"),
  mean      = c(mean(x80_treated), mean(x80_untreated)),
  median    = c(median(x80_treated), median(x80_untreated)),
  
  equal_tail_lower  = c(quantile(x80_treated, 0.025), quantile(x80_untreated, 0.025)),
  equal_tail_upper  = c(quantile(x80_treated, 0.975), quantile(x80_untreated, 0.975)),
  
  hpd_lower = c(hpd_x80_treated[1, "lower"], hpd_x80_untreated[1, "lower"]),
  hpd_upper = c(hpd_x80_treated[1, "upper"], hpd_x80_untreated[1, "upper"])
)
print(x80_summary, row.names = FALSE, digits = 3)

x80_df <- data.frame(conc  = c(x80_treated, x80_untreated),
                     group = rep(c("Treated", "Untreated"), each = length(x80_treated)))

g7 <- ggplot(x80_df, aes(x = conc, fill = group, colour = group)) +
  geom_density(alpha = 0.2, linewidth = 0.8) +
  scale_fill_manual(values = c(
    "Treated" = "steelblue",
    "Untreated" = "darkorange"
  )) +
  scale_colour_manual(values = c(
    "Treated" = "steelblue",
    "Untreated" = "darkorange"
  )) +
  labs(
    x = "substrate concentration at 80% Vmax (ppm)",
    y = "Posterior density",
    title = "Both groups need similar substrate levels to reach 80% capacity",
    fill = NULL,
    colour = NULL
  )
g7
