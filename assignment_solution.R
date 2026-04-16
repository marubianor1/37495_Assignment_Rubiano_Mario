library(MASS)
library(car)
library(emmeans)

alpha <- 0.05
show_plots <- TRUE
save_plots <- TRUE

if (!dir.exists("figures")) {
  dir.create("figures")
}

strength <- c(
  0.46, 0.67, 0.69, 0.73, 0.77, 0.78, 0.79, 0.80, 0.84, 0.85,
  0.86, 0.87, 0.88, 0.88, 0.89, 0.91, 0.92, 0.93, 0.95, 1.15,
  0.80, 0.84, 0.84, 0.85, 0.86, 0.87, 0.89, 0.97, 1.00, 1.01,
  1.03, 1.03, 1.06, 1.08, 1.09, 1.14, 1.16, 1.17, 1.18, 1.32,
  0.52, 0.52, 0.58, 0.59, 0.59, 0.60, 0.63, 0.63, 0.64, 0.64,
  0.64, 0.65, 0.65, 0.68, 0.71, 0.72, 0.75, 0.79, 0.80, 0.81,
  0.79, 0.79, 0.81, 0.81, 0.82, 0.82, 0.84, 0.86, 0.86, 0.87,
  0.88, 0.89, 0.90, 0.92, 0.93, 0.93, 0.96, 0.97, 0.98, 1.06,
  0.44, 0.45, 0.47, 0.47, 0.47, 0.48, 0.49, 0.51, 0.52, 0.53,
  0.53, 0.54, 0.55, 0.56, 0.57, 0.59, 0.60, 0.61, 0.67, 0.72
)

run <- factor(rep(1:5, each = 20))
insulation <- data.frame(run, strength)

cat("\nQuestion 2(a): one-sample t-test for run 1 mean > 0.78\n")
run1 <- insulation$strength[insulation$run == 1]
n_run1 <- length(run1)
mean_run1 <- mean(run1)
sd_run1 <- sd(run1)
t_run1 <- (mean_run1 - 0.78) / (sd_run1 / sqrt(n_run1))
df_run1 <- n_run1 - 1
p_run1 <- 1 - pt(t_run1, df_run1)
lower_ci_run1 <- mean_run1 - qt(0.95, df_run1) * sd_run1 / sqrt(n_run1)
cat("mean =", mean_run1, "\n")
cat("t =", t_run1, "\n")
cat("df =", df_run1, "\n")
cat("p-value =", p_run1, "\n")
cat("95 percent one-sided lower confidence bound =", lower_ci_run1, "\n")

cat("\nQuestion 2(b): bootstrap distribution of run 1 sample mean\n")
set.seed(37495)
boot_means <- replicate(
  100000,
  mean(sample(insulation$strength[insulation$run == 1], replace = TRUE))
)
print(summary(boot_means))
cat("Bootstrap SD:", sd(boot_means), "\n")

if (show_plots) {
  hist(boot_means, breaks = 100, probability = TRUE,
       main = "Bootstrap distribution of run 1 sample mean",
       xlab = "Bootstrap sample mean")
  curve(dnorm(x, mean = mean(boot_means), sd = sd(boot_means)),
        add = TRUE, col = "red", lwd = 2)
}

if (save_plots) {
  png("figures/q2b_bootstrap_histogram.png", width = 900, height = 650)
  hist(boot_means, breaks = 100, probability = TRUE,
       main = "Bootstrap distribution of run 1 sample mean",
       xlab = "Bootstrap sample mean")
  curve(dnorm(x, mean = mean(boot_means), sd = sd(boot_means)),
        add = TRUE, col = "red", lwd = 2)
  dev.off()
}

set.seed(37495)
cat("\nShapiro-Wilk test on a random sample of 50 bootstrap means\n")
print(shapiro.test(sample(boot_means, 50)))

cat("\nQuestion 2(c): one-way ANOVA\n")
fit <- aov(strength ~ run, data = insulation)
print(summary(fit))

cat("\nQuestion 2(d): residual checks for original ANOVA\n")
print(shapiro.test(residuals(fit)))
print(leveneTest(strength ~ run, data = insulation, center = median))

if (show_plots) {
  qqnorm(residuals(fit), main = "Original ANOVA residual Q-Q plot")
  qqline(residuals(fit), col = "red", lwd = 2)
}

if (save_plots) {
  png("figures/q2d_original_residual_qq.png", width = 900, height = 650)
  qqnorm(residuals(fit), main = "Original ANOVA residual Q-Q plot")
  qqline(residuals(fit), col = "red", lwd = 2)
  dev.off()
}

if (show_plots) {
  plot(fit$fitted.values, fit$residuals,
       xlab = "Fitted values", ylab = "Residuals",
       main = "Original ANOVA residuals vs fitted values")
  abline(h = 0, col = "red", lwd = 2)
}

if (save_plots) {
  png("figures/q2d_original_residuals_vs_fitted.png", width = 900, height = 650)
  plot(fit$fitted.values, fit$residuals,
       xlab = "Fitted values", ylab = "Residuals",
       main = "Original ANOVA residuals vs fitted values")
  abline(h = 0, col = "red", lwd = 2)
  dev.off()
}

cat("\nQuestion 2(e): Box-Cox transformation\n")
bc_grid <- boxcox(fit, lambda = seq(-3, 3, length = 600), plotit = FALSE)
lambda <- bc_grid$x[which.max(bc_grid$y)]
cat("Optimal lambda:", lambda, "\n")
insulation$strength_bc <- if (abs(lambda) < 1e-8) {
  log(insulation$strength)
} else {
  (insulation$strength^lambda - 1) / lambda
}

if (show_plots) {
  boxcox(fit, lambda = seq(-3, 3, length = 600))
  title("Box-Cox profile log-likelihood")
  abline(v = lambda, col = "red", lwd = 2)
}

if (save_plots) {
  png("figures/q2e_boxcox_profile.png", width = 900, height = 650)
  boxcox(fit, lambda = seq(-3, 3, length = 600))
  title("Box-Cox profile log-likelihood")
  abline(v = lambda, col = "red", lwd = 2)
  dev.off()
}

cat("\nQuestion 2(f): ANOVA and residual checks after Box-Cox transform\n")
fit_bc <- aov(strength_bc ~ run, data = insulation)
print(summary(fit_bc))
print(shapiro.test(residuals(fit_bc)))
print(leveneTest(strength_bc ~ run, data = insulation, center = median))

if (show_plots) {
  qqnorm(residuals(fit_bc), main = "Box-Cox ANOVA residual Q-Q plot")
  qqline(residuals(fit_bc), col = "red", lwd = 2)
}

if (save_plots) {
  png("figures/q2f_boxcox_residual_qq.png", width = 900, height = 650)
  qqnorm(residuals(fit_bc), main = "Box-Cox ANOVA residual Q-Q plot")
  qqline(residuals(fit_bc), col = "red", lwd = 2)
  dev.off()
}

if (show_plots) {
  plot(fit_bc$fitted.values, fit_bc$residuals,
       xlab = "Fitted values", ylab = "Residuals",
       main = "Box-Cox ANOVA residuals vs fitted values")
  abline(h = 0, col = "red", lwd = 2)
}

if (save_plots) {
  png("figures/q2f_boxcox_residuals_vs_fitted.png", width = 900, height = 650)
  plot(fit_bc$fitted.values, fit_bc$residuals,
       xlab = "Fitted values", ylab = "Residuals",
       main = "Box-Cox ANOVA residuals vs fitted values")
  abline(h = 0, col = "red", lwd = 2)
  dev.off()
}

cat("\nQuestion 2(g): Scheffe contrasts after Box-Cox transform\n")
run_means <- emmeans(fit_bc, "run")
contrast_list <- list(
  "run1_vs_avg_run2_5" = c(1, -0.25, -0.25, -0.25, -0.25),
  "run1_vs_run3" = c(1, 0, -1, 0, 0),
  "avg_run124_vs_avg_run35" = c(1 / 3, 1 / 3, -1 / 2, 1 / 3, -1 / 2)
)
scheffe_results <- contrast(run_means, method = contrast_list, adjust = "scheffe")
print(summary(scheffe_results, infer = c(TRUE, TRUE), level = 0.95))

cat("\nQuestion 2(h): Durbin-Watson test noted in assignment\n")
print(durbinWatsonTest(fit_bc))

cat("\nQuestion 2(i): Cook's distance filtering\n")
insulation$cooksD <- cooks.distance(fit_bc)
filtered <- insulation[insulation$cooksD <= 0.05, c("strength_bc", "run")]
cat("Removed observations:", sum(insulation$cooksD > 0.05), "\n")
cat("Remaining observations:", nrow(filtered), "\n")
print(filtered[1:40, ], row.names = FALSE)
cat("\nRemoved observations were:\n")
print(insulation[insulation$cooksD > 0.05, c("run", "strength", "strength_bc", "cooksD")])
