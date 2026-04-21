out_file <- "37495_Assignment_Rubiano_Mario.pdf"

source_code <- readLines("assignment_solution.R", warn = FALSE)
source_code <- gsub("\t", "  ", source_code)

run_section <- function() {
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
  insulation_pdf <- insulation
  assign("insulation_pdf", insulation_pdf, envir = .GlobalEnv)

  run1 <- insulation$strength[insulation$run == 1]
  n_run1 <- length(run1)
  mean_run1 <- mean(run1)
  sd_run1 <- sd(run1)
  t_run1 <- (mean_run1 - 0.78) / (sd_run1 / sqrt(n_run1))
  df_run1 <- n_run1 - 1
  p_run1 <- 1 - pt(t_run1, df_run1)
  lower_ci_run1 <- mean_run1 - qt(0.95, df_run1) * sd_run1 / sqrt(n_run1)

  set.seed(37495)
  boot_means <- replicate(
    100000,
    mean(sample(insulation$strength[insulation$run == 1], replace = TRUE))
  )
  set.seed(37495)
  boot_shapiro <- shapiro.test(sample(boot_means, 50))

  fit <- aov(strength ~ run, data = insulation_pdf)
  insulation_pdf$resid <- resid(fit)
  assign("insulation_pdf", insulation_pdf, envir = .GlobalEnv)
  bc_grid <- MASS::boxcox(fit, lambda = seq(-3, 3, length = 600), plotit = FALSE)
  lambda <- bc_grid$x[which.max(bc_grid$y)]
  insulation_pdf$strength_bc <- DescTools::BoxCox(insulation_pdf$strength, lambda)
  assign("insulation_pdf", insulation_pdf, envir = .GlobalEnv)
  fit_bc <- aov(strength_bc ~ run, data = insulation_pdf)
  insulation_pdf$resid_bc <- resid(fit_bc)

  run_means <- emmeans::emmeans(fit_bc, "run")
  contrast_list <- list(
    "run1_vs_avg_run2_5" = c(1, -0.25, -0.25, -0.25, -0.25),
    "run1_vs_run3" = c(1, 0, -1, 0, 0),
    "avg_run124_vs_avg_run35" = c(1 / 3, 1 / 3, -1 / 2, 1 / 3, -1 / 2)
  )
  scheffe_results <- emmeans::contrast(
    run_means, method = contrast_list, adjust = "scheffe"
  )

  insulation_pdf$cooksD <- cooks.distance(fit_bc)
  filtered <- insulation_pdf[insulation_pdf$cooksD <= 0.05, c("strength_bc", "run")]

  list(
    insulation = insulation_pdf,
    run1 = c(
      mean = mean_run1,
      t = t_run1,
      df = df_run1,
      p_value = p_run1,
      lower_ci = lower_ci_run1
    ),
    boot_means = boot_means,
    boot_shapiro = boot_shapiro,
    fit = fit,
    lambda = lambda,
    fit_bc = fit_bc,
    scheffe_results = scheffe_results,
    filtered = filtered
  )
}

res <- run_section()

wrap_lines <- function(x, width = 88) {
  x <- gsub("\t", "  ", x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  unlist(strwrap(x, width = width, simplify = FALSE))
}

add_text_page <- function(title, body, cex = 0.83) {
  plot.new()
  par(mar = c(0, 0, 0, 0))
  y <- 0.965
  text(0.06, y, title, adj = c(0, 1), font = 2, cex = 1.2)
  y <- y - 0.055
  for (paragraph in body) {
    if (paragraph == "") {
      y <- y - 0.018
      next
    }
    lines <- wrap_lines(paragraph, width = 92)
    for (line in lines) {
      if (y < 0.055) {
        plot.new()
        par(mar = c(0, 0, 0, 0))
        y <- 0.965
      }
      text(0.06, y, line, adj = c(0, 1), cex = cex)
      y <- y - 0.027
    }
    y <- y - 0.016
  }
}

add_mono_pages <- function(title, lines, cex = 0.62, per_page = 48) {
  start <- 1
  lines <- gsub("\t", "  ", lines)
  lines <- iconv(lines, from = "", to = "ASCII//TRANSLIT", sub = "")
  while (start <= length(lines)) {
    end <- min(start + per_page - 1, length(lines))
    plot.new()
    par(mar = c(0, 0, 0, 0))
    text(0.06, 0.965, title, adj = c(0, 1), font = 2, cex = 1.05)
    y <- 0.915
    for (line in lines[start:end]) {
      text(0.06, y, line, adj = c(0, 1), family = "mono", cex = cex)
      y <- y - 0.018
    }
    start <- end + 1
  }
}

capture_lines <- function(expr) {
  capture.output(expr)
}

pdf(out_file, width = 8.27, height = 11.69, onefile = TRUE)

add_text_page(
  "37495 Statistical Design and Models for Evaluation Studies",
  c(
    "Assessment Task 2: R Assignment",
    "Student: Mario Rubiano",
    "Student ID: 24900627",
    "Submission file: 37495_Assignment_Rubiano_Mario.pdf",
    "",
    "This report includes the R code and the output that is used in the answers. The analysis uses the R functions and packages introduced in the laboratory solution files, including aov, resid, shapiro.test, nortest::cvm.test, nortest::ad.test, lawstat::levene.test, MASS::boxcox, DescTools::BoxCox, emmeans, contrast with Scheffe adjustment, durbinWatsonTest, and cooks.distance."
  )
)

add_text_page(
  "Question 1",
  c(
    "An appropriate design for this situation is a randomised complete block design. The response variable is the yield of the chemical process. This yield must be measured in the units used by the plant, for example grams, kilograms, or percent yield. The experimental factor is the chemical input, with five treatment levels because there are five chemicals. The blocking factor is technician, with three blocks because there are three technicians.",
    "There are 15 experimental units in total. Each experimental unit is one individual execution of the chemical process by one specific technician using one specified chemical input. The measurement unit is the physical batch or final product sample where the yield is measured. Within each technician block, the five chemicals should be randomly allocated to the five process runs done by that technician. In this way, each chemical has three observations, and the design controls for differences between technicians.",
    "A suitable model is y_ij = mu + tau_i + beta_j + e_ij, where tau_i is the effect of chemical i, beta_j is the effect of technician j, and e_ij is the random error."
  )
)

add_text_page(
  "Question 2(a)-(b)",
  c(
    sprintf("For production run 1, H0: mu_1 <= 0.78 and H1: mu_1 > 0.78. The sample mean is %.3f, t = %.4f, df = %.0f, and p-value = %.5f. The 95 percent one-sided lower confidence bound is %.7f. Since p > 0.05, there is not enough evidence at the 5 percent level to conclude that the mean impact strength for production run 1 is greater than 0.78.", res$run1["mean"], res$run1["t"], res$run1["df"], res$run1["p_value"], res$run1["lower_ci"]),
    sprintf("The bootstrap distribution was generated using 100000 re-samples with replacement. The bootstrap mean is approximately %.4f and the bootstrap standard deviation is approximately %.5f.", mean(res$boot_means), sd(res$boot_means)),
    sprintf("A Shapiro-Wilk test on a random sample of 50 bootstrap means gave W = %.5f and p-value = %.4f. Since p > 0.05, the test does not show evidence against normality for the sampled bootstrap means. Together with the histogram, this suggests that the bootstrap distribution is approximately normal.", res$boot_shapiro$statistic, res$boot_shapiro$p.value)
  )
)

hist(res$boot_means, breaks = 100, probability = TRUE,
     main = "Question 2(b): Bootstrap distribution of run 1 sample mean",
     xlab = "Bootstrap sample mean")
curve(dnorm(x, mean = mean(res$boot_means), sd = sd(res$boot_means)),
      add = TRUE, col = "red", lwd = 2)

add_mono_pages(
  "Output for Questions 2(a)-(b)",
  c(
    "Manual one-sample t-test output:",
    sprintf("mean = %.3f", res$run1["mean"]),
    sprintf("t = %.5f", res$run1["t"]),
    sprintf("df = %.0f", res$run1["df"]),
    sprintf("p-value = %.8f", res$run1["p_value"]),
    sprintf("95 percent one-sided lower confidence bound = %.7f", res$run1["lower_ci"]),
    "",
    "Bootstrap summary:",
    capture_lines(summary(res$boot_means)),
    "",
    "Shapiro-Wilk test on 50 bootstrap means:",
    capture_lines(res$boot_shapiro)
  )
)

add_text_page(
  "Question 2(c)-(d)",
  c(
    "The one-way ANOVA model strength ~ run tests whether the mean impact strength differs among production runs. Since the ANOVA p-value is less than 0.05, the null hypothesis that all run means are equal is rejected. This result shows strong evidence that the mean impact strength changes with production run.",
    "For the original ANOVA residuals, Shapiro-Wilk, Cramer-von Mises and Anderson-Darling all do not reject normality at the 5 percent level. However, the modified Brown-Forsythe Levene test from lawstat rejects the constant variance assumption at the 5 percent level. Thus, the original ANOVA residuals are acceptable for normality, but they show evidence of unequal variances."
  )
)

add_mono_pages(
  "Output for Questions 2(c)-(d)",
  c(
    "ANOVA table:",
    capture_lines(summary(res$fit)),
    "",
    "Normality tests for original residuals:",
    capture_lines(shapiro.test(res$insulation$resid)),
    capture_lines(nortest::cvm.test(res$insulation$resid)),
    capture_lines(nortest::ad.test(res$insulation$resid)),
    "",
    "Modified Brown-Forsythe Levene test for original residuals:",
    capture_lines(lawstat::levene.test(res$insulation$resid, res$insulation$run,
                                       location = "median",
                                       correction.method = "zero.correction"))
  )
)

qqnorm(res$insulation$resid, main = "Question 2(d): Original ANOVA residual Q-Q plot")
qqline(res$insulation$resid, col = "red", lwd = 2)

plot(res$fit$fitted.values, res$fit$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Question 2(d): Original residuals vs fitted values")
abline(h = 0, col = "red", lwd = 2)

add_text_page(
  "Question 2(e)-(f)",
  c(
    sprintf("Using boxcox from the MASS package, the optimal Box-Cox parameter was lambda = %.6f. The transformed response was computed using DescTools::BoxCox(strength, lambda), matching the Lab 3 solution pattern.", res$lambda),
    "For the Box-Cox transformed ANOVA, the production run effect remains significant with p-value less than 0.05.",
    "For the transformed residuals, Shapiro-Wilk rejects normality at the 5 percent level, while Cramer-von Mises and Anderson-Darling do not reject it. The modified Brown-Forsythe Levene test does not reject the constant variance assumption. The transformation improves the variance condition."
  )
)

MASS::boxcox(res$fit, lambda = seq(-3, 3, length = 600))
title("Question 2(e): Box-Cox profile log-likelihood")
abline(v = res$lambda, col = "red", lwd = 2)

add_mono_pages(
  "Output for Questions 2(e)-(f)",
  c(
    sprintf("Optimal lambda = %.6f", res$lambda),
    "",
    "Box-Cox transformed ANOVA table:",
    capture_lines(summary(res$fit_bc)),
    "",
    "Normality tests for transformed residuals:",
    capture_lines(shapiro.test(res$insulation$resid_bc)),
    capture_lines(nortest::cvm.test(res$insulation$resid_bc)),
    capture_lines(nortest::ad.test(res$insulation$resid_bc)),
    "",
    "Modified Brown-Forsythe Levene test for transformed residuals:",
    capture_lines(lawstat::levene.test(res$insulation$resid_bc, res$insulation$run,
                                       location = "median",
                                       correction.method = "zero.correction"))
  )
)

qqnorm(res$insulation$resid_bc, main = "Question 2(f): Box-Cox residual Q-Q plot")
qqline(res$insulation$resid_bc, col = "red", lwd = 2)

plot(res$fit_bc$fitted.values, res$fit_bc$residuals,
     xlab = "Fitted values", ylab = "Residuals",
     main = "Question 2(f): Box-Cox residuals vs fitted values")
abline(h = 0, col = "red", lwd = 2)

add_text_page(
  "Question 2(g)-(i)",
  c(
    "Using Scheffe adjustment on the Box-Cox transformed scale, the contrast comparing run 1 with the average of runs 2-5 is not significant at the 5 percent level. The contrast comparing run 1 with run 3 is significant. The contrast comparing the average of runs 1, 2 and 4 with the average of runs 3 and 5 is also significant.",
    "The Durbin-Watson test rejects the null hypothesis of no autocorrelation. This clear pattern occurs because the observations in the table are sorted within each production run, and not listed in random experimental order. Therefore, the residual plot mainly reflects the artificial order of the data, not necessarily time-based autocorrelation in the process.",
    "Cook's distances greater than 0.05 remove 3 observations, leaving 97 observations. The first 40 filtered records are included in the output below."
  )
)

add_mono_pages(
  "Output for Questions 2(g)-(i)",
  c(
    "Scheffe contrasts:",
    capture_lines(summary(res$scheffe_results, infer = c(TRUE, TRUE), level = 0.95)),
    "",
    "Durbin-Watson test:",
    capture_lines(car::durbinWatsonTest(res$fit_bc)),
    "",
    "Cook's distance filtering:",
    sprintf("Removed observations: %s", sum(res$insulation$cooksD > 0.05)),
    sprintf("Remaining observations: %s", nrow(res$filtered)),
    "",
    "First 40 records of filtered data:",
    capture_lines(print(res$filtered[1:40, ], row.names = FALSE)),
    "",
    "Removed observations:",
    capture_lines(print(res$insulation[res$insulation$cooksD > 0.05,
                                      c("run", "strength", "strength_bc", "cooksD")]))
  ),
  cex = 0.56,
  per_page = 52
)

add_text_page(
  "Question 3",
  c(
    "The model is y_ij = mu + tau_i + e_ij, where i = 1,...,a and j = 1,...,n, with constraint sum_i tau_i = 0. The least squares criterion is S(mu, tau) = sum_i sum_j (y_ij - mu - tau_i)^2.",
    "Using a Lagrange multiplier for the constraint, minimise L = sum_i sum_j (y_ij - mu - tau_i)^2 + 2 gamma sum_i tau_i.",
    "Differentiating with respect to mu gives -2 sum_i sum_j (y_ij - mu - tau_i) = 0. Therefore sum_i sum_j y_ij - an mu - n sum_i tau_i = 0. Since sum_i tau_i = 0, y_dotdot = an mu and mu_hat = ybar_dotdot.",
    "Differentiating with respect to tau_i gives -2 sum_j (y_ij - mu - tau_i) + 2 gamma = 0, so y_i_dot - n mu - n tau_i + gamma = 0. Summing over i shows gamma = 0. Substituting back gives y_i_dot - n ybar_dotdot - n tau_i = 0, hence tau_i_hat = ybar_i_dot - ybar_dotdot.",
    "The fitted value is yhat_ij = mu_hat + tau_i_hat = ybar_i_dot, and the residual is ehat_ij = y_ij - ybar_i_dot. Since ybar_i_dot is the mean of the n observations in treatment i, Var(ehat_ij) = Var(y_ij - ybar_i_dot) = Var(y_ij) + Var(ybar_i_dot) - 2 Cov(y_ij, ybar_i_dot).",
    "Now Var(y_ij) = sigma^2, Var(ybar_i_dot) = sigma^2/n, and Cov(y_ij, ybar_i_dot) = sigma^2/n because only the j-th term contributes to the covariance. Therefore Var(ehat_ij) = sigma^2 + sigma^2/n - 2 sigma^2/n = sigma^2(1 - 1/n)."
  )
)

add_mono_pages(
  "Appendix: R code used for the analysis",
  source_code,
  cex = 0.48,
  per_page = 58
)

dev.off()

cat("Created", out_file, "\n")
