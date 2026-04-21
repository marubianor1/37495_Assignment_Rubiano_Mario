library(MASS)
library(car)
library(emmeans)
library(nortest)
library(lawstat)
library(DescTools)

out_file <- "37495_Assignment_Rubiano_Mario.docx"
work_dir <- "docx_build"

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

clean_text <- function(x) {
  x <- gsub("\t", "  ", x)
  iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
}

capture_lines <- function(expr) clean_text(capture.output(expr))

wrap_text <- function(x, width = 95) {
  unlist(strwrap(clean_text(x), width = width, simplify = FALSE))
}

w_p <- function(text = "", bold = FALSE, size = 22, style = NULL) {
  lines <- wrap_text(text)
  if (length(lines) == 0) lines <- ""
  props <- ""
  if (!is.null(style)) props <- paste0("<w:pPr><w:pStyle w:val=\"", style, "\"/></w:pPr>")
  paste0(
    "<w:p>", props,
    paste0(vapply(lines, function(line) {
      rpr <- paste0("<w:rPr>", if (bold) "<w:b/>" else "", "<w:sz w:val=\"", size, "\"/></w:rPr>")
      paste0("<w:r>", rpr, "<w:t xml:space=\"preserve\">", xml_escape(line), "</w:t></w:r>")
    }, character(1)), collapse = "<w:br/>"),
    "</w:p>"
  )
}

w_pre <- function(lines) {
  lines <- clean_text(lines)
  lines <- unlist(lapply(lines, function(z) {
    if (nchar(z) <= 105) z else strwrap(z, width = 105)
  }))
  paste0(vapply(lines, function(line) {
    paste0(
      "<w:p><w:r><w:rPr><w:rFonts w:ascii=\"Courier New\" w:hAnsi=\"Courier New\"/>",
      "<w:sz w:val=\"16\"/></w:rPr><w:t xml:space=\"preserve\">",
      xml_escape(line), "</w:t></w:r></w:p>"
    )
  }, character(1)), collapse = "\n")
}

w_break <- function() {
  "<w:p><w:r><w:br w:type=\"page\"/></w:r></w:p>"
}

png_size <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  readBin(con, "raw", n = 16)
  width <- readBin(con, "integer", n = 1, size = 4, endian = "big", signed = FALSE)
  height <- readBin(con, "integer", n = 1, size = 4, endian = "big", signed = FALSE)
  c(width = width, height = height)
}

w_image <- function(path, rid, caption, max_width_in = 5.8) {
  sz <- png_size(path)
  width_emu <- round(max_width_in * 914400)
  height_emu <- round(width_emu * sz["height"] / sz["width"])
  paste0(
    "<w:p><w:r><w:drawing>",
    "<wp:inline distT=\"0\" distB=\"0\" distL=\"0\" distR=\"0\">",
    "<wp:extent cx=\"", width_emu, "\" cy=\"", height_emu, "\"/>",
    "<wp:docPr id=\"", sub("rId", "", rid), "\" name=\"", xml_escape(caption), "\"/>",
    "<a:graphic><a:graphicData uri=\"http://schemas.openxmlformats.org/drawingml/2006/picture\">",
    "<pic:pic><pic:nvPicPr><pic:cNvPr id=\"0\" name=\"", basename(path), "\"/>",
    "<pic:cNvPicPr/></pic:nvPicPr><pic:blipFill>",
    "<a:blip r:embed=\"", rid, "\"/><a:stretch><a:fillRect/></a:stretch>",
    "</pic:blipFill><pic:spPr><a:xfrm><a:off x=\"0\" y=\"0\"/>",
    "<a:ext cx=\"", width_emu, "\" cy=\"", height_emu, "\"/></a:xfrm>",
    "<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom></pic:spPr></pic:pic>",
    "</a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p>",
    w_p(caption, size = 18)
  )
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

run1 <- insulation$strength[insulation$run == 1]
n_run1 <- length(run1)
mean_run1 <- mean(run1)
sd_run1 <- sd(run1)
t_run1 <- (mean_run1 - 0.78) / (sd_run1 / sqrt(n_run1))
df_run1 <- n_run1 - 1
p_run1 <- 1 - pt(t_run1, df_run1)
lower_ci_run1 <- mean_run1 - qt(0.95, df_run1) * sd_run1 / sqrt(n_run1)

set.seed(37495)
boot_means <- replicate(100000, mean(sample(run1, replace = TRUE)))
set.seed(37495)
boot_shapiro <- shapiro.test(sample(boot_means, 50))

fit <- aov(strength ~ run, data = insulation)
insulation$resid <- resid(fit)
bc_grid <- boxcox(fit, lambda = seq(-3, 3, length = 600), plotit = FALSE)
lambda <- bc_grid$x[which.max(bc_grid$y)]
insulation$strength_bc <- DescTools::BoxCox(insulation$strength, lambda)
fit_bc <- aov(strength_bc ~ run, data = insulation)
insulation$resid_bc <- resid(fit_bc)

run_means <- emmeans(fit_bc, "run")
contrast_list <- list(
  "run1_vs_avg_run2_5" = c(1, -0.25, -0.25, -0.25, -0.25),
  "run1_vs_run3" = c(1, 0, -1, 0, 0),
  "avg_run124_vs_avg_run35" = c(1 / 3, 1 / 3, -1 / 2, 1 / 3, -1 / 2)
)
scheffe_results <- contrast(run_means, method = contrast_list, adjust = "scheffe")

insulation$cooksD <- cooks.distance(fit_bc)
filtered <- insulation[insulation$cooksD <= 0.05, c("strength_bc", "run")]

image_files <- c(
  "figures/q2b_bootstrap_histogram.png",
  "figures/q2d_original_residual_qq.png",
  "figures/q2d_original_residuals_vs_fitted.png",
  "figures/q2e_boxcox_profile.png",
  "figures/q2f_boxcox_residual_qq.png",
  "figures/q2f_boxcox_residuals_vs_fitted.png"
)

body <- c(
  w_p("37495 Statistical Design and Models for Evaluation Studies", bold = TRUE, size = 34),
  w_p("Assessment Task 2: R Assignment", bold = TRUE, size = 26),
  w_p("Student: Mario Rubiano"),
  w_p("Student ID: 24900627"),
  w_p("Submission file: 37495_Assignment_Rubiano_Mario.docx"),
  w_p("This report includes the R code and the output that is used in the answers. The analysis uses the R functions and packages introduced in the laboratory solution files, including aov, resid, shapiro.test, nortest::cvm.test, nortest::ad.test, lawstat::levene.test, MASS::boxcox, DescTools::BoxCox, emmeans, contrast with Scheffe adjustment, durbinWatsonTest, and cooks.distance."),
  w_p("Question 1", bold = TRUE, size = 28),
  w_p("An appropriate design for this situation is a randomised complete block design. The response variable is the yield of the chemical process. This yield must be measured in the units used by the plant, for example grams, kilograms, or percent yield. The experimental factor is the chemical input, with five treatment levels because there are five chemicals. The blocking factor is technician, with three blocks because there are three technicians."),
  w_p("There are 15 experimental units in total. Each experimental unit is one individual execution of the chemical process by one specific technician using one specified chemical input. The measurement unit is the physical batch or final product sample where the yield is measured. Within each technician block, the five chemicals should be randomly allocated to the five process runs done by that technician. In this way, each chemical has three observations, and the design controls for differences between technicians."),
  w_p("A suitable model is y_ij = mu + tau_i + beta_j + e_ij, where tau_i is the effect of chemical i, beta_j is the effect of technician j, and e_ij is the random error."),
  w_p("Question 2", bold = TRUE, size = 28),
  w_p("(a)", bold = TRUE, size = 24),
  w_p(sprintf("For production run 1, H0: mu_1 <= 0.78 and H1: mu_1 > 0.78. The sample mean is %.3f, t = %.4f, df = %.0f, and p-value = %.5f. The 95 percent one-sided lower confidence bound is %.7f. Since p > 0.05, there is not enough evidence at the 5 percent level to conclude that the mean impact strength for production run 1 is greater than 0.78.", mean_run1, t_run1, df_run1, p_run1, lower_ci_run1)),
  w_pre(c("Manual one-sample t-test output:", sprintf("mean = %.3f", mean_run1), sprintf("t = %.5f", t_run1), sprintf("df = %.0f", df_run1), sprintf("p-value = %.8f", p_run1), sprintf("95 percent one-sided lower confidence bound = %.7f", lower_ci_run1))),
  w_p("(b)", bold = TRUE, size = 24),
  w_p(sprintf("The bootstrap distribution was generated using 100000 re-samples with replacement. The bootstrap mean is approximately %.4f and the bootstrap standard deviation is approximately %.5f.", mean(boot_means), sd(boot_means))),
  w_p(sprintf("A Shapiro-Wilk test on a random sample of 50 bootstrap means gave W = %.5f and p-value = %.4f. Since p > 0.05, the test does not show evidence against normality for the sampled bootstrap means. Together with the histogram, this suggests that the bootstrap distribution is approximately normal.", boot_shapiro$statistic, boot_shapiro$p.value)),
  w_image(image_files[1], "rId1", "Question 2(b): Bootstrap distribution of run 1 sample mean."),
  w_pre(c("Bootstrap summary:", capture_lines(summary(boot_means)), "", "Shapiro-Wilk test:", capture_lines(boot_shapiro))),
  w_p("(c)", bold = TRUE, size = 24),
  w_p("The one-way ANOVA model strength ~ run tests whether mean impact strength differs among production runs. Since the ANOVA p-value is less than 0.05, the null hypothesis that all run means are equal is rejected. This result shows strong evidence that the mean impact strength changes with production run."),
  w_pre(c("ANOVA table:", capture_lines(summary(fit)))),
  w_p("(d)", bold = TRUE, size = 24),
  w_p("For the original ANOVA residuals, Shapiro-Wilk, Cramer-von Mises and Anderson-Darling all do not reject normality at the 5 percent level. However, the modified Brown-Forsythe Levene test from lawstat rejects the constant variance assumption at the 5 percent level. Thus, the original ANOVA residuals are acceptable for normality, but they show evidence of unequal variances."),
  w_pre(c("Normality tests for original residuals:", capture_lines(shapiro.test(insulation$resid)), capture_lines(nortest::cvm.test(insulation$resid)), capture_lines(nortest::ad.test(insulation$resid)), "", "Modified Brown-Forsythe Levene test for original residuals:", capture_lines(lawstat::levene.test(insulation$resid, insulation$run, location = "median", correction.method = "zero.correction")))),
  w_image(image_files[2], "rId2", "Question 2(d): Original ANOVA residual Q-Q plot."),
  w_image(image_files[3], "rId3", "Question 2(d): Original residuals vs fitted values."),
  w_p("(e)", bold = TRUE, size = 24),
  w_p(sprintf("Using boxcox from the MASS package, the optimal Box-Cox parameter was lambda = %.6f. The transformed response was computed using DescTools::BoxCox(strength, lambda), matching the Lab 3 solution pattern.", lambda)),
  w_image(image_files[4], "rId4", "Question 2(e): Box-Cox profile log-likelihood."),
  w_p("(f)", bold = TRUE, size = 24),
  w_p("For the Box-Cox transformed ANOVA, the production run effect remains significant with p-value less than 0.05. For the transformed residuals, Shapiro-Wilk rejects normality at the 5 percent level, while Cramer-von Mises and Anderson-Darling do not reject it. The modified Brown-Forsythe Levene test does not reject the constant variance assumption. The transformation improves the variance condition."),
  w_pre(c("Box-Cox transformed ANOVA table:", capture_lines(summary(fit_bc)), "", "Normality tests for transformed residuals:", capture_lines(shapiro.test(insulation$resid_bc)), capture_lines(nortest::cvm.test(insulation$resid_bc)), capture_lines(nortest::ad.test(insulation$resid_bc)), "", "Modified Brown-Forsythe Levene test for transformed residuals:", capture_lines(lawstat::levene.test(insulation$resid_bc, insulation$run, location = "median", correction.method = "zero.correction")))),
  w_image(image_files[5], "rId5", "Question 2(f): Box-Cox residual Q-Q plot."),
  w_image(image_files[6], "rId6", "Question 2(f): Box-Cox residuals vs fitted values."),
  w_p("(g)", bold = TRUE, size = 24),
  w_p("Using Scheffe adjustment on the Box-Cox transformed scale, the contrast comparing run 1 with the average of runs 2-5 is not significant at the 5 percent level. The contrast comparing run 1 with run 3 is significant. The contrast comparing the average of runs 1, 2 and 4 with the average of runs 3 and 5 is also significant."),
  w_pre(c("Scheffe contrasts:", capture_lines(summary(scheffe_results, infer = c(TRUE, TRUE), level = 0.95)))),
  w_p("(h)", bold = TRUE, size = 24),
  w_p("The Durbin-Watson test rejects the null hypothesis of no autocorrelation. This clear pattern occurs because the observations in the table are sorted within each production run, and not listed in random experimental order. Therefore, the residual plot mainly reflects the artificial order of the data, not necessarily time-based autocorrelation in the process."),
  w_pre(c("Durbin-Watson test:", capture_lines(durbinWatsonTest(fit_bc)))),
  w_p("(i)", bold = TRUE, size = 24),
  w_p("Cook's distances greater than 0.05 remove 3 observations, leaving 97 observations. The first 40 filtered records are included below."),
  w_pre(c(sprintf("Removed observations: %s", sum(insulation$cooksD > 0.05)), sprintf("Remaining observations: %s", nrow(filtered)), "", "First 40 records of filtered data:", capture_lines(print(filtered[1:40, ], row.names = FALSE)), "", "Removed observations:", capture_lines(print(insulation[insulation$cooksD > 0.05, c("run", "strength", "strength_bc", "cooksD")])))),
  w_p("Question 3", bold = TRUE, size = 28),
  w_p("The model is y_ij = mu + tau_i + e_ij, where i = 1,...,a and j = 1,...,n, with constraint sum_i tau_i = 0. The least squares criterion is S(mu, tau) = sum_i sum_j (y_ij - mu - tau_i)^2."),
  w_p("Using a Lagrange multiplier for the constraint, minimise L = sum_i sum_j (y_ij - mu - tau_i)^2 + 2 gamma sum_i tau_i."),
  w_p("Differentiating with respect to mu gives -2 sum_i sum_j (y_ij - mu - tau_i) = 0. Therefore sum_i sum_j y_ij - an mu - n sum_i tau_i = 0. Since sum_i tau_i = 0, y_dotdot = an mu and mu_hat = ybar_dotdot."),
  w_p("Differentiating with respect to tau_i gives -2 sum_j (y_ij - mu - tau_i) + 2 gamma = 0, so y_i_dot - n mu - n tau_i + gamma = 0. Summing over i shows gamma = 0. Substituting back gives y_i_dot - n ybar_dotdot - n tau_i = 0, hence tau_i_hat = ybar_i_dot - ybar_dotdot."),
  w_p("The fitted value is yhat_ij = mu_hat + tau_i_hat = ybar_i_dot, and the residual is ehat_ij = y_ij - ybar_i_dot. Since ybar_i_dot is the mean of the n observations in treatment i, Var(ehat_ij) = Var(y_ij - ybar_i_dot) = Var(y_ij) + Var(ybar_i_dot) - 2 Cov(y_ij, ybar_i_dot)."),
  w_p("Now Var(y_ij) = sigma^2, Var(ybar_i_dot) = sigma^2/n, and Cov(y_ij, ybar_i_dot) = sigma^2/n because only the j-th term contributes to the covariance. Therefore Var(ehat_ij) = sigma^2 + sigma^2/n - 2 sigma^2/n = sigma^2(1 - 1/n)."),
  w_break(),
  w_p("Appendix: R code used for the analysis", bold = TRUE, size = 28),
  w_pre(readLines("assignment_solution.R", warn = FALSE))
)

unlink(work_dir, recursive = TRUE, force = TRUE)
dir.create(file.path(work_dir, "_rels"), recursive = TRUE)
dir.create(file.path(work_dir, "word", "_rels"), recursive = TRUE)
dir.create(file.path(work_dir, "word", "media"), recursive = TRUE)

for (i in seq_along(image_files)) {
  file.copy(image_files[i], file.path(work_dir, "word", "media", paste0("image", i, ".png")))
}

writeLines(c(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
  '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
  '<Default Extension="xml" ContentType="application/xml"/>',
  '<Default Extension="png" ContentType="image/png"/>',
  '<Override PartName="/word/document.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>',
  '</Types>'
), file.path(work_dir, "[Content_Types].xml"))

writeLines(c(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="word/document.xml"/>',
  '</Relationships>'
), file.path(work_dir, "_rels", ".rels"))

rel_lines <- c(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
)
for (i in seq_along(image_files)) {
  rel_lines <- c(rel_lines, paste0(
    '<Relationship Id="rId', i,
    '" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/image',
    i, '.png"/>'
  ))
}
rel_lines <- c(rel_lines, '</Relationships>')
writeLines(rel_lines, file.path(work_dir, "word", "_rels", "document.xml.rels"))

document_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
  'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ',
  'xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" ',
  'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
  'xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">',
  '<w:body>',
  paste(body, collapse = "\n"),
  '<w:sectPr><w:pgSz w:w="12240" w:h="15840"/><w:pgMar w:top="1440" w:right="1080" w:bottom="1440" w:left="1080" w:header="720" w:footer="720" w:gutter="0"/></w:sectPr>',
  '</w:body></w:document>'
)
writeLines(document_xml, file.path(work_dir, "word", "document.xml"))

if (file.exists(out_file)) unlink(out_file)
old_wd <- getwd()
setwd(work_dir)
on.exit(setwd(old_wd), add = TRUE)
zip_status <- system(sprintf("zip -qr %s .", shQuote(file.path(old_wd, out_file))))
setwd(old_wd)

if (zip_status != 0) stop("Failed to create DOCX zip package")
cat("Created", out_file, "\n")
