# 37495 Assignment Draft

## Question 1

An appropriate design is a randomised complete block design. The response variable is the yield of the chemical process, measured in the yield units used by the plant, for example grams, kilograms, percent yield, or another specified production yield unit. The experimental factor is the chemical input, with five treatment levels corresponding to the five chemicals. The blocking factor is technician, with three blocks corresponding to the three technicians.

There are 15 experimental units in total: each technician should run one observation for each of the five chemical inputs. Within each technician block, randomly allocate the five chemicals to the five experimental units/runs handled by that technician. This gives three observations per chemical and controls for systematic differences among technicians.

The statistical model would be

```text
y_ij = mu + tau_i + beta_j + e_ij,
```

where `tau_i` is the effect of chemical `i`, `beta_j` is the effect of technician `j`, and `e_ij` is random error.

## Question 2

### (a)

For production run 1, the one-sample t-test is

```text
H0: mu_1 <= 0.78
H1: mu_1 > 0.78
```

The sample mean is 0.831. The test gives `t = 1.6719`, `df = 19`, and `p = 0.05546`. Since `p > 0.05`, there is insufficient evidence at the 5 percent level to conclude that the mean impact strength for production run 1 is greater than 0.78.

### (b)

Using `set.seed(37495)`, 100000 bootstrap resamples of production run 1 gave a bootstrap mean distribution with mean about 0.8309 and standard deviation about 0.02986. The histogram with the fitted normal density is saved by the R script as `figures/q2b_bootstrap_histogram.png`.

A Shapiro-Wilk test on a random sample of 50 bootstrap means gave `W = 0.96968` and `p = 0.2246`. Since `p > 0.05`, there is no evidence against normality for the sampled bootstrap means. Together with the histogram, the bootstrap distribution is approximately normal.

### (c)

For the ANOVA model `strength ~ run`, the ANOVA table is:

```text
            Df Sum Sq Mean Sq F value Pr(>F)
run          4  2.791  0.6978   61.33 <2e-16
Residuals   95  1.081  0.0114
```

Since `p < 0.05`, reject the null hypothesis that all production run means are equal. There is strong evidence that mean impact strength varies with production run.

### (d)

For the original ANOVA residuals, Shapiro-Wilk gave `W = 0.97714`, `p = 0.07935`, so normality is not rejected at the 5 percent level. Levene's test with median centre gave `F = 2.9995`, `p = 0.02227`, so constant variance is rejected at the 5 percent level.

### (e)

Using `boxcox` from the `MASS` package, the optimal Box-Cox parameter was approximately

```text
lambda = 0.245409
```

The transformed response used in the script is

```text
strength_bc = (strength^lambda - 1) / lambda
```

### (f)

For the Box-Cox transformed ANOVA, the ANOVA table is:

```text
            Df Sum Sq Mean Sq F value Pr(>F)
run          4  4.306  1.0765   66.75 <2e-16
Residuals   95  1.532  0.0161
```

Shapiro-Wilk for transformed residuals gave `W = 0.97188`, `p = 0.03082`, so normality is rejected at the 5 percent level. Levene's test gave `F = 1.2694`, `p = 0.2875`, so the constant variance assumption is not rejected after transformation.

### (g)

Using Scheffe adjustment through `emmeans`, the fitted contrasts on the Box-Cox transformed scale are:

```text
contrast                estimate     SE df lower.CL upper.CL t.ratio p.value
run1_vs_avg_run2_5         0.085 0.0317 95 -0.00532    0.175   2.679  0.0734
run1_vs_run3               0.213 0.0402 95  0.09892    0.328   5.310 <0.0001
avg_run124_vs_avg_run35    0.388 0.0259 95  0.31425    0.462  14.969 <0.0001
```

At the 5 percent level, contrast (i) is not significant, while contrasts (ii) and (iii) are significant.

### (h)

The apparent autocorrelation pattern occurs because the observations were entered in sorted order within each production run, not in random experimental order. In the data table, each run's 20 strengths are listed from small to large before moving to the next run. The residuals therefore inherit a visible sequence pattern from the ordering of the data. The Durbin-Watson result is responding to the artificial ordering in the table, not necessarily to time-based autocorrelation in the experimental process.

### (i)

The Cook's distance filtering rule `Cook's D <= 0.05` removes 3 observations, leaving 97 observations. The first 40 records of the filtered data set are:

```text
 strength_bc run
 -0.38142711   1
 -0.35467008   1
 -0.30286466   1
 -0.25315901   1
 -0.24103808   1
 -0.22903385   1
 -0.21714374   1
 -0.17067592   1
 -0.15932067   1
 -0.14806578   1
 -0.13690923   1
 -0.12584902   1
 -0.12584902   1
 -0.11488325   1
 -0.09322766   1
 -0.08253430   1
 -0.07192829   1
 -0.05097181   1
 -0.21714374   2
 -0.17067592   2
 -0.17067592   2
 -0.15932067   2
 -0.14806578   2
 -0.13690923   2
 -0.11488325   2
 -0.03034565   2
  0.00000000   2
  0.00996249   2
  0.02966627   2
  0.02966627   2
  0.05868752   2
  0.07769242   2
  0.08709543   2
  0.13315767   2
  0.15115612   2
  0.16006766   2
  0.16892191   2
 -0.60415333   3
 -0.60415333   3
 -0.50988698   3
```

The removed observations were the first observation in run 1, the twentieth observation in run 1, and the twentieth observation in run 2.

## Question 3

The model is

```text
y_ij = mu + tau_i + e_ij,  i = 1,...,a,  j = 1,...,n,
```

with constraint

```text
sum_i tau_i = 0.
```

The least squares criterion is

```text
S(mu, tau) = sum_i sum_j (y_ij - mu - tau_i)^2.
```

Using a Lagrange multiplier for the constraint, minimise

```text
L = sum_i sum_j (y_ij - mu - tau_i)^2 + 2 gamma sum_i tau_i.
```

Differentiating with respect to `mu` gives

```text
-2 sum_i sum_j (y_ij - mu - tau_i) = 0.
```

Therefore

```text
sum_i sum_j y_ij = an mu + n sum_i tau_i.
```

Since `sum_i tau_i = 0`,

```text
mu_hat = y_..
```

Differentiating with respect to `tau_i` gives

```text
-2 sum_j (y_ij - mu - tau_i) + 2 gamma = 0,
```

so

```text
sum_j y_ij - n mu - n tau_i = gamma.
```

Summing this equation over `i` gives

```text
sum_i sum_j y_ij - an mu - n sum_i tau_i = a gamma.
```

Using `mu_hat = y_..` and `sum_i tau_i = 0`, the left side is zero, hence `gamma = 0`. Therefore

```text
n y_i. - n mu - n tau_i = 0,
```

and

```text
tau_i_hat = y_i. - y_..
```

The fitted value is

```text
yhat_ij = mu_hat + tau_i_hat = y_i.
```

Therefore the residual is

```text
e_ij_hat = y_ij - y_i.
```

Because `y_i.` is the mean of the `n` observations in treatment `i`,

```text
Var(e_ij_hat) = Var(y_ij - y_i.)
              = Var(y_ij) + Var(y_i.) - 2 Cov(y_ij, y_i.).
```

Now `Var(y_ij) = sigma^2`, `Var(y_i.) = sigma^2 / n`, and

```text
Cov(y_ij, y_i.) = Cov(y_ij, (1/n) sum_k y_ik) = sigma^2 / n,
```

since only the term with `k = j` contributes to the covariance. Hence

```text
Var(e_ij_hat) = sigma^2 + sigma^2/n - 2 sigma^2/n
              = sigma^2(1 - 1/n).
```
