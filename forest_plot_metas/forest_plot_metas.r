#install.packages(c("meta", "metafor", "forestplot"))

dat <- data.frame(
  study = c("I", "II", "III", "IV", "V"),
  a = c(63, 35, 300, 250, 4),
  n_t = c(100, 40, 500, 1000, 10),
  c = c(20, 10, 100, 300, 3),
  n_c = c(100, 20, 300, 1000, 10),
  stringsAsFactors = FALSE
)

dat$b <- dat$n_t - dat$a
dat$d <- dat$n_c - dat$c

library(meta)

m <- metabin(
  event.e = a,
  n.e     = n_t,
  event.c = c,
  n.c     = n_c,
  studlab = study,
  data    = dat,
  sm      = "OR",
  method  = "Inverse",
  method.tau = "DL",
  common  = TRUE,
  random  = TRUE
)

library(metafor)

es <- escalc(
  measure = "OR",
  ai = a, bi = b, ci = c, di = d,
  data = dat
)

res_RE <- rma(yi, vi, data = es, method = "REML")
res_FE <- rma(yi, vi, data = es, method = "FE")

library(forestplot)

dat$OR <- (dat$a * dat$d) / (dat$b * dat$c)
dat$logOR <- log(dat$OR)
dat$SE <- sqrt(1/dat$a + 1/dat$b + 1/dat$c + 1/dat$d)
dat$lower <- exp(dat$logOR - 1.96 * dat$SE)
dat$upper <- exp(dat$logOR + 1.96 * dat$SE)


cat("\n=== meta ===\n")
cat("Random-effects OR =", round(exp(m$TE.random), 3), "\n")
cat("95% CI            = [", round(exp(m$lower.random), 3), ";", round(exp(m$upper.random), 3), "]\n")
cat("Q =", round(m$Q, 3), "\n")
cat("p-value (Q) =", signif(m$pval.Q, 3), "\n")
cat("I^2 =", round(m$I2, 1), "%\n")
cat("tau^2 =", round(m$tau2, 3), "\n")

cat("\n=== metafor ===\n")
cat("Random-effects OR =", round(exp(res_RE$b[1,1]), 3), "\n")
cat("95% CI            = [", round(exp(res_RE$ci.lb), 3), ";", round(exp(res_RE$ci.ub), 3), "]\n")
cat("Q =", round(res_RE$QE, 3), "\n")
cat("p-value (Q) =", signif(res_RE$QEp, 3), "\n")
cat("I^2 =", round(res_RE$I2, 1), "%\n")
cat("tau^2 =", round(res_RE$tau2, 3), "\n")

#For Jupyter Notebook
#options(repr.plot.width = 8, repr.plot.height = 6)

forest(
  m,
  comb.fixed = FALSE,
  comb.random = TRUE,
  leftcols = "studlab",
  rightcols = c("effect", "ci"),
  rightlabs = c("OR", "95% CI"),
  xlab = "Odds Ratio",
  smlab = "OR"
)

forest(
  res_RE,
  slab = dat$study,
  atransf = exp,
  refline = 0,  # log(OR)=0 <=> OR=1
  xlab = "Odds Ratio",
  mlab = "Overall (random-effects)",
  cex = 1.0,
  psize = 1.2,
  at = log(c(0.25, 0.5, 1, 2, 4, 8, 16))
)


mean_vals  <- c(NA, dat$OR, exp(res_RE$b[1,1]))
lower_vals <- c(NA, dat$lower, exp(res_RE$ci.lb))
upper_vals <- c(NA, dat$upper, exp(res_RE$ci.ub))

labeltext <- cbind(
  c("Study", dat$study, "Overall"),
  c(
    "OR (95% CI)",
    paste0(
      sprintf("%.2f", dat$OR),
      " [", sprintf("%.2f", dat$lower),
      "; ", sprintf("%.2f", dat$upper), "]"
    ),
    paste0(
      sprintf("%.2f", exp(res_RE$b[1,1])),
      " [", sprintf("%.2f", exp(res_RE$ci.lb)),
      "; ", sprintf("%.2f", exp(res_RE$ci.ub)), "]"
    )
  )
)

forestplot(
  labeltext = labeltext,
  mean  = mean_vals,
  lower = lower_vals,
  upper = upper_vals,
  is.summary = c(TRUE, rep(FALSE, nrow(dat)), TRUE),
  zero = 1,
  xlog = TRUE,
  boxsize = 0.2,
  col = fpColors(box = "black", line = "black", summary = "black"),
  xlab = "Odds Ratio"
)
