source("src/fit_chisq.R")

set.seed(42)

n_rep <- 1000
n_inf_comp <- 6
n_per_inf_comp <- 10000

sim_test_stat <- function(samp_size_per, n_inf_comp) {
  mu <- rnorm(1)
  sigma <- rexp(1)
  x <- rnorm(samp_size_per * n_inf_comp, mu, sigma)

  lnl_1 <- dnorm(x, mean(x), sd(x), log = TRUE) |> sum()

  lnl_multi <- sapply(1:n_inf_comp, function(i) {
    xx <- x[((i - 1) * samp_size_per + 1):(i * samp_size_per)]
    lnl <- dnorm(xx, mean(xx), sd(xx), log = TRUE) |> sum()
    return(lnl)
  }) |> sum()

  return(2 * (lnl_multi - lnl_1))
}

stats <- replicate(n_rep, sim_test_stat(n_per_inf_comp, n_inf_comp))

est_df <- fitChiSquared(stats)

r <- range(stats)

pdf(
  fs::path("figures", "norm_dim.pdf"),
  width = 4,
  height = 4
)
par(mai = c(0.85, 0.85, 0.01, 0.01))
hist(stats, border = NA, freq = FALSE, breaks = 50, main = "", xlab = "LRT statistic")
curve(dchisq(x, 2 * (n_inf_comp - 1)), r[1], r[2], 1001, add = TRUE, col = "black")
legend("topright",
  legend = c("Fitted Chi-squared", "Simulated values"), border = NA, bty = "n",
  lty = c(1, NA), fill = c(NA, "grey"), col = c("black", NA), pch = c(NA, 16)
)
dev.off()
