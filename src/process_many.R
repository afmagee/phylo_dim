source("src/fit_chisq.R")

ntips <- 4:10
tl <- c(0.25, 0.5, 1.0)

fs::dir_create("figures")

dim.diff <- sapply(ntips, function(n) {
  sapply(tl, function(treelen) {
    lrt <- scan(
      fs::path(
        "output",
        paste0(
          "ntips_",
          n,
          "_tl_",
          treelen,
          ".txt"
        )
      )
    )
    fitChiSquared(lrt)
  })
})

png(
  fs::path("figures", "LRT_dim.png"),
  width = 4,
  height = 4
)
par(mai = c(0.85, 0.85, 0.01, 0.01))
plot(NULL, NULL,
  xlim = range(ntips), ylim = range(dim.diff),
  xlab = "Number of tips", ylab = "Dimension"
)
abline(a = -3, b = 2, col = "grey")
points(ntips, dim.diff[1, ],
  pch = 16, col = "black", cex = 1.5
)
legend("bottomright",
  legend = c("Fit", "Number of branches"), border = NA, bty = "n",
  lty = c(NA, 1), fill = c("black", NA), col = c(NA, "grey"), pch = c(16, NA)
)
dev.off()
