fitChiSquared <- function(x) {
  guess <- round(mean(x))
  check <- seq(
    floor(guess / 2),
    ceiling(2 * guess),
    1
  )
  lnL <- sapply(check, function(df) {
    sum(dchisq(x, df, log = TRUE))
  })
  return(check[which.max(lnL)])
}