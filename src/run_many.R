source("src/run_1_sim.R")
source("src/fit_chisq.R")

iqt <- "~/phylo_programs/iqtree-2.2.2.6-Linux/bin/iqtree2"
iqto <- "-m JC69 -nt 2"

set.seed(42)

ntips <- 4:10
tl <- c(0.25, 0.5, 1.0)

fs::dir_create("output")

for (n in ntips) {
  for (treelen in tl) {
    lrt <- sapply(1:200, function(i) {
      doSim(n, treelen, iqt, iqto, half.n = 100000)
    })
    cat(
      lrt,
      sep = "\n",
      file = fs::path(
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
  }
}

# sapply(res, function(res_) {
#   sapply(res_, fitChiSquared)
# })

# 2 * ntips - 3