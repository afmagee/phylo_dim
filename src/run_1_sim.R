library(phangorn)
library(fs)

doSim <- function(ntips, tree.length, iqtree, iqtree.opts, half.n = 10000, er = rep(1, 6), bf = rep(0.25, 4)) {
  er <- er / sum(er)
  bf <- bf / sum(bf)

  phy <- ape::rtree(ntips, rooted = FALSE, br = rexp)
  phy$edge.length <- tree.length / sum(phy$edge.length) * phy$edge.length

  n.sites <- half.n * 2

  aln <- simSeq(phy, l = n.sites, Q = er, bf = bf)

  tmp_dir <- tempfile()
  fs::dir_create(tmp_dir)

  aln_path <- fs::path(tmp_dir, "aln.phy")
  write.phyDat(
    aln,
    aln_path
  )

  aln1_path <- fs::path(tmp_dir, "aln1.phy")
  write.phyDat(
    subset(aln, select = 1:half.n, site.pattern = FALSE),
    aln1_path
  )

  aln2_path <- fs::path(tmp_dir, "aln2.phy")
  write.phyDat(
    subset(aln, select = half.n + (1:half.n), site.pattern = FALSE),
    aln2_path
  )

  fpaths <- c(aln1_path, aln2_path, aln_path)
  for (fpath in fpaths) {
    system(
      paste0(
        iqtree,
        " ",
        iqtree.opts,
        " ",
        " -s ",
        fpath,
        " > ",
        fs::path(
          tmp_dir,
          "out.txt"
        )
      )
    )
  }
  # fs::dir_ls(tmp_dir)

  lnL <- sapply(fpaths, function(fpath) {
    logf <- scan(
      paste0(fpath, ".log"),
      what = character(),
      sep = "\n"
    )
    score_line <- logf[grep("BEST SCORE FOUND", logf)]
    score <- as.numeric(
      trimws(
        strsplit(
          score_line,
          ":",
          fixed = TRUE
        )[[1]][2]
      )
    )
  })
  l0 <- lnL[3]
  l <- lnL[1] + lnL[2]
  lrt_stat <- -2 * (l0 - l)

  fs::dir_delete(tmp_dir)

  res <- lrt_stat
  names(res) <- NULL
  return(res)
}