## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE, cache=FALSE, 
               comment=NA, verbose=TRUE, fig.width=7, fig.height=5, dev='jpeg',dev.args=list(quality=80))

## ------------------------------------------------------------------------
library(scorepeak)
data("ecgca102")
local_peaks <- detect_localmaxima(ecgca102)
idx_true_peaks <- c(239, 255, 387, 439, 625)
plot(ecgca102, type = "l", main = "Local Peaks")
points(which(local_peaks), ecgca102[local_peaks], col = "red", pch = 19)
points(idx_true_peaks, ecgca102[idx_true_peaks], col = "blue", pch = 19)

## ------------------------------------------------------------------------
local_peaks_screened <- detect_localmaxima(ecgca102, 13)
score <- score_type1(ecgca102, 51)
plot(ecgca102, type = "l", main = "Screened Local Peaks", ylim = c(-0.38, 0.53))
points(which(local_peaks), ecgca102[local_peaks], col = "red", pch = 19)
points(seq(length(score)), score, type = "l", col = "green")

## ------------------------------------------------------------------------
true_peaks <- score > 0.03 & local_peaks_screened
plot(ecgca102, type = "l", main = "Detected True Peaks")
points(which(true_peaks), ecgca102[true_peaks], col = "blue", pch = 19)

## ----eval = requireNamespace("cluster", quietly = TRUE)------------------
classified_peaks <- cluster::pam(score, 2, cluster.only = TRUE)
cp1 <- classified_peaks == 1 & local_peaks_screened
cp2 <- classified_peaks == 2 & local_peaks_screened
plot(ecgca102, type = "l", main = "Classified Peaks")
points(which(cp1), ecgca102[cp1], col = "red", pch = 19)
points(which(cp2), ecgca102[cp2], col = "blue", pch = 19)

