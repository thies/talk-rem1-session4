try( setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

r <- read.csv("reits_2021.csv")

r <- subset(r, SP_GEOGRAPHY == "United States and Canada")

r$lev <- r$SP_TOTAL_DEBT / r$SP_TOTAL_ASSETS