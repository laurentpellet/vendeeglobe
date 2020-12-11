source("functions.R")

filesToDownload <- getMissingFiles()
invisible(lapply(filesToDownload, downloadFile))
invisible(lapply(filesToDownload, convertXLStoDTAndSave))

vg <- createCompleteSet()
write.csv2(vg, "vg.csv", row.names = F)

nbCoureurs <- 30
nbJours <- 6

g1 <- createGraphe(nbCoureurs,nbJours, "VMG24H")
g2 <- createGraphe(nbCoureurs,nbJours, "DTL")
g3 <- createGraphe(nbCoureurs,nbJours, "Vit24H")
g4 <- createGraphe(nbCoureurs,nbJours, "Dist24H")

g5 <- grid.arrange(g1,g2,g3,g4,ncol=2)

suppressMessages(ggsave(sprintf("plot.pdf", max(vg$dt)), g5, width = 40, height=25, units = "cm", dpi=300, device=cairo_pdf))
