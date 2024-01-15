setwd("/Users/meganbartlett/Desktop/HydroShootData/canopy_example_plot")

read.csv("mtg19900730150000_oak_hist.pckl.csv")-> oh
read.csv("mtg19900716150000_oak_rcp45.pckl.csv")-> orcp
read.csv("mtg19900715150000_sjv_hist.pckl.csv")-> fh
read.csv("mtg19900702150000_sjv_rcp45.pckl.csv")-> frcp


####################################################################################
####################################################################################
#### Plot LWP vs. Height
####################################################################################
####################################################################################

dev.new(width = 9, height = 6.5)
plot.new()
par(family = 'serif', mfrow = c(2,2), mar = c(3, 2, 1, 1), oma = c(2, 4, 2, 2))

nlevels=5
seq(min(frcp$Rg), max(oh$Rg), length.out = nlevels)
colorRampPalette(c("blue", "gold"))(nlevels) -> col
col[cut(oh$Rg, nlevels)] -> colz

plot(oh$Z, oh$LWP, las =1, ylim = c(-2.1, -0.5), col = colz, xlab = " ", ylab = expression(paste(Psi[L], " (MPa)")), cex.axis = 1, cex.lab = 1.2, xaxt = 'n')
axis(1, at = seq(100, 200, by = 50), labels = TRUE, las =1, cex.axis = 1)

col[cut(orcp$Rg, nlevels)] -> colz45
plot(orcp$Z, orcp$LWP, las =1, ylim = c(-2.1, -0.5), col = colz45, xlab = " ", ylab = " ", yaxt = 'n', xaxt = 'n')
axis(1, at = seq(100, 200, by = 50), labels = TRUE, las =1, cex.axis = 1)
axis(2, at = seq(-2, -0.5, by = 0.5), labels = FALSE)

col[cut(fh$Rg, nlevels)] -> colzf

plot(fh$Z, fh$LWP, las =1, ylim = c(-2.1, -0.5), col = colzf, xlab = " ", ylab = expression(paste(Psi[L], " (MPa)")), cex.axis = 1, cex.lab = 1.2)
#axis(1, at = seq(100, 175, by = 25), labels = TRUE, las =1, cex.axis = 1)

col[cut(frcp$Rg, nlevels)] -> colz45f
plot(frcp$Z, frcp$LWP, las =1, ylim = c(-2.1, -0.5), col = colz45f, xlab = " ", ylab = " ", yaxt = 'n')
axis(2, at = seq(-2, -0.5, by = 0.5), labels = FALSE)

mtext(expression(paste(Psi["L"], " (MPa)")), side = 2, line = 2, cex = 1.2, outer = TRUE)
mtext("Height (m)", side = 1, line = 0, cex = 1, outer = TRUE)
mtext("Historical", side = 3, line = 0, cex = 0.9, outer = TRUE, adj = 0.25)
mtext("RCP 4.5", side = 3, line = 0, cex = 0.9, outer = TRUE, adj = 0.8)
#legend("topleft", legend = c(0, 220, 440, 660, 880), pch = 21, col = col)

####################################################################################
####################################################################################






####################################################################################
####################################################################################
#### Plot A vs. Rg
####################################################################################
####################################################################################

dev.new(width = 9, height = 6.5)
plot.new()
par(family = 'serif', mfrow = c(2,2), mar = c(3, 2, 1, 1), oma = c(2, 4, 2, 2))

nlevels=5
seq(min(frcp$LWP), max(oh$LWP), length.out = nlevels)
colorRampPalette(c("red", "cyan"))(nlevels) -> col
col[cut(oh$LWP, nlevels)] -> colz

plot(oh$Rg, oh$A, las =1, ylim = c(-1, 20), col = colz, xaxt = 'n')
axis(1, at = seq(100, 800, by = 200), labels = TRUE, las =1, cex.axis = 1)

col[cut(orcp$LWP, nlevels)] -> colz45
plot(orcp$Rg, orcp$A, las =1, ylim = c(-1, 20), col = colz45, ylab = " ", yaxt = 'n', xaxt = 'n')
axis(1, at = seq(100, 800, by = 200), labels = TRUE, las =1, cex.axis = 1)
axis(2, at = seq(-1, 20, by = 5), labels = FALSE, las =1, cex.axis = 1)
legend("bottomright", legend = c(-2.1, -1.8, -1.4, -1.1, -0.8), pch = 21, col = col)

col[cut(fh$LWP, nlevels)] -> colzf

plot(fh$Rg, fh$A, las =1, ylim = c(-1, 20), col = colzf, xaxt = 'n')
axis(1, at = seq(100, 800, by = 200), labels = TRUE, las =1, cex.axis = 1)

col[cut(frcp$LWP, nlevels)] -> colz45f
plot(frcp$Rg, frcp$A, las =1, ylim = c(-1, 20), col = colz45f, ylab = " ", xlab = "Height (m)", yaxt = 'n', xaxt = 'n')
axis(1, at = seq(100, 800, by = 200), labels = TRUE, las =1, cex.axis = 1)
axis(2, at = seq(-1, 20, by = 5), labels = FALSE, las =1, cex.axis = 1)

mtext(expression(paste(italic(A), " (", mu, "mol m-"^{"-2"}, " s-"^{"-1"}, ")")), side = 2, line = 2, cex = 1.2, outer = TRUE)
mtext(expression(paste("Solar Radiation (", mu, "mol m-"^{"-2"}, " s"^{"-1"}, ")")), side = 1, line = 0, cex = 1, outer = TRUE)
mtext("Historical", side = 3, line = 0, cex = 0.9, outer = TRUE, adj = 0.25)
mtext("RCP 4.5", side = 3, line = 0, cex = 0.9, outer = TRUE, adj = 0.8)

####################################################################################
####################################################################################




