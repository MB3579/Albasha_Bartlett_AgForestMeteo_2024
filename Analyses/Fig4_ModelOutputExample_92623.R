setwd("~/Desktop/HydroShootData/simulation_results_summary") -> main_dir
list.dirs(main_dir, recursive=TRUE) -> list_dirs

length(list_dirs) -> tot_dirs

results_table_old = matrix(NA, nrow = 1, ncol = 10)
results_table = matrix(NA, nrow = 697, ncol = 10)

kk = 1

for (ii in 1:tot_dirs){
	cur_path = list_dirs[ii]
	
	if (length(list.files(path = cur_path, pattern = "\\.csv$")) > 0){
		setwd(cur_path)
		read.csv("time_series.csv", sep = ";", header = TRUE) -> ts

		as.data.frame(strsplit(cur_path, '/'))[7,1] -> results_table[1:697,1]
		as.data.frame(strsplit(cur_path, '/'))[8,1]  -> results_table[1:697,2]
		as.data.frame(strsplit(cur_path, '/'))[9,1]  -> results_table[1:697,3]
		as.data.frame(strsplit(cur_path, '/'))[10,1] -> results_table[1:697,4]
		
		ts$X[1:697] -> results_table[1:697,5]
		ts$E[1:697] -> results_table[1:697,6] # only sum the same # of days
		ts$An[1:697] -> results_table[1:697,7]
		ts$psi_leaf[1:697] -> results_table[1:697,8]
		ts$psi_soil[1:697] -> results_table[1:697,9]
		
		ts$Tleaf[1:697] -> results_table[1:697,10]
		
		rbind(results_table_old, results_table) -> results_table_old
		
		kk = kk + 1
	}
	
}

results_table_old[-1,] -> results_table
as.data.frame(results_table) -> results_table

c("Site", "Climate", "Orientation", "Stomatal", "Time", "E_L", "An_g", "PsiL", "PsiSoil", "Tleaf") -> colnames(results_table)

as.numeric(results_table$E_L) -> results_table$E_L
as.numeric(results_table$An_g) -> results_table$An_g
as.numeric(results_table$PsiL) -> results_table$PsiL
as.numeric(results_table$PsiSoil) -> results_table$PsiSoil
as.numeric(results_table$Tleaf) -> results_table$Tleaf
results_table$E_L/1000 -> results_table$E_Lh # L per hr
results_table$An_g*3600/1E6 -> results_table$An_mol #mol CO2 per hr



results_table[which(results_table$Climate == "historical" & results_table$Orientation == "north_south" & results_table$Site == "oakville"),] -> sub_res
results_table[which(results_table$Climate == "rcp45" & results_table$Orientation == "north_south" & results_table$Site == "oakville"),] -> sub_res2
results_table[which(results_table$Climate == "rcp85" & results_table$Orientation == "north_south" & results_table$Site == "oakville"),] -> sub_res3


results_table[which(results_table$Climate == "historical" & results_table$Orientation == "north_south" & results_table$Site == "fresno"),] -> sub_res4
results_table[which(results_table$Climate == "rcp45" & results_table$Orientation == "north_south" & results_table$Site == "fresno"),] -> sub_res5
results_table[which(results_table$Climate == "rcp85" & results_table$Orientation == "north_south" & results_table$Site == "fresno"),] -> sub_res6


unique(results_table$Stomatal) -> trait_levels



###########################################################################################
###########################################################################################
### FIGURE
###########################################################################################
###########################################################################################

dev.new(width = 6.5, height = 7.5)
plot.new()
par(family = 'serif', mfrow = c(4,4), mar = c(0, 0, 1, 0), oma = c(2, 7, 4, 3))

# Eplant, Napa, Hist 
ii =1
trait_levels[ii] -> foc_trait
plot(1:697, sub_res$E_Lh[which(sub_res$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(0, 2.6),  col = "blue", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(0, 2.6, by = 1), labels = TRUE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
text(x= 160, y = 2.5, expression(bold("Historical")), cex = 1.2)


# Eplant, Napa, RCP 4.5
plot(1:697, sub_res2$E_Lh[which(sub_res2$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(0, 2.6),  col = "blue", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(0, 2.6, by = 1), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
text(x= 170, y = 2.5, expression(bold("RCP 4.5")), cex = 1.2)


# Eplant, SJV, Hist 
ii =1
trait_levels[ii] -> foc_trait
plot(1:697, sub_res4$E_Lh[which(sub_res4$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(0, 2.6),  col = "blue", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(0, 2.6, by = 1), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
text(x= 160, y = 2.5, expression(bold("Historical")), cex = 1.2)


# Eplant, SJV, RCP 4.5
plot(1:697, sub_res5$E_Lh[which(sub_res4$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(0, 2.6),  col = "blue", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(0, 2.6, by = 1), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
text(x= 170, y = 2.5, expression(bold("RCP 4.5")), cex = 1.2)



# Aplant, Napa, Hist 
plot(1:697, sub_res$An_mol[which(sub_res$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(-0.1, 0.5),  col = "red", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(0, 0.5, by = 0.25), labels = TRUE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
#text(x= 115, y = 0.47, expression(bold("Hist.")), cex = 1.2)


# Aplant, Napa, RCP 4.5
plot(1:697, sub_res2$An_mol[which(sub_res2$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(-0.1, 0.5),  col = "red", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(0, 0.5, by = 0.25), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
#text(x= 200, y = 0.47, expression(bold("RCP 4.5")), cex = 1.2)


# Aplant, SJV, Hist 
ii =1
trait_levels[ii] -> foc_trait
plot(1:697, sub_res4$An_mol[which(sub_res4$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(-0.1, 0.5),  col = "red", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(0, 0.5, by = 0.25), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
#text(x= 115, y = 0.47, expression(bold("Hist.")), cex = 1.2)


# Aplant, SJV, RCP 4.5
plot(1:697, sub_res5$An_mol[which(sub_res4$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(-0.1, 0.5),  col = "red", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(0, 0.5, by = 0.25), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
#text(x= 200, y = 0.47, expression(bold("RCP 4.5")), cex = 1.2)


# PsiL, Napa, Hist
plot(1:697, sub_res$PsiL[which(sub_res$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(-2.1, -0.4),  col = "blue", axes = FALSE)
box(lty =1, col = "black")
lines(1:697, sub_res$PsiSoil[which(sub_res$Stomatal == foc_trait)], col = "black")
axis(2, at = c(-2, -1.5, -1, -0.5), labels = TRUE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
#text(x= 115, y = -0.47, expression(bold("Hist.")), cex = 1.2)
abline(h = -1.7, col = "blue", lwd =1, lty =2)


# PsiL, Napa, RCP 4.5
plot(1:697, sub_res2$PsiL[which(sub_res2$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(-2.1, -0.4),  col = "blue", axes = FALSE)
box(lty =1, col = "black")
lines(1:697, sub_res2$PsiSoil[which(sub_res2$Stomatal == foc_trait)], col = "black")
axis(2, at = c(-2, -1.5, -1, -0.5), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
#text(x= 195, y = -0.47, expression(bold("RCP 4.5")), cex = 1.2)
abline(h = -1.7, col = "blue", lwd =1, lty =2)


# PsiL, SJV, Hist
plot(1:697, sub_res4$PsiL[which(sub_res4$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(-2.1, -0.4),  col = "blue", axes = FALSE)
box(lty =1, col = "black")
lines(1:697, sub_res4$PsiSoil[which(sub_res4$Stomatal == foc_trait)], col = "black")
axis(2, at = c(-2, -1.5, -1, -0.5), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
#text(x= 115, y = -0.47, expression(bold("Hist.")), cex = 1.2)
abline(h = -1.7, col = "blue", lwd =1, lty =2)


# PsiL, SJV, RCP 4.5
plot(1:697, sub_res5$PsiL[which(sub_res5$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(-2.1, -0.4),  col = "blue", axes = FALSE)
box(lty =1, col = "black")
lines(1:697, sub_res5$PsiSoil[which(sub_res5$Stomatal == foc_trait)], col = "black")
axis(2, at = c(-2, -1.5, -1, -0.5), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = FALSE, las =1)
#text(x= 195, y = -0.47, expression(bold("RCP 4.5")), cex = 1.2)
abline(h = -1.7, col = "blue", lwd =1, lty =2)


# Tleaf, Napa, Hist
plot(1:697, sub_res$Tleaf[which(sub_res$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(10, 45),  col = "red", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(10, 50, 10), labels = TRUE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = c(0, 1, 2, 3, 4), las =1, cex.axis = 1.2)
#text(x= 115, y = 43, expression(bold("Hist.")), cex = 1.2)
abline(h = 40, col = "red", lwd =1, lty =2)	
	

# Tleaf, Napa, RCP 4.5
plot(1:697, sub_res2$Tleaf[which(sub_res2$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(10, 45),  col = "red", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(10, 50, 10), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = c(0, 1, 2, 3, 4), las =1, cex.axis = 1.2)
#text(x= 195, y = 43, expression(bold("RCP 4.5")), cex = 1.2)
abline(h = 40, col = "red", lwd =1, lty =2)


# Tleaf, SJV, Hist
plot(1:697, sub_res4$Tleaf[which(sub_res4$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(10, 45),  col = "red", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(10, 50, 10), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = c(0, 1, 2, 3, 4), las =1, cex.axis = 1.2)
#text(x= 115, y = 43, expression(bold("Hist.")), cex = 1.2)
abline(h = 40, col = "red", lwd =1, lty =2)	
	

# Tleaf, SJV, RCP 4.5
plot(1:697, sub_res5$Tleaf[which(sub_res5$Stomatal == foc_trait)], las =1, type = "l", xaxt = 'n', ylim = c(10, 45),  col = "red", axes = FALSE)
box(lty =1, col = "black")
axis(2, at = seq(10, 50, 10), labels = FALSE, las =1, cex.axis = 1.3)
axis(1, at = seq(1, 30*24, by = 7*24), labels = c(0, 1, 2, 3, 4), las =1, cex.axis = 1.2)
#text(x= 195, y = 43, expression(bold("RCP 4.5")), cex = 1.2)
abline(h = 40, col = "red", lwd =1, lty =2)


mtext(expression(paste(italic("E")["plant"], " (L h"^{"-1"}, ")")), side = 2, line = 4, cex = 1.2, outer = TRUE, adj = 0.92)

mtext(expression(paste(italic("A")["plant"], " (mol h"^{"-1"}, ")")), side = 2, line = 4, cex = 1.2, outer = TRUE, adj = 0.62)

mtext(expression(paste(Psi["canopy"], " or ", Psi["soil"], " (MPa)")), side = 2, line = 4, cex = 1.1, outer = TRUE, adj = 0.32)

mtext(expression(paste(italic("T")["canopy"], " (", degree, "C)")), side = 2, line = 4, cex = 1.2, outer = TRUE, adj = 0.05)


mtext(expression(paste(bold("Napa"))), side = 3, line = 1.5, cex = 1.1, outer = TRUE, adj = 0.22)

mtext(expression(paste(bold("SJV"))), side = 3, line = 1.5, cex = 1.1, outer = TRUE, adj = 0.77)

###########################################################################################
###########################################################################################

