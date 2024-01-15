
"~/Desktop/HydroShootData/simulation_results_summary" -> main_dir
list.dirs(main_dir, recursive=TRUE) -> list_dirs

length(list_dirs) -> tot_dirs

results_table = matrix(NA, nrow = 104, ncol = 8) 


kk = 1

for (ii in 1:tot_dirs){
	cur_path = list_dirs[ii]
	
	if (length(list.files(path = cur_path, pattern = "\\.csv$")) > 0){
		setwd(cur_path)
		read.csv("time_series.csv", sep = ";", header = TRUE) -> ts

		as.data.frame(strsplit(cur_path, '/'))[7,1] -> results_table[kk,1]
		as.data.frame(strsplit(cur_path, '/'))[8,1]  -> results_table[kk,2]
		as.data.frame(strsplit(cur_path, '/'))[9,1]  -> results_table[kk,3]
		as.data.frame(strsplit(cur_path, '/'))[10,1] -> results_table[kk,4]
		
		sum(ts$E[1:697]) -> results_table[kk,5] # only sum the same # of days
		sum(ts$An[1:697]) -> results_table[kk,6]
		sum(ts$irr[1:697]) -> results_table[kk,7]
				
		length(ts$E[1:697]) -> results_table[kk,8]
		
		kk = kk + 1
	}
	
}

c("Site", "Climate", "Orientation", "Stomatal", "TotalE_L", "TotalAn_g", "TotalIrr_L", "NumHours") -> colnames(results_table)

results_table[which(is.na(results_table[,1]) == FALSE),] -> results_table
as.data.frame(results_table) -> results_table
as.numeric(results_table$TotalE_L) -> results_table$TotalE_L
as.numeric(results_table$TotalAn_g) -> results_table$TotalAn_g
as.numeric(results_table$TotalIrr_L) -> results_table$TotalIrr_L
results_table$TotalAn_g/results_table$TotalE_L -> results_table$WUE
results_table$TotalAn_g/results_table$TotalE_L -> results_table$WUE #g per L

## Set the rows to normalize by - here, the historical simulations for -gsP50 from each site

results_table[which(results_table$Orientation == "north_south"),] -> results_table ###################
 
100*(results_table$TotalE_L[1:18] - results_table$TotalE_L[6])/results_table$TotalE_L[6] -> results_table$EPercent
100*(results_table$TotalAn_g[1:18] - results_table$TotalAn_g[6])/results_table$TotalAn_g[6] -> results_table$AnPercent
100*(results_table$WUE[1:18] - results_table$WUE[6])/results_table$WUE[6] -> results_table$WUEPercent
100*(results_table$TotalIrr_L[1:18] - results_table$TotalIrr_L[6])/results_table$TotalIrr_L[6] -> results_table$IrrPercent


100*(results_table$TotalE_L[19:36] - results_table$TotalE_L[24])/results_table$TotalE_L[24] -> results_table$EPercent[19:36]
100*(results_table$TotalAn_g[19:36] - results_table$TotalAn_g[24])/results_table$TotalAn_g[24] -> results_table$AnPercent[19:36]
100*(results_table$WUE[19:36] - results_table$WUE[24])/results_table$WUE[24] -> results_table$WUEPercent[19:36]
100*(results_table$TotalIrr_L[19:36] - results_table$TotalIrr_L[24])/results_table$TotalIrr_L[24] -> results_table$IrrPercent[19:36]




## Plot the comparisons
c(1, 6, 2, 5, 4, 3) -> results_table$Order 
results_table[order(results_table$Order),] -> results_table

results_table[which(results_table$Climate == "historical" & results_table$Orientation == "north_south" & results_table$Site == "oakville"),] -> sub_res
results_table[which(results_table$Climate == "rcp45" & results_table$Orientation == "north_south" & results_table$Site == "oakville"),] -> sub_res2
results_table[which(results_table$Climate == "rcp85" & results_table$Orientation == "north_south" & results_table$Site == "oakville"),] -> sub_res3
results_table[which(results_table$Climate == "rcp45_hot" & results_table$Orientation == "north_south" & results_table$Site == "oakville"),] -> sub_res4
results_table[which(results_table$Climate == "rcp85_hot" & results_table$Orientation == "north_south" & results_table$Site == "oakville"),] -> sub_res5

results_table[which(results_table$Climate == "historical" & results_table$Orientation == "north_south" & results_table$Site == "fresno"),] -> sub_res11
results_table[which(results_table$Climate == "rcp45" & results_table$Orientation == "north_south" & results_table$Site == "fresno"),] -> sub_res12
results_table[which(results_table$Climate == "rcp85" & results_table$Orientation == "north_south" & results_table$Site == "fresno"),] -> sub_res13
results_table[which(results_table$Climate == "rcp45_hot" & results_table$Orientation == "north_south" & results_table$Site == "fresno"),] -> sub_res14
results_table[which(results_table$Climate == "rcp85_hot" & results_table$Orientation == "north_south" & results_table$Site == "fresno"),] -> sub_res15

###########################################################################################
###########################################################################################
###########################################################################################







###########################################################################################
###########################################################################################
### PLOT
###########################################################################################
###########################################################################################

dev.new(width = 6.5, height = 8)
plot.new()
par(family = 'serif', mfrow = c(3,2), mar = c(3, 2, 2, 0), oma = c(4, 5, 1, 2))

# A, Oakville, Percent E
barplot(sub_res$EPercent, ylim = c(-100,100), las = 1, col = "white", border = "white", main = "Napa", cex.axis = 1.3, cex.main = 1.5) -> bp
abline(h = 0, lty = 2, col = "gray")
lines(bp[,1], sub_res$EPercent, col = "black", type = "b", lwd = 2)
lines(bp[,1], sub_res2$EPercent, col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub_res3$EPercent, col = "cyan", type = "b", lwd = 2)
#lines(bp[,1], sub_res4$EPercent, col = "blue", type = "b", lwd = 2, lty =2)
#lines(bp[,1], sub_res5$EPercent, col = "purple3", type = "b", lwd = 2, lty =2)
mtext(expression(paste(Delta, italic("E")["plant"], " (%)")), side = 2, line = 2, cex = 1.2, outer = TRUE, adj = 0.9)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
mtext("a", side = 3, line = -1, cex = 1.5, adj = 0.05)


# B, Fresno, Percent E
barplot(sub_res$EPercent, ylim = c(-100,100), las = 1, col = "white", border = "white", yaxt = 'n', main = "SJV", cex.axis = 1.3, cex.main = 1.5) -> bp
abline(h = 0, lty = 2, col = "gray")
lines(bp[,1], sub_res11$EPercent, col = "black", type = "b", lwd = 2)
lines(bp[,1], sub_res12$EPercent, col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub_res13$EPercent, col = "cyan", type = "b", lwd = 2)
#lines(bp[,1], sub_res14$EPercent, col = "blue", type = "b", lwd = 2, lty =2)
#lines(bp[,1], sub_res15$EPercent, col = "purple3", type = "b", lwd = 2, lty =2)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(-100, 100, by = 50), labels = FALSE)
mtext("d", side = 3, line = -1, cex = 1.5, adj = 0.05)
#legend(8, -75,  c("Hist.", "RCP 4.5", "RCP 8.5"), lty = 1, lwd = 2, bty = "o", col = c("black", "blue", "purple"), xpd = NA, cex = 1.2)


# C, Oakville, Percent An
barplot(sub_res$AnPercent, ylim = c(-50,100), las = 1, col = "white", border = "white", yaxt = 'n', cex.axis = 1.3) -> bp
abline(h = 0, lty = 2, col = "gray")
lines(bp[,1], sub_res$AnPercent, col = "black", type = "b", lwd = 2)
lines(bp[,1], sub_res2$AnPercent, col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub_res3$AnPercent, col = "cyan", type = "b", lwd = 2)
#lines(bp[,1], sub_res4$AnPercent, col = "blue", type = "b", lwd = 2, lty =2)
#lines(bp[,1], sub_res5$AnPercent, col = "purple3", type = "b", lwd = 2, lty =2)
mtext(expression(paste(Delta, italic("A")["plant"], " (%)")), side = 2, line = 2, cex = 1.2, outer = TRUE)
mtext("b", side = 3, line = -1, cex = 1.5, adj = 0.05)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(-50, 100, by = 25), labels = TRUE, las = 1, cex.axis = 1.2)


# D, Fresno, Percent An
barplot(sub_res$AnPercent, ylim = c(-50,100), las = 1, col = "white", border = "white", yaxt = 'n', cex.axis = 1.3) -> bp
abline(h = 0, lty = 2, col = "gray")
lines(bp[,1], sub_res11$AnPercent, col = "black", type = "b", lwd = 2)
lines(bp[,1], sub_res12$AnPercent, col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub_res13$AnPercent, col = "cyan", type = "b", lwd = 2)
#lines(bp[,1], sub_res14$AnPercent, col = "blue", type = "b", lwd = 2, lty =2)
#lines(bp[,1], sub_res15$AnPercent, col = "purple3", type = "b", lwd = 2, lty =2)
mtext("e", side = 3, line = -1, cex = 1.5, adj = 0.05)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(-50, 100, by = 25), labels = FALSE)



###########################################################################################
###########################################################################################
###########################################################################################

