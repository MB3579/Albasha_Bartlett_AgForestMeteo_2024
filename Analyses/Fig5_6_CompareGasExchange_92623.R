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

## Set the rows to normalize by - here, the baseline historical simulations from each site

results_table[which(results_table$Orientation == "north_south"),] -> results_table ###################
 
100*(results_table$TotalE_L[1:18] - results_table$TotalE_L[1])/results_table$TotalE_L[1] -> results_table$EPercent
100*(results_table$TotalAn_g[1:18] - results_table$TotalAn_g[1])/results_table$TotalAn_g[1] -> results_table$AnPercent
100*(results_table$WUE[1:18] - results_table$WUE[1])/results_table$WUE[1] -> results_table$WUEPercent
100*(results_table$TotalIrr_L[1:18] - results_table$TotalIrr_L[1])/results_table$TotalIrr_L[1] -> results_table$IrrPercent


100*(results_table$TotalE_L[19:36] - results_table$TotalE_L[19])/results_table$TotalE_L[19] -> results_table$EPercent[19:36]
100*(results_table$TotalAn_g[19:36] - results_table$TotalAn_g[19])/results_table$TotalAn_g[19] -> results_table$AnPercent[19:36]
100*(results_table$WUE[19:36] - results_table$WUE[19])/results_table$WUE[19] -> results_table$WUEPercent[19:36]
100*(results_table$TotalIrr_L[19:36] - results_table$TotalIrr_L[19])/results_table$TotalIrr_L[19] -> results_table$IrrPercent[19:36]




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



# E, Oakville, Percent WUE
barplot(sub_res$WUEPercent, ylim = c(-50,150), las = 1, col = "white", border = "white", yaxt = 'n', cex.axis = 1.3) -> bp
abline(h = 0, lty = 2, col = "gray")
lines(bp[,1], sub_res$WUEPercent, col = "black", type = "b", lwd = 2)
lines(bp[,1], sub_res2$WUEPercent, col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub_res3$WUEPercent, col = "cyan", type = "b", lwd = 2)
mtext(expression(paste(Delta, italic("WUE"), " (%)")), side = 2, line = 2, cex = 1.2, outer = TRUE, adj = 0.1)
mtext("c", side = 3, line = -1, cex = 1.5, adj = 0.05)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(-50, 150, by = 50), labels = TRUE, las = 1, cex.axis = 1.2)

c("Base", expression(paste("+", italic(g)["max"])), expression(paste("-", italic(g)["s"], Psi["50"])), expression(paste("-", italic(g)["max"])), expression(paste("+", italic(g)["s"], Psi["50"])), "Elite") -> names_list
text(seq(1, 7, by = 1.2), par("usr")[3] - 15, srt =60, adj =1, xpd = NA, labels = names_list, cex = 1.75)



# F, Fresno, Percent WUE
barplot(sub_res$WUEPercent, ylim = c(-50,150), las = 1, col = "white", border = "white", yaxt = 'n', cex.axis = 1.3) -> bp
abline(h = 0, lty = 2, col = "gray")
lines(bp[,1], sub_res11$WUEPercent, col = "black", type = "b", lwd = 2)
lines(bp[,1], sub_res12$WUEPercent, col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub_res13$WUEPercent, col = "cyan", type = "b", lwd = 2)
mtext("f", side = 3, line = -1, cex = 1.5, adj = 0.05)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(-50, 150, by = 50), labels = FALSE)

c("Base", expression(paste("+", italic(g)["max"])), expression(paste("-", italic(g)["s"], Psi["50"])), expression(paste("-", italic(g)["max"])), expression(paste("+", italic(g)["s"], Psi["50"])), "Elite") -> names_list
text(seq(1, 7, by = 1.2), par("usr")[3] - 15, srt =60, adj =1, xpd = NA, labels = names_list, cex = 1.75)

###########################################################################################
###########################################################################################
###########################################################################################









###########################################################################################
###########################################################################################
### Calculate mean daily gs
###########################################################################################
###########################################################################################

"~/Desktop/HydroShootData/simulation_results_leaflevel" -> main_dir
list.dirs(main_dir, recursive=TRUE) -> list_dirs

length(list_dirs) -> tot_dirs
 
results_table_old = matrix(NA, nrow = 1, ncol = 13) 

for (ii in 1:tot_dirs){
	cur_path = list_dirs[ii]
	
	if (length(list.files(path = cur_path, pattern = "\\.csv$")) > 0){
		
		setwd(cur_path)

		list.files(path = cur_path) -> file_list # all timepoints

		
		results_table = matrix(NA, nrow = 721, ncol = 13) ##
		
		for (kk in 1:length(file_list)){
			
			read.csv(file_list[kk], sep = ",", header = TRUE) -> ts
			file_list[kk] -> file_name
			
			file_name -> results_table[kk, 1]
			
			as.data.frame(strsplit(cur_path, '/'))[7,1] -> file_site
			as.data.frame(strsplit(cur_path, '/'))[8,1]  -> file_clim
			
			file_site -> results_table[kk,2]
			file_clim -> results_table[kk,3]
			as.data.frame(strsplit(cur_path, '/'))[9,1]  -> results_table[kk,4]
			as.data.frame(strsplit(cur_path, '/'))[10,1] -> results_table[kk,5]		
			
			substr(file_name, 4, 7) -> file_year
			substr(file_name, 8, 9) -> file_mo
			substr(file_name, 10, 11) -> file_day
			substr(file_name, 12, 13) -> file_hr
			paste(file_year, file_mo, file_day, sep = "-") -> file_date
			paste(file_hr, ":00:00", sep = "") -> file_time
			paste(file_date, file_time, sep =" ") -> file_date_time
			
			file_date -> results_table[kk,6]
			file_time -> results_table[kk,7]
			file_hr -> results_table[kk,8]
			file_date_time -> results_table[kk,9]
			
			mean(ts$gs) -> results_table[kk,10]
			mean(ts$An) -> results_table[kk,11]
			mean(ts$LWP) -> results_table[kk,12]
			mean(ts$Tleaf) -> results_table[kk,13]
			
		}
		
		rbind(results_table_old, results_table) -> results_table_old

	}
	
}





c("File", "Site", "Climate", "Orientation", "Traits", "Date", "Time", "Hour", "Date_Time", "MeanGs", "MeanA", "MeanLWP", "MeanTleaf") -> colnames(results_table_old)

results_table_old[-which(is.na(results_table_old[,1]) == TRUE),] -> results_table_old

as.data.frame(results_table_old) -> results_table_old

paste(results_table_old$Site, results_table_old$Climate, results_table_old$Orientation, results_table_old$Traits, sep = "_") -> results_table_old$Label

as.numeric(results_table_old[,8]) -> results_table_old[,8]
as.numeric(results_table_old[,10]) -> results_table_old[,10]
as.numeric(results_table_old[,11]) -> results_table_old[,11]
as.numeric(results_table_old[,12]) -> results_table_old[,12]
as.numeric(results_table_old[,13]) -> results_table_old[,13]


results_table_old[which(results_table_old$Hour > 8 & results_table_old$Hour < 17),] -> results_table_sub

aggregate(MeanGs ~ Label, data = results_table_sub, FUN = mean) -> mean_gs
aggregate(Climate ~ Label, data = results_table_sub, FUN = unique) -> aa1
aggregate(Site ~ Label, data = results_table_sub, FUN = unique) -> bb1
aggregate(Traits ~ Label, data = results_table_sub, FUN = unique) -> cc1

cbind(mean_gs, aa1[,2], bb1[,2], cc1[,2]) -> mean_gs
c("Label", "MeanGs", "Climate", "Site", "Traits") -> colnames(mean_gs)


## Plot the comparisons
c(1, 6, 2, 5, 4, 3) -> mean_gs$Order 
mean_gs[order(mean_gs$Order),] -> mean_gs

mean_gs[which(mean_gs$Climate == "historical" & mean_gs$Site == "oakville"),] -> sub_res_gs
mean_gs[which(mean_gs$Climate == "rcp45" & mean_gs$Site == "oakville"),] -> sub_res2_gs
mean_gs[which(mean_gs$Climate == "rcp85" & mean_gs$Site == "oakville"),] -> sub_res3_gs


mean_gs[which(mean_gs$Climate == "historical" & mean_gs$Site == "fresno"),] -> sub_res11_gs
mean_gs[which(mean_gs$Climate == "rcp45" & mean_gs$Site == "fresno"),] -> sub_res12_gs
mean_gs[which(mean_gs$Climate == "rcp85" & mean_gs$Site == "fresno"),] -> sub_res13_gs



###########################################################################################
###########################################################################################
###########################################################################################










###########################################################################################
###########################################################################################
### Mean gs
###########################################################################################
###########################################################################################


dev.new(width = 6.5, height = 4.5)
plot.new()
par(family = 'serif', mfrow = c(1,2), mar = c(3, 2, 2, 1), oma = c(3, 5, 1, 2))


# a, Oakville, Mean gs
barplot(sub_res_gs$MeanGs*1000, ylim = c(0,150), las = 1, col = "white", border = "white", yaxt = 'n', cex.axis = 1.3) -> bp
abline(h = 0, lty = 2, col = "gray")
lines(bp[,1], sub_res_gs$MeanGs*1000, col = "black", type = "b", lwd = 2)
lines(bp[,1], sub_res2_gs$MeanGs*1000, col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub_res3_gs$MeanGs*1000, col = "cyan", type = "b", lwd = 2)
mtext(expression(paste(italic("g")["s"], " (mmol m"^{"-2"}, " s"^{"-1"}, ")")), side = 2, line = 2, cex = 1.4, outer = TRUE)
mtext("a", side = 3, line = -1, cex = 1.4, adj = 0.05)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(0, 150, by = 50), labels = TRUE, las = 1, cex.axis = 1.2)

c("Base", expression(paste("+", italic(g)["max"])), expression(paste("-", italic(g)["s"], Psi["50"])), expression(paste("-", italic(g)["max"])), expression(paste("+", italic(g)["s"], Psi["50"])), "Elite") -> names_list
text(seq(1, 7, by = 1.2), par("usr")[3] - 15, srt =60, adj =1, xpd = NA, labels = names_list, cex = 1.2)




# F, Fresno, Mean Gs
barplot(sub_res11_gs$MeanGs*1000, ylim = c(0,150), las = 1, col = "white", border = "white", yaxt = 'n', cex.axis = 1.3) -> bp
abline(h = 0, lty = 2, col = "gray")
lines(bp[,1], sub_res11_gs$MeanGs*1000, col = "black", type = "b", lwd = 2)
lines(bp[,1], sub_res12_gs$MeanGs*1000, col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub_res13_gs$MeanGs*1000, col = "cyan", type = "b", lwd = 2)
mtext("b", side = 3, line = -1, cex = 1.5, adj = 0.05)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(0, 150, by = 50), labels = TRUE, las = 1, cex.axis = 1.2)

c("Base", expression(paste("+", italic(g)["max"])), expression(paste("-", italic(g)["s"], Psi["50"])), expression(paste("-", italic(g)["max"])), expression(paste("+", italic(g)["s"], Psi["50"])), "Elite") -> names_list
text(seq(1, 7, by = 1.2), par("usr")[3] - 15, srt =60, adj =1, xpd = NA, labels = names_list, cex = 1.2)


#legend(8, 200,  c("Hist.", "RCP 4.5", "RCP 8.5"), lty = 1, lwd = 2, bty = "o", col = c("black", "blue", "cyan"), xpd = NA, cex = 1.2)
