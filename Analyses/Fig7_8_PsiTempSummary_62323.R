# To reduce file size, we have included only the data used to generate the plots, from the hottest time of day (3pm), in a csv file in the Data folder ("Data_Wp_Temp_HottestTimeDay.csv"). The data for the other times will be made available upon request to the authors. You can load the data provided and skip directly to line 175 to reproduce the plots.   

setwd("/Users/meganbartlett/Desktop/HydroshootData/climate")

read.csv("Fre_RCP45_2079_99_ClimateCompiled_entireseason.csv", sep = ";") -> fre45
read.csv("Fre_RCP85_2079_99_ClimateCompiled_entireseason.csv", sep = ";") -> fre85

read.csv("Oak_RCP45_2079_99_ClimateCompiled_entireseason.csv", sep = ";") -> oak45
read.csv("Oak_RCP85_2079_99_ClimateCompiled_entireseason.csv", sep = ";") -> oak85

read.csv("Fresno_historical_climate_input_entireseason.csv", sep = ";") -> freH
read.csv("Oak_historical_climate_input_entireseason.csv", sep = ";") -> oakH



"~/Desktop/HydroShootData/simulation_results_leaflevel" -> main_dir
list.dirs(main_dir, recursive=TRUE) -> list_dirs

length(list_dirs) -> tot_dirs
 
results_table_old = matrix(NA, nrow = 1, ncol = 22) 

for (ii in 1:tot_dirs){
	cur_path = list_dirs[ii]
	
	if (length(list.files(path = cur_path, pattern = "\\.csv$")) > 0){
		
		setwd(cur_path)

		list.files(path = cur_path, pattern = "150000.p") -> file_list # for right now, focus on the hottest time of day, 3PM
		
		results_table = matrix(NA, nrow = 30, ncol = 22) ##
		
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
			
			if (file_site == "oakville" & file_clim == "historical"){
				oakH -> data_ref
				deparse(substitute(oakH)) -> results_table[kk,16]
			} else if (file_site == "oakville" & file_clim == "rcp45"){
				oak45 -> data_ref
				deparse(substitute(oak45)) -> results_table[kk,16]
			} else if (file_site == "oakville" & file_clim == "rcp85"){
				oak85 -> data_ref
				deparse(substitute(oak85)) -> results_table[kk,16]
			} else if (file_site == "fresno" & file_clim == "historical"){
				freH -> data_ref
				deparse(substitute(freH)) -> results_table[kk,16]
			} else if (file_site == "fresno" & file_clim == "rcp45"){
				fre45 -> data_ref
				deparse(substitute(fre45)) -> results_table[kk,16]
			} else if (file_site == "fresno" & file_clim == "rcp85"){
				fre85 -> data_ref
				deparse(substitute(fre85)) -> results_table[kk,16]
			}
			
			
			data_ref$Tac[which(is.na(match(data_ref$time, file_date_time)) == FALSE)] -> t_air
			t_air -> results_table[kk,17]
			file_date_time -> results_table[kk,18]
			data_ref$time[which(is.na(match(data_ref$time, file_date_time)) == FALSE)] -> results_table[kk,19]		
			mean(ts$LWP) -> results_table[kk,6]
			min(ts$LWP) -> results_table[kk,7]
			max(ts$LWP) -> results_table[kk,8]
			
			#max(ts$LWP) -> results_table[kk,9] ## sunlit mean?
			
			length(which(ts$LWP <= -1.7))/length(ts$LWP) -> results_table[kk,10] # percent canopy below TLP (mean date III Alsina 2008)
			
			mean(ts$Tleaf) -> results_table[kk,11]
			min(ts$Tleaf) -> results_table[kk,12]
			max(ts$Tleaf) -> results_table[kk,13]
			
			max(ts$Tleaf) -> results_table[kk,14] ## sunlit mean?
			
			length(which(ts$Tleaf >= 40))/length(ts$Tleaf) -> results_table[kk,15] # percent canopy above 40C
			length(which(ts$Tleaf >= t_air))/length(ts$Tleaf) -> results_table[kk,20] # percent canopy above 40C
			mean(ts$Tleaf) - t_air -> results_table[kk,21]
			max(ts$Tleaf) - t_air -> results_table[kk,22]
			
		}
		
		rbind(results_table_old, results_table) -> results_table_old

	}
	
}





c("File", "Site", "Climate", "Orientation", "Traits", "LWP_mean", "LWP_min", "LWP_max", "LWP_mean_sunlit", "PCanopy_TLP", "TL_mean", "TL_min", "TL_max", "TL_mean_sunlit", "PCanopy_T40", "Source", "Tair", "Date_ref1", "Date_ref2", "PCanopy_Tair", "Tmean_Diff", "Tmax_Diff") -> colnames(results_table_old)

results_table_old[-which(is.na(results_table_old[,1]) == TRUE),] -> results_table_old

as.data.frame(results_table_old) -> results_table_old

paste(results_table_old$Site, results_table_old$Climate, results_table_old$Orientation, results_table_old$Traits, sep = "_") -> results_table_old$Label

as.numeric(results_table_old[,6]) -> results_table_old[,6]
as.numeric(results_table_old[,7]) -> results_table_old[,7]
as.numeric(results_table_old[,8]) -> results_table_old[,8]
as.numeric(results_table_old[,9]) -> results_table_old[,9]
as.numeric(results_table_old[,10]) -> results_table_old[,10]
as.numeric(results_table_old[,11]) -> results_table_old[,11]
as.numeric(results_table_old[,12]) -> results_table_old[,12]
as.numeric(results_table_old[,13]) -> results_table_old[,13]
as.numeric(results_table_old[,14]) -> results_table_old[,14]
as.numeric(results_table_old[,15]) -> results_table_old[,15]
as.numeric(results_table_old[,17]) -> results_table_old[,17]
as.numeric(results_table_old[,20]) -> results_table_old[,20]
as.numeric(results_table_old[,21]) -> results_table_old[,21]
as.numeric(results_table_old[,22]) -> results_table_old[,22]



aggregate(LWP_mean ~ Label, data = results_table_old, FUN = mean) -> aa
aggregate(LWP_max ~ Label, data = results_table_old, FUN = mean) -> bb
aggregate(LWP_min ~ Label, data = results_table_old, FUN = mean) -> cc
aggregate(PCanopy_TLP ~ Label, data = results_table_old, FUN = mean) -> dd
aggregate(TL_mean ~ Label, data = results_table_old, FUN = mean) -> ee
aggregate(TL_max ~ Label, data = results_table_old, FUN = mean) -> ff
aggregate(TL_min ~ Label, data = results_table_old, FUN = mean) -> gg
aggregate(PCanopy_T40 ~ Label, data = results_table_old, FUN = mean) -> hh
aggregate(Tmean_Diff ~ Label, data = results_table_old, FUN = mean) -> ll
aggregate(Tmax_Diff ~ Label, data = results_table_old, FUN = mean) -> mm
aggregate(Tair ~ Label, data = results_table_old, FUN = mean) -> nn
aggregate(PCanopy_Tair ~ Label, data = results_table_old, FUN = mean) -> pp
aggregate(Tmean_Diff ~ Label, data = results_table_old, FUN = count_me_tair) -> qq


count_me_tlp <- function(x){
	length(which(x <= -1.7)) -> num_crit
	return(num_crit)
}

aggregate(LWP_mean ~ Label, data = results_table_old, FUN = count_me_tlp) -> ii

count_me_t40 <- function(x){
	length(which(x >= 40)) -> num_crit
	return(num_crit)
}

count_me_tair <- function(x){
	length(which(x >= 0)) -> num_crit
	return(num_crit)
}

aggregate(TL_mean ~ Label, data = results_table_old, FUN = count_me_t40) -> kk





###########################################################################################
###########################################################################################
### PLOT - WATER POTENTIAL
###########################################################################################
###########################################################################################

dev.new(width = 6.5, height = 8)
plot.new()
par(family = 'serif', mfrow = c(3,2), mar = c(3, 2, 2, 0), oma = c(4, 5, 1, 2))


# A, Oakville, Mean Canopy WP
barplot(aa$LWP_mean[19:24], ylim = c(-2.5, -0.5), las = 1, col = "white", border = "white", main = "Napa", cex.axis = 1.3, cex.main = 1.5) -> bp
#abline(h = 0, lty = 2, col = "gray")
aa[19:24,] -> sub1
aa[25:30,] -> sub2
aa[31:36,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub3[,2], col = "cyan", type = "b", lwd = 2)
mtext(expression(paste(bar(Psi)["canopy"], " (MPa)")), side = 2, line = 2, cex = 1.2, outer = TRUE, adj = 0.9)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
mtext("a", side = 3, line = -1, cex = 1.5, adj = 0.05)
abline(h = -1.7, lty = 2, col = 'gray')


# C, Fresno, Mean Canopy WP
barplot(aa$LWP_mean[1:6], ylim = c(-2.5, -0.5), las = 1, col = "white", border = "white", main = "SJV", cex.axis = 1.3, yaxt = 'n', cex.main = 1.5) -> bp
#abline(h = 0, lty = 2, col = "gray")
aa[1:6,] -> sub1
aa[7:12,] -> sub2
aa[13:18,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub3[,2], col = "cyan", type = "b", lwd = 2)
#mtext(expression(paste(Psi["canopy"], " (MPa)")), side = 2, line = 3, cex = 1.2)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(-0.5, -2.5, by = -0.5), labels = FALSE)
mtext("d", side = 3, line = -1, cex = 1.5, adj = 0.05)
abline(h = -1.7, lty = 2, col = 'gray')


# B, Oakville, days below TLP
barplot(aa$LWP_mean[19:24], ylim = c(-2, 35), las = 1, col = "white", border = "white", cex.axis = 1.3) -> bp
#abline(h = 0, lty = 2, col = "gray")
ii[19:24,] -> sub1
ii[25:30,] -> sub2
ii[31:36,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub3[,2], col = "cyan", type = "b", lwd = 2)
mtext(expression(paste("Days ", Psi["canopy"], " < ", italic("TLP"))), side = 2, line = 2, cex = 1.2, outer = TRUE)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
mtext("b", side = 3, line = -1, cex = 1.5, adj = 0.05)


# E, Fresno, days below TLP
barplot(aa$LWP_mean[19:24], ylim = c(-2, 35), las = 1, col = "white", border = "white", cex.axis = 1.3, yaxt = 'n') -> bp
#abline(h = 0, lty = 2, col = "gray")
ii[1:6,] -> sub1
ii[7:12,] -> sub2
ii[13:18,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub3[,2], col = "cyan", type = "b", lwd = 2)
#mtext(expression(paste("Days ", Psi["canopy"], " < TLP")), side = 2, line = 3, cex = 1.2)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(0, 35, by = 5), labels = FALSE)
mtext("e", side = 3, line = -1, cex = 1.5, adj = 0.05)


# C, Oakville, PLA WP
barplot(dd$PCanopy_TLP[19:24], ylim = c(-10, 100), las = 1, col = "white", border = "white",  cex.axis = 1.3) -> bp
#abline(h = 0, lty = 2, col = "gray")
dd[19:24,] -> sub1
dd[25:30,] -> sub2
dd[31:36,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], 100*sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], 100*sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], 100*sub3[,2], col = "cyan", type = "b", lwd = 2)
mtext(expression(paste(italic("PLA")["tlp"], " (%)")), side = 2, line = 2, cex = 1.2, outer = TRUE, adj = 0.1)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
mtext("c", side = 3, line = -1, cex = 1.5, adj = 0.05)

c("Base", expression(paste("+", italic(g)["max"])), expression(paste("-", italic(g)["s"], Psi["50"])), expression(paste("-", italic(g)["max"])), expression(paste("+", italic(g)["s"], Psi["50"])), "Elite") -> names_list
text(seq(1, 7, by = 1.2), par("usr")[3] - 5, srt =60, adj =1, xpd = NA, labels = names_list, cex = 1.75)


# F, Fresno, PLA WP
barplot(dd$PCanopy_TLP[19:24], ylim = c(-10, 100), las = 1, col = "white", border = "white", cex.axis = 1.3, yaxt = 'n') -> bp
#abline(h = 0, lty = 2, col = "gray")
dd[1:6,] -> sub1
dd[7:12,] -> sub2
dd[13:18,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], 100*sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], 100*sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], 100*sub3[,2], col = "cyan", type = "b", lwd = 2)
#mtext(expression(paste(italic("PLA")["tlp"], " (%)")), side = 2, line = 3, cex = 1.2)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(0, 100, 20), labels = FALSE)
mtext("f", side = 3, line = -1, cex = 1.5, adj = 0.05)

text(seq(1, 7, by = 1.2), par("usr")[3] - 5, srt =60, adj =1, xpd = NA, labels = names_list, cex = 1.75)

###########################################################################################
###########################################################################################











###########################################################################################
###########################################################################################
### TEMPERATURE
###########################################################################################
###########################################################################################

dev.new(width = 6.5, height = 8)
plot.new()
par(family = 'serif', mfrow = c(3,2), mar = c(3, 2, 2, 0), oma = c(4, 5, 1, 2))


# A, Oakville, Mean Canopy WP
barplot(aa$LWP_mean[19:24], ylim = c(25, 45), las = 1, col = "white", border = "white", main = "Napa", cex.axis = 1.3, cex.main = 1.5) -> bp
#abline(h = 0, lty = 2, col = "gray")
ee[19:24,] -> sub1
ee[25:30,] -> sub2
ee[31:36,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub3[,2], col = "cyan", type = "b", lwd = 2)
mtext(expression(paste(bar(italic("T"))["canopy"], " (", degree,"C)")), side = 2, line = 2, cex = 1.2, outer = TRUE, adj = 0.9)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
mtext("a", side = 3, line = -1, cex = 1.5, adj = 0.05)
abline(h = 40, lty = 2, col = 'gray')



# D, Fresno, Mean Canopy WP
barplot(aa$LWP_mean[1:6], ylim = c(25, 45), las = 1, col = "white", border = "white", main = "SJV", cex.axis = 1.3, yaxt = 'n', cex.main = 1.5) -> bp
#abline(h = 0, lty = 2, col = "gray")
ee[1:6,] -> sub1
ee[7:12,] -> sub2
ee[13:18,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub3[,2], col = "cyan", type = "b", lwd = 2)
#mtext(expression(paste(Psi["canopy"], " (MPa)")), side = 2, line = 3, cex = 1.2)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(25, 45, by = 5), labels = FALSE)
mtext("d", side = 3, line = -1, cex = 1.5, adj = 0.05)
abline(h = 40, lty = 2, col = 'gray')


# B, Oakville, days below TLP
barplot(aa$LWP_mean[19:24], ylim = c(-1.2, 0), las = 1, col = "white", border = "white", cex.axis = 1.3) -> bp
#abline(h = 0, lty = 2, col = "gray")
ll[19:24,] -> sub1
ll[25:30,] -> sub2
ll[31:36,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub3[,2], col = "cyan", type = "b", lwd = 2)
mtext(expression(paste(italic("T")["canopy"], " - ", italic("T")["air"], " (", degree, "C)")), side = 2, line = 2, cex = 1.2, outer = TRUE)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(-1.2, 0, by = 0.2), labels = FALSE)
mtext("b", side = 3, line = -1, cex = 1.5, adj = 0.05)


# E, Fresno, days above 40
barplot(aa$LWP_mean[19:24], ylim = c(-1.2, 0), las = 1, col = "white", border = "white", cex.axis = 1.3, yaxt = 'n') -> bp
#abline(h = 0, lty = 2, col = "gray")
ll[1:6,] -> sub1
ll[7:12,] -> sub2
ll[13:18,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], sub3[,2], col = "cyan", type = "b", lwd = 2)
#mtext(expression(paste("Days ", Psi["canopy"], " < TLP")), side = 2, line = 3, cex = 1.2)
axis(2, at = seq(-1.2, 0, 0.2), labels = FALSE)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)


mtext("e", side = 3, line = -1, cex = 1.5, adj = 0.05)
#par(xpd = NA)
#legend(8, 14, legend = c("Hist.", "RCP 4.5", "RCP 8.5"), lty = 1, bty = 'o', col = c('black', 'blue', 'purple'), cex = 1.2, xpd = FALSE)


# C, Oakville, PLA T40
barplot(dd$PCanopy_TLP[19:24], ylim = c(-10, 100), las = 1, col = "white", border = "white",  cex.axis = 1.3) -> bp
#abline(h = 0, lty = 2, col = "gray")
hh[19:24,] -> sub1
hh[25:30,] -> sub2
hh[31:36,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], 100*sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], 100*sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], 100*sub3[,2], col = "cyan", type = "b", lwd = 2)
mtext(expression(paste(italic("PLA")["T40"], " (%)")), side = 2, line = 2, cex = 1.2, outer = TRUE, adj = 0.1)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
mtext("c", side = 3, line = -1, cex = 1.5, adj = 0.05)

c("Base", expression(paste("+", italic(g)["max"])), expression(paste("-", italic(g)["s"], Psi["50"])), expression(paste("-", italic(g)["max"])), expression(paste("+", italic(g)["s"], Psi["50"])), "Elite") -> names_list
text(seq(1, 7, by = 1.2), par("usr")[3] - 2, srt =60, adj =1, xpd = NA, labels = names_list, cex = 1.75)


# F, Fresno, PLA T40
barplot(dd$PCanopy_TLP[19:24], ylim = c(-10, 100), las = 1, col = "white", border = "white", cex.axis = 1.3, yaxt = 'n') -> bp
#abline(h = 0, lty = 2, col = "gray")
hh[1:6,] -> sub1
hh[7:12,] -> sub2
hh[13:18,] -> sub3
c(1, 6, 2, 5, 4, 3) -> sub1$Order
c(1, 6, 2, 5, 4, 3) -> sub2$Order
c(1, 6, 2, 5, 4, 3) -> sub3$Order
sub1[order(sub1$Order),] -> sub1
sub2[order(sub2$Order),] -> sub2
sub3[order(sub3$Order),] -> sub3

lines(bp[,1], 100*sub1[,2], col = "black", type = "b", lwd = 2)
lines(bp[,1], 100*sub2[,2], col = "blue", type = "b", lwd = 2)
lines(bp[,1], 100*sub3[,2], col = "cyan", type = "b", lwd = 2)
#mtext(expression(paste(italic("PLA")["tlp"], " (%)")), side = 2, line = 3, cex = 1.2)
axis(1, at = c(0.75, 1.95, 3.15, 4.35, 5.55, 6.75), labels = FALSE)
axis(2, at = seq(-10, 100, by = 20), labels = FALSE)

mtext("f", side = 3, line = -1, cex = 1.5, adj = 0.05)
text(seq(1, 7, by = 1.2), par("usr")[3] - 2, srt =60, adj =1, xpd = NA, labels = names_list, cex = 1.75)

###########################################################################################
###########################################################################################

