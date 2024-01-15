setwd("HydroShootData/climate") # update with desired wd

read.csv("Fre_DailyClimateVars_2079_99_RCP45_entireseason.csv", sep = ",") -> fre45
read.csv("Fre_DailyClimateVars_2079_99_RCP85_entireseason.csv", sep = ",") -> fre85

read.csv("Oak_DailyClimateVars_2079_99_RCP45_entireseason.csv", sep = ",") -> oak45
read.csv("Oak_DailyClimateVars_2079_99_RCP85_entireseason.csv", sep = ",") -> oak85

read.csv("Fresno_historical_climate_input_entireseason.csv", sep = ";") -> freH
read.csv("Oak_historical_climate_input_entireseason.csv", sep = ";") -> oakH

substr(freH$time, 1, 10) -> freH$Day # need to calculate daily from hourly values for the historical data
substr(oakH$time, 1, 10) -> oakH$Day


aggregate(Tac ~ Day, data = freH, FUN = max) -> tmaxF
aggregate(Tac ~ Day, data = freH, FUN = min) -> tminF
aggregate(Tac ~ Day, data = oakH, FUN = max) -> tmaxO
aggregate(Tac ~ Day, data = oakH, FUN = min) -> tminO

aggregate(hs ~ Day, data = freH, FUN = max) -> rhmaxF
aggregate(hs ~ Day, data = freH, FUN = min) -> rhminF
aggregate(hs ~ Day, data = oakH, FUN = max) -> rhmaxO
aggregate(hs ~ Day, data = oakH, FUN = min) -> rhminO

aggregate(Rg ~ Day, data = freH, FUN = max) -> rgF
aggregate(Rg ~ Day, data = oakH, FUN = max) -> rgO

aggregate(u ~ Day, data = freH, FUN = max) -> uF
aggregate(u ~ Day, data = oakH, FUN = max) -> uO



dev.new(width = 6.5, height = 9)
plot.new()
par(family = 'serif', mfrow = c(4,2), mar = c(0, 0, 3, 0), oma = c(4, 6, 1, 10))

# A) Oakville, Temp
plot(1:274, tmaxO[1:274,2], type = "l", col = "black", ylim = c(0,50), las = 1, xaxt = 'n', cex.axis = 1.2, main = "Napa", cex.main = 1.4)
lines(1:274, oak85$TmaxC[1:274], type = "l", col = "cyan")
lines(1:274, oak45$TmaxC[1:274], type = "l", col = "blue")

lines(1:274, tminO[1:274,2], type = "l", col = "black", lty =3)
lines(1:274, oak85$TminC[1:274], type = "l", col = "cyan", lty =3)
lines(1:274, oak45$TminC[1:274], type = "l", col = "blue", lty =3)

arrows(211, 50, 211, 45,  col = "black", length = 0.1)
arrows(197, 50, 197, 45,  col = "blue", length = 0.1)
arrows(187, 50, 187, 45,  col = "cyan", length = 0.1)

mtext(expression(paste(italic("T")["air"], " (", degree, "C)")), side = 2, line = 4, cex = 1.2, outer = TRUE, adj = 0.9)
axis(1, at = seq(1, 274, 31), labels = FALSE)
mtext("a", side = 3, line = -1.7, cex = 1.3, adj = 0.05)




# B) Fresno, Temp
plot(1:274, tmaxF[1:274,2], type = "l", col = "black", ylim = c(0,50), las = 1, xaxt = 'n', cex.axis = 1.2, yaxt = 'n', main = "SJV", cex.main = 1.4)
lines(1:274, fre85$TmaxC[1:274], type = "l", col = "cyan")
lines(1:274, fre45$TmaxC[1:274], type = "l", col = "blue")

lines(1:274, tminF[1:274,2], type = "l", col = "black", lty =3)
lines(1:274, fre85$TminC[1:274], type = "l", col = "cyan", lty =3)
lines(1:274, fre45$TminC[1:274], type = "l", col = "blue", lty =3)

arrows(196, 50, 196, 45,  col = "black", length = 0.1)
arrows(183, 50, 183, 45,  col = "blue", length = 0.1)
arrows(176, 50, 176, 45,  col = "cyan", length = 0.1)
axis(1, at = seq(1, 274, 31), labels = FALSE)
mtext("b", side = 3, line = -1.7, cex = 1.3, adj = 0.05)



# C) Oakville, RH
plot(1:274, rhmaxO[1:274,2], type = "l", col = "black", ylim = c(0,120), las = 1, xaxt = 'n', cex.axis = 1.2)
lines(1:274, oak85$RHmax[1:274], type = "l", col = "cyan")
lines(1:274, oak45$RHmax[1:274], type = "l", col = "blue")

lines(1:274, rhminO[1:274,2], type = "l", col = "black", lty =3)
lines(1:274, oak85$RHmin[1:274], type = "l", col = "cyan", lty =3)
lines(1:274, oak45$RHmin[1:274], type = "l", col = "blue", lty =3)

mtext(expression(paste("Relative Humidity", " (%)")), side = 2, line = 4, cex = 1, outer = TRUE, adj = 0.62)

axis(1, at = seq(1, 274, 31), labels = FALSE)
mtext("c", side = 3, line = -1.7, cex = 1.3, adj = 0.05)




# D) Fresno, RH
plot(1:274, rhmaxF[1:274,2], type = "l", col = "black", ylim = c(0,120), las = 1, xaxt = 'n', cex.axis = 1.2, yaxt = 'n')
lines(1:274, fre85$RHmax[1:274], type = "l", col = "cyan")
lines(1:274, fre45$RHmax[1:274], type = "l", col = "blue")

lines(1:274, rhminF[1:274,2], type = "l", col = "black", lty =3)
lines(1:274, fre85$RHmin[1:274], type = "l", col = "cyan", lty =3)
lines(1:274, fre45$RHmin[1:274], type = "l", col = "blue", lty =3)

axis(1, at = seq(1, 274, 31), labels = FALSE)
mtext("d", side = 3, line = -1.7, cex = 1.3, adj = 0.05)



# E) Oakville, Solar radiation
plot(1:274, rgO[1:274,2], type = "l", col = "black", ylim = c(0,1100), las = 1, xaxt = 'n', cex.axis = 1.2)
lines(1:274, oak85$SolRadMax[1:274], type = "l", col = "cyan")
lines(1:274, oak45$SolRadMax[1:274], type = "l", col = "blue")

mtext(expression(paste("Solar Radiation", " (W m"^{"-2"}, ")")), side = 2, line = 4, cex = 1, outer = TRUE, adj = 0.3)
axis(1, at = seq(1, 274, 31), labels = FALSE)
mtext("e", side = 3, line = -1.7, cex = 1.3, adj = 0.05)


# F) Fresno, Solar radiation
plot(1:274, rgF[1:274,2], type = "l", col = "black", ylim = c(0, 1100), las = 1, xaxt = 'n', cex.axis = 1.2, yaxt = 'n')
lines(1:274, fre85$SolRadMax[1:274], type = "l", col = "cyan")
lines(1:274, fre45$SolRadMax[1:274], type = "l", col = "blue")
axis(1, at = seq(1, 274, 31), labels = FALSE)
mtext("f", side = 3, line = -1.7, cex = 1.3, adj = 0.05)



# E) Oakville, windspeed
plot(1:274, uO[1:274,2], type = "l", col = "black", ylim = c(0,6), las = 1, xaxt = 'n', cex.axis = 1.2)
lines(1:274, oak85$WSmax[1:274], type = "l", col = "cyan")
lines(1:274, oak45$WSmax[1:274], type = "l", col = "blue")
mtext(expression(paste("Windspeed", " (m s"^{"-1"}, ")")), side = 2, line = 4, cex = 1, outer = TRUE, adj = 0.02)
axis(1, at = seq(1, 274, 31), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S"))
mtext("g", side = 3, line = -1.7, cex = 1.3, adj = 0.05)


# F) Fresno, windspeed
plot(1:274, uF[1:274,2], type = "l", col = "black", ylim = c(0, 6), las = 1, xaxt = 'n', cex.axis = 1.2, yaxt = 'n')
lines(1:274, fre85$WSmax[1:274], type = "l", col = "cyan")
lines(1:274, fre45$WSmax[1:274], type = "l", col = "blue")
axis(1, at = seq(1, 274, 31), labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S"))
mtext("h", side = 3, line = -1.7, cex = 1.3, adj = 0.05)

legend(315, 16,  c("Hist.", "RCP 4.5", "RCP 8.5"), lty = 1, lwd = 2, bty = "o", col = c("black", "blue", "cyan"), xpd = NA, cex = 1.2)
