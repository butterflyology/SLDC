# My impromptu digression on base level R plotting from NGS2015

#Using plot.new() 
#I discovered this plotting method when trying to add an inset figure to a plot

#plot.new is part of the traditional graphics. This function starts a new plot with the scale of the x- and y- axes to (0,1)
#This allows you to specify the location of each pane or inset and provides a high level of customizability for R graphics
setwd("~/Desktop/Projects/NGS-course/SLDC")
load("Data/Fen-data.RData")

###############
#Plotting multiple panes, the old way
par(mfrow=c(3,1))

plot(P2B1$TMP, ylab = "Degrees C", xlab = '3 January - 13 March, 2011', type = 'l', lty = 1, ylim = c(-25, 25), xaxt = 'n', lwd = 2)
lines(P2T1$TMP, type = 'l', lty = 2, col = 'red', lwd = 2)
#line at DD50 (10C = 50F)
legend('topleft', legend = c('Ground Level', '1m Height'), col = c('black', 'red'), lty = c(1,2), lwd = c(2,2), bty = 'n')

plot(P2B2$TMP, ylab = "Degrees C", xlab = '13 March - 7 May, 2011', type = 'l', lty = 1, lwd = 2, ylim = c(-10, 35), xaxt = 'n')
lines(P2T2$TMP, col = 'red', lwd = 2, lty =  2)

plot(P2B3$TMP, ylab = 'Degrees C', xlab = '7 May - 16 July, 2011', type = 'l', ylim = c(0, 38), xaxt = 'n', lwd = 2)
lines(P2T3$TMP, col = 'red', lwd = 2, lty = 2)

####Using the new way
par(mar = c(1, 5, 0, 0))#set up the margins for the overall plot
quartz(height = 8, width = 8)
par(fig = c(0, 1, 0.65, 1), new = TRUE)#now tell it where you want the new plot, reads x1 x2, y1 y2 (left and right bounds, top and bottom bounds)
par(mar = c(3, 6, 1, 1))#set the margins for the new plot
plot.new()
plot(P2B1$TMP, ylab = '', xlab = '', type = 'l', lty = 1, ylim = c(-25, 15), xaxt = 'n', lwd = 3, las = 1, bty = 'l', cex.axis = 1.5)
mtext('3 January - 13 March, 2011', side = 1, line = 0.5, adj = 0.5, cex = 1.5)
lines(P2T1$TMP, type = 'l', lty = 4, col = 'red', lwd = 2)
legend(x = 0, y = 19, legend = c('1m Height', 'Ground Level'), col = c('red', 'black'), lty = c(4, 1), lwd = c(3, 2), bty = 'n')

par(fig = c(0, 1, 0.35, 0.70), new = TRUE)
par(mar = c(3,6,1,1))
plot.new()
plot(P2B2$TMP, ylab = (expression(paste('Temperature (', degree, 'C)'))), xlab = '', type = 'l', lty = 1, lwd = 3, ylim = c(-10,35), xaxt = 'n', bty = 'l', las = 1, cex.axis = 1.5, cex.lab = 1.5)
lines(P2T2$TMP, col = 'red', lwd = 2, lty = 4)
mtext('13 March - 7 May, 2011', side = 1, line = 0.5, adj = 0.5, cex = 1.5)

par(fig = c(0, 1, 0, 0.35), new = TRUE)
par(mar = c(3, 6, 1 ,1))
plot.new()
plot(P2B3$TMP, ylab = '', xlab = '', type = 'l', lty = 1, ylim = c(0, 38), xaxt = 'n', lwd = 3, las = 1, bty = 'l', cex.axis = 1.5)
lines(P2T3$TMP, col = 'red', lwd = 2, lty = 4)
mtext('7 May - 16 July, 2011', side = 1, line = 0.5, adj = 0.5, cex = 1.5)

#############
#Puting it all together
# Figure from 
library("MCMCpack")
library("compositions")
library("grDevices")

w <- MCMCpack::rdirichlet(50000, c(5, 0.5, 2))
colnames(w)<-c("A","B","C")
ind1<-rmultinom(1, 40, w[1,])
ind2<-rmultinom(1, 22, w[2,])


quartz(width = 6.8,height = 4.5)
par(fig = c(0.4, 0.6, 0.75, 0.95), mar = c(0, 0, 0, 0))
plot.rcomp(w,col = rgb(1, 0, 0, 0.1), pch = ".", labels = c("A", "B", "C"))

# Individual count data
par(fig = c(0, 1, 0.1, 0.3), new = TRUE)
#plot.window(xlim=c(0,1),ylim=c(0,0.75))
plot.new()
#text(0.2,0.2,"x",font=3,family="Arial",cex=2)
text(0.1, 0.6, expression(italic(x[paste("1A")]) == 36), pos = 4)
text(0.1, 0.4, expression(italic(x[paste("1B")]) == 1), pos = 4)
text(0.1, 0.2, expression(italic(x[paste("1C")]) == 3), pos = 4)

text(0.4, 0.6,expression(italic(x[paste("2A")]) == 15), pos = 4)
text(0.4, 0.4,expression(italic(x[paste("2B")]) == 3), pos = 4)
text(0.4, 0.2,expression(italic(x[paste("2C")]) == 11), pos = 4)

text(0.74, 0.6,expression(italic(x[paste("iA")]) == 13), pos = 4)
text(0.74, 0.4,expression(italic(x[paste("iB")]) == 4), pos = 4)
text(0.74, 0.2,expression(italic(x[paste("iC")]) == 5), pos = 4)


#individual probs given count data
Px1 <-c(36, 1, 3) / 40
Px2 <-c(15, 3, 11) / 29
Px3 <-c(13, 4, 5) / 22

par(fig = c(0.15, 0.25, 0.4, 0.6), new = TRUE)
plot.new()
barplot(Px1,names.arg = c(" "), ylim = c(0, 1), col = "red", cex.axis = 0.5, cex.names = 0.75, las = 1)
mtext("A  B  C", side = 1)
abline(h = 0, lwd = 2)

par(fig = c(0.43, 0.53, 0.4, 0.6), new = TRUE)
plot.new()
barplot(Px2,names.arg = c(" "), ylim = c(0, 1), col = "red", cex.axis = 0.5, cex.names = 0.75, las = 1)
mtext("A  B  C",side = 1)
abline(h = 0, lwd = 2)

par(fig = c(0.74, 0.84, 0.4, 0.6), new = TRUE)
#par(fig=c(0.71,0.81,0.4,0.6),new=TRUE)
plot.new()
barplot(Px3,names.arg=c(" "), ylim = c(0,1), col = "red", cex.axis = 0.5, cex.names = 0.75, las = 1)
mtext("A  B  C", side = 1)
abline(h = 0, lwd = 2)

par(fig = c(0, 1, 0, 1), new = TRUE)
plot.new()
text(0.62, 0.51,expression(...),pos=2,cex=2)
text(0.08, 0.5, expression(italic(Px[paste("1j", sep = "")]) == ""), pos = 2, cex = 1)
text(0.38, 0.5, expression(italic(Px[paste("2j", sep = "")]) == ""), pos = 2, cex = 1)
text(0.71, 0.5, expression(italic(Px[paste("ij", sep = "")]) == ""), pos = 2, cex = 1)
#text(0.68,0.5,expression(italic(Px[paste("ij",sep="")])==""),pos=2,cex=1)

text(0.04, 0.22, "Individual\ncounts", pos = 1, cex = 1)
text(-0.05, 0.65, "Individual probabilities", pos = 4, cex = 1)
text(0.04, 0.87, "Population", pos = 1, cex = 1)

arrows(0.16, 0.23, 0.16, 0.33, angle = 20, length = 0.15, lwd = 2)
arrows(0.46, 0.23, 0.46, 0.33, angle = 20, length = 0.15, lwd = 2)
arrows(0.795, 0.23, 0.795, 0.33, angle = 20, length = 0.15, lwd = 2)

arrows(0.53, 0.75, 0.7, 0.63, angle = 20, length = 0.15, lwd = 2, code = 3)
arrows(0.47, 0.75, 0.3, 0.63, angle = 20, length = 0.15, lwd = 2, code = 3)
arrows(0.5, 0.75, 0.5, 0.63, angle = 20, length = 0.15, lwd = 2, code = 3)

text(0.72, 0.89, expression(italic(alpha["A"]) == "5.0"), pos = 1, cex = 1)
text(0.72, 0.85, expression(italic(alpha["B"]) == "0.5"), pos = 1, cex = 1)
text(0.72, 0.81, expression(italic(alpha["C"]) == "2.0"), pos = 1, cex = 1)

text(0.92, 0.65, "Multinomial")
text(0.94, 0.87, "Dirichlet")

text(0.083, 0.95, expression(underline("Hierarchical level")), cex = 1.2)
text(0.89, 0.95, expression(underline("Modeled distribution")), cex = 1.2)


