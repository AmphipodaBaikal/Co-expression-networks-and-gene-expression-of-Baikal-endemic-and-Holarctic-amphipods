library("openxlsx")
setwd("/media/DATA/учебное-лабораторное/лаборатория/R и прочее/Ксю/2019 июль")
f <- "Calculation_Realtime_Verrucosus.xlsx"


nam <- c()
adress <- c(25, 83, 69, 195, 153, 223, 167, 209, 97)
for (i in 1:length(adress)) {
  t <- read.xlsx(f, colNames=FALSE, cols=adress[i]-4, rows=1)
  nam <- c(nam, substr(t, 13, 20))
}
nam[3] <- "Actin"
nam[4] <- "NAKA"
nam[6] <- "PFK1"


timp <- function(cn) {
  t <- read.xlsx(f, colNames=FALSE, cols=c(6, cn), rows=3:92)
  t <- t[!is.na(t[,2]),]
}

cimp <- function(cn) {
  c <- read.xlsx(f, colNames=FALSE, cols=c(6, cn), rows=93:140)
  c <- c[!is.na(c[,2]),]
}

thsp <- timp(25); chsp <- cimp(25)
tef <- timp(83); cef <- cimp(83)
tact <- timp(69); cact <- cimp(69)
tatna <- timp(195); catna <- cimp(195)
thk <- timp(153); chk <- cimp(153)
tpfk1 <- timp(223); cpfk1 <- cimp(223)
togdh <- timp(167); cogdh <- cimp(167)
tatpa <- timp(209); catpa <- cimp(209)
tgapd <- timp(97); cgapd <- cimp(97)


max(c(tact[,2], cact[,2], togdh[,2], cogdh[,2], thsp[,2], chsp[,2], tef[,2], cef[,2], tatpa[,2], catpa[,2], tgapd[,2], cgapd[,2], tatna[,2], catna[,2], tpfk1[,2], cpfk1[,2], thk[,2], chk[,2]))
min(c(tact[,2], cact[,2], togdh[,2], cogdh[,2], thsp[,2], chsp[,2], tef[,2], cef[,2], tatpa[,2], catpa[,2], tgapd[,2], cgapd[,2], tatna[,2], catna[,2], tpfk1[,2], cpfk1[,2], thk[,2], chk[,2]))


contr <- "dodgerblue3"
treat <- "chocolate1"
plotbox <- function(x, y) {
par(mar=c(1,2,0.5,0.3))
s <- 0.4
yl <- c(-2.5, 3)
contr <- "dodgerblue3"
treat <- "chocolate1"
col <- c(contr, treat, contr, treat, contr, treat, treat, contr, treat, treat, contr)

z <- list(x[x[,1]==6,2], x[x[,1]==9.2,2], y[y[,1]==9.2,2], x[x[,1]==12.4,2], y[y[,1]==12.4,2], x[x[,1]==15.6,2], x[x[,1]==18.8,2], y[y[,1]==18.8,2], x[x[,1]==22,2], x[x[,1]==23.6,2], y[y[,1]==23.6,2])

boxplot(z, type="n", range=200, ylim=yl, at=c(6, 9.2+s, 9.2-s, 12.4+s, 12.4-s, 15.6, 18.8+s, 18.8-s, 22, 23.6+s, 23.6-s), border=col, yaxt="n", xaxt="n", boxwex=0.7, frame.plot=FALSE)
abline(0, 0, lty=2)

boxplot(z, range=200, ylim=yl, at=c(6, 9.2+s, 9.2-s, 12.4+s, 12.4-s, 15.6, 18.8+s, 18.8-s, 22, 23.6+s, 23.6-s), border=col, yaxt="n", xaxt="n", boxwex=0.7, frame.plot=FALSE, add=TRUE)
axis(2, -3:3, cex.axis=0.9, las=1,  mgp=c(3,1,0))
axis(2, -1:3-0.5, cex.axis=0.9, labels=FALSE, mgp=c(3,1,0), tck=-0.01)
axis(1, c(0, 6, 9.2, 12.4, 15.6, 18.8, 22, 23.6, 30), cex.axis=0.9, tcl=0.5, lwd.ticks=1, lwd=1, mgp=c(3,0,0))
axis(1, c(23.6), tick=FALSE, cex.axis=0.9,  mgp=c(3,0,0))
}




png(filename="eve.png", units="in", width=10, height=7, res=1000)

mat <- matrix(c(1,2,2,2, 1,3,4,5, 1,6,7,8, 1,9,10,11, 1,12,12,12), nrow=5, byrow=TRUE)
layout(mat, widths=c(0.5, 5, 5, 5), heights=c(0.7, 3, 3, 3, 0.7))

par(mar=c(0,0,0,0))
plot(1, type="n", frame.plot=FALSE, yaxt="n", xaxt="n")
text(1, 1, labels="log₂(fold expression)", srt=90, cex=1.7)
plot(1, type="n", frame.plot=FALSE, yaxt="n", xaxt="n")
legend("center", legend=c("Parallel control group at 6°C", "Experimental group at rising temperature"), col=c(contr, treat), ncol=2, bty="n", lwd=1.5, cex=1.2)

s <- 0.4

plotbox(thsp, chsp); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[1]))))
text(18.8+s, 2.1, "*", cex=1.5)
text(22, 2.1, "*", cex=1.5)
text(23.6-s, 1.45, "a", cex=1, pos=3)
text(23.6+s, 2.1, "*", cex=1.5)
text(23.6+s, 1.45, "b", cex=1, pos=3)

plotbox(tef, cef); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[2]))))
plotbox(tact, cact); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[3]))))
plotbox(tatna, catna); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[4]))))
text(15.6, -1.3, "*", cex=1.5)
text(18.8+s, -1.3, "*", cex=1.5)
text(22, -1.3, "*", cex=1.5)
text(23.6+s, -1.3, "*", cex=1.5)
text(18.8-s, 0.2, "a", cex=1, pos=3)
text(18.8+s, 0.2, "b", cex=1, pos=3)
text(23.6-s, 0.2, "a", cex=1, pos=3)
text(23.6+s, 0.2, "b", cex=1, pos=3)

plotbox(thk, chk); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[5]))))
plotbox(tpfk1, cpfk1); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[6]))))
text(23.6+s, -1.6, "*", cex=1.5)

plotbox(togdh, cogdh); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[7]))))
plotbox(tatpa, catpa); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[8]))))
text(23.6-s, 0.4, "a", cex=1, pos=3)
text(23.6+s, 0.4, "b", cex=1, pos=3)

plotbox(tgapd, cgapd); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[9]))))

par(mar=c(0,0,0,0))
plot(1, type="n", frame.plot=FALSE, yaxt="n", xaxt="n")
text(1, 0.9, labels="Temperature, °C", cex=1.7)

dev.off()




