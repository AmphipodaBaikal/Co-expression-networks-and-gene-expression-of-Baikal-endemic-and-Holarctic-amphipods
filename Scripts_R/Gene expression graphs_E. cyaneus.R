library("openxlsx")
setwd("/media/DATA/учебное-лабораторное/лаборатория/R и прочее/Ксю/2019 июль")
f <- "Calculation_Realtime_Cyaneus.xlsx"


nam <- c()
adress <- c(53, 85, 23, 229, 181, 197, 165, 245, 117)
for (i in 1:length(adress)) {
  n <- 6
  if (i == 3) n <-4
  t <- read.xlsx(f, colNames=FALSE, cols=adress[i]-n, rows=1)
  nam <- c(nam, substr(t, 13, 20))
}
nam[8] <- "ATPa"
nam[6] <- "PFK1"
nam[4] <- "NAKA"


timp <- function(cn) {
  t <- read.xlsx(f, colNames=FALSE, cols=c(5, cn), rows=3:56)
  t <- t[!is.na(t[,2]),]
}

cimp <- function(cn) {
  c <- read.xlsx(f, colNames=FALSE, cols=c(5, cn), rows=57:98)
  c <- c[!is.na(c[,2]),]
}

thsp <- timp(53); chsp <- cimp(53)
tef <- timp(85); cef <- cimp(85)
tact <- timp(23); cact <- cimp(23)
tatna <- timp(229); catna <- cimp(229)
thk <- timp(181); chk <- cimp(181)
tpfk1 <- timp(197); cpfk1 <- cimp(197)
togdh <- timp(165); cogdh <- cimp(165)
tatpa <- timp(245); catpa <- cimp(245)
tatpg <- timp(117); catpg <- cimp(117)


max(c(tact[,2], cact[,2], togdh[,2], cogdh[,2], thsp[,2], chsp[,2], tef[,2], cef[,2], tatpa[,2], catpa[,2], tatpg[,2], catpg[,2], tatna[,2], catna[,2], tpfk1[,2], cpfk1[,2], thk[,2], chk[,2]))
min(c(tact[,2], cact[,2], togdh[,2], cogdh[,2], thsp[,2], chsp[,2], tef[,2], cef[,2], tatpa[,2], catpa[,2], tatpg[,2], catpg[,2], tatna[,2], catna[,2], tpfk1[,2], cpfk1[,2], thk[,2], chk[,2]))


contr <- "dodgerblue3"
treat <- "chocolate1"
plotbox <- function(x, y) {
par(mar=c(1,2,0.5,0.3))
s <- 0.4
yl <- c(-2.5, 3)
contr <- "dodgerblue3"
treat <- "chocolate1"
col <- c(contr, treat, contr, treat, contr, treat, contr)

z <- list(x[x[,1]==6,2], x[x[,1]==12.4,2], y[y[,1]==12.4,2], x[x[,1]==18.8,2], y[y[,1]==18.8,2], x[x[,1]==23.6,2], y[y[,1]==23.6,2])

boxplot(z, type="n", range=10, ylim=yl, at=c(6, 12.4+s, 12.4-s, 18.8+s, 18.8-s, 23.6+s, 23.6-s), border=col, yaxt="n", xaxt="n", boxwex=0.7, frame.plot=FALSE)
abline(0, 0, lty=2)

boxplot(z, range=10, ylim=yl, at=c(6, 12.4+s, 12.4-s, 18.8+s, 18.8-s, 23.6+s, 23.6-s), border=col, yaxt="n", xaxt="n", boxwex=0.7, frame.plot=FALSE, add=TRUE)
axis(2, -3:3, cex.axis=0.9, las=1,  mgp=c(3,1,0))
axis(2, -1:3-0.5, cex.axis=0.9, labels=FALSE, mgp=c(3,1,0), tck=-0.01)
axis(1, c(0, 6, 12.4, 18.8, 23.6, 30), cex.axis=0.9, tcl=0.5, lwd.ticks=1, lwd=1, mgp=c(3,0,0))
#axis(1, c(23.6), tick=FALSE, cex.axis=0.9,  mgp=c(3,0,0))
}




png(filename="ecy.png", units="in", width=10, height=7, res=1000)

mat <- matrix(c(1,2,2,2, 1,3,4,5, 1,6,7,8, 1,9,10,11, 1,12,12,12), nrow=5, byrow=TRUE)
layout(mat, widths=c(0.5, 5, 5, 5), heights=c(0.7, 3, 3, 3, 0.7))

par(mar=c(0,0,0,0))
plot(1, type="n", frame.plot=FALSE, yaxt="n", xaxt="n")
text(1, 1, labels="log₂(fold expression)", srt=90, cex=1.7)
plot(1, type="n", frame.plot=FALSE, yaxt="n", xaxt="n")
legend("center", legend=c("Parallel control group at 6°C", "Experimental group at rising temperature"), col=c(contr, treat), ncol=2, bty="n", lwd=1.5, cex=1.2)

s <- 0.4

plotbox(thsp, chsp); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[1]))))
text(23.6-s, 1.3, "a", cex=1, pos=3)
text(23.6+s, 1.95, "*", cex=1.5)
text(23.6+s, 1.3, "b", cex=1, pos=3)

plotbox(tef, cef); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[2]))))
plotbox(tact, cact); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[3]))))
text(18.8-s, 1, "a", cex=1, pos=3)
text(18.8+s, 1.9, "*", cex=1.5)
text(18.8+s, 1, "b", cex=1, pos=3)
text(23.6+s, 1.9, "*", cex=1.5)

plotbox(tatna, catna); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[4]))))
text(12.4-s, 0.8, "a", cex=1, pos=3)
text(12.4+s, -1.2, "*", cex=1.5)
text(12.4+s, 0.8, "b", cex=1, pos=3)
text(23.6+s, -1.2, "*", cex=1.5)

plotbox(thk, chk); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[5]))))
text(18.8+s, 2.3, "*", cex=1.5)
text(23.6+s, 2.3, "*", cex=1.5)

plotbox(tpfk1, cpfk1); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[6]))))
plotbox(togdh, cogdh); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[7]))))
text(18.8+s, 2.1, "*", cex=1.5)
text(23.6-s, 1.45, "a", cex=1, pos=3)
text(23.6+s, 2.1, "*", cex=1.5)
text(23.6+s, 1.45, "b", cex=1, pos=3)

plotbox(tatpa, catpa); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[8]))))
text(18.8+s, -1.95, "*", cex=1.5)

plotbox(tatpg, catpg); text(x=14.8, y=2.6, labels=bquote(bolditalic(.(nam[9]))))
text(18.8+s, 1.5, "*", cex=1.5)
text(23.6+s, 1.5, "*", cex=1.5)

par(mar=c(0,0,0,0))
plot(1, type="n", frame.plot=FALSE, yaxt="n", xaxt="n")
text(1, 0.9, labels="Temperature, °C", cex=1.7)

dev.off()




