# 2019
#Thu 06 Oct 2016 11:27:11 AM CEST
# ===============================================================================================================
# UNA ÚNICA FIGURA (Detalles en Fraccionamiento_2016.R)
# ===============================================================================================================
rm(list=ls())
cl <- function(){system("clear")}
library(Hmisc)
library(grid)
library(gridExtra)
#par(mfrow=c(2,4), mar=c(4, 4, 2 , 2)+0.1, ann=T)
#layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
jpeg(file="Fig1_V2.jpg", quality=100, res=300, width = 20, height = 29, units = "cm")
error<-function(x)c(smean.cl.normal(x, mult=1))
read.table(file="Tabla_Resultados_Canarios_2016_V2.csv", header=T, sep=",", dec=",")->ana
attach(ana)
#===============================================================================
# Resin-P
#===============================================================================
s<-summarize(Resin_P, llist(Age2), error)
attach(s)
mypanel<-function(x,y,...){
	panel.xYplot(x,y, ...)
	panel.text(6,-1, labels="500 yr", srt=90, cex=1, adj=c(0.5,0.5))
	panel.text(7.3,-1, labels="3,000 yr", srt=90, cex=1, adj=c(0.45,0.5))
	panel.text(10.6,-1, labels="60,000 yr", srt=90, cex=1, adj=c(0.35,0.5))
	panel.text(13.2,4, labels="575,000 yr", srt=90, cex=1, adj=c(0.5,0.5))
	panel.text(14,4, labels="1,200,000 yr", srt=90, cex=1, adj=c(0.48,0.5))
	panel.text(15.6,4, labels="6,500,000 yr", srt=90, cex=1, adj=c(0.45,0.5))
	}
xYplot(Cbind(Resin_P, Lower, Upper) ~ log(Age2), data=s, panel=mypanel, 
	type="b", ylim=c(-3.5, 8),xlab="Substrate age (log)",
	ylab= list(label=expression(paste("Resin-P (mg kg"^-1,"soil)")), cex=1.2), 
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020,
	lty.bar=1, lwd=1, scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0,
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot1
# ============================================================================
# NaHCO3_PI
#=============================================================================
attach(ana)
s<-summarize(NaHCO3_PI, llist(Age2), error)
attach(s)
xYplot(Cbind(NaHCO3_PI, Lower, Upper) ~ log(Age2), data=s, type="b", 
	ylim=c(-5, max(s$Upper)+1),xlab="Substrate age (log)", 
	ylab= list(label=expression(paste("NaHCO"[3],"-Pi (mg kg"^-1," soil)")), cex=1.2), 
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020, 
	lty.bar=1, lwd=1, scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot2

# =============================================================================
# NaHCO3_PO
#==============================================================================
attach(ana)
s<-summarize(NaHCO3_PO, llist(Age2), error)
attach(s)
xYplot(Cbind(NaHCO3_PO, Lower, Upper) ~ log(Age2), data=s, type="b", 
	ylim=c(0, max(s$Upper)+100),xlab="Substrate age (log)",
	ylab= list(label=expression(paste("NaHCO"[3],"-Po (mg kg"^-1," soil)")), cex=1.2), pch=c(21,16), 
	col=1, label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot3
#==============================================================================
#NaHCO3_PT
#==============================================================================
attach(ana)
s<-summarize(NaHCO3_PT, llist(Age2), error)
attach(s)
xYplot(Cbind(NaHCO3_PT, Lower, Upper) ~ log(Age2), data=s, type="b", 
	ylim=c(0, max(s$Upper)+50),xlab="Substrate age (log)", 
	ylab= list(label=expression(paste("NaHCO"[3],"-Pt (mg kg"^-1," soil)")), cex=1.2), pch=c(21,16), col=1, 
	label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot4
# ============================================================================
# NaOH_PI
# ============================================================================
attach(ana)
s<-summarize(NaOH_PI, llist(Age2), error)
attach(s)
xYplot(Cbind(NaOH_PI, Lower, Upper) ~ log(Age2), data=s, type="b", ylim=c(-5, max(s$Upper)+1),
	xlab="Substrate age (log)", ylab= list(label=expression(paste("NaOH-Pi (mg kg"^-1," soil)")), cex=1.2),
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot5
# ============================================================================
# NaOH_PO
# ============================================================================
attach(ana)
s<-summarize(NaOH_PO, llist(Age2), error)
attach(s)
xYplot(Cbind(NaOH_PO, Lower, Upper) ~ log(Age2), data=s, type="b", ylim=c(-20, max(s$Upper)+10),
	xlab="Substrate age (log)", ylab= list(label=expression(paste("NaOH-Po (mg kg"^-1," soil)")), cex=1.2), 
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot6
# ============================================================================
# NaOH_PT
# ============================================================================
attach(ana)
s<-summarize(NaOH_PT, llist(Age2), error)
attach(s)
xYplot(Cbind(NaOH_PT, Lower, Upper) ~ log(Age2), data=s, type="b", ylim=c(-20, max(s$Upper)+10),
	xlab="Substrate age (log)", ylab= list(label=expression(paste("NaOH-Pt (mg kg"^-1," soil)")), cex=1.2), 
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot7
# ============================================================================
# HCL_37_PT
# ============================================================================
attach(ana)
s<-summarize(HCL_37_PT, llist(Age2), error)
attach(s)
xYplot(Cbind(HCL_37_PT, Lower, Upper) ~ log(Age2), data=s, type="b", ylim=c(-4, max(s$Upper)+1),
	xlab="Substrate age (log)", ylab= list(label=expression(paste("HCl"[conc],"-P (mg kg"^-1," soil)")), cex=1.2), 
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot8

# ============================================================================
# H2SO4_P
# ============================================================================
attach(ana)
s<-summarize(H2SO4_P, llist(Age2), error)
attach(s)
xYplot(Cbind(H2SO4_P, Lower, Upper) ~ log(Age2), data=s, type="b", ylim=c(-50, max(s$Upper)+10),
	xlab="Substrate age (log)", ylab= list(label=expression(paste("H"[2],"SO"[4],"-P (mg kg"^-1," soil)")), cex=1.2), 
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot9
# ============================================================================
# Total_P
# ============================================================================
attach(ana)
# Medias por edad y muestreo
s<-summarize(P_total, llist(Age2), error)
attach(s)
xYplot(Cbind(P_total, Lower, Upper) ~ log(Age2), data=s, type="b", ylim=c(0, max(s$Upper)+10),
	xlab="Substrate age (log)", ylab= list(label=expression(paste("Total_P (mg kg"^-1," soil)")), cex=1.2),
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot10
# ============================================================================
# HCl_1M_P
# ============================================================================
attach(ana)
s<-summarize(HCl_1M_P, llist(Age2), error)
attach(s)
xYplot(Cbind(HCl_1M_P, Lower, Upper) ~ log(Age2), data=s, type="b", ylim=c(-3, max(s$Upper)+1),
	xlab="Substrate age (log)", ylab= list(label=expression(paste("HCl"["1M"],"-P (mg kg"^-1," soil)")), cex=1.2),
	pch=c(21,16), col=1, label.curves=FALSE, cex=1.1, cap=0.020, lty.bar=1, lwd=1,scales=list(cex=1),
	par.settings=list(layout.heights=list(top.padding=0, 
	bottom.padding=0),plot.symbol=list(col="black", fill="black", pch=2)))->plot11
grid.arrange(plot1, plot5, plot2, plot6, plot3, plot9, plot11, plot10, ncol=2)
#=====================================================================================
# Imprimir las figuras individualmente
jpeg(file="Resin_P.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot1
jpeg(file="NaHCO3_PI.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot2
jpeg(file="NaHCO3_PO.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot3
jpeg(file="NaHCO3_PT.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot4
jpeg(file="NaOH_PI.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot5
jpeg(file="NaOH_PO.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot6
jpeg(file="NaOH_PT.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot7
jpeg(file="HCL_37_PT.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot8
jpeg(file="H2SO4_P.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot9
jpeg(file="Total_P.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot10
jpeg(file="HCl_1M_P.jpg", quality=100, pointsize=12, units="cm", width=16, height=16, res=300)
plot11
quit(save="yes")

