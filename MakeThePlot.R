require(Ranadu)
require(ggplot2)
library(scales)
XYplot <- function (.T, .p) { 
  return (data.frame(X=(.T-tBot) / (tTop-tBot) - log10(.p/pBot) / log10(pBot/pTop), 
                     Y=log10(.p)))
}
load ("./SkewTData.Rdata")

####
##   here are characteristics for the plot, which can be changed as desired

##   background color for plot area
bColor <- "gray95"
bColor <- "lightyellow"
bColor <- "lemonchiffon1"
bColor <- "cornsilk"
bColor <- "aliceblue"
bColor <- "ivory"
##   color for pressure lines and plot border, and thickness of lines
pColor <- 'darkblue'
plwd <- 0.4
## color for isotherms
tColor <- 'darkblue'
tlwd=0.4

pBot <- 1000
pTop <- 100
tBot <- -40
tTop <- 40
tMin <- -140
pLevels <- seq (pBot, pTop, by=-50)
tLevels <- seq (tMin, tTop, by=5)
####
##   end of changeable plot characteristics
####

XP <- vector()
YP <- vector()
for (p in pLevels) {  # in general, this is poor, a very slow method in R. But ...
  lp <- log10(p)
  XP <- c(XP, c(0,1,NA))
  YP <- c(YP, lp, lp, 0)
}
XYP <- data.frame (X=XP, Y=YP)
XYP2 <- XYP
sq <- 4:5
j <- 1
while (4+6*j+1 <= length (XYP2$X)) {
  sq <- c(sq, (4+6*j):(4+6*j+1))
  j <- j + 1
}
XYP2$X[sq] <- NA
g <- ggplot (data=XYP, aes(x=X, y=Y)) +ylim(3,2)
g <- g + theme(panel.background = element_rect(fill = bColor))
g <- g + geom_path (data=XYP, aes(x=X, y=Y), color=pColor, lty=2)
g <- g + geom_path (data=XYP2, aes(x=X, y=Y), color=pColor, lty=1)
g <- g + theme(panel.grid.major = element_blank())
g <- g + theme(panel.grid.minor = element_blank())
#g <- g + theme (axis.title.y = element_blank ()) 
g <- g + theme (axis.text = element_blank ())
g <- g + theme (axis.ticks = element_blank ())
g <- g + theme (axis.title=element_text(face="plain",size=12,colour="blue"))
border <- data.frame (X=c(0,0,NA,1,1), 
                      Y=c(log10(pBot),log10(pTop),log10(pBot),log10(pBot),log10(pTop)))
g <- g + geom_path (data=border, color=pColor, lwd=plwd)
g <- g + xlab (expression(paste("Temperature or dew point [",degree,"C]")))
g <- g + ylab ("Pressure [hPa]")
#g <- g + geom_text (aes (x=-0.075, y=2.5, label="Pressure [hPa]", 
#                         family="mono", fontface=1, angle=90, hjust=0.5))
pls <- seq (pBot, pTop, by=-100)
LPLS <- length(pls)
labl <- character()
for (i in 1:LPLS) {labl <- c(labl, sprintf("%d", pls[i]))}
DLPLS <- data.frame (X=rep(-0.025, LPLS), Y=log10(pls), LABEL=labl)
g <- g + geom_text (data=DLPLS, aes(x=X, y=Y, label=LABEL), size=4)
xt <- vector ()
yt <- vector ()
for (t in tLevels) {
  XPb <- XYplot (t, pBot)
  XPt <- XYplot (t, pTop)
  if (XPt$X <+ 0) {next}
  if (XPb$X >= 1) {next}
  if (XPb$X < 0) {
    XPb$Y <- XPb$Y+XPb$X
    XPb$X <- 0
  }
  if (XPt$X > 1) {
    XPt$Y <- XPt$Y+XPt$X-1
    XPt$X <- 1
  }
  xt <- c(xt, c(XPb$X, XPt$X,NA))
  yt <- c(yt, c(XPb$Y, XPt$Y, NA))
}
DT <- data.frame (X = xt, Y = yt)
DT2 <- DT
g <- g + geom_path (data=DT, aes(x=X, y=Y), color=tColor, lwd=tlwd, lty=2)
sq <- 4:5
j <- 1
while (4+6*j+1 <= length(DT2$X)) {
  sq <- c(sq, (4+6*j):(4+6*j+1))
  j <- j + 1
}
DT2$X[sq] <- NA
g <- g + geom_path (data=DT2, aes(x=X, y=Y), color=tColor, lwd=tlwd, lty=1)

## add text for temperature
pltt <- seq (tLevels[3], tLevels[length(tLevels)-2], by=2*(tLevels[2]-tLevels[1]))
LPLT <- length(pltt)
lablt <- character()
for (i in 1:LPLT) {lablt <- c(lablt, sprintf("%d", pltt[i]))}
lablt[lablt=="0"] <- "0  "
DLPLT <- data.frame(X=XYplot (pltt, pBot)$X, Y=rep(log10(pBot), LPLT), LABEL=lablt)
DLPLT2 <- DLPLT [DLPLT$X > 0, ]
g <- g + geom_text (data=DLPLT2, aes(x=X, y=Y, label=LABEL, angle=45, hjust=1.5, vjust=0.5),
                    size=4)
g <- g + geom_text (data=DLPLT2, aes(x=1.02, y=Y-1+X, label=LABEL, angle=45, hjust=-0.3, vjust=-0.0),
                    size=3.5)
DLPLT3 <- DLPLT[DLPLT$X <= 0, ]
DLPLT3 <- DLPLT3[DLPLT3$X > -1, ]
g <- g + geom_text (data=DLPLT3, aes(x=X+1, y=2, label=LABEL, angle=45, hjust=-0.2, vjust=0.2),
                    size=3.5)




print(g)
