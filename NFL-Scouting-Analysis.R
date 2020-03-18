# NFL Scouting Combine
# Alex Mimura, Ricky Pang, Angel Henriquez

# Clear the working space
rm(list = ls())

# Load packages
library(doBy)
library(plyr)
library(dplyr)
library(foreign)
library(ggplot2)
library(knitr)
library(lmtest)
library(readstata13)
library(sandwich)
library(stargazer)
library(AER)
library(maps)
library(rvest)
library(readxl)
library(gdata)

cse=function(reg) {
  rob=sqrt(diag(vcovHC(reg, type="HC1")))
  return(rob)
}

# relevant data
qb <- read.csv("~/Downloads/qbfinal.csv")
wrrb <- read.csv("~/Downloads/wr&rbsfinal.csv")

qb$rnd <- as.numeric(qb$rnd)
qb$pick <- as.numeric(qb$pick)
qb$forty <- as.numeric(qb$forty)
qb$vertical <- as.numeric(qb$vertical)
qb$bench <- as.numeric(qb$bench)
qb$broad.jump <- as.numeric(qb$broad.jump)
qb$cone <- as.numeric(qb$cone)
qb$shuttle <- as.numeric(qb$shuttle)

wrrb$rnd <- as.numeric(wrrb$rnd)
wrrb$pick <- as.numeric(wrrb$pick)
wrrb$forty <- as.numeric(wrrb$forty)
wrrb$vertical <- as.numeric(wrrb$vertical)
wrrb$bench <- as.numeric(wrrb$bench)
wrrb$broad.jump <- as.numeric(wrrb$broad.jump)
wrrb$cone <- as.numeric(wrrb$cone)
wrrb$shuttle <- as.numeric(wrrb$shuttle)

stargazer(qb[c( "shuttle")], type="text", median=TRUE,digits=2, title="QB")
stargazer(wrrb[c("Y.G","rnd","pick","forty","vertical","bench","broad.jump", "cone", "shuttle")], type="text", median=TRUE,digits=2, title="WR & RB")

##Scatter Plot

#want to find correlation between combine scores & draft order
#can plot multiple variables against draft order? (forty, vertical, etc)
#need trend line to show correlation 
ggplot(qb, aes(x=pick, y=Rate)) + geom_point(col="brown") + 
  labs(title = "Pick vs. Rate", x = "Pick", y = "Rate") + stat_smooth(method = lm, col = "pink", se=TRUE) + geom_text(aes(label=Player), hjust=1, vjust=1)

ggplot(wrrb, aes(x=pick, y=Y.G)) + geom_point(col="brown") + 
  labs(title = "Pick vs. Y.G", x = "Pick", y = "Y.G") + stat_smooth(method = lm, col = "pink", se=TRUE)

#Base Specification 
#want to regress yards/game on combine variables to find effect 
reg11 <- lm(Y.G ~ forty+vertical, data=wrrb)
reg12 <- lm(Y.G ~ forty+vertical+bench, data=wrrb)
reg13 <- lm(Y.G ~ forty+vertical+bench+broad.jump, data=wrrb)
reg14 <- lm(Y.G ~ forty+vertical+bench+broad.jump+cone, data=wrrb)

#table regressions, rows are coming out incorrectly, should be 5 total rows
stargazer(reg14, se=list(cse(reg14)), title="Regression - Trade, Distance, and GDP", type="text", column.labels=c("combine"), df=FALSE, digits=3)

#same for qbs
reg1 <- lm(Rate ~ forty+vertical, data=qb)
reg2 <- lm(Rate ~ forty+vertical+broad.jump, data=qb)
reg3 <- lm(Rate ~ forty+vertical+broad.jump+cone, data=qb)

#table regressions
stargazer(reg1, reg2, reg3, title="Regression - Trade, Distance, and GDP", type="text", column.labels=c("Single","Multiple"),  df=FALSE, digits=3)

#lht to find p-value
lht(reg4, c("SI.POV.GINI=0", "SH.MED.PHYS.ZS=0","SH.PRV.SMOK=0","SH.STA.BASS.ZS=0"), white.adjust="hc1")
