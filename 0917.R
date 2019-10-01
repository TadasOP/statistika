m1 <- matrix(1:6, nrow=2, ncol=3, byrow=T)
m2 <- matrix(1:6, nrow=2, ncol=3, byrow=F)
m5 <- rbind(m1,m2)
m6 <- cbind(m1,m2)

m1
m2
m5
m6
#######################

dat <- data.frame(x=1:3, y=c("a", "b", "c"),
                  f=factor(c("v", "m", "v")))
dat
class(dat)
str(dat)
summary(dat)
########################

(salis <- c("Prancuzija", "Olandija", "Vokietija", "Anglija", "Ispanija"))
(oro_bendrove <- c("Wizzair", "Lufthansa", "Lufthansa", "Ryanair", "Ryanair"))
(kaina <- c(60, 120, 80, 150, 130))
(viesbutis <- c(250, 400, 200, 380, 300))
(naktys <- c(3, 7, 5, 4, 7))

matr <- cbind(salis, oro_bendrove, kaina, viesbutis, naktys)
matr

sistema <- data.frame(salis, oro_bendrove, kaina, viesbutis, naktys)
sistema

mode(sistema)
class(sistema)
sistema[,2]
class(sistema[,2])
sistema[,4]
class(sistema[,4])

sistema[,3]
sistema[,"kaina"]
sistema$kaina

attach(sistema)
kaina
oro_bendrove
viesbutis/naktys
kaina+viesbutis
detach(sistema)

#################

srs <- list(c("gerai", "blogai"), c(TRUE, FALSE), c(1,5,2), c(4,6))
srs
srs[[2]]
srs[[3]][[2]]

anketa <- list(vardas=c("Kazys Jonaitis", "Povilas Petraitis"),
               pareigos=c("mokytojas", "vairuotojas"),
               seima=c("nevedes", "vedes"),
               vaiku_skaicius=c(2,3),
               vaiku_amzius= matrix(c(5,6, NA, 1,4,7),
                                    ncol=3 ,byrow=TRUE))
anketa               

mode(anketa)
class(anketa)
attributes(anketa)
dim(anketa)
anketa[[2]]
anketa$pareigos
names(anketa)

###################

install.packages("xlsx")
library("xlsx")

setwd("C:/Users/Studentas/Desktop/Tadas")

library(readxl)
Nusikalstamumas <- read_excel("C:/Users/Studentas/Desktop/Tadas/Nusikalstamumas.xlsx")
View(Nusikalstamumas)

mmm <- read.table("Nusikalstamumas.csv", header = TRUE, sep = ";")
head(mmm)
summary(mmm)

nnn <- read.table("Nusikalstamumas.csv", header = TRUE, sep = ";", dec = ",")
head(nnn)
summary(nnn)

ooo <- read.csv("Nusikalstamumas.csv", header = TRUE)

ppp <- read.csv2("Nusikalstamumas.csv", header = TRUE)
head(ppp)
summary(ppp)

