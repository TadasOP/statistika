1.Parsisiųskite nurodytus duomenis (duomenų rinkmena toliau vadinama „apklausa“).
# Sudarykite duomenų rinkinį (parašyti R kodą), kuriame būtų nemažiau penkių stulpelių (kintamųjų). 
# Bent vienas kintamasis turi būti kategorinis, bei bent vienas intervalinis. 
# 2.Parsisiųskite ne mažiau kaip 15-ospaskutiniųjų metų nurodytų rodiklių duomenis 
# iš pasirinktų šaltinių (Eurostat, Pasaulio banko, Tarptautinio valiutos fondo, Lietuvos statistikos departamentoir t.t.).
# Sudarykite duomenų rinkinį (parašyti R kodą),kuriame būtų laiko stulpelis (METAI) 
# ir rodiklių stulpeliai((duomenų rinkmena toliau vadinama „macro“).
# 3.Rinkmenoje „apklausa“ 
# a.Kategoriniams kintamiesiems sudarykite dažnių bei santykinių dažnių lenteles ir
#  išbrėžkite stulpelių diagramą.
#  b.Intervaliniams kintamiesiems apskaičiuokite padėties, sklaidos bei formos skaitines charakteristikas 
#  pagal kategorinio kintamojo(-ųjų) pjūvį(-ius). 
#  c.Išbrėžkite pasirinktų rodiklių stačiakampę diagramą bei histogramą pagal kategorinio kintamojo(-ųjų) pjūvį(-ius).
#  4.Apibūdinkite „macro“  kintamuosius; apskaičiuokite padėties, sklaidos bei formos skaitines charakteristikas;
#  pateikite gautų charakteristikų interpretaciją bei ekonominį pagrindimą; 
#  Išbrėžkite pasirinktų rodiklių linijos grafiką bei histogramą.
#
#
# duomenų nuskaitymas (apklausa)
#
dat<-read.csv2("nubt2016.csv",header=T)
dim(dat)
View(dat)
#
#   kintamųjų pasirinkimas (5 stupeliai --- "DWLTYPE" -- būsto tipas;"NROOMS" - kambarių skaičius;
# urb_rur - gyvenamoji vietovė; AGE,AHE00 --Visos namų ūkio vartojimo išlaidos (mėnesinės);
# "AHE01"--Maisto produktai ir nealkoholiniai gėrimai (mėnesinės išlaidos).
#   DWLTYP,urb_rur --- kategoriniai; NROOMS --- kiekybinis diskretusis;AHE00,AHE01 --- intervaliniai
#
library(dplyr)
duom<-select(dat,DWLTYPE,NROOMS,urb_rur,AHE00,AHE01)
head(duom)

duom$DWLTYPE <- factor(duom$DWLTYPE)
duom$NROOMS <- factor(duom$NROOMS)
duom$urb_rur <- factor(duom$urb_rur)
#
#   Kintamųjų charakteristikos
###################################################
######################################################################
#
# 3.Rinkmenoje „apklausa“ 
# a.Kategoriniams kintamiesiems sudarykite dažnių bei santykinių dažnių lenteles ir
#  išbrėžkite stulpelių diagramą.
#   
########################################################################
table(duom$DWLTYPE)
prop.table(table(duom$DWLTYPE))
round(prop.table(table(duom$DWLTYPE))*100, 2)
##
#  Kitaip
###
library(summarytools)
summarytools::freq(duom$DWLTYPE, order = "freq")

#  Stulpelinė diagrama --- pavyzdys vienam kategoriniam kintamajam NROOMS
#   y ašyje procentai ne dažniai!
#
library(ggplot2)
p <- ggplot(duom, aes(NROOMS)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  labs(x="Kambariai",y="Procentai")+
  ggtitle("Kambarių skaičius (procentai)")+
  theme(axis.text.x = element_text(face="bold",  size=8, angle=90))
p
# x-ašyje visos žymelės paryškintos
########################################################################
# 3.Rinkmenoje „apklausa“ 
#  b.Intervaliniams kintamiesiems apskaičiuokite padėties, sklaidos bei formos skaitines charakteristikas 
#  pagal kategorinio kintamojo(-ųjų) pjūvį(-ius). 
######################################################
library(dplyr)
#
#   charakteristikos (atskirai dviems intervaliniams kintamiesiems)
#
#  Penkiaskaitė suvestinė
#
dm<-select(duom,AHE00,AHE01)
summary(dm)
# 
#    Papildomos sklaidos  charakteristikos
#
summarise_all(dm,list(StdNuok = sd, Dispersija = var, MAD = mad),na.rm=T)
#
# papildomos charakteristikos (asimetrijos koef. ir ekscesas)
#
library(psych)
describe(dm,na.rm=T)
######################################################
#
#  Charakteristikos (pjūvis -- urb_rur (miestas-kaimas))
#
#####################################################
dm<-select(duom, urb_rur, AHE00,AHE01)
dm %>% group_by(urb_rur)%>% summarise_all(list(StdNuok = sd, Dispersija = var),na.rm=T)
##########################################################
#
# 3.Rinkmenoje „apklausa“ 
#  c.Išbrėžkite pasirinktų rodiklių stačiakampę diagramą bei histogramą pagal kategorinio kintamojo(-ųjų) pjūvį(-ius).
#
####################################################

# stačiakampė diagrama (dvi grupės)
#  vienam intervaliniam kintamajam (pavyzdys)
# 
p <- ggplot(duom, aes(x=urb_rur, y=AHE00,color=as.factor(urb_rur))) +
  labs(x="Mietas,kaimas",y="Išlaidos",color="Vietovė")+
  geom_boxplot()+ggtitle("Stačiakampė diagrama")+
  scale_x_discrete(labels = c('Miestas','Kaimas'))
p
#
# Paprasta histograma
#
ggplot(data=duom, aes(x=AHE00)) + 
  geom_histogram()
#
# Histograma, pasirinkus grupavimo intervalo ilgį 100
#
ggplot(data=duom, aes(x=AHE00)) + 
  geom_histogram(binwidth=100)+
  scale_x_continuous(breaks=seq(0,4500, 500),labels = seq(0,4500, 500))+
  labs(x="Išlaidos",y="Dažnis")+
  ggtitle("Išlaidų histograma")
#
# Histograma AHE00(išlaidos) pagal pjūvį urb_rur(miestas-kaimas)
#
duom$urb_rur<-as.factor(duom$urb_rur)
levels(duom$urb_rur)<-c("Miestas","Kaimas")
ggplot(duom, aes(x=AHE00, group=urb_rur, color=urb_rur)) +
  geom_histogram(fill="white",binwidth=200, alpha=0.5, position="identity")+
  labs(x="Išlaidos",y="Dažnis",group="Vietovė",color="Vietovė")+
  ggtitle("Išlaidų histograma")
####################################################################
#
#    macro.csv
#
###################################################################
# duomenų nuskaitymas
###################################################################
dt<-read.csv2("macro.csv",header=T)
head(dt)
####################################################################
#  4.Apibūdinkite „macro“  kintamuosius; apskaičiuokite padėties, sklaidos bei formos skaitines charakteristikas;
#  pateikite gautų charakteristikų interpretaciją bei ekonominį pagrindimą; 
#  Išbrėžkite pasirinktų rodiklių linijos grafiką bei histogramą.
##################################################################
library(dplyr)
#########################################
# 2018m. nepilni duomenys (filtruojam)
#########################################
dt<-filter(dt,Metai!=2018)
tail(dt)
#
#   charakteristikos
#
#  Penkiaskaitė sistema
#
summary(dt[,-1])
# 
#    Papildomos sklaidos  charakteristikos
#
summarise_all(dt[,-1],list(StdNuok = sd, Dispersija = var))
#
# papildomos charakteristikos
#
library(psych)
describe(dt[,-1],na.rm=T)
###########################
#
#   pasirinkto rodiklio linijų grafikas ir histograma
#
##############################
#
#  linijų grafikas
#
#################################
library(ggplot2)
ggplot(data=dt, aes(x=Metai, y=BVP)) +
  geom_line() +
  scale_x_continuous(breaks=dt$Metai,labels = dt$Metai)+
  geom_point()+
  theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
  ggtitle("BVP 2007-2017m.m.")

p <- ggplot() + 
  geom_line(data = dt, aes(x = Metai, y = BVP), color = "blue") +
  geom_line(data = dt, aes(x = Metai, y = C), color = "red") +
  scale_x_continuous(breaks=dt$Metai,labels = dt$Metai)+
  geom_point()+
  theme(axis.text.x = element_text(face="bold",  size=8, angle=90))+
  ggtitle("BVP ir Vartojimas 2007-2017m.m.")
p


p <- ggplot(dt, aes(x = Metai)) + 
  geom_line(aes(y = BVP, colour = "BVP")) + 
  geom_line(aes(y = U1564*1000, colour = "Nedarbas")) + 
  scale_y_continuous(sec.axis = sec_axis(~.*0.001, name = "Nedarbas [%]")) + 
  scale_colour_manual(values = c("blue", "red")) + 
  scale_x_continuous(breaks=dt$Metai,labels = dt$Metai) +
  labs(y = "BVP [mln. eur.]", x = "Metai",
       colour = "Parameter") + 
  theme(legend.position = c(0.1, 0.9),
        axis.text.x = element_text(face="bold",  size=8, angle=90))+
  ggtitle("BVP ir Nedarbo lygis 2007-2017m.m.")
p
##########################################
#
#  histograma, pasirinkus grupavimo intervalo ilgį
#
####################################
ggplot(data=dt, aes(x=BVP)) + 
  geom_histogram(binwidth=5000)+
  ylab("Dažnis")+
  ggtitle("BVP histograma")
#
#
#
#   5.Rinkmenoje „apklausa“ pasirinkite du kintamuosius ir atlikite t-testą. 
#
#     (stjudento kriterijus nepriklausomoms imtims)
#
########################################################################################
t.test(duom$AHE00~as.factor(duom$urb_rur))
urb <- filter(duom, urb_rur==1)$AHE00
rur <- filter(duom, urb_rur==2)$AHE00
var.test(urb, rur) #Situo atveju, dispersijos nera lygios ^
t.test(urb, rur, paired = F, var.equal = F)



#vienos imties testai

x <- rnorm(500, 10, 2)
t.test(x, mu=10)
t.test(x, mu=6)

#dvieju imciu testai

y <- rnorm(300, 9, 5)
t.test(x, y, paired = F, var.equal = F)

z <- rnorm(300, 8, 6)
t.test(y,z, paired = T)
############################################################################################
#
#
# 6.Rinkmenoje „apklausa“ pasirinkite intervalinį kintamajį bei suskaičiuokite vidurkio 
#  ir dispersijos taškinius įverčius bei vidurkio pasikliautinąjį intervalą. 
#
#########################################################################################
rez<-summarise(duom,vid=mean(AHE00), std=sd(AHE00),disp=var(AHE00)) # papildomai standartinis nuokrypis
error <- qt(0.975,df=length(duom$AHE00-1))*sd(duom$AHE00)/sqrt(length(duom$AHE00)) # pasikliovimo lygmuo 0,95
left <- mean(duom$AHE00)-error
right <- mean(duom$AHE00)+error
left
right

#
#   kitaip
#
summarise(duom,lower = mean(AHE00) - qt(0.975,df=length(AHE00)-1) * (sd(AHE00) / sqrt(length(AHE00))),
          upper = mean(AHE00) + qt(0.975,df=length(AHE00)-1) * (sd(AHE00) / sqrt(length(AHE00))))
