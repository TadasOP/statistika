###############################################################################
####                    Klausimų pavyzdžiai spalio 8 d.                    ####
###############################################################################

# Įvykdykite komandą
B<-2:7
# Klausimai:
# Koks objekto B tipas?
Inegralas
# Kokie objekto matavimai (elementų skaičius)?
length(B)
# Parašykite komandų (kurias įvykdžius gaunami atsakymai į klausimus) 
head(B)
# tekstą ir atsakymus.
#
#Įvykdykite komandą

library(car)
install.packages("car")
#      Klausimai apie duomenų rinkinį UN
# 
#
###########################################################
#
#   Klausimai:
#
# 1)	Kiek eilučių ir stulpelių turi duomenų aibė
# (parašykit atitinkamą komandą ir atsakymą)?
nrow(UN)
ncol(UN)

# 2)	Parašykit komandą, kurią įvykdžius, kintamojo “ppgdp”  
# pavadinimas pasikeis į “ppbvp”.
install.packages("dplyr")
library(dplyr)
data <- UN
data <- rename(data, ppbvp=ppgdp)

# 3) Parašykite komandas, kurias įvykdžius, iš duomenų aibės bus išrinktos:
#    a) eilutės (šalių duomenys), kuriose tikėtina moterų gyvenimo trukmė didesnė nei 45 metai ir 
#        ne didesnė nei 70,
filter(data, lifeExpF >45 & lifeExpF <=70)

#    b) eilutės su gimstamumo rodikliu (fertility) didesniu nei 2 arba kūdikių 
#        mirtingumu (infantMortality) mažesniu nei 50,
filter(data, fertility > 2 & infantMortality < 50)

#    c) eilutes, kuriose kintamasis pctUrban neturi praleistų reikšmių.
filter(data, pctUrban != "NA")

# 4)	Parašykit komandą (ir jos atsakymą), kurias įvydžius, apskaičiuosime  
# vidutinį kūdikių mirštamumo rodiklį šalims, kurių ppbvp>1000.   
data4 <- filter(data, ppbvp >1000)
mean(data4[,"infantMortality"], na.rm = TRUE)
#####################################################
#
#   Įvykdykite žemiau parašytas komandas
#
####################################################
library(ggplot2)
UNN<-UN
UNN$country<-rownames(UN)
UNN<-na.omit(sample_n(UNN,10))
ggplot(UNN,aes(x=ppgdp,y=infantMortality,color=country))+
  geom_point()+
  labs(title="Kūdikių mirtingumo priklausomybė nuo PPbvp",
     x = "PPbvp",
     y = "kūdikių mirtingumas",
     color="Šalis")+
  theme(axis.text.x= element_text(angle=90))
##########################################################################
#
# 1) Papildykite ggplot komandą taip, kad x ašies pavadinimas būtų “PPbvp”, 
#    o y ašies “kūdikių mirtingumas”.+
# 2) Papildykite komandą, kad legendos pavadinimas būtų "šalis".+
# 3) Papildykite  komandą, kad būtų pateiktas grafiko pavadinimas: 
# “Kūdikių mirtingumo priklausomybė nuo PPbvp”.+
# 4) Perrašykite komandą taip, kad x ašies žymelės būtų pateiktos vertikaliai.+
# 5) Papildykite komandą taip, kad būtų nubrėžta horizontali atskaitos linija y=20.
