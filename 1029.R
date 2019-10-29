#World bank
library(WDI) 

#World bank search
WDIsearch(string = 'GDP', field = 'indicator', cache = NULL)

# duomenu nuskaitymas world bank rodikliu bazese, 1994-2017m
dat = WDI(indicator = c('NY.GDP.DISC.KN'), country = 'LV', start = 2008, end = 2018, extra = TRUE)

#duomenu issaugojimas
write.csv2(dat, "duomenys.csv", row.names = FALSE)

teina041 <- get_eurostat(id="teina041", stringsAsFactors = FALSE)
library(dplyr)
data <- teina041 %>%
   filter(geo %in% c("LV"),
         time>="2004-01-01")




################################

library(eurostat)
install.packages("eurostat")
library(knitr)

#suranda viska su GDP
kable(head(search_eurostat("GDP")))

#suranda viska su GDP ... 
kable(head(search_eurostat("GDP and main components  (output, expenditure and income)")))

id <- search_eurostat("GDP and main components  (output, expenditure and income)")$code[1]
print(id)

#Duomenu atsisiuntimas
dat <- get_eurostat(id, time_format = "num")

#Duomenu filtravimas (atsirinkimas)
dat1 <- dat[dat$geo=="LT" & dat$time==2017,]
head(dat1)
dat2 <- dat[dat$geo=="LT" & dat$na_item=="B1GQ" & dat$unit=="CP_MEUR",]


