library("ggplot2")
dat <- read.csv("nations.csv", header = TRUE)
head(dat)

dt <- select(dat,country,year,gdp_percap,birth_rate,population, region)
dt1 <- filter(dt,year=="2013")
head(dt1)

p <- ggplot(data=dt1, mapping = aes(x=gdp_percap, y=birth_rate))+
  geom_point()
p

p <- ggplot(data=dt1, mapping=aes(x=gdp_percap, y= birth_rate, size=population, col=region))+
  geom_point()
p

options(scipen=8)
p <- ggplot(data=dt1, mapping = aes(x=gdp_percap, y=birth_rate))

p+geom_point(colour="green", size=2, shape=21, fill="white")+
  geom_smooth(method="loess") + scale_x_log10()+
  theme(axis.text.x = element_text(angle=45, hjust=1, size="12", colour="brown"))+
  xlab("LOG_BVP") +ylab("Gimstamumas")+
  ggtitle("BVP vs Gimstamumas")

ggplot(mtcars,aes(x="",y=mpg))+
  geom_boxplot(stat="boxplot", outlier.color = "red")

ggplot(dt1, aes(x="", y=gdp_percap))+
  geom_boxplot(stat="boxplot", outlier.color = "blue", outlier.shape = 23, outlier.fill = "green")

is_outlier <- function(x) {
  return(x < quantile(x, 0.25, na.rm=TRUE)-
           1.5 * IQR(x, na.rm = TRUE) |
           x > quantile(x, 0.75, na.rm = TRUE) +
           1.5 * IQR(x, na.rm = TRUE))
}


dt1%>%
  group_by(region) %>%
  mutate(outlier= ifelse(is_outlier(gdp_percap), country, NA)) %>%
  ggplot(.,aes(x=factor(region), y=gdp_percap))+
  geom_boxplot()+
  geom_text(aes(label=outlier), na.rm=TRUE, hjust=-0.3)+
  theme(axis.text.x = element_text(angle=25))

ggplot(mtcars,aes(x=as.factor(cyl)))+
  geom_bar(colour="blue", fill="white")+
  xlab("Cilindru skaicius")+ ylab("Daznis")

ggplot(mtcars, aes(x=mpg))+
  geom_histogram(binwidth=5, col="blue", fill="white")+
  scale_x_continuous(breaks=seq(10,40,5))

dt1 %>% filter(region == "Europe & Central Asia") %>%
  mutate(cuts=cut(gdp_percap, breaks=seq(0, 155000, by=10000))) %>%
  ggplot(., aes(x=as.factor(cuts)))+
  geom_histogram(binwidth = 5,col="blue", fill="yellow", stat="count")+
  theme(axis.text.x= element_text(angle=45))

ggplot(mtcars, aes(x=hp,y=mpg))+
  geom_point()+
  geom_line()

ggplot(mtcars, aes(x=hp,y=mpg))+
  geom_point()+
  geom_smooth(method="loess")
