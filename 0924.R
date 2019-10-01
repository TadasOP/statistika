library(dplyr)

data()
head(mtcars)
?mtcars

select(mtcars, am:wt)
select(mtcars, am:mpg)
select(mtcars, ends_with("t"))
select(mtcars, contains("m"))

mtcars %>% select(am:wt)
filter(mtcars, cyl %in% c(4,6))
filter(mtcars, cyl == 4 | cyl == 6)
filter(mtcars,  mpg >= 15, mpg <= 22)
filter(mtcars, mpg >= 15 & mpg <= 22)
filter(mtcars, hp > 100)

mtcars %>% filter(cyl %in% c(4, 6))

arrange(mtcars, hp, mpg)
arrange(mtcars, desc(hp))

rename(mtcars, svoris=wt)
rename(mtcars, svoris=wt, aj=hp)

mtcars2 <- mutate(mtcars,
                  kml= mpg/3.7854*1.6093)
grpby <- group_by(mtcars, am)
summarise(grpby, vid= mean(mpg))

select(mtcars, hp)
select(mtcars, -hp)

filter(mtcars, am==0)
filter(mtcars, am==0 & wt>4)
filter(mtcars, !cyl %in% c(4,6))

arrange(mtcars, wt)
arrange(mtcars, desc(cyl))

mutate(mtcars, wtkg=wt/2.2046)

mtcarsgrp <- group_by(mtcars, am)
summarise(mtcarsgrp, nmb=n(),
          vidsvoris = mean(wt))

vidurkiai <- mtcars %>%
  group_by(am) %>%
  summarise_all(list(~mean(.)))
vidurkiai

medianos <- mtcars %>%
  group_by(am) %>%
  summarise_all(list(~median(.)))
medianos

sample_n(mtcars, 5)
sample_frac(mtcars, 0.1)

distinct(mtcars)
distinct(mtcars, hp, .keep_all = TRUE)
distinct(mtcars, hp, vs, .keep_all = TRUE)

mtcars3 <- data.frame(rownames(mtcars),
                     mtcars)
colnames(mtcars3) <- c("Car", colnames(mtcars))
rownames(mtcars3) <- NULL
filter(mtcars3, grepl("er", Car))

summarise_at(mtcars, vars(mpg,wt),
             funs(n(), mean, median))

t <- mtcars %>%
  filter(cyl %in% c(4, 8)) %>%
  group_by(am) %>%
  do(head( . , 2))
t

p <- mtcars %>% select(mpg, cyl, wt, am) %>%
  filter(cyl %in% c(4, 8)) %>%
  group_by(am) %>%
  do(arrange(.,desc(wt))) %>% slice(3)
p
