#install.packages('readr')
#install.packages("dplyr")
#install.packages("swirl")
#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("lubridate")



library(readr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(dplyr)
library(swirl)



pokemons <- read_csv("C:/Users/Netti/Documents/GitHub/adatelemzes/Pokemon.csv")
pokemons



#The Type 1:



gbType1 <- pokemons %>%
  group_by(`Type 1`)%>%
  count()
#counting by Type 1



gbType1sorted <- arrange(gbType1,n)
#sorting



gbType1sorted$`Type 1` <- factor(gbType1sorted$`Type 1`, levels = c("Flying", "Fairy", "Ice", "Fighting", "Steel", "Poison", "Dark", "Dragon", "Ghost", "Ground", "Electric", "Rock", "Fire", "Psychic", "Bug", "Grass", "Normal", "Water"))
#factors for plot



ggplot(gbType1sorted, aes(x= `Type 1`, y = `n`))+
  geom_col()
#create plot
#The legendary rate



gbLegendaryRate <- pokemons %>%
  group_by(`Legendary`)%>%
  count()
#counting by Legendary



ggplot(data = gbLegendaryRate, aes(x = "", y = n, fill = Legendary)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y") +
  labs(title = "Rate of legendary pokemons in %") +
  theme_void() +
  geom_text(aes(x="", y = n, label=n/800*100))
#pie chart of legendary pokemons



#The heat map



types <- pokemons %>%
  transmute(
    typeone = as.factor(pokemons$`Type 1`),
    typetwo = as.factor(pokemons$`Type 2`)
  )%>%
  filter(!is.na(typetwo), !is.na(typeone))%>%
  group_by(typeone, typetwo)%>%
  count() %>%
  complete(typeone, typetwo, fill = list(n=0))
#Create the table for heat map



ggplot(types, aes(x=typeone, y= typetwo , fill = n))+
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "Oranges", direction = 1)+
  theme_minimal()
#Type 1-2 heat map



#leiro statisztika
summary(pokemons)

points <- pokemons[6:11]
cor(points)


total <- pokemons[5]
total
legendary <- pokemons[13]
kor <- cor(legendary,total)
sqrt(kor)


str(pokemons)
gen <- pokemons[12]
gen

poke<- read.table("C:/Users/Netti/Documents/GitHub/adatelemzes/Pokemon.csv", sep=",", header=T)
head(poke)

reg <- lm(Total~Generation, data=pokemons)
summary(reg)
plot(Generation ~ Total, data=pokemons) 
abline(reg)

ggplot(pokemons, aes(x= Generation, y= Total)) + geom_point() + stat_smooth(method = "lm", col ="red")

#generacionkent hany darab
gbGenCount <- pokemons %>%
  group_by(`Generation`)%>%
  count()
gbGenCount


#generacionkenti pontok atlaga
aggregate(pokemons[5:11], list(pokemons$Generation), count)

#legendasok szama generacionkent
aggregate(pokemons[13], list(pokemons$Generation), sum)

