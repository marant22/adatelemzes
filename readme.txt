This data set includes 721 Pokemon, including their number, name, first and second type, and basic stats:
HP, Attack, Defense, Special Attack, Special Defense, and Speed.
It has been of great use when teaching statistics to kids.
With certain types you can also give a geeky introduction to machine learning.

This are the raw attributes that are used for calculating how much damage an attack will do in the games.
This dataset is about the pokemon games (NOT pokemon cards or Pokemon Go).

The data as described by Myles O'Neill is:

#: ID for each pokemon
Name: Name of each pokemon
Type 1: Each pokemon has a type, this determines weakness/resistance to attacks
Type 2: Some pokemon are dual type and have 2
Total: sum of all stats that come after this, a general guide to how strong a pokemon is
HP: hit points, or health, defines how much damage a pokemon can withstand before fainting
Attack: the base modifier for normal attacks (eg. Scratch, Punch)
Defense: the base damage resistance against normal attacks
SP Atk: special attack, the base modifier for special attacks (e.g. fire blast, bubble beam)
SP Def: the base damage resistance against special attacks
Speed: determines which pokemon attacks first each round

`#` = col_double(),
  Name = col_character(),
  `Type 1` = col_character(),
  `Type 2` = col_character(),
  Total = col_double(),
  HP = col_double(),
  Attack = col_double(),
  Defense = col_double(),
  `Sp. Atk` = col_double(),
  `Sp. Def` = col_double(),
  Speed = col_double(),
  Generation = col_double(),
  Legendary = col_logical()

Code:

#installed packages: swirl, dplyr, readr, ggplot2, tidyverse, lubridate (not all needed)

pokemons <- read_csv("C:/Users/Máté/Documents/Beadando/Pokemon.csv")
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
#creat plot

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





