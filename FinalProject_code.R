## ----setup, message=FALSE----------------------------------------------------------------------------
library(readr)
library(dplyr)
library(ggplot2)

pokemon = read_csv("Pokemon.csv")

head(pokemon)



## ----message=FALSE-----------------------------------------------------------------------------------
#rename columns with spaces/dots
pokemon = pokemon %>%
  rename(
    Sp_Atk = 'Sp. Atk',
    Sp_Def = 'Sp. Def',
    Type1 = 'Type 1',
    Type2 = 'Type 2'
  )


## ----------------------------------------------------------------------------------------------------
#handle missing values
pokemon = pokemon %>%
  mutate(Type2 = ifelse(is.na(Type2), "None", Type2))

#create new variable called HighStat to determine whether or not a Pokemon's total stats are >500
pokemon = pokemon %>%
  mutate(
    HighStat = ifelse(Total >= 500, TRUE, FALSE)
  )


## ----------------------------------------------------------------------------------------------------
#remove mega pokemon since they aren't part of our analysis
pokemon = pokemon %>%
  filter(!grepl("Mega", Name))

head(pokemon)


## ----------------------------------------------------------------------------------------------------

summary(pokemon)

#summary of key stats by Legendary and HighStat
summary_table = pokemon %>%
  group_by(Legendary, HighStat) %>%
  summarise(
    mean_total = mean(Total),
    median_total = median(Total),
    count = n()
  )

summary_table 


## ----------------------------------------------------------------------------------------------------
ggplot(pokemon, aes(x = Legendary, y = Total, fill = Legendary)) + 
  geom_boxplot() + 
  labs(title = "Total Stats by Legendary Status", x = "Legendary", y = "Total Stats")

ggplot(pokemon, aes(x = Type1, fill = HighStat)) + 
  geom_bar(position = "fill") + 
  coord_flip() + 
  labs(title = "Proportion of High Stat Pokemon by Type", x = "Type", y = "Proportion")


## ----------------------------------------------------------------------------------------------------
model = glm(HighStat ~ Legendary + Type1 + Generation, data = pokemon, family = binomial)
summary(model)

library(broom)
tidy(model)

