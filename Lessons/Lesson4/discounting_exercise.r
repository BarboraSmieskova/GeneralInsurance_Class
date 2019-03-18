
# Libraries
library(dplyr)
library(ggplot2)
library(ChainLadder)

# data about KPIs
dt_KPI <- read.csv("./Data/lesson2_KPI.csv")
dt_LatestView <- read.csv("./Data/lesson4_latestView.csv")
dt_PaidCase <- read.csv("./Data/lesson4_PaidCase.csv")

########################################
## Exercise 1
#  As we will be working with ChainLadder library, explore it a little bit.
#  To understand what it is doing run the example below (demo from ChainLadder library)

GenIns
plot(GenIns)
plot(GenIns, lattice=TRUE)

# think about some math, that could project the furure for the "unfinished lines" (e.g. liner models?)

# to explore a bit of math, look at the exmaples from help section on "chainladder" function
?chainladder

# the thing that is actually predicting the future is the age-to-age factors.
# Can you find them in the chainladder object?

# now try to predict what happens with the linse using the chain ladder technique (hint: search for predict)

chainladder(GenIns)
predict(chainladder(GenIns))

########################################
## Exercise 2
#  Now let's have a look at a couple of different triangles. The data is provided for 2 different businesses.
#  It also shows 2 different claim types for each. Can you describe how different they are?
#  Hint: Consider the length of tail, volatility, average sizes...
#  Hint2: There are 2 types of data - paid and case. Start using Paid...

## STEP1: Take paid data for House business and Small claim size
# Paid_HH_sml <- dt_PaidCase %>% filter(...) 
Paid_HH_sml <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Small" & dataset_type == "PAID")


## STEP2: Now convert the standard table into a triangle
# %>% as.triangle(...)
triangle_sml <- Paid_HH_sml %>% as.triangle(Paid_HH_sml, origin = "ay", dev = "dy", value = "SumOfamount")
View(triangle_smll) # zobrazim tabulku
head(triangle_sml) 

## STEP3: Now start plotting things to see more information
plot(triangle_sml,lattice=TRUE)

plot(predict(chainladder(triangle_sml)))

## STEP4: And get the aging factors and some other stat's out to see more details
# Hint: ata(...)

ata(triangle_tri)

## Now repeat for all types of buiness and sizes of claims. Compare the finding "ay", dev = "dy", value = "SumOfamount")

Paid_HH_lrg <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Large" & dataset_type == "PAID")
triangle_lrg <- Paid_HH_lrg %>% as.triangle(Paid_HH_lrg, origin = "ay", dev = "dy", value = "SumOfamount")
plot(triangle_lrg,lattice=TRUE)
plot(predict(chainladder(triangle_lrg)))
ata(triangle_lrg)
###
Paid_3P_sml <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Small" & dataset_type == "PAID")
triangle_sml_ou <- Paid_3P_sml %>% as.triangle(Paid_3P_sml, origin = "ay", dev = "dy", value = "SumOfamount")
plot(triangle_sml_ou,lattice=TRUE)
plot(predict(chainladder(triangle_sml_ou)))
ata(triangle_sml_ou)
###
Paid_3P_lrg <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Large" & dataset_type == "PAID")
triangle_lrg_ou <- Paid_3P_lrg %>% as.triangle(Paid_3P_lrg, origin = "ay", dev = "dy", value = "SumOfamount")
plot(triangle_lrg_ou,lattice=TRUE)
plot(predict(chainladder(triangle_lrg_ou)))
ata(triangle_lrg_ou)



## If you are now comforatble with what this does, try doing the same, but using additional information: The Case data!
## Hint: Sum Paid and Case together to come up with the final claims estimates (the Incurred claims)


#summed paid and case, using additional information

Paid_HH_lrg <- dt_PaidCase %>% filter(Business == "House" & ClaimSize == "Large")
triangle_lrg <- Paid_HH_lrg %>% as.triangle(Paid_HH_lrg, origin = "ay", dev = "dy", value = "SumOfamount")
plot(triangle_lrg,lattice=TRUE)
plot(predict(chainladder(triangle_lrg)))
ata(triangle_lrg)
###
Paid_3P_sml <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Small")
triangle_sml_ou <- Paid_3P_sml %>% as.triangle(Paid_3P_sml, origin = "ay", dev = "dy", value = "SumOfamount")
plot(triangle_sml_ou,lattice=TRUE)
plot(predict(chainladder(triangle_sml_ou)))
ata(triangle_sml_ou)
###
Paid_3P_lrg <- dt_PaidCase %>% filter(Business == "3rd Party" & ClaimSize == "Large")
triangle_lrg_ou <- Paid_3P_lrg %>% as.triangle(Paid_3P_lrg, origin = "ay", dev = "dy", value = "SumOfamount")
plot(triangle_lrg_ou,lattice=TRUE)
plot(predict(chainladder(triangle_lrg_ou)))
ata(triangle_lrg_ou)

########################################
## Exercise 3
## There are a couple of different types of business in the data from previous lesson. 
## What do you think, how long are the average delays in payments? Set up a table, that will 
## show your assumptions and commit it to your git repository.

#COMM
#Pri houses mame vyssie simple averages ako 3rd Party.
#Jedno plnenie je postacujuce \lahke chvosty\ na rozdiel od 3rd party (problemy \tazke chvosty\).
#Takmer minimalny rozdiel medzi small a large houses.A tiez rozdiel small a large 3rd Party nie je az tak moc velky, co sme mohli ocakavat.











