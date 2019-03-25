library(ggplot2)
library(dplyr)
library(lubridate)


dt_Policy <- read.csv("D:\\General\\GeneralInsurance_Class-Class2019\\Data\\lesson5_PolicyHistory.csv") %>% distinct(NrPolicy, NrObject, .keep_all = TRUE) 
dt_Claims <- read.csv("D:\\General\\GeneralInsurance_Class-Class2019\\Data\\lesson5_Claims.csv") %>% distinct(NrClaim, .keep_all = TRUE)

#spojenie suborov
dt_pol_w_claims <- left_join(dt_Policy, dt_Claims, by = c("NrPolicy", "NrObject")

#time exposure-ako dlho trva poistka
#celkova strata pripadajuca na jednu poistnu zmluvu
#"cena" cloveka pre poistovnu na den

########

dt_pol_w_claims %>% group_by(is.na(Paid)) %>% summarise(cnt = n())


#
dt_pol_w_claims %>% filter(!is.na(Paid)) %>% select(Paid, Dt_Exp_Start, Dt_Exp_End) %>% arrange(desc(Paid)) %>% head()

dt_pol_w_claims <- 
  dt_pol_w_claims %>% mutate(Time_Exposure = lubridate::dmy(Dt_Exp_End) - lubridate::dmy(Dt_Exp_Start))


#
dt_pol_w_claims %>% filter(!is.na(Paid)) %>% select(Paid, Dt_Exp_Start, Dt_Exp_End, Time_Exposure)  %>% arrange(desc(Paid)) %>% head()

dt_pol_w_claims <- 
  dt_pol_w_claims %>% mutate(Ult_Loss = Paid + Reserves,Burning_Cost = ifelse(is.na(Ult_Loss), 0,  Ult_Loss / as.integer(Time_Exposure)))

#
dt_pol_w_claims %>% filter(!is.na(Paid)) %>% select(Paid, Reserves, Ult_Loss, Burning_Cost) %>% head()


dt_pol_w_claims %>% group_by(D_age)%>%ggplot(aes(y = Burning_Cost, x = D_age)) + geom_jitter()
#Najvacsie hodnoty su v intervale priblizne 40-45 rokov. Kritickejsie obdobie pre cloveka (pripadne situacia, kedy rodicia pozicaju svoje auto detom).

dt_pol_w_claims %>% ggplot(aes(group=Construct_year, y = Burning_Cost, x = Construct_year)) + geom_boxplot() + ylim(0, 250)
#Vidime, ze by si poistovna mala davat pozor na auta vyrobene v 2009-2013. 

dt_pol_w_claims %>%  group_by(Construct_year) %>% summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),cnt = n()) %>% 
  arrange(desc(BC_avg)) %>% head() 
#Najrizikovejsim sa podla priemeru Burning cost zda rok 1997. Ale treba poznamenat ze mame malo dat. Mozme to povazvat za nie velmi vyznamny faktor.

dt_pol_w_claims %>% group_by(Construct_year) %>% summarise(BC_avg = mean(Burning_Cost, na.rm  = TRUE),  cnt = n()) %>% 
  arrange(desc(cnt)) %>% head() 
#V tomto pripadeje najrizikovejsie su vozidla vyrobene v roku 2011. Ale kedze poistovna mala davat pozor na auta vyrobene v 2009-2013, tak tu to relativne nove automobily. 


##################################################################################

ggplot(data = dt_pol_w_claims %>% filter(Burning_Cost != 0, Burning_Cost < 100),
                aes(x = Burning_Cost)) +geom_histogram()
                

#model pre vek poistovatela
model_first <- glm(data = dt_pol_w_claims  %>% filter(Burning_Cost != 0, Burning_Cost < 110),
              formula = Burning_Cost ~ D_age,
              family = Gamma())
summary(model_first)
#Mozme tvrdit, ze vsetky veky su statisticky vyznamne. 


model_second <- glm(data = dt_pol_w_claims  %>% filter(Burning_Cost != 0, Burning_Cost < 250),
              formula = Burning_Cost ~ Construct_year,
              family = Gamma())
summary(model_second)





