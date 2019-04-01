
#balicky
library(dplyr)
library(ggplot2)


dt_pol_w_claims <- readRDS("D:\\General\\GeneralInsurance_Class-Class2019\\Data\\lesson6_dt_pol_w_claims.rds")

ind <- sample(2, nrow(dt_pol_w_claims), replace=TRUE, prob=c(0.80, 0.20))
dt_pol_w_claims <- mutate(dt_pol_w_claims,
                data_status = ifelse(ind == 1, 
                                     "Training",
                                     ifelse(ind == 2, 
                                            "Validation", 
                                            "Unseen")
                ))
                
train <- dt_pol_w_claims %>% filter(data_status == "Training")
val <- dt_pol_w_claims %>% filter(data_status == "Validation")

mse <- function(prediction, actual)
{
return(sum((prediction-actual)^2, na.rm = TRUE)/length(prediction))
}
#######################
#moj model
model_first <- glm(data = dt_pol_w_claims  %>% filter(Burning_Cost != 0, Burning_Cost < 110),
              formula = Burning_Cost ~ D_age,
              family = Gamma())
summary(model_first)

#moj glm model
model_first_g<- glm(data = train,
              formula = Burning_Cost ~ D_age,
              family = Gamma())
summary(model_first_g)


mse(predict(model_first_g, train, type = "response"), train$Burning_Cost) 
prediction <- predict(model_first_g, val, type = "response") #predikcia  na Validation 
mse(prediction, val$Burning_Cost) 

#Chceme zlepsit model, teda aby bol presnejsi, preto pridam dalsiu premennu napr typ vozidla.Pripadne by sme mohli pridat este aj ine premenne a pozriet sa aky by to malo dopad.

model_type<- glm(data = train,
              formula = Burning_Cost ~ D_age +Veh_type1,
              family = Gamma())
summary(model_type)
mse(predict(model_type, train, type = "response"), train$Burning_Cost) #191.48
mse(predict(model_type, val, type = "response"), val$Burning_Cost) #282.87

 #pridanie dalsiej premennej sposobilo zmensenie hodnoty mse, cim sa "spresnil" nas model.

#graf pre model_type
 
 source("Support/emb_chart.R")
 emblem_graph(
  dt.frm = train %>% cbind(data.frame(pred = predict(model_type, train, type = "response"))),
  x_var =  "Veh_type1",
  target = "Burning_Cost",
  prediction =  "pred"
   
  )
  

