attach(Household_Pulse_data)

#I used the variables gender, anxious, and worry to see relationships between mental health and the decision to get the vaccine for each gender

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 


model_logit1 <- glm(vaxx ~ GENID_DESCRIBE + ANXIOUS + WORRY,
                    family = binomial, data = Household_Pulse_data)

#south region

pick_use <- (Household_Pulse_data$REGION == "South") 

dat_use<- subset(Household_Pulse_data, pick_use1)

dat_use$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 

summary(model_logit1)

table(Household_Pulse_data$vaxx,Household_Pulse_data$GENID_DESCRIBE)

table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)


#output

Call:
glm(formula = vaxx ~ GENID_DESCRIBE + ANXIOUS + WORRY, family = binomial, 
    data = Household_Pulse_data)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.2991   0.4126   0.4607   0.4941   0.8474  

Coefficients:
                                                       Estimate Std. Error z value Pr(>|z|)
(Intercept)                                             1.19064    0.11910   9.997  < 2e-16
GENID_DESCRIBEmale                                      0.19781    0.11939   1.657  0.09756
GENID_DESCRIBEfemale                                    0.04966    0.11872   0.418  0.67571
GENID_DESCRIBEtransgender                              -0.21793    0.22585  -0.965  0.33457
GENID_DESCRIBEother                                    -0.35111    0.15260  -2.301  0.02140
ANXIOUSno anxiety over past 2 wks                       0.45456    0.22098   2.057  0.03969
ANXIOUSseveral days anxiety over past 2 wks             0.83419    0.22188   3.760  0.00017
ANXIOUSmore than half the days anxiety over past 2 wks  0.68073    0.22592   3.013  0.00259
ANXIOUSnearly every day anxiety                         0.61856    0.22719   2.723  0.00648
WORRYno worry over past 2 wks                           0.34648    0.22053   1.571  0.11615
WORRYseveral days worried over past 2 wks               0.30016    0.22166   1.354  0.17570
WORRYmore than half the days worried over past 2 wks    0.05497    0.22666   0.243  0.80836
WORRYnearly every day worry                            -0.16497    0.22788  -0.724  0.46911
                                                          
(Intercept)                                            ***
GENID_DESCRIBEmale                                     .  
GENID_DESCRIBEfemale                                      
GENID_DESCRIBEtransgender                                 
GENID_DESCRIBEother                                    *  
ANXIOUSno anxiety over past 2 wks                      *  
ANXIOUSseveral days anxiety over past 2 wks            ***
ANXIOUSmore than half the days anxiety over past 2 wks ** 
ANXIOUSnearly every day anxiety                        ** 
WORRYno worry over past 2 wks                             
WORRYseveral days worried over past 2 wks                 
WORRYmore than half the days worried over past 2 wks      
WORRYnearly every day worry                               
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 49071  on 68262  degrees of freedom
Residual deviance: 48055  on 68250  degrees of freedom
  (851 observations deleted due to missingness)
AIC: 48081

Number of Fisher Scoring iterations: 5

> 
> table(Household_Pulse_data$vaxx,Household_Pulse_data$GENID_DESCRIBE)
       
           NA  male female transgender other
  FALSE    87  2828   4855          33   134
  TRUE    505 23862  35218         167   574
> 
> table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)
       
        less than hs some hs HS diploma some coll assoc deg bach deg adv deg
  FALSE          115     269       1647      2396      1132     1565     813
  TRUE           290     652       6097     12022      6266    18272   16727
> 


