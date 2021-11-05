attach(Household_Pulse_data)

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 

model_logit1 <- glm(vaxx ~ GENID_DESCRIBE + ANXIOUS + WORRY,
                    family = binomial, data = Household_Pulse_data)

model_logit1 <- glm(vaxx ~ EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)

summary(model_logit1)

table(Household_Pulse_data$vaxx,Household_Pulse_data$GENID_DESCRIBE)

table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)
