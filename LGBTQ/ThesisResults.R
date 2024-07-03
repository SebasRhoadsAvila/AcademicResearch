# Exploratory analysis

library(readxl)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(scales)
library(reshape2)
library(kableExtra)

TMODULO <- read.csv("C:/Users/acer/Downloads/TMODULO.csv")

# Comenzamos por generar las variables "anylgbt", "male" y "female"

TMODULO <- TMODULO %>%
  mutate(anylgbt=0,                           #"anylgbt" solo con el valor de 0 por ahora
         male= case_when(P7_1==1 ~ 1,         #Persona con el sexo masculino asignado al nacer
                         P7_1==2 ~ 0),
         female= case_when(P7_1==2 ~ 1,
                           P7_1==1 ~ 0))      #Persona con el sexo femenino asignado al nacer

TMODULO <- TMODULO %>%
  mutate(anylgbt= case_when(male==1 & P7_4_1==1 ~ 1,    #Hombre cuyo primer encuentro erótico fue con un hombre
                            female==1 & P7_4_2==1 ~ 1,  #Mujer cuyo primer encuentro erótico fue con una mujer
                            P7_4_3==1 ~ 1,              #Primer encuentro erótico con una persona con un sexo distinto al femenino y al masculino
                            male==1 & P7_6_1==1 ~ 1,    #Hombre cuyo primer encuentro sexual fue con un hombre
                            female==1 & P7_6_2==1 ~ 1,  #Mujer cuyo primer encuentro sexual fue con una mujer
                            P7_6_3==1 ~ 1,              #Primer encuentro sexual con una persona con un sexo distinto al femenino y al masculino
                            male==1 & P7_7==1 ~ 1, #Hombre atraido en cierta medida por hombres
                            male==1 & P7_7==2 ~ 1,
                            male==1 & P7_7==3 ~ 1,
                            male==1 & P7_7==4 ~ 1,
                            female==1 & P7_7==1 ~ 1, #MUJER ATRAIDA EN CIERTA MEDIDA POR MUJERES
                            female==1 & P7_7==2 ~ 1,
                            female==1 & P7_7==3 ~ 1,
                            female==1 & P7_7==5 ~ 1,
                            male==1 & P7_8==1 ~ 1, #Hombre que ha tenido relaciones sexuales con hombres en cierta medida
                            male==1 & P7_8==2 ~ 1,
                            male==1 & P7_8==3 ~ 1,
                            male==1 & P7_8==4 ~ 1,
                            female==1 & P7_8==1 ~ 1, #Mujer que ha tenido relaciones sexuales con mujeres en cierta medida
                            female==1 & P7_8==2 ~ 1,
                            female==1 & P7_8==3 ~ 1,
                            female==1 & P7_8==5 ~ 1,
                            male==1 & P7_9==1 ~ 1, #Hombres que ha tenido relaciones sexuales con hombres en los últimos 12 meses en cierta medida
                            male==1 & P7_9==2 ~ 1,
                            male==1 & P7_9==3 ~ 1,
                            male==1 & P7_9==4 ~ 1,
                            female==1 & P7_9==1 ~ 1,  #Mujeres que han tenido relaciones sexuales con mujeres en los últimos 12 meses en cierta medida
                            female==1 & P7_9==2 ~ 1,
                            female==1 & P7_9==3 ~ 1,
                            female==1 & P7_9==5 ~ 1,
                            P8_1==1 | P8_1==2 | P8_1==3 | P8_1==6 ~ 1,  #Gustos no heterosexuales
                            P8_1A!=NA ~ 1,        #Orientación no heterosexual
                            male==1 & P9_1!=1 ~ 1,   #Hombre no cisgenero
                            female==1 & P9_1!=2 ~ 1)) %>% #Mujer no cisgenero
  mutate(anylgbt = ifelse(is.na(anylgbt),0,anylgbt))                                     #Aquí convierto los missing values en 0

summary(as.factor(TMODULO$anylgbt))



#| Emotional disturbance

TMODULO <- TMODULO %>%
  mutate(insomnio= case_when(P10_1_1== 2 ~ 0,
                             P10_1_1== 1 ~ 1),
         estres= case_when(P10_1_2== 2 ~ 0,
                           P10_1_2== 1 ~ 1),
         depresion= case_when(P10_1_3== 2 ~ 0,
                              P10_1_3== 1 ~ 1),
         apetito_peso= case_when(P10_1_4== 2 ~ 0,
                                 P10_1_4== 1 ~ 1),
         angustia_miedo_o_ansiedad= case_when(P10_1_5== 2 ~ 0,
                                              P10_1_5== 1 ~ 1),
         suicide_ideation= case_when(P10_2== 2 ~ 0,
                                     P10_2== 1 ~ 1))  #Como queremos trabajaro con dummies, vamos a reemplazar los valores '2' con '0'

# Summarize the count for each emotional problem
emotional_problems_summary <- TMODULO %>% 
  filter(anylgbt == 1) %>%
  summarise(Insomnia = sum(insomnio)/7368,
            Stress = sum(estres)/7368,
            Depression = sum(depresion)/7368,
            Appetite_Weight_Change = sum(apetito_peso)/7368,
            Anguish_Fear_Anxiety = sum(angustia_miedo_o_ansiedad)/7368,
            "Suicide Ideation"= sum(suicide_ideation)/7368) %>% 
  mutate(Insomnia_SEM = sqrt(Insomnia * (1 - Insomnia) / 7368),
         Stress_SEM = sqrt(Stress * (1 - Stress) / 7368),
         Depression_SEM = sqrt(Depression * (1 - Depression) / 7368),
         Appetite_Weight_Change_SEM = sqrt(Appetite_Weight_Change * (1 - Appetite_Weight_Change) / 7368),
         Anguish_Fear_Anxiety_SEM = sqrt(Anguish_Fear_Anxiety * (1 - Anguish_Fear_Anxiety) / 7368),
         Suicide_Ideation_SEM = sqrt(`Suicide Ideation` * (1 - `Suicide Ideation`) / 7368))
  


non_lgbt_emotional_problems_summary <- TMODULO %>% 
  filter(anylgbt == 0) %>%
  summarise(Insomnia = sum(insomnio)/36821,
            Stress = sum(estres)/36821,
            Depression = sum(depresion)/36821,
            Appetite_Weight_Change = sum(apetito_peso)/36821,
            Anguish_Fear_Anxiety = sum(angustia_miedo_o_ansiedad)/36821,
            "Suicide Ideation"= sum(suicide_ideation)/36821) %>%
  mutate(Insomnia_SEM = sqrt(Insomnia * (1 - Insomnia) / 36821),
         Stress_SEM = sqrt(Stress * (1 - Stress) / 36821),
         Depression_SEM = sqrt(Depression * (1 - Depression) / 36821),
         Appetite_Weight_Change_SEM = sqrt(Appetite_Weight_Change * (1 - Appetite_Weight_Change) / 36821),
         Anguish_Fear_Anxiety_SEM = sqrt(Anguish_Fear_Anxiety * (1 - Anguish_Fear_Anxiety) / 36821),
         Suicide_Ideation_SEM = sqrt(`Suicide Ideation` * (1 - `Suicide Ideation`) / 36821))

# Combine the summaries and add a group column
combined_summary <- rbind(emotional_problems_summary %>% mutate(Group = 'LGBTQ'),
                          non_lgbt_emotional_problems_summary %>% mutate(Group = 'Non-LGBTQ'))

# Using pivot_longer instead of gather to create additional value columns
long_data <- combined_summary %>%
  pivot_longer(
    cols = c(Insomnia, Stress, Depression, Appetite_Weight_Change, Anguish_Fear_Anxiety, `Suicide Ideation`),
    names_to = "Emotional_Problem",
    values_to = "Mean"
  ) %>%
  pivot_longer(
    cols = c(Insomnia_SEM, Stress_SEM, Depression_SEM, Appetite_Weight_Change_SEM, Anguish_Fear_Anxiety_SEM, `Suicide_Ideation_SEM`),
    names_to = "SEM_name",
    values_to = "SEM"
  ) %>%
  mutate(
    Emotional_Problem = str_replace(Emotional_Problem, "_SEM", ""),
    SEM_name = str_replace(SEM_name, "_SEM", "")
  ) %>%
  filter(str_replace(Emotional_Problem, " ", "_") == SEM_name) %>%
  dplyr::select(-SEM_name)

# Calculate the confidence intervals (95% CI for example)
long_data <- long_data %>%
  mutate(Lower_CI = Mean - qnorm(0.975) * SEM,
         Upper_CI = Mean + qnorm(0.975) * SEM)
# Create the bar graph with bars side by side
ggplot(long_data, aes(x = Emotional_Problem, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  geom_text(aes(label = sprintf("%.2f%%", Mean * 100)), 
            position = position_dodge(width = 0.7), 
            vjust = 2.5, 
            size = 3.5, 
            fontface = "bold") + # Make the label text bold
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                width = .6, 
                position = position_dodge(width = .9),
                size = 1,    # Makes the error bars thicker
                color = "black") +
  theme_minimal() +
  labs(title = "Emotional Health Among LGBTQ and Non-LGBTQ Individuals",
       x = "Emotional Problem",
       y = "Proportion for Each Group") +
  scale_y_continuous(labels = percent_format(scale = 100)) + # Convert y-axis to percentage
  scale_x_discrete(labels = c("Anguish_Fear_Anxiety" = "Feelings of anguish, 
  fear or anxiety",
                              "Appetite_Weight_Change" = "Changes in 
  appetite or weight",
                              "Depression" = "Depression",
                              "Insomnia" = "Insomnia",
                              "Stress" = "Stress",
                              "Suicide Ideation" = "Suicide Ideation")) +
  scale_fill_manual(values = c("LGBTQ" = "red3", "Non-LGBTQ" = "skyblue2")) +
  theme(legend.position = "bottom",
        text = element_text(face = "bold"),  # Correct argument to 'face'
        axis.title = element_text(size = 15,
                                  face = "bold"),  # Bold axis titles
        axis.text.x = element_text(size = 10,
                                   face = "bold", 
                                   angle = 0, hjust = 0.5), # Rotate x-axis labels for better fit
        axis.text.y = element_text(size = 10,
                                   face = "bold"), # Bold y-axis labels
        plot.title = element_text(size = 18)  )

#| SEXUAL VIOLENCE


TMODULO <- TMODULO %>%
  mutate(sexual_attact_threat= case_when(P11_1_1!= 1 ~ 0,
                                         P11_1_1== 1 ~ 1),
         sexual_solicitation= case_when(P11_1_2!= 1 ~ 0,
                                     P11_1_2== 1 ~ 1),
         forced_sex= case_when(P11_1_3!= 1 ~ 0,
                               P11_1_3== 1 ~ 1),
         groping= case_when(P11_1_6!= 1 ~ 0,
                            P11_1_6== 1 ~ 1)) %>%
  mutate(sexual_attact_threat = ifelse(is.na(sexual_attact_threat),0,sexual_attact_threat),
         sexual_solicitation = ifelse(is.na(sexual_solicitation),0,sexual_solicitation),
         forced_sex = ifelse(is.na(forced_sex),0,forced_sex),
         groping = ifelse(is.na(groping),0,groping))  

# Summarize the count for each example of sexual violence 
sexual_violence_summary <- TMODULO %>% 
  filter(anylgbt == 1) %>%
  summarise(sexual_attact_threat = sum(sexual_attact_threat)/7368,
            sexual_solicitation = sum(sexual_solicitation)/7368,
            forced_sex = sum(forced_sex)/7368,
            groping = sum(groping)/7368) %>%
  mutate(sexual_attact_threat_SEM = sqrt(sexual_attact_threat * (1 - sexual_attact_threat) / 7368),
         sexual_solicitation_SEM = sqrt(sexual_solicitation * (1 - sexual_solicitation) / 7368),
         forced_sex_SEM = sqrt(forced_sex * (1 - forced_sex) / 7368),
         groping_SEM = sqrt(groping * (1 - groping) / 7368))


non_lgbt_sexual_violence_summary <- TMODULO %>% 
  filter(anylgbt == 0) %>%
  summarise(sexual_attact_threat = sum(sexual_attact_threat)/36821,
            sexual_solicitation = sum(sexual_solicitation)/36821,
            forced_sex = sum(forced_sex)/36821,
            groping = sum(groping)/36821) %>%
  mutate(sexual_attact_threat_SEM = sqrt(sexual_attact_threat * (1 - sexual_attact_threat) / 36821),
         sexual_solicitation_SEM = sqrt(sexual_solicitation * (1 - sexual_solicitation) / 36821),
         forced_sex_SEM = sqrt(forced_sex * (1 - forced_sex) / 36821),
         groping_SEM = sqrt(groping * (1 - groping) / 36821))

# Combine the summaries and add a group column
combined_sviolence <- rbind(sexual_violence_summary %>% mutate(Group = 'LGBTQ'),
                            non_lgbt_sexual_violence_summary %>% mutate(Group = 'Non-LGBTQ'))

# Using pivot_longer instead of gather to create additional value columns
long_violence_data <- combined_sviolence %>%
  pivot_longer(
    cols = c(sexual_attact_threat, sexual_solicitation, forced_sex, groping),
    names_to = "Sexual_Violence",
    values_to = "Mean"
  ) %>%
  pivot_longer(
    cols = c(sexual_attact_threat_SEM, sexual_solicitation_SEM, forced_sex_SEM, groping_SEM),
    names_to = "SEM_name",
    values_to = "SEM"
  ) %>%
  mutate(
    Sexual_Violence = str_replace(Sexual_Violence, "_SEM", ""),
    SEM_name = str_replace(SEM_name, "_SEM", "")
  ) %>%
  filter(str_replace(Sexual_Violence, " ", "_") == SEM_name) %>%
  dplyr::select(-SEM_name)

# Calculate the confidence intervals (95% CI for example)
long_violence_data <- long_violence_data %>%
  mutate(Lower_CI = Mean - qnorm(0.975) * SEM,
         Upper_CI = Mean + qnorm(0.975) * SEM)

# Create the bar graph with bars side by side
ggplot(long_violence_data, aes(x = Sexual_Violence, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  geom_text(aes(label = sprintf("%.2f%%", Mean * 100)), 
            position = position_dodge(width = 0.7), 
            vjust = 3.5, 
            size = 3.5, 
            fontface = "bold") + # Make the label text bold
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                width = .6, 
                position = position_dodge(width = .9),
                size = 1,    # Makes the error bars thicker
                color = "black") +
  scale_x_discrete(labels = c("sexual_attact_threat" = "Attack/Threat",
                              "sexual_solicitation" = "Solicitation",
                              "forced_sex" = "Forced Sex",
                              "groping" = "Groping")) +
  theme_minimal() +
  labs(title = "Sexual Violence Among LGBTQ and Non-LGBTQ Individuals",
       x = "Type of Sexual Violence",
       y = "Proportion for Each Group") +
  scale_y_continuous(labels = percent_format(scale = 100)) + # Convert y-axis to percentage
  scale_fill_manual(values = c("LGBTQ" = "red3", "Non-LGBTQ" = "skyblue2")) +
  theme(legend.position = "bottom",
        text = element_text(face = "bold"),  # Correct argument to 'face'
        axis.title = element_text(size = 15,
                                  face = "bold"),  # Bold axis titles
        axis.text.x = element_text(size = 10,
                                   face = "bold", 
                                   angle = 0, hjust = 0.5), # Rotate x-axis labels for better fit
        axis.text.y = element_text(size = 10,
                                   face = "bold"), # Bold y-axis labels
        plot.title = element_text(size = 18)  )  # Bold y-axis labels



#| SEXUAL VIOLENCE as 1 variable


TMODULO <- TMODULO %>%
  mutate(sexual_violence =0) %>%
  mutate(sexual_violence= case_when(sexual_attact_threat== 1 ~ 1,
                                    sexual_solicitation == 1 ~ 1,
                                    forced_sex == 1 ~ 1,
                                    groping == 1 ~ 1)) %>%
  mutate(sexual_violence = ifelse(is.na(sexual_violence),0,sexual_violence))  

summary(as.factor(TMODULO$sexual_violence))

# Victims and non victims

sviolence_data <- TMODULO %>%
  filter(sexual_violence==1) %>%
  summarise(Insomnia=sum(insomnio)/9491,
            Stress=sum(estres)/9491,
            Depression=sum(depresion)/9491,
            Appetite_Weight_Change=sum(apetito_peso)/9491,
            Anguish_Fear_Anxiety=sum(angustia_miedo_o_ansiedad)/9491,
            suicide_ideation=sum(suicide_ideation)/9491) %>%
  mutate(Insomnia_SEM = sqrt(Insomnia * (1 - Insomnia) / 9491),
         Stress_SEM = sqrt(Stress * (1 - Stress) / 9491),
         Depression_SEM = sqrt(Depression * (1 - Depression) / 9491),
         Appetite_Weight_Change_SEM = sqrt(Appetite_Weight_Change * (1 - Appetite_Weight_Change) / 9491),
         Anguish_Fear_Anxiety_SEM = sqrt(Anguish_Fear_Anxiety * (1 - Anguish_Fear_Anxiety) / 9491),
         suicide_ideation_SEM = sqrt(suicide_ideation * (1 - suicide_ideation) / 9491))



nsviolence_data <- TMODULO %>%
  filter(sexual_violence==0) %>%
  summarise(Insomnia=sum(insomnio)/34698,
            Stress=sum(estres)/34698,
            Depression=sum(depresion)/34698,
            Appetite_Weight_Change=sum(apetito_peso)/34698,
            Anguish_Fear_Anxiety=sum(angustia_miedo_o_ansiedad)/34698,
            suicide_ideation=sum(suicide_ideation)/34698) %>%
  mutate(Insomnia_SEM = sqrt(Insomnia * (1 - Insomnia) / 34698),
         Stress_SEM = sqrt(Stress * (1 - Stress) / 34698),
         Depression_SEM = sqrt(Depression * (1 - Depression) / 34698),
         Appetite_Weight_Change_SEM = sqrt(Appetite_Weight_Change * (1 - Appetite_Weight_Change) / 34698),
         Anguish_Fear_Anxiety_SEM = sqrt(Anguish_Fear_Anxiety * (1 - Anguish_Fear_Anxiety) / 34698),
         suicide_ideation_SEM = sqrt(suicide_ideation * (1 - suicide_ideation) / 34698))

# Combine the summaries and add a group column
combined_violence <- rbind(sviolence_data %>% mutate(Group = 'Victims'),
                           nsviolence_data %>% mutate(Group = 'Non-Victims'))

# Using pivot_longer instead of gather to create additional value columns
long_sviolence_data <- combined_violence %>%
  pivot_longer(
    cols = c(Insomnia, Stress, Depression, Appetite_Weight_Change,Anguish_Fear_Anxiety,suicide_ideation),
    names_to = "Emotional_Discomfort",
    values_to = "Mean"
  ) %>%
  pivot_longer(
    cols = c(Insomnia_SEM, Stress_SEM, Depression_SEM, Appetite_Weight_Change_SEM,Anguish_Fear_Anxiety_SEM,suicide_ideation_SEM),
    names_to = "SEM_name",
    values_to = "SEM"
  ) %>%
  mutate(
    Emotional_Discomfort = str_replace(Emotional_Discomfort, "_SEM", ""),
    SEM_name = str_replace(SEM_name, "_SEM", "")
  ) %>%
  filter(str_replace(Emotional_Discomfort, " ", "_") == SEM_name) %>%
  dplyr::select(-SEM_name)

# Calculate the confidence intervals (95% CI for example)
long_sviolence_data <- long_sviolence_data %>%
  mutate(Lower_CI = Mean - qnorm(0.975) * SEM,
         Upper_CI = Mean + qnorm(0.975) * SEM)

# Adjust the factor levels to ensure Victims are plotted first
long_sviolence_data$Group <- factor(long_sviolence_data$Group, levels = c("Victims", "Non-Victims"))

# Create the bar graph with bars side by side
ggplot(long_sviolence_data, aes(x = Emotional_Discomfort, y = Mean, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9)) +
  geom_text(aes(label = sprintf("%.2f%%", Mean * 100)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.89, # Adjust this value as needed to position your labels correctly
            size = 3.5, 
            fontface = "bold") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                width = .6, 
                position = position_dodge(width = .9),
                size = 1,    # Makes the error bars thicker
                color = "black") +
  scale_x_discrete(labels = c("Insomnia" = "Insomnia",
                              "Stress" = "Stress",
                              "Depression" = "Depression",
                              "Appetite_Weight_Change" = "Changes in apetite 
 or weight",
                              "Anguish_Fear_Anxiety" = "Feelings of anguish, 
 fear or anxiety",
                              "suicide_ideation" = "Suicide Ideation")) +
  theme_minimal() +
  labs(title = "Mental Health Among Victims and Non-Victims of Sexual Violence",
       x = "Emotional Problem",
       y = "Proportion for Each Group") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + # Convert y-axis to percentage
  scale_fill_manual(values = c("Victims" = "orangered", "Non-Victims" = "chartreuse4")) +
  theme(legend.position = "bottom",
        text = element_text(face = "bold"),  # Correct argument to 'face'
        axis.title = element_text(size = 15,
                                  face = "bold"),  # Bold axis titles
        axis.text.x = element_text(size = 10,
                                   face = "bold", 
                                   angle = 0, hjust = 0.5), # Rotate x-axis labels for better fit
        axis.text.y = element_text(size = 10,
                                   face = "bold"), # Bold y-axis labels
        plot.title = element_text(size = 18)  )  # Bold y-axis labels

#| REGRESSION

insomnio0 <- lm(insomnio ~ anylgbt, data = TMODULO)
stress0 <- lm(estres ~ anylgbt, data = TMODULO)
depre0 <- lm(depresion ~ anylgbt, data = TMODULO)
apetito0 <- lm(apetito_peso ~ anylgbt, data = TMODULO)
angustia0 <- lm(angustia_miedo_o_ansiedad ~ anylgbt, data = TMODULO)
suici0 <- lm(suicide_ideation ~ anylgbt, data = TMODULO)

stargazer(insomnio0,
          stress0,
          depre0,
          apetito0,
          angustia0,
          suici0,
          type = "text")

TMODULO <- TMODULO %>%  #Rename sexual violence indicators for clarity
  mutate(SexualAttackThreat= sexual_attact_threat,
         Solicitation= sexual_solicitation,
         ForcedSex= forced_sex,
         Gropping= groping)


M1 <- lm(SexualAttackThreat ~ anylgbt, TMODULO)
M2 <- lm(Solicitation ~ anylgbt, TMODULO)
M3 <- lm(ForcedSex ~ anylgbt, TMODULO)
M4 <- lm(Gropping ~ anylgbt, TMODULO)
stargazer(M1,M2,M3,M4,type = "text")





insomnio1 <- lm(insomnio ~ anylgbt + SexualAttackThreat, TMODULO)
insomnio2 <- lm(insomnio ~ anylgbt + Solicitation, TMODULO)
insomnio3 <- lm(insomnio ~ anylgbt + ForcedSex, TMODULO)
insomnio4 <- lm(insomnio ~ anylgbt  + Gropping, TMODULO)
insomnio5 <- lm(insomnio ~ anylgbt + SexualAttackThreat + Solicitation + ForcedSex + Gropping, TMODULO)
stress1 <- lm(estres ~ anylgbt + SexualAttackThreat, TMODULO)
stress2 <- lm(estres ~ anylgbt + Solicitation, TMODULO)
stress3 <- lm(estres ~ anylgbt + ForcedSex, TMODULO)
stress4 <- lm(estres ~ anylgbt  + Gropping, TMODULO)
stress5 <- lm(estres ~ anylgbt + SexualAttackThreat + Solicitation + ForcedSex + Gropping, TMODULO)
depre1 <- lm(depresion ~ anylgbt + SexualAttackThreat, TMODULO)
depre2 <- lm(depresion ~ anylgbt + Solicitation, TMODULO)
depre3 <- lm(depresion ~ anylgbt + ForcedSex, TMODULO)
depre4 <- lm(depresion ~ anylgbt  + Gropping, TMODULO)
depre5 <- lm(depresion ~ anylgbt + SexualAttackThreat + Solicitation + ForcedSex + Gropping, TMODULO)
apetito1 <- lm(apetito_peso ~ anylgbt + SexualAttackThreat, TMODULO)
apetito2 <- lm(apetito_peso ~ anylgbt + Solicitation, TMODULO)
apetito3 <- lm(apetito_peso ~ anylgbt + ForcedSex, TMODULO)
apetito4 <- lm(apetito_peso ~ anylgbt  + Gropping, TMODULO)
apetito5 <- lm(apetito_peso ~ anylgbt + SexualAttackThreat + Solicitation + ForcedSex + Gropping, TMODULO)
angustia1 <- lm(angustia_miedo_o_ansiedad ~ anylgbt + SexualAttackThreat, TMODULO)
angustia2 <- lm(angustia_miedo_o_ansiedad ~ anylgbt + Solicitation, TMODULO)
angustia3 <- lm(angustia_miedo_o_ansiedad ~ anylgbt + ForcedSex, TMODULO)
angustia4 <- lm(angustia_miedo_o_ansiedad ~ anylgbt  + Gropping, TMODULO)
angustia5 <- lm(angustia_miedo_o_ansiedad ~ anylgbt + SexualAttackThreat + Solicitation + ForcedSex + Gropping, TMODULO)
suici1 <- lm(suicide_ideation ~ anylgbt + SexualAttackThreat, TMODULO)
suici2 <- lm(suicide_ideation ~ anylgbt + Solicitation, TMODULO)
suici3 <- lm(suicide_ideation ~ anylgbt + ForcedSex, TMODULO)
suici4 <- lm(suicide_ideation ~ anylgbt  + Gropping, TMODULO)
suici5 <- lm(suicide_ideation ~ anylgbt + SexualAttackThreat + Solicitation + ForcedSex + Gropping, TMODULO)


stargazer(insomnio0,insomnio1,insomnio2,insomnio3,insomnio4,insomnio5,type = "text",df = FALSE)
stargazer(stress0,stress1,stress2,stress3,stress4,stress5,type = "text",df = FALSE)
stargazer(depre0,depre1,depre2,depre3,depre4,depre5,type = "text",df = FALSE)
stargazer(apetito0,apetito1,apetito2,apetito3,apetito4,apetito5,type = "text",df = FALSE)
stargazer(angustia0,angustia1,angustia2,angustia3,angustia4,angustia5,type = "text",df = FALSE)
stargazer(suici0,suici1,suici2,suici3,suici4,suici5,type = "text",df = FALSE)







