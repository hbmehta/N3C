

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.781c12a2-0b8b-425f-9e15-2331bcd08c12"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
DEX_Logistic_Regr <- function(covariate_v02) {
    library(dplyr)
    library(tibble)

    df <- covariate_v02 %>%
        mutate(week_v1 = ifelse(week=="2020-03-16", "1_2020-03-16", week)) %>%
        mutate(week_v1 = as.factor(week_v1))    

    logit <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week_v1 + cv_quar + race_eth*cv_quar + race_eth*Invasive_Ventilation, 
                data = df, family = "binomial")

    summary <- data.frame(summary(logit)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = Estimate,
               Std..Error = Std..Error,
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep=""),
               P_value = round(Pr...z..,3)) %>%
        select(variables, Estimate, Std..Error, OR_CI, P_value, Pr...z..)

    return(OR)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f8f8d565-69de-4ca6-950c-6bd8777148de"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
Dex_poisson_regr <- function(covariate_v02) {
    library(dplyr)
    require(sandwich)
    library(tibble)

    df <- covariate_v02 %>%
        mutate(week_v1 = ifelse(week=="2020-03-16", "1_2020-03-16", week)) %>%
        mutate(week_v1 = as.factor(week_v1))    

    logit <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week_v1 + center_volume, data = df, family = poisson(link = "log"))
    
    summary <- data.frame(summary(logit)$coefficients) %>%
            rownames_to_column(var = "variables")

    cov.m1 <- vcovHC(logit, type = "HC1")
    std.err <- sqrt(diag(cov.m1))   

    RR <- summary %>%
        mutate(Robust_SE = std.err,
               RR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Robust_SE),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Robust_SE),2),
               P_value= round(2 * pnorm(abs(Estimate/Robust_SE), lower.tail = FALSE),3)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               RR_CI = paste(RR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, RR_CI, P_value, Robust_SE, RR, Lower_CI, Upper_CI)

    return(RR)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8ef2fb24-b717-475e-9e89-c6b28c875e00"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
HCQ_Logistic_Regr <- function(covariate_v02) {
    library(dplyr)
    library(tibble)

    logit <- glm(HCQ ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar, 
                 data = covariate_v02, family = "binomial")

    summary <- data.frame(summary(logit)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep=""),
               P_value = round(Pr...z..,3)) %>%
        select(variables, Estimate, Std..Error, OR_CI, P_value, Pr...z..) 

    return(OR)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7fdb68ee-c640-476e-be2a-c43cdae0e7f8"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
Hcq_poisson_regr <- function(covariate_v02) {
    library(dplyr)
    require(sandwich)
    library(tibble)

    model <- glm(HCQ ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar, data = covariate_v02,                   family = poisson(link="log"))
    
    summary <- data.frame(summary(model)$coefficients) %>%
            rownames_to_column(var = "variables")

    cov.m1 <- vcovHC(model, type = "HC0")
    std.err <- sqrt(diag(cov.m1))   

    RR <- summary %>%
        mutate(Robust_SE = std.err,
               RR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               RR_CI = paste(RR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) 

    return(RR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3cee188d-ff0b-48a5-8f91-48fb962b31bd"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
REM_Logistic_Regr <- function(covariate_v02) {
    library(dplyr)
    library(tibble)

    df <- covariate_v02 %>%
        mutate(week_v1 = ifelse(week=="2020-03-09", "1_2020-03-09", week)) %>%
        mutate(week_v1 = as.factor(week_v1))

    logit <- glm(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week_v1 + cv_quar, 
                data = df, family = "binomial")

    summary <- data.frame(summary(logit)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep=""),
               P_value = round(Pr...z..,3)) %>%
        select(variables, Estimate, Std..Error, OR_CI, P_value, Pr...z..)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c04fccf4-42b1-4f04-a0aa-b512364bc1f3"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
Rem_poisson_regr <- function(covariate_v02) {
    library(dplyr)
    require(sandwich)
    library(tibble)

    df <- covariate_v02 %>%
        mutate(week_v1 = ifelse(week=="2020-03-09", "1_2020-03-09", week)) %>%
        mutate(week_v1 = as.factor(week_v1)) 

    logit <- glm(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week_v1 + cv_quar, data = df,                   family = poisson(link = "log"))
    
    summary <- data.frame(summary(logit)$coefficients) %>%
            rownames_to_column(var = "variables")

    cov.m1 <- vcovHC(logit, type = "HC1")
    std.err <- sqrt(diag(cov.m1))   

    RR <- summary %>%
        mutate(Robust_SE = std.err,
               RR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Robust_SE),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Robust_SE),2),
               P_value= round(2 * pnorm(abs(Estimate/Robust_SE), lower.tail = FALSE),3)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               RR_CI = paste(RR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) 

    return(RR)

    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
covariate_v02 <- function(covariate) {
    library(dplyr)
    library(lubridate)

    df <- covariate %>%
        mutate(week_num = cut.Date(covid_admission, breaks = "1 week", labels = FALSE)) %>%
        mutate(week = ymd( "2020-02-10" ) + weeks(week_num - 1 )) %>%
        mutate(BMI_cat=as.factor(BMI_cat),
               age_group=as.factor(age_group),
               sex = as.factor(sex),
               race_eth = as.factor(race_eth),
               Charlson_range = as.factor(Charlson_range),
               week = as.factor(week),
               AKI_in_hospital = as.factor(AKI_in_hospital),
               ECMO = as.factor(ECMO),
               Invasive_Ventilation = as.factor(Invasive_Ventilation),
               Severity_Type = as.factor(Severity_Type),
               Q_Score_cat = as.factor(Q_Score_cat),
               cv_quar = as.factor(cv_quar))
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.234b96ea-53da-4c17-9cad-210fd2c78f21"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
dex_interaction_sig <- function(covariate_v02) {
    library(dplyr)
    library(tibble)

    model <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + Severity_Type + week + cv_quar, 
                 , data = covariate_v02, family = "binomial")

    # week*Invasive_Ventilation
    Inter_0 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + Severity_Type + week + cv_quar +  week*Invasive_Ventilation, 
                 , data = covariate_v02, family = "binomial")

    # race_eth*center_volume
    Inter_1 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + Severity_Type + week + cv_quar  + race_eth*cv_quar, 
                 , data = covariate_v02, family = "binomial")
    
    # race_eth*Invasive_Ventilation
    Inter_2 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + Severity_Type + week + cv_quar  + race_eth*Invasive_Ventilation, 
                 , data = covariate_v02, family = "binomial")
    
    # race_eth*ECMO
    Inter_3 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + Severity_Type + week + cv_quar  + race_eth*ECMO, 
                 , data = covariate_v02, family = "binomial")

    test_0 <- anova(model, Inter_0, test="LRT")
    test_1 <- anova(model, Inter_1, test="LRT")
    test_2 <- anova(model, Inter_2, test="LRT")
    test_3 <- anova(model, Inter_3, test="LRT")

    return(data.frame(test_2))
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7a5ce76b-3552-428a-bd4f-922617434b3c"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
dex_race_cv_interaction <- function(covariate_v02) {
    library(dplyr)
    library(tibble)

    df <- covariate_v02 %>%
        mutate(week_v1 = ifelse(week=="2020-03-16", "1_2020-03-16", week)) %>%
        mutate(week_v1 = as.factor(week_v1))    

    cv_1 <- df %>% filter(cv_quar==1) 
    cv_2 <- df %>% filter(cv_quar==2) 
    cv_3 <- df %>% filter(cv_quar==3) 
    cv_4 <- df %>% filter(cv_quar==4)

    m_cv_1 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week_v1 + race_eth*Invasive_Ventilation, data = cv_1, family = "binomial")
    m_cv_2 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week_v1 + race_eth*Invasive_Ventilation, data = cv_2, family = "binomial")
    m_cv_3 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week_v1 + race_eth*Invasive_Ventilation, data = cv_3, family = "binomial")
    m_cv_4 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week_v1 + race_eth*Invasive_Ventilation, data = cv_4, family = "binomial")
    
    summary_1 <- data.frame(summary(m_cv_1)$coefficients) %>%
            rownames_to_column(var = "variables")
    summary_2 <- data.frame(summary(m_cv_2)$coefficients) %>%
            rownames_to_column(var = "variables")
    summary_3 <- data.frame(summary(m_cv_3)$coefficients) %>%
            rownames_to_column(var = "variables")
    summary_4 <- data.frame(summary(m_cv_4)$coefficients) %>%
            rownames_to_column(var = "variables")

    OR_1 <- summary_1 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(cv_1_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, cv_1_OR_CI)
    OR_2 <- summary_2 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(cv_2_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, cv_2_OR_CI)

    OR_3 <- summary_3 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(cv_3_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, cv_3_OR_CI)

    OR_4 <- summary_4 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(cv_4_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, cv_4_OR_CI)
    
    final_OR <- OR_1 %>%
            left_join(OR_2, by="variables") %>%
            left_join(OR_3, by="variables") %>%
            left_join(OR_4, by="variables") 

    return(final_OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2ff1be31-52c3-486c-a69d-31b283380e5c"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
dex_race_vent_interaction <- function(covariate_v02) {
    library(dplyr)
    library(tibble)

    df <- covariate_v02 %>%
        mutate(week_v1 = ifelse(week=="2020-03-16", "1_2020-03-16", week)) %>%
        mutate(week_v1 = as.factor(week_v1))    
    vent_0 <- df %>% filter(Invasive_Ventilation==0)
    vent_1 <- df %>% filter(Invasive_Ventilation==1)

    m_vent_0 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week_v1 + cv_quar + race_eth*cv_quar, data = vent_0, family = "binomial")
    m_vent_1 <- glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week_v1 + cv_quar + race_eth*cv_quar, data = vent_1, family = "binomial")

    
    summary_0 <- data.frame(summary(m_vent_0)$coefficients) %>%
            rownames_to_column(var = "variables")
    summary_1 <- data.frame(summary(m_vent_1)$coefficients) %>%
            rownames_to_column(var = "variables")

    OR_0 <- summary_0 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(vent_0_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, vent_0_OR_CI)

    OR_1 <- summary_1 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(vent_1_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, vent_1_OR_CI)
    
    final_OR <- OR_0 %>%
            left_join(OR_1, by="variables")

    return(final_OR)
}

