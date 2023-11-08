

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.059ee1ff-8292-41e3-8347-91e31b09a81c"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.87466566-74a2-4651-96b0-ce7e7e23d57b")
)
Case_1_dex <- function(complete_case_1_bmi) {
    
    library(lme4)
    library(dplyr)

    library(tibble)
    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_1_bmi, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b1786ab3-e61b-433d-94a6-1447fc77a6be"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.87466566-74a2-4651-96b0-ce7e7e23d57b")
)
Case_1_dex_no_vent <- function(complete_case_1_bmi) {
    
    library(lme4)
    library(dplyr)

    library(tibble)
    df <- complete_case_1_bmi %>%
        filter(Invasive_Ventilation==0)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), data = df, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = exp(Estimate),
               Lower_CI = exp(Estimate + qnorm(0.025)*Std..Error),
               Upper_CI = exp(Estimate + qnorm(0.975)*Std..Error)
        ) %>%
        mutate(Estimate = round(Estimate,3),
               Std..Error = round(Std..Error,3),
               OR_CI = paste(round(OR,2), " (", round(Lower_CI,2), ", ", round(Upper_CI,2), ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR, Lower_CI, Upper_CIOR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c8473843-7a77-4cfd-a186-211f277d82a5"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.87466566-74a2-4651-96b0-ce7e7e23d57b")
)
Case_1_dex_vent <- function(complete_case_1_bmi) {
    
    library(lme4)
    library(dplyr)

    library(tibble)
    df <- complete_case_1_bmi %>%
        filter(Invasive_Ventilation==1)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), data = df, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = exp(Estimate),
               Lower_CI = exp(Estimate + qnorm(0.025)*Std..Error),
               Upper_CI = exp(Estimate + qnorm(0.975)*Std..Error)
        ) %>%
        mutate(Estimate = round(Estimate,3),
               Std..Error = round(Std..Error,3),
               OR_CI = paste(round(OR,2), " (", round(Lower_CI,2), ", ", round(Upper_CI,2), ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1d8e7bc0-6d6f-4335-bebd-977320d2056c"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.a9ef25f4-8b53-4bb5-a56d-52b310411137")
)
Case_2_dex <- function(complete_case_2) {
    library(lme4)
    library(dplyr)

    library(tibble)
    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_2, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)

    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.225bdab2-a93a-4bbd-9c7b-c1fe92506920"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.a9ef25f4-8b53-4bb5-a56d-52b310411137")
)
Case_2_dex_no_vent <- function(complete_case_2) {
    library(lme4)
    library(dplyr)

    library(tibble)
    df <- complete_case_2 %>%
        filter(Invasive_Ventilation==0)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), data = df, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = exp(Estimate),
               Lower_CI = exp(Estimate + qnorm(0.025)*Std..Error),
               Upper_CI = exp(Estimate + qnorm(0.975)*Std..Error)
        ) %>%
        mutate(Estimate = round(Estimate,3),
               Std..Error = round(Std..Error,3),
               OR_CI = paste(round(OR,2), " (", round(Lower_CI,2), ", ", round(Upper_CI,2), ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.664755c2-e8f5-4a42-aaa2-2fbf2d6ed847"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.a9ef25f4-8b53-4bb5-a56d-52b310411137")
)
Case_2_dex_vent <- function(complete_case_2) {
    library(lme4)
    library(dplyr)

    library(tibble)
    df <- complete_case_2 %>%
        filter(Invasive_Ventilation==1)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), data = df, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = exp(Estimate),
               Lower_CI = exp(Estimate + qnorm(0.025)*Std..Error),
               Upper_CI = exp(Estimate + qnorm(0.975)*Std..Error)
        ) %>%
        mutate(Estimate = round(Estimate,3),
               Std..Error = round(Std..Error,3),
               OR_CI = paste(round(OR,2), " (", round(Lower_CI,2), ", ", round(Upper_CI,2), ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.80dc8ae8-8b3c-4aae-b2f3-c45198df75ce"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.a9ef25f4-8b53-4bb5-a56d-52b310411137")
)
Case_2_rem <- function(complete_case_2) {
    
    library(lme4)
    library(dplyr)

    library(tibble)
    rem_mem <- glmer(REM ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_2, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(rem_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)

    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.068e84b1-c9e8-46c4-a14d-8eb2add920fb"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.87466566-74a2-4651-96b0-ce7e7e23d57b")
)
case_1_hcq <- function(complete_case_1_bmi) {
    
    library(lme4)
    library(dplyr)

    library(tibble)
    hcq_mem <- glmer(HCQ ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), 
                    data = complete_case_1_bmi, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(hcq_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)

    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b7c6713f-a67a-4038-993a-024ab98547c4"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.87466566-74a2-4651-96b0-ce7e7e23d57b")
)
case_1_rem <- function(complete_case_1_bmi) {
    
    library(lme4)
    library(dplyr)

    library(tibble)
    rem_mem <- glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_1_bmi, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(rem_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a237caf5-0a5c-41d7-9f79-848603cf1edb"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.a9ef25f4-8b53-4bb5-a56d-52b310411137")
)
case_2_hcq <- function(complete_case_2) {
    
    library(lme4)
    library(dplyr)

    library(tibble)
    hcq_mem <- glmer(HCQ ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), 
                    data = complete_case_2, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(hcq_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)

    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.87466566-74a2-4651-96b0-ce7e7e23d57b"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
complete_case_1_bmi <- function(covariate_v02) {
    library(dplyr)
    df <- covariate_v02 %>%
            mutate(log_cv = log(center_volume)) %>%
            filter(!sex %in% "NO MATCHING CONCEPT") %>%
            filter(!race_eth %in% "6_Missing") %>%
            filter(!BMI_cat %in% "7_Missing")
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a9ef25f4-8b53-4bb5-a56d-52b310411137"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
complete_case_2 <- function(covariate_v02) {
    library(dplyr)
    df <- covariate_v02 %>%
            mutate(log_cv = log(center_volume)) %>%
            filter(!sex %in% "NO MATCHING CONCEPT") %>%
            filter(!race_eth %in% "6_Missing") 
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c2f2fbf9-4823-49ae-be53-a87652ed8c43"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.87466566-74a2-4651-96b0-ce7e7e23d57b")
)
unnamed <- function(complete_case_1_bmi) {
    
    library(dplyr)

    library(tibble)
    rem_mem <- glm(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv, data = complete_case_1_bmi, family = "binomial")

    summary <- data.frame(summary(rem_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

