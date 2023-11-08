

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b5340e56-d122-4ecf-8e19-412b36c7618f"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.5cf52c83-2daa-4e01-86bf-e5ce17ccab1d")
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
        select(variables, Estimate, Std..Error, OR, Lower_CI, Upper_CI, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7f1ae8fb-f9a8-4692-9e4c-c4ebdf0f4b98"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.5cf52c83-2daa-4e01-86bf-e5ce17ccab1d")
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
    Output(rid="ri.foundry.main.dataset.1086b72b-10c9-4c53-9424-6548fe938b61"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.ed6c1a7f-c2d2-4269-8a72-40b401220115")
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
    Output(rid="ri.foundry.main.dataset.3e16289c-997e-4a7f-aa2b-2a066399bb34"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.ed6c1a7f-c2d2-4269-8a72-40b401220115")
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
    Output(rid="ri.foundry.main.dataset.373f50ab-8841-470f-87b6-6f562b6e8123"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.ed6c1a7f-c2d2-4269-8a72-40b401220115")
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
    Output(rid="ri.foundry.main.dataset.9e3add2d-609e-49f7-925c-d4da617f9e4e"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
Covariate_model_2 <- function(Cohort_v05, Covariate_model) {
    library(dplyr)

    df <- Covariate_model %>%
        left_join(Cohort_v05 %>% select(person_id, drug_hcq_v1), by="person_id") %>%
        mutate(HCQ = ifelse(is.na(drug_hcq_v1), 0, 1))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.87a04ffe-8519-4ae8-92a9-107ca15a8c2a"),
    case_1_icc_sd=Input(rid="ri.foundry.main.dataset.76c541f2-7d81-4927-b978-f2cb6c181e95")
)
case_1_icc <- function(case_1_icc_sd) {
    library(dplyr)
    df <- case_1_icc_sd %>%
        mutate(ICC_NULL = round(sd_NULL^2/(sd_NULL^2+(pi^2 / 3)),4),
               ICC_MEM = round(sd_MEM^2/(sd_MEM^2+(pi^2 / 3)),4))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.12d4f3df-3659-4d95-9eff-0ce9d4a40d30"),
    case_1_icc_sd_dex=Input(rid="ri.foundry.main.dataset.dfdf528c-c568-4cb5-80c1-fbb0720ff1bb")
)
case_1_icc_dex <- function(case_1_icc_sd_dex) {
    library(dplyr)
    df <- case_1_icc_sd_dex %>%
        mutate(ICC_MEM = round(dex_sd^2/(dex_sd^2+(pi^2 / 3)),4))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.00228c55-00a0-4d8b-b9e8-cd03470963fc"),
    case_1_icc_sd_hcq=Input(rid="ri.foundry.main.dataset.c647b53f-70e4-4469-9d7b-c4c67ed4485c")
)
case_1_icc_hcq <- function(case_1_icc_sd_hcq) {
    library(dplyr)
    df <- case_1_icc_sd_hcq %>%
        mutate(ICC_MEM = round(hcq_sd^2/(hcq_sd^2+(pi^2 / 3)),4))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.76c541f2-7d81-4927-b978-f2cb6c181e95"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.5cf52c83-2daa-4e01-86bf-e5ce17ccab1d")
)
case_1_icc_sd <- function(complete_case_1_bmi) {
    
    library(lme4)
    library(dplyr) 
    vent <- complete_case_1_bmi %>%
            filter(Invasive_Ventilation==1)
    no_vent <- complete_case_1_bmi %>%
            filter(Invasive_Ventilation==0)

    rem_null <- glmer(REM ~ (1|data_partner_id), 
                    data = complete_case_1_bmi, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex_vent_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex_no_vent_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = no_vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    

    rem_mem <- glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_1_bmi, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    dex_vent_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), data = vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    dex_no_vent_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), data = no_vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    sd <- data.frame(
        Drug = c("HCQ", "REM", "DEX"),
        sd_NULL = c(
            attr(summary(rem_null)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_vent_null)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_no_vent_null)$varcor$data_partner_id, "stddev")
        ),
        sd_MEM = c(
            attr(summary(rem_mem)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_vent_mem)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_no_vent_mem)$varcor$data_partner_id, "stddev")
        )
    )
    return(sd)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.dfdf528c-c568-4cb5-80c1-fbb0720ff1bb"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.5cf52c83-2daa-4e01-86bf-e5ce17ccab1d")
)
case_1_icc_sd_dex <- function(complete_case_1_bmi) {
    
    library(lme4)
    library(dplyr)
    
    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_1_bmi, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    
    df <- data.frame(
            dex_sd = attr(summary(dex_mem)$varcor$data_partner_id, "stddev")
        )

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c647b53f-70e4-4469-9d7b-c4c67ed4485c"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.5cf52c83-2daa-4e01-86bf-e5ce17ccab1d")
)
case_1_icc_sd_hcq <- function(complete_case_1_bmi) {
    
    library(lme4)
    library(dplyr)
    
    hcq_mem <- glmer(HCQ ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_1_bmi, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    
    df <- data.frame(
            hcq_sd = attr(summary(hcq_mem)$varcor$data_partner_id, "stddev")
        )

    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.96fcbe6a-f05e-44a3-a49d-e147fea0047c"),
    complete_case_1_bmi=Input(rid="ri.foundry.main.dataset.5cf52c83-2daa-4e01-86bf-e5ce17ccab1d")
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
    Output(rid="ri.foundry.main.dataset.5f0d04d6-419a-4168-9d2e-b25d1d4b5627"),
    case_2_icc_sd=Input(rid="ri.foundry.main.dataset.22d526f6-4a0f-46e8-8ca2-90129f94e846")
)
case_2_icc <- function(case_2_icc_sd) {
    library(dplyr)
    df <- case_2_icc_sd %>%
        mutate(ICC_NULL = round(sd_NULL^2/(sd_NULL^2+(pi^2 / 3)),4),
               ICC_MEM = round(sd_MEM^2/(sd_MEM^2+(pi^2 / 3)),4))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d34b92e7-d07c-4c33-9873-e9da7e520918"),
    case_2_icc_sd_2=Input(rid="ri.foundry.main.dataset.d1679fba-d190-4688-a9e9-486c99a604ef")
)
case_2_icc_2 <- function(case_2_icc_sd_2) {
    library(dplyr)
    df <- case_2_icc_sd_2 %>%
        mutate(ICC_MEM = round(sd_MEM^2/(sd_MEM^2+(pi^2 / 3)),4))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.22d526f6-4a0f-46e8-8ca2-90129f94e846"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.ed6c1a7f-c2d2-4269-8a72-40b401220115")
)
case_2_icc_sd <- function(complete_case_2) {
    
    library(lme4)
    library(dplyr) 
    vent <- complete_case_2 %>%
            filter(Invasive_Ventilation==1)
    no_vent <- complete_case_2 %>%
            filter(Invasive_Ventilation==0)

    rem_null <- glmer(REM ~ (1|data_partner_id), 
                    data = complete_case_2, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex_vent_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex_no_vent_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = no_vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    

    rem_mem <- glmer(REM ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_2, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    dex_vent_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), data = vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    dex_no_vent_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), data = no_vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    sd <- data.frame(
        Drug = c("HCQ", "REM", "DEX"),
        sd_NULL = c(
            attr(summary(rem_null)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_vent_null)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_no_vent_null)$varcor$data_partner_id, "stddev")
        ),
        sd_MEM = c(
            attr(summary(rem_mem)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_vent_mem)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_no_vent_mem)$varcor$data_partner_id, "stddev")
        )
    )
    return(sd)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d1679fba-d190-4688-a9e9-486c99a604ef"),
    complete_case_2=Input(rid="ri.foundry.main.dataset.ed6c1a7f-c2d2-4269-8a72-40b401220115")
)
case_2_icc_sd_2 <- function(complete_case_2) {
    
    library(lme4)
    library(dplyr)
    

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_2, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    hcq_mem <- glmer(HCQ ~ age_group + sex + race_eth + Charlson_range + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = complete_case_2, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    sd <- data.frame(
        Drug = c("DEX", "HCQ"),
        sd_MEM = c(
            attr(summary(dex_mem)$varcor$data_partner_id, "stddev"),
            attr(summary(hcq_mem)$varcor$data_partner_id, "stddev")
        )
    )
    return(sd)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5cf52c83-2daa-4e01-86bf-e5ce17ccab1d"),
    Covariate_model_2=Input(rid="ri.foundry.main.dataset.9e3add2d-609e-49f7-925c-d4da617f9e4e")
)
complete_case_1_bmi <- function(Covariate_model_2) {
    library(dplyr)
    df <- Covariate_model_2 %>%
            filter(!is.na(race_eth)) %>%
            filter(!is.na(BMI_cat))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ed6c1a7f-c2d2-4269-8a72-40b401220115"),
    Covariate_model_2=Input(rid="ri.foundry.main.dataset.9e3add2d-609e-49f7-925c-d4da617f9e4e")
)
complete_case_2 <- function(Covariate_model_2) {
    library(dplyr)
    df <- Covariate_model_2 %>%
            filter(!is.na(race_eth))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1dbf8064-4628-41e3-ac7b-33b0074377dd"),
    Case_1_dex_no_vent=Input(rid="ri.foundry.main.dataset.b5340e56-d122-4ecf-8e19-412b36c7618f"),
    Case_1_dex_vent=Input(rid="ri.foundry.main.dataset.7f1ae8fb-f9a8-4692-9e4c-c4ebdf0f4b98"),
    case_1_rem=Input(rid="ri.foundry.main.dataset.96fcbe6a-f05e-44a3-a49d-e147fea0047c")
)
etable2 <- function(case_1_rem, Case_1_dex_vent, Case_1_dex_no_vent) {
    library(dplyr)
    df <- case_1_rem %>%
        select(variables, OR_CI) %>%
        rename(rem_OR = OR_CI) %>%
        left_join(Case_1_dex_vent %>%
                select(variables, OR_CI) %>%
                rename(dex_vent_OR = OR_CI), by = "variables") %>%
        left_join(Case_1_dex_no_vent %>%
                select(variables, OR_CI) %>%
                rename(dex_no_vent_OR = OR_CI), by = "variables") %>%
        filter(grepl('age', variables)|grepl('sex', variables)|grepl('race', variables)|grepl('Charlson', variables)|grepl('BMI', variables)|grepl('AKI', variables)|grepl('ECMO', variables)|grepl('Invasive', variables)|grepl('cv', variables))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.29498b72-54c5-4db2-a8b5-23fd52d17dd1"),
    Case_2_dex_no_vent=Input(rid="ri.foundry.main.dataset.1086b72b-10c9-4c53-9424-6548fe938b61"),
    Case_2_dex_vent=Input(rid="ri.foundry.main.dataset.3e16289c-997e-4a7f-aa2b-2a066399bb34"),
    Case_2_rem=Input(rid="ri.foundry.main.dataset.373f50ab-8841-470f-87b6-6f562b6e8123")
)
etable3 <- function(Case_2_rem, Case_2_dex_vent, Case_2_dex_no_vent) {
    library(dplyr)
    df <- Case_2_rem %>%
        select(variables, OR_CI) %>%
        rename(rem_OR = OR_CI) %>%
        left_join(Case_2_dex_vent %>%
                select(variables, OR_CI) %>%
                rename(dex_vent_OR = OR_CI), by = "variables") %>%
        left_join(Case_2_dex_no_vent %>%
                select(variables, OR_CI) %>%
                rename(dex_no_vent_OR = OR_CI), by = "variables") %>%
        filter(grepl('age', variables)|grepl('sex', variables)|grepl('race', variables)|grepl('Charlson', variables)|grepl('BMI', variables)|grepl('AKI', variables)|grepl('ECMO', variables)|grepl('Invasive', variables)|grepl('cv', variables))
    return(df)
    
}

