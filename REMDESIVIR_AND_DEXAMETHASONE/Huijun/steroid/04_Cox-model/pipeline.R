

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca"),
    Cohort_steroids_2=Input(rid="ri.foundry.main.dataset.3a25a2ee-af9d-4ef3-b49d-e4e63f661d4a")
)
Cohort_steroids_3 <- function(Cohort_steroids_2) {
    library(dplyr)
    df <- Cohort_steroids_2 %>%
            mutate(time_to_death = ifelse(death_date<=drug_start_date, 1, as.numeric(death_date-drug_start_date)+1)) %>%
            mutate(time_to_end = as.numeric(covid_discharge - drug_start_date)+1) %>%
            mutate(futime = pmin(time_to_death, time_to_end, na.rm = T)) %>%
            mutate(censor_flag = ifelse(futime==time_to_death, 1, 0)) %>%
            mutate(censor_flag = ifelse(is.na(censor_flag),0,censor_flag)) %>%

            mutate(time_to_death_2 = ifelse(death_date<=covid_admission, 1, as.numeric(death_date-covid_admission)+1)) %>%
            mutate(time_to_end_2 = as.numeric(covid_discharge - covid_admission)+1) %>%
            mutate(futime_2 = pmin(time_to_death_2, time_to_end_2, na.rm = T)) %>%
            mutate(censor_flag_2 = ifelse(futime_2==time_to_death_2, 1, 0)) %>%
            mutate(censor_flag_2 = ifelse(is.na(censor_flag_2),0,censor_flag_2))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.89acc2ae-52b9-4353-aa10-5c9682a82275"),
    count_data_duration=Input(rid="ri.foundry.main.dataset.2569f402-6812-49fd-9dd3-36c435d6a314")
)
Duration_adjusted <- function(count_data_duration) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(tstart, tstop, death_flag_2) ~ Drug_use_flag + age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + log_cv,
        data= count_data_duration, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(-z, -Lower_CI, -Upper_CI)

    return(HR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.53d7a753-9595-44f9-9438-7c6f2e31b6de"),
    count_data_duration=Input(rid="ri.foundry.main.dataset.2569f402-6812-49fd-9dd3-36c435d6a314")
)
Duration_unadjusted <- function(count_data_duration) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(tstart, tstop, death_flag_2) ~ Drug_use_flag,
        data= count_data_duration, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")
    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep=""))

    return(HR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a5dcd527-9002-4913-a2d1-16ec0fa0bde4"),
    count_data_timing=Input(rid="ri.foundry.main.dataset.a882f7f4-767f-4ec5-a0d2-e009740f62a1")
)
Timing_unadjusted <- function(count_data_timing) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(tstart, tstop, death_flag_2) ~ Drug_use,
        data= count_data_timing, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep=""))

    return(HR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2569f402-6812-49fd-9dd3-36c435d6a314"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
count_data_duration <- function(Cohort_steroids_3) {
    library(dplyr)
    library(survival)

    df <- Cohort_steroids_3 %>%
            mutate(Drug_timing = Drug_timing+1) %>%
            mutate(Time_to_drug_end = Drug_timing + Drug_duration) 
    
    df1 <- tmerge(df, df, id=person_id,
                 death_flag_2 = event(futime, death_flag),
                 Drug_use = tdc(Drug_timing),
                 Drug_end = tdc(Time_to_drug_end)) %>% 
            mutate(Drug_use_flag = ifelse(Drug_end==1,0,Drug_use))

    return(df1)
    
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a882f7f4-767f-4ec5-a0d2-e009740f62a1"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
count_data_timing <- function(Cohort_steroids_3) {
    library(survival)
    data <- Cohort_steroids_3 %>%
            mutate(Drug_timing = Drug_timing+1)
    df <- tmerge(data, data, id=person_id,
                 death_flag_2 = event(futime, death_flag),
                 Drug_use = tdc(Drug_timing))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.98bc9d8c-ed12-4ad6-b44f-b7894516f77f"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
logit_adj_duration <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)
    
    logit <- glm(death_flag ~ Drug_duration_cat + age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + log_cv, 
                data = Cohort_steroids_3, family = "binomial")
                
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
    Output(rid="ri.foundry.main.dataset.bc1967e5-fd7c-4d2b-b7ce-55245e948dad"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
logit_adj_timing <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)
    
    logit <- glm(death_flag ~ time_to_drug_use_cat + age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + log_cv, 
                data = Cohort_steroids_3, family = "binomial")
                
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
    Output(rid="ri.foundry.main.dataset.d1f34cbd-83a2-465e-b8b7-3510c5b7cb59"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
logit_unad_duration <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)
    
    logit <- glm(death_flag ~ Drug_duration_cat, 
                data = Cohort_steroids_3, family = "binomial")
                
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
    Output(rid="ri.foundry.main.dataset.adf4c94c-d8a2-4a4a-aec0-242775caed7b"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
logit_unad_timing <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)
    
    logit <- glm(death_flag ~ time_to_drug_use_cat, 
                data = Cohort_steroids_3, family = "binomial")
                
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
    Output(rid="ri.foundry.main.dataset.2d4688d6-6616-4b26-bb79-f9f83ed4f8a9"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
survival_time_duration <- function(Cohort_steroids_3) {
    library(dplyr)
    df <- Cohort_steroids_3 %>%
            group_by(Drug_duration_cat) %>%
            summarize(overall_survival = sum(futime),
                      overall_survival_2 = sum(futime_2))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e2dd32da-5186-4905-a5d0-7b90744df296"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
survival_time_timing <- function(Cohort_steroids_3) {
    library(dplyr)
    df <- Cohort_steroids_3 %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(overall_survival = sum(futime),
                      overall_survival_2 = sum(futime_2))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.378148ed-e35f-4c87-a30f-a6732752559e"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
thought_adj_1_duration <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(futime, censor_flag) ~ Drug_duration_cat + age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + log_cv,
        data= Cohort_steroids_3, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(-z, -Lower_CI, -Upper_CI)

    return(HR)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.93007124-76eb-43a9-b9f5-e9aed21a580e"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
thought_adj_1_timing <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(futime, censor_flag) ~ time_to_drug_use_cat + age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + log_cv,
        data= Cohort_steroids_3, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(-z, -Lower_CI, -Upper_CI)

    return(HR)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f01125b3-72b3-4d7d-aead-c177a8b0b591"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
thought_adj_2_duration <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(futime_2, censor_flag_2) ~ Drug_duration_cat + age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + log_cv,
        data= Cohort_steroids_3, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(-z, -Lower_CI, -Upper_CI)

    return(HR)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7ac908d8-1557-4fa8-ab99-84754c100029"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
thought_adj_2_timing <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(futime_2, censor_flag_2) ~ time_to_drug_use_cat + age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + log_cv,
        data= Cohort_steroids_3, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(-z, -Lower_CI, -Upper_CI)

    return(HR)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.dcee9e30-ee65-4434-bb74-b4b6fd5fb0c8"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
thought_unadj_1_duration <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(futime, censor_flag) ~ Drug_duration_cat,
        data= Cohort_steroids_3, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep=""))

    return(HR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1bbc94de-5938-438e-97fa-d9670e4734a1"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
thought_unadj_1_timing <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(futime, censor_flag) ~ time_to_drug_use_cat,
        data= Cohort_steroids_3, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep=""))

    return(HR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.319eb25a-b09e-4ef0-9ff4-a6cf6ae01561"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
thought_unadj_2_duration <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(futime_2, censor_flag_2) ~ Drug_duration_cat,
        data= Cohort_steroids_3, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep=""))

    return(HR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f6159a91-36f7-4303-9f1f-127dac06d700"),
    Cohort_steroids_3=Input(rid="ri.foundry.main.dataset.8c6f59d8-fc43-44dd-97d9-538fc6a7e1ca")
)
thought_unadj_2_timing <- function(Cohort_steroids_3) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(futime_2, censor_flag_2) ~ time_to_drug_use_cat,
        data= Cohort_steroids_3, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep=""))

    return(HR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3f07233f-9c61-49b5-a8b8-a2319b463fe0"),
    count_data_timing=Input(rid="ri.foundry.main.dataset.a882f7f4-767f-4ec5-a0d2-e009740f62a1")
)
timing_adjusted <- function(count_data_timing) {
    library(survival)
    library(dplyr)
    library(tibble)

    cox <- coxph(Surv(tstart, tstop, death_flag_2) ~ Drug_use + age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + log_cv,
        data= count_data_timing, ties="breslow")
    
    summary <- data.frame(summary(cox)$coefficients) %>%
            rownames_to_column(var = "variables")

    HR <- summary %>%
        mutate(Lower_CI = round(exp(coef + qnorm(0.025)*se.coef.),2),
               Upper_CI = round(exp(coef + qnorm(0.975)*se.coef.),2)) %>%
        mutate(exp.coef. = round(exp.coef.,2),
               se.coef. = round(se.coef.,2),
               HR_CI = paste(exp.coef., " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(-z, -Lower_CI, -Upper_CI)

    return(HR)
}

