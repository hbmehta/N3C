

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.bd750826-98ae-48ee-8f11-ecfc726777a7"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
Covariate_model_dex_no_vent <- function(Covariate_model) {
    library(dplyr)
    df <- Covariate_model %>%
        filter(Invasive_Ventilation==0) %>%
        select(-covid_admission, -REM, -Invasive_Ventilation, -week_num) %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b92dac51-c738-4bef-ac95-7b8d5199b738"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
Covariate_model_dex_vent <- function(Covariate_model) {
    library(dplyr)
    df <- Covariate_model %>%
        filter(Invasive_Ventilation==1) %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a85bba74-6516-4b09-ab02-43186b4a375b"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
covariate_model_rem <- function(Covariate_model) {
    library(dplyr)
    
    df <- Covariate_model %>%
        select(-covid_admission, -DEX, -week_num)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4108adb4-b5cf-4238-b786-51e675d5e2c2"),
    Covariate_model_dex_no_vent=Input(rid="ri.foundry.main.dataset.bd750826-98ae-48ee-8f11-ecfc726777a7")
)
dex_no_vent_logit_all <- function(Covariate_model_dex_no_vent) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    library(survival)

    imp = mice(Covariate_model_dex_no_vent,  method = c(rep('norm',2),rep('polyreg',5), rep('norm',5)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi <- with(data=imp, exp = glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + as.factor(data_partner_id), family="binomial"))
    combFit <- pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 
    
    return(summary)   
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.390242fe-6675-4a11-a684-9998d2f71adf"),
    dex_no_vent_logit_all=Input(rid="ri.foundry.main.dataset.4108adb4-b5cf-4238-b786-51e675d5e2c2")
)
dex_no_vent_logit_all_2 <- function(dex_no_vent_logit_all) {
    
    library(dplyr)
    OR <- dex_no_vent_logit_all %>%
        mutate(OR = round(exp(estimate),2),
               Lower_CI = round(exp(estimate + qnorm(0.025)*std_error),2),
               Upper_CI = round(exp(estimate + qnorm(0.975)*std_error),2)
        ) %>%
        mutate(OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(term, estimate, std_error, OR_CI)
    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9ddf5b17-525d-4880-87e3-9f202a9d7fbe"),
    Covariate_model_dex_vent=Input(rid="ri.foundry.main.dataset.b92dac51-c738-4bef-ac95-7b8d5199b738")
)
dex_vent_logit_all <- function(Covariate_model_dex_vent) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    library(survival) 
        
    imp <- mice(Covariate_model_dex_vent,  method = c(rep('norm',5),rep('polyreg',5), rep('norm',6)), m=5, printFlag=FALSE, maxit = 40, seed=2525)
    fit.mi <- with(data=imp, exp = glm(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + as.factor(data_partner_id), family="binomial"))
    combFit <- pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 
            

    return(summary)  
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b9484b94-3b77-4e6b-b78f-a0c2a6066c0b"),
    dex_vent_logit_all=Input(rid="ri.foundry.main.dataset.9ddf5b17-525d-4880-87e3-9f202a9d7fbe")
)
dex_vent_logit_all_2 <- function(dex_vent_logit_all) {
    
    library(dplyr)
    OR <- dex_vent_logit_all %>%
        mutate(OR = round(exp(estimate),2),
               Lower_CI = round(exp(estimate + qnorm(0.025)*std_error),2),
               Upper_CI = round(exp(estimate + qnorm(0.975)*std_error),2)
        ) %>%
        mutate(OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(term, estimate, std_error, OR_CI)
    return(OR)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d0e7ca86-edec-40f0-8406-459113d99d45"),
    Covariate_model_dex_vent=Input(rid="ri.foundry.main.dataset.b92dac51-c738-4bef-ac95-7b8d5199b738")
)
dex_vent_logit_by_site <- function(Covariate_model_dex_vent) {
    library(mice)
    data_by_site <-split(Covariate_model_dex_vent, Covariate_model_dex_vent$data_partner_id)
    fit <- list()
    for (i in 1:length(data_by_site)){
        fit[[i]] <- glm(data=data_by_site[[i]], formula = DEX ~ age_group, family = "binomial")
    }
    summary(pool(fit))
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f436768c-8b58-4502-872f-96115300625a"),
    covariate_model_rem=Input(rid="ri.foundry.main.dataset.a85bba74-6516-4b09-ab02-43186b4a375b")
)
rem_logit_all <- function(covariate_model_rem) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    library(survival)

    imp = mice(covariate_model_rem, method = c(rep('norm',3),rep('polyreg',5), rep('norm',5)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi <- with(data=imp, exp = glm(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + Invasive_Ventilation + ECMO + week + log_cv + as.factor(data_partner_id), family="binomial"))
    combFit <- pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 
    
    return(summary)  
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fff450e7-2231-4869-8c82-3ab0f7187144"),
    rem_logit_all=Input(rid="ri.foundry.main.dataset.f436768c-8b58-4502-872f-96115300625a")
)
rem_logit_all_2 <- function(rem_logit_all) {
    
    library(dplyr)
    OR <- rem_logit_all %>%
        mutate(OR = round(exp(estimate),2),
               Lower_CI = round(exp(estimate + qnorm(0.025)*std_error),2),
               Upper_CI = round(exp(estimate + qnorm(0.975)*std_error),2)
        ) %>%
        mutate(OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(term, estimate, std_error, OR_CI)
    return(OR)
    
    
}

