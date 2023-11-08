

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
covariate_Model <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    
    center_vol <- Covariate_01 %>%
                    group_by(data_partner_id) %>%
                    summarize(center_volume = n()) %>%
                    mutate(cv_quar = ntile(center_volume, 4),
                           log_cv = log(center_volume))
    
    df <- Covariate_01 %>%
        left_join(center_vol, by="data_partner_id") %>%
        select(person_id, covid_admission, DEX, REM, Invasive_Ventilation, age_group, sex, race_eth, Charlson_range, BMI_cat, AKI_in_hospital, ECMO, log_cv, data_partner_id) %>%
        mutate(week_num = cut.Date(covid_admission, breaks = "1 week", labels = FALSE)) %>%
        mutate(week = ymd( "2020-01-27" ) + weeks(week_num - 1 )) %>%
        mutate(race_eth = ifelse(race_eth=="6_Missing", NA, race_eth),
               BMI_cat = ifelse(BMI_cat=="7_Missing", NA, BMI_cat)) %>%
        mutate(BMI_cat=as.factor(BMI_cat),
               age_group=as.factor(age_group),
               sex = as.factor(sex),
               race_eth = as.factor(race_eth),
               Charlson_range = as.factor(Charlson_range),
               week = as.factor(week),
               AKI_in_hospital = as.factor(AKI_in_hospital),
               ECMO = as.factor(ECMO),
               Invasive_Ventilation = as.factor(Invasive_Ventilation))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ef6f1d8b-d292-4125-90c6-6c17efc2cfdd"),
    covariate_Model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
covariate_Model_no_vent <- function(covariate_Model) {
    library(dplyr)
    
    df <- covariate_Model %>%
        filter(Invasive_Ventilation==0) %>%
        select(-covid_admission, -REM, -Invasive_Ventilation, -week_num)
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.39654530-e334-463a-8588-05882d60f17a"),
    covariate_Model_no_vent=Input(rid="ri.foundry.main.dataset.ef6f1d8b-d292-4125-90c6-6c17efc2cfdd")
)
covariate_Model_no_vent_2 <- function(covariate_Model_no_vent) {
    library(dplyr)
    df <- covariate_Model_no_vent %>% sample_frac(.5)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.85b6273a-b5c4-4167-ae66-3e8eb7297737"),
    covariate_Model_no_vent=Input(rid="ri.foundry.main.dataset.ef6f1d8b-d292-4125-90c6-6c17efc2cfdd")
)
dex_no_vent <- function(covariate_Model_no_vent) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    
    df <- covariate_Model_no_vent %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 

    imp = mice(df,  method = c(rep('norm',2),rep('polyreg',5), rep('norm',5)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                   family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))
    combFit = pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 
   
    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)
    df2 <- summary %>% 
       add_row(term = "sd", estimate = a$var.comp[1], std.error=NA, statistic=NA, df=NA, p.value=NA)

    return(df2)        
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6f466824-e25e-4082-9da4-8b506eeb30c0"),
    covariate_Model_no_vent_2=Input(rid="ri.foundry.main.dataset.39654530-e334-463a-8588-05882d60f17a")
)
dex_no_vent_2 <- function(covariate_Model_no_vent_2) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    
    df <- covariate_Model_no_vent_2 %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 

    imp = mice(df,  method = c(rep('norm',2),rep('polyreg',5), rep('norm',5)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                   family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))
    combFit = pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 
   
    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)
    df2 <- summary %>% 
       add_row(term = "sd", estimate = a$var.comp[1], std.error=NA, statistic=NA, df=NA, p.value=NA)

    return(df2)        
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d8307a0c-9000-447f-acb1-2f7d6fe5bb3a"),
    dex_no_vent_2=Input(rid="ri.foundry.main.dataset.6f466824-e25e-4082-9da4-8b506eeb30c0")
)
dex_no_vent_OR_2 <- function(dex_no_vent_2) {
    library(dplyr)
    OR <- dex_no_vent_2 %>%
        mutate(OR = round(exp(estimate),2),
               Lower_CI = round(exp(estimate + qnorm(0.025)*std_error),2),
               Upper_CI = round(exp(estimate + qnorm(0.975)*std_error),2)
        ) %>%
        mutate(OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(term, estimate, std_error, OR_CI)
    return(OR)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.342c195f-3582-47f3-baa1-0ef7b853f3ee"),
    covariate_Model_no_vent=Input(rid="ri.foundry.main.dataset.ef6f1d8b-d292-4125-90c6-6c17efc2cfdd")
)
dex_no_vent_icc <- function( covariate_Model_no_vent) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)

    df <- covariate_Model_no_vent %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 

    imp = mice(df,  method = c(rep('norm',2),rep('polyreg',5), rep('norm',5)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                   family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))

    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)
    icc <- data.frame(sd = a$var.comp[1]) %>%
        mutate(icc = sd^2/(sd^2+(pi^2 / 3)))

    return(icc) 
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6baa127e-15f2-43a5-b950-acef8143093b"),
    covariate_Model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
dex_vent <- function(covariate_Model) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)

    df <- covariate_Model %>%
        filter(Invasive_Ventilation==1)
    imp = mice(df, m=10, printFlag=FALSE, maxit = 40, seed=2525)
    fit.mi = with(data=imp, exp = glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))
    combFit = pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 
            

    return(summary)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.322f543e-868a-4d65-8a6d-121d4e9b24ee"),
    covariate_Model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
dex_vent_2 <- function(covariate_Model) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)

    df <- covariate_Model %>%
        filter(Invasive_Ventilation==1) %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 
        
    imp <- mice(df,  method = c(rep('norm',5),rep('polyreg',5), rep('norm',6)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))
    combFit = pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 
            

    return(summary)     
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6e17452f-ccc0-4bf0-9a38-fb56ecc470bd"),
    dex_vent=Input(rid="ri.foundry.main.dataset.6baa127e-15f2-43a5-b950-acef8143093b")
)
dex_vent_OR <- function(dex_vent) {
    library(dplyr)
    OR <- dex_vent %>%
        mutate(OR = round(exp(estimate),2),
               Lower_CI = round(exp(estimate + qnorm(0.025)*std_error),2),
               Upper_CI = round(exp(estimate + qnorm(0.975)*std_error),2)
        ) %>%
        mutate(OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(term, estimate, std_error, OR_CI)
    return(OR)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.730250de-0ca9-4358-9523-8eb518558601"),
    dex_vent_2=Input(rid="ri.foundry.main.dataset.322f543e-868a-4d65-8a6d-121d4e9b24ee")
)
dex_vent_OR_2 <- function(dex_vent_2) {
    library(dplyr)
    OR <- dex_vent_2 %>%
        mutate(OR = round(exp(estimate),2),
               Lower_CI = round(exp(estimate + qnorm(0.025)*std_error),2),
               Upper_CI = round(exp(estimate + qnorm(0.975)*std_error),2)
        ) %>%
        mutate(OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(term, estimate, std_error, OR_CI)
    return(OR)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3165bc9a-4911-4eea-a3e3-1235722cbe56"),
    covariate_Model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
dex_vent_complete <- function(covariate_Model) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)

    df <- covariate_Model %>%
        filter(Invasive_Ventilation==1) %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 
        
    imp <- mice(df,  method = c(rep('norm',5),rep('polyreg',5), rep('norm',6)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    imp.data <- complete(imp, "long")

    return(imp.data)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fc2be55b-7f8f-462b-99ea-90421975bf33"),
    dex_vent_complete=Input(rid="ri.foundry.main.dataset.3165bc9a-4911-4eea-a3e3-1235722cbe56")
)
dex_vent_complete_sd <- function(dex_vent_complete) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                    data = dex_vent_complete, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    return(data.frame(
        summary(mem)$varcor
    ))
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3edc413a-4895-46da-a564-cfdf0c271788"),
    covariate_Model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
dex_vent_icc <- function(covariate_Model) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)

    df <- covariate_Model %>%
        filter(Invasive_Ventilation==1)
    imp = mice(df, m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))

    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)
    b <- data.frame(a$var.comp) %>%
            rownames_to_column(var = "term")
    return(b)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.20333fbe-d2dc-4308-bafb-c1392e082727"),
    covariate_Model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
dex_vent_sd <- function(covariate_Model) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)

    df <- covariate_Model %>%
        filter(Invasive_Ventilation==1) %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 
        
    imp <- mice(df,  method = c(rep('norm',5),rep('polyreg',5), rep('norm',6)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))

            
    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)

    return(data.frame(a$var.comp))    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8bd22608-38b8-4818-bb19-c8948bb0be65"),
    sd_null=Input(rid="ri.foundry.main.dataset.3eb7b879-9790-4ef3-99f0-a0e842ee61ad")
)
icc_null <- function(sd_null) {
    library(dplyr)
    df <- sd_null %>%
        mutate(ICC_NULL = round(sd_NULL^2/(sd_NULL^2+(pi^2 / 3)),4))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3eb7b879-9790-4ef3-99f0-a0e842ee61ad"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
sd_null <- function(Covariate_01) {
    
    
    library(lme4)
    library(dplyr) 

    vent <- Covariate_01 %>%
            filter(Invasive_Ventilation==1)
    no_vent <- Covariate_01 %>%
            filter(Invasive_Ventilation==0) 

    hcq <- glmer(HCQ ~ (1|data_partner_id), 
                    data = Covariate_01, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    rem <- glmer(REM ~ (1|data_partner_id), 
                    data = Covariate_01, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex <- glmer(DEX ~ (1|data_partner_id), 
                    data = Covariate_01, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex_vent_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex_no_vent_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = no_vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    

    sd <- data.frame(
        Model = c("HCQ","REM","DEX","DEX_Vent", "DEX_No_Vent"),
        sd_NULL = c(
            attr(summary(hcq)$varcor$data_partner_id, "stddev"),
            attr(summary(rem)$varcor$data_partner_id, "stddev"),
            attr(summary(dex)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_vent_null)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_no_vent_null)$varcor$data_partner_id, "stddev")
        )
    )
    return(sd)
}

