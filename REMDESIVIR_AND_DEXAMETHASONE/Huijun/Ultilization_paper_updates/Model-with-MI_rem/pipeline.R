

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.92122ef4-3a05-4128-87cf-654fc4ae94b2"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
Covariate_model_2 <- function(Covariate_model) {
    return(head(Covariate_model,800))
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9eac4894-a2a5-4b38-b5c1-28d658dd5a01"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
covariate_Model_rem <- function(Covariate_model) {
    library(dplyr)
    
    df <- Covariate_model %>%
        select(-covid_admission, -DEX, -week_num)
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.47eca24e-a4d7-41b3-bf09-c0fadcf95495"),
    covariate_Model_rem=Input(rid="ri.foundry.main.dataset.9eac4894-a2a5-4b38-b5c1-28d658dd5a01")
)
covariate_Model_rem_2 <- function(covariate_Model_rem) {
    library(dplyr)
    df <- covariate_Model_rem %>% sample_frac(.5)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.82c9e1d6-003a-4d4c-9ecc-87440073a808"),
    covariate_Model_rem=Input(rid="ri.foundry.main.dataset.9eac4894-a2a5-4b38-b5c1-28d658dd5a01")
)
rem <- function( covariate_Model_rem) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)

    imp = mice(covariate_Model_rem, m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + Invasive_Ventilation + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))
    combFit = pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 

    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)
    df <- summary %>% 
        add_row(term = "ICC", estimate = a$var.comp[3], std.error=NA, statistic=NA, df=NA, p.value=NA)

    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4b4f351d-daa7-47ee-bbdd-851b07166938"),
    covariate_Model_rem_2=Input(rid="ri.foundry.main.dataset.47eca24e-a4d7-41b3-bf09-c0fadcf95495")
)
rem_2 <- function(covariate_Model_rem_2) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    
    df <- covariate_Model_rem_2 %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 

    imp = mice(df, method = c(rep('norm',3),rep('polyreg',5), rep('norm',5)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + Invasive_Ventilation + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))
    combFit = pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 

    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)
    df2 <- summary %>% 
        add_row(term = "sd", estimate = a$var.comp[1], std.error=NA, statistic=NA, df=NA, p.value=NA)

    return(df2)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.31dfc8e9-9e6e-4bd1-9309-d009a79eddca"),
    covariate_Model_rem=Input(rid="ri.foundry.main.dataset.9eac4894-a2a5-4b38-b5c1-28d658dd5a01")
)
rem_full <- function(covariate_Model_rem) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    
    df <- covariate_Model_rem %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 

    imp = mice(df, method = c(rep('norm',3),rep('polyreg',5), rep('norm',5)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + Invasive_Ventilation + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))
    combFit = pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 

    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)
    df2 <- summary %>% 
        add_row(term = "sd", estimate = a$var.comp[1], std.error=NA, statistic=NA, df=NA, p.value=NA)

    return(df2)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ed3e233d-a503-4aa6-b469-3846c5fe05e2"),
    covariate_Model_rem=Input(rid="ri.foundry.main.dataset.9eac4894-a2a5-4b38-b5c1-28d658dd5a01")
)
rem_icc <- function( covariate_Model_rem) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)

    imp = mice(covariate_Model_rem, m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + Invasive_Ventilation + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))

    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)

    return(data.frame(a$var.comp) %>%
            rownames_to_column(var = "term"))
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.833f0c04-ff96-4e05-a8fc-7d2e76ab5c1b"),
    Covariate_model_2=Input(rid="ri.foundry.main.dataset.92122ef4-3a05-4128-87cf-654fc4ae94b2")
)
rem_test <- function(Covariate_model_2) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    data(studentratings)

    imp = mice(studentratings, m=5, printFlag=FALSE, maxit = 40, seed=2525)
    fit.mi = with(data=imp, exp = lmer(ReadDis + SES ~ 1 + Sex + (1|ID)))
    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)

    return(data.frame(a$var.comp)%>%
            rownames_to_column(var = "term"))

}

