

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.04990d45-2fdd-48e3-94a5-650205b7f776"),
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
    Output(rid="ri.foundry.main.dataset.6e9e8d6c-94a9-4308-a06c-585ecbbed7d8"),
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
    Output(rid="ri.foundry.main.dataset.c8ceefca-eca7-43ce-98d3-0a89c271fd27"),
    Covariate_model_dex_vent=Input(rid="ri.foundry.main.dataset.6e9e8d6c-94a9-4308-a06c-585ecbbed7d8")
)
Covariate_model_dex_vent_head <- function(Covariate_model_dex_vent) {
    library(dplyr)
    df <- Covariate_model_dex_vent 
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c26ee56a-1ded-4e78-b06f-16f15771a0e5"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
covariate_model_rem <- function(Covariate_model) {
    library(dplyr)
    library(epi)
    df <- Covariate_model %>%
        select(-covid_admission, -DEX, -week_num)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0051fdde-305d-4406-a395-ed47934c63e6"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
cross_check <- function(Covariate_model) {
    library(dplyr)

    df <- Covariate_model %>%
        group_by(data_partner_id) %>%
        summarize(n = n_distinct(person_id))

    df1 <- Covariate_model %>%
        filter(ECMO==1) %>%
        group_by(data_partner_id) %>%
        summarize(ECMO_1 = n_distinct(person_id))

    df2 <- Covariate_model %>%
        filter(AKI_in_hospital==1) %>%
        group_by(data_partner_id) %>%
        summarize(aki_1 = n_distinct(person_id))
    df3 <- Covariate_model %>%
        filter(Invasive_Ventilation==1) %>%
        group_by(data_partner_id) %>%
        summarize(IV_1 = n_distinct(person_id))

    df_final <- df %>%
        left_join(df1, by="data_partner_id") %>%
        left_join(df2, by="data_partner_id") %>%
        left_join(df3, by="data_partner_id")
    return(df_final)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0c9c7ab1-7d3e-4ceb-9318-4891d3e5a86c"),
    Covariate_model_dex_vent_head=Input(rid="ri.foundry.main.dataset.c8ceefca-eca7-43ce-98d3-0a89c271fd27")
)
dex_vent_logit_all <- function(Covariate_model_dex_vent_head) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    library(survival) 
        
    imp <- mice(Covariate_model_dex_vent_head,  method = c(rep('norm',5),rep('polyreg',5), rep('norm',6)), m=5, 
                printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi <- with(data=imp, exp = clogit(DEX ~ ECMO + AKI_in_hospital, strata(data_partner_id)))
    combFit <- pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 
    return(summary)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5427a066-7542-4f6d-a04e-17195651c72c"),
    Covariate_model=Input(rid="ri.foundry.main.dataset.cfb23b29-bf8f-4881-bbcf-9a15cacccfec")
)
site_pt_n <- function(Covariate_model) {
    library(dplyr)
    df <- Covariate_model %>%
        select(person_id, data_partner_id) %>%
        group_by(data_partner_id) %>%
        summarize(n = n()) %>%
        arrange(n)
    return(df)

}

