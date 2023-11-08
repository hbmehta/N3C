

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2e081f0a-dcc0-431e-863f-bc6b7dc4ccc0"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
covariate_Model_2 <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    
    center_vol <- Covariate_01 %>%
                    group_by(data_partner_id) %>%
                    summarize(center_volume = n()) %>%
                    mutate(cv_quar = ntile(center_volume, 4),
                           log_cv = log(center_volume))
    
    df <- Covariate_01 %>%
        left_join(center_vol, by="data_partner_id") %>%
        select(person_id, covid_admission, DEX, HCQ, REM, Invasive_Ventilation, age_group, sex, race_eth, Charlson_range, BMI_cat, AKI_in_hospital, ECMO, log_cv, data_partner_id) %>%
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
    Output(rid="ri.foundry.main.dataset.42c4d84f-7525-4c9a-bfb2-e2a67b8b43ef"),
    covariate_Model_2=Input(rid="ri.foundry.main.dataset.2e081f0a-dcc0-431e-863f-bc6b7dc4ccc0")
)
covariate_Model_dex <- function(covariate_Model_2) {
    library(dplyr)
    
    df <- covariate_Model_2 %>%
        select(-covid_admission, -HCQ, -REM, -week_num)
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0ce21123-9d0d-47d1-abb8-93d6ff3e2591"),
    covariate_Model_dex=Input(rid="ri.foundry.main.dataset.42c4d84f-7525-4c9a-bfb2-e2a67b8b43ef")
)
covariate_Model_dex_2 <- function(covariate_Model_dex) {
    library(dplyr)
    df <- covariate_Model_dex %>% sample_frac(.5)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3932d9ee-eec9-4c35-9017-1ab8268b9ebc"),
    covariate_Model_dex_2=Input(rid="ri.foundry.main.dataset.0ce21123-9d0d-47d1-abb8-93d6ff3e2591")
)
dex <- function(covariate_Model_dex_2) {
    library(lme4)
    library(dplyr)
    library(tibble)
    library(mice)
    library(mitml)
    
    df <- covariate_Model_dex_2 %>%
        mutate(race_eth = factor(race_eth)) %>%
        mutate(BMI_cat = factor(BMI_cat)) 

    imp = mice(df, method = c(rep('norm',3),rep('polyreg',5), rep('norm',5)), m=5, printFlag=FALSE, maxit = 10, seed=2525)
    fit.mi = with(data=imp, exp = glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + Invasive_Ventilation + (1|data_partner_id), 
                    family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10))
    combFit = pool(fit.mi) 
    summary <- data.frame(summary(combFit)) 

    a <- testEstimates(model = as.mitml.result(fit.mi), var.comp = TRUE)
    df2 <- summary %>% 
        add_row(term = "sd", estimate = a$var.comp[1], std.error=NA, statistic=NA, df=NA, p.value=NA)

    return(df2)
}

