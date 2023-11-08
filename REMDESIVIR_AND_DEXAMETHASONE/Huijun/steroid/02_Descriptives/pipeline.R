

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.13938215-1231-46f0-b2b1-11d66016e00d"),
    Cohort_v07=Input(rid="ri.foundry.main.dataset.f38495ba-c045-4223-ad32-435359cfed04")
)
cohort_steroids <- function(Cohort_v07) {
    library(dplyr)

    center_vol <- Cohort_v07 %>%
                    group_by(data_partner_id) %>%
                    summarize(center_volume = n()) %>%
                    mutate(cv_quar = ntile(center_volume, 4))

    df <- Cohort_v07 %>%
            left_join(center_vol, by="data_partner_id") %>%
            mutate(drug_start_date = pmin(dex_start_date, pred_start_date, mpred_start_date, na.rm=T)) %>%
            mutate(
                init_drug = case_when(
                    drug_start_date==dex_start_date ~ "DEX",
                    drug_start_date==pred_start_date ~ "PRED",
                    drug_start_date==mpred_start_date ~ "MPRED"
                ),
                drug_end_date = case_when(
                    drug_start_date==dex_start_date ~ dex_end_date,
                    drug_start_date==pred_start_date ~ pred_end_date,
                    drug_start_date==mpred_start_date ~ mpred_end_date
                )
            ) %>%
            mutate(Drug_timing = as.numeric(drug_start_date-covid_admission)) %>%
            mutate(Drug_duration = ifelse(is.na(drug_end_date)|drug_end_date<drug_start_date, 1,
                                as.numeric(drug_end_date-drug_start_date)+1)) %>%
            mutate(
                time_to_drug_use_cat = case_when(
                    Drug_timing==0 ~ "0 day",
                    Drug_timing==1 ~ "1 day",
                    Drug_timing==2 ~ "2 day",
                    Drug_timing>=3 ~ "3+ day"
                ),
                Drug_duration_cat = case_when(
                    Drug_duration==1 ~ "1 day",
                    Drug_duration==2 ~ "2 day",
                    Drug_duration==3 ~ "3 day",
                    Drug_duration==4 ~ "4 day",
                    Drug_duration==5 ~ "5 day",
                    Drug_duration>=6 ~ "6+ day"
                )
            ) 
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0d21755a-12f0-43bc-b5da-fc7a9a8beef3"),
    cohort_steroids=Input(rid="ri.foundry.main.dataset.13938215-1231-46f0-b2b1-11d66016e00d")
)
cv_median_iqr <- function(cohort_steroids) {
    library(dplyr)
    df <- cohort_steroids %>%
        group_by(time_to_drug_use_cat) %>%
        summarize(median = median(center_volume), lower = quantile(center_volume, probs = 0.25), higher = quantile(center_volume, probs = 0.75),
                  mean_age = mean(age), SD_age = sd(age), 
                  mean_BMI = mean(BMI, na.rm=T), SD_BMI = sd(BMI, na.rm=T), 
                  mean_Charlson = mean(CCI_INDEX), SD_Charlson = sd(CCI_INDEX))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a1fab8e0-f7d8-4800-80b3-d6a1f5ec1e16"),
    table1_0day=Input(rid="ri.foundry.main.dataset.e4275f35-62c0-4a5d-994f-415126cecab7"),
    table1_1day=Input(rid="ri.foundry.main.dataset.941ddbba-2b92-41a2-b7a3-0988ac235e1f"),
    table1_2day=Input(rid="ri.foundry.main.dataset.db0d165b-d036-49ac-8eaf-5a83b0c90264"),
    table1_3day=Input(rid="ri.vector.main.execute.5d061178-a70a-4e1e-83af-06041541210c")
)
table1 <- function(table1_0day, table1_1day, table1_2day, table1_3day) {

    library(dplyr)
    df1 <- table1_0day %>%
        mutate(zero_day = paste(person_count, " (", percent,")", sep="")) %>%
        select(covarite_cat, zero_day)

    df2 <- table1_1day %>%
        mutate(one_day = paste(person_count, " (", percent,")", sep="")) %>%
        select(covarite_cat, one_day)
        
    df3 <- table1_2day %>%
        mutate(two_day = paste(person_count, " (", percent,")", sep="")) %>%
        select(covarite_cat, two_day)
        
    df4 <- table1_3day %>%
        mutate(three_day = paste(person_count, " (", percent,")", sep="")) %>%
        select(covarite_cat, three_day)
        
    df <- df1 %>%
        full_join(df2, by="covarite_cat") %>%
        full_join(df3, by="covarite_cat") %>%
        full_join(df4, by="covarite_cat")

    return(df)

    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e4275f35-62c0-4a5d-994f-415126cecab7"),
    cohort_steroids=Input(rid="ri.foundry.main.dataset.13938215-1231-46f0-b2b1-11d66016e00d")
)
table1_0day <- function(cohort_steroids) {
    
    library(dplyr)
    covariate <- cohort_steroids %>%
                filter(time_to_drug_use_cat=="0 day")

    age_group <- covariate %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    BMI <- covariate %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    sex <- covariate %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- covariate %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- covariate %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    AKI_in_hospital <- covariate %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- covariate %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- covariate %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(age_group, BMI, sex, race_eth, Charlson_range, AKI_in_hospital, ECMO, cv_quar)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.941ddbba-2b92-41a2-b7a3-0988ac235e1f"),
    cohort_steroids=Input(rid="ri.foundry.main.dataset.13938215-1231-46f0-b2b1-11d66016e00d")
)
table1_1day <- function(cohort_steroids) {
    
    library(dplyr)
    covariate <- cohort_steroids %>%
                filter(time_to_drug_use_cat=="1 day")

    age_group <- covariate %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    BMI <- covariate %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    sex <- covariate %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- covariate %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- covariate %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    AKI_in_hospital <- covariate %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- covariate %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- covariate %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(age_group, BMI, sex, race_eth, Charlson_range, AKI_in_hospital, ECMO, cv_quar)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.db0d165b-d036-49ac-8eaf-5a83b0c90264"),
    cohort_steroids=Input(rid="ri.foundry.main.dataset.13938215-1231-46f0-b2b1-11d66016e00d")
)
table1_2day <- function(cohort_steroids) {
    
    library(dplyr)
    covariate <- cohort_steroids %>%
                filter(time_to_drug_use_cat=="2 day")

    age_group <- covariate %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    BMI <- covariate %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    sex <- covariate %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- covariate %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- covariate %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    AKI_in_hospital <- covariate %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- covariate %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- covariate %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(age_group, BMI, sex, race_eth, Charlson_range, AKI_in_hospital, ECMO, cv_quar)
    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.5d061178-a70a-4e1e-83af-06041541210c"),
    cohort_steroids=Input(rid="ri.foundry.main.dataset.13938215-1231-46f0-b2b1-11d66016e00d")
)
table1_3day <- function(cohort_steroids) {
    
    library(dplyr)
    covariate <- cohort_steroids %>%
                filter(time_to_drug_use_cat=="3+ day")

    age_group <- covariate %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    BMI <- covariate %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    sex <- covariate %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- covariate %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- covariate %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    AKI_in_hospital <- covariate %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- covariate %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- covariate %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),sep="")) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(age_group, BMI, sex, race_eth, Charlson_range, AKI_in_hospital, ECMO, cv_quar)
    return(df)
    
    
    
}

