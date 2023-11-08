

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3b43e16f-c8db-4f21-88d1-36883d7f906a"),
    Table_2_all=Input(rid="ri.foundry.main.dataset.49dd3449-6cac-47a2-86e2-e5df1b30bc3c"),
    Table_2_dex=Input(rid="ri.foundry.main.dataset.8f1ff040-8ba5-48c9-8677-b6a97e649fb6"),
    Table_2_hcq=Input(rid="ri.foundry.main.dataset.ad8a07bb-536c-4669-90fa-3eab58bcb63d"),
    Table_2_rem=Input(rid="ri.foundry.main.dataset.7fb47df0-17de-40a4-a028-c74dc8fe578c")
)
Table_2 <- function(Table_2_all, Table_2_rem, Table_2_hcq, Table_2_dex) {

    library(dplyr)
    df1 <- Table_2_all %>%
        mutate(All_Patients = paste(person_count, " (", percent,")", sep="")) %>%
        select(covarite_cat, All_Patients)

    df2 <- Table_2_all %>%
        select(covarite_cat, person_count) %>%
        left_join(Table_2_hcq %>% select(covarite_cat, person_count) %>% rename(HCQ_count = person_count), by="covarite_cat") %>%
        mutate(hcq_percent = round(HCQ_count/person_count*100,2)) %>%
        mutate(hcq_Patients = paste(HCQ_count, " (", hcq_percent,"%)", sep="")) %>%
        select(covarite_cat, hcq_Patients)

    df3 <- Table_2_all %>%
        select(covarite_cat, person_count) %>%
        left_join(Table_2_rem %>% select(covarite_cat, person_count) %>% rename(REM_count = person_count), by="covarite_cat") %>%
        mutate(rem_percent = round(REM_count/person_count*100,2)) %>%
        mutate(rem_Patients = paste(REM_count, " (", rem_percent,"%)", sep="")) %>%
        select(covarite_cat, rem_Patients)

    df4 <- Table_2_all %>%
        select(covarite_cat, person_count) %>%
        left_join(Table_2_dex %>% select(covarite_cat, person_count) %>% rename(DEX_count = person_count), by="covarite_cat") %>%
        mutate(dex_percent = round(DEX_count/person_count*100,2)) %>%
        mutate(dex_Patients = paste(DEX_count, " (", dex_percent,"%)", sep="")) %>%
        select(covarite_cat, dex_Patients)
    
    df <- df1 %>%
        full_join(df2, by="covarite_cat") %>%
        full_join(df3, by="covarite_cat") %>%
        full_join(df4, by="covarite_cat")

    return(df)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.49dd3449-6cac-47a2-86e2-e5df1b30bc3c"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
Table_2_all <- function(covariate) {
    
    library(dplyr)
    BMI <- covariate %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    age_group <- covariate %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    sex <- covariate %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)
            
    race_v1 <- covariate %>%
            select(person_id, race_v1) %>%
            group_by(race_v1) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("race: ",race_v1,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ethnicity <- covariate %>%
            select(person_id, ethnicity) %>%
            group_by(ethnicity) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("ethnicity: ",ethnicity,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- covariate %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    admission_month_range <- covariate %>%
            select(person_id, admission_month_range) %>%
            group_by(admission_month_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("admission_month_range: ",admission_month_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- covariate %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    AKI_in_hospital <- covariate %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- covariate %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Invasive_Ventilation <- covariate %>%
            select(person_id, Invasive_Ventilation) %>%
            group_by(Invasive_Ventilation) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Invasive Ventilation: ",Invasive_Ventilation,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Severity_v1 <- covariate %>%
            select(person_id, Severity_v1) %>%
            group_by(Severity_v1) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Severity Type: ",Severity_v1,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Q_Score <- covariate %>%
            select(person_id, Q_Score_cat) %>%
            group_by(Q_Score_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Q Score: ",Q_Score_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- covariate %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(BMI, age_group, sex, race_v1, ethnicity, race_eth, admission_month_range, Charlson_range, AKI_in_hospital, ECMO, Invasive_Ventilation, Severity_v1, Q_Score, cv_quar)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8f1ff040-8ba5-48c9-8677-b6a97e649fb6"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
Table_2_dex <- function(covariate) {
    
    library(dplyr)

    df <- covariate %>%
        filter(drug_dex_v1==1)

    BMI <- df %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent) 

    age_group <- df %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    sex <- df %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)
            
    race_v1 <- df %>%
            select(person_id, race_v1) %>%
            group_by(race_v1) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("race: ",race_v1,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ethnicity <- df %>%
            select(person_id, ethnicity) %>%
            group_by(ethnicity) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("ethnicity: ",ethnicity,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- df %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    admission_month_range <- df %>%
            select(person_id, admission_month_range) %>%
            group_by(admission_month_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("admission_month_range: ",admission_month_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- df %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)
            
    AKI_in_hospital <- df %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- df %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Invasive_Ventilation <- df %>%
            select(person_id, Invasive_Ventilation) %>%
            group_by(Invasive_Ventilation) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Invasive Ventilation: ",Invasive_Ventilation,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Severity_v1 <- df %>%
            select(person_id, Severity_v1) %>%
            group_by(Severity_v1) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Severity Type: ",Severity_v1,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Q_Score <- df %>%
            select(person_id, Q_Score_cat) %>%
            group_by(Q_Score_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Q Score: ",Q_Score_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- df %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(BMI, age_group, sex, race_v1, ethnicity, race_eth, admission_month_range, Charlson_range, AKI_in_hospital, ECMO, Invasive_Ventilation, Severity_v1, Q_Score, cv_quar)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ad8a07bb-536c-4669-90fa-3eab58bcb63d"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
Table_2_hcq <- function(covariate) {
    
    library(dplyr)

    df <- covariate %>%
        filter(drug_hcq_v1==1)

    BMI <- df %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)  

    age_group <- df %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    sex <- df %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)
            
    race_v1 <- df %>%
            select(person_id, race_v1) %>%
            group_by(race_v1) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("race: ",race_v1,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ethnicity <- df %>%
            select(person_id, ethnicity) %>%
            group_by(ethnicity) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("ethnicity: ",ethnicity,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- df %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    admission_month_range <- df %>%
            select(person_id, admission_month_range) %>%
            group_by(admission_month_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("admission_month_range: ",admission_month_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- df %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)
            
    AKI_in_hospital <- df %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- df %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Invasive_Ventilation <- df %>%
            select(person_id, Invasive_Ventilation) %>%
            group_by(Invasive_Ventilation) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Invasive Ventilation: ",Invasive_Ventilation,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Severity_v1 <- df %>%
            select(person_id, Severity_v1) %>%
            group_by(Severity_v1) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Severity Type: ",Severity_v1,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Q_Score <- df %>%
            select(person_id, Q_Score_cat) %>%
            group_by(Q_Score_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Q Score: ",Q_Score_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- df %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(BMI, age_group, sex, race_v1, ethnicity, race_eth, admission_month_range, Charlson_range, AKI_in_hospital, ECMO, Invasive_Ventilation, Severity_v1, Q_Score, cv_quar)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7fb47df0-17de-40a4-a028-c74dc8fe578c"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
Table_2_rem <- function(covariate) {
    
    library(dplyr)

    df <- covariate %>%
        filter(drug_rem_v1==1)

    BMI <- df %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    age_group <- df %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    sex <- df %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)
            
    race_v1 <- df %>%
            select(person_id, race_v1) %>%
            group_by(race_v1) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("race: ",race_v1,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ethnicity <- df %>%
            select(person_id, ethnicity) %>%
            group_by(ethnicity) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("ethnicity: ",ethnicity,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- df %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    admission_month_range <- df %>%
            select(person_id, admission_month_range) %>%
            group_by(admission_month_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("admission_month_range: ",admission_month_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- df %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)
            
    AKI_in_hospital <- df %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- df %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Invasive_Ventilation <- df %>%
            select(person_id, Invasive_Ventilation) %>%
            group_by(Invasive_Ventilation) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Invasive Ventilation: ",Invasive_Ventilation,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Severity_v1 <- df %>%
            select(person_id, Severity_v1) %>%
            group_by(Severity_v1) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Severity Type: ",Severity_v1,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Q_Score <- df %>%
            select(person_id, Q_Score_cat) %>%
            group_by(Q_Score_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Q Score: ",Q_Score_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- df %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,2),"%",sep="")) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(BMI, age_group, sex, race_v1, ethnicity, race_eth, admission_month_range, Charlson_range, AKI_in_hospital, ECMO, Invasive_Ventilation, Severity_v1, Q_Score, cv_quar)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5c28b45a-eed8-4ed4-9eac-6dd73d13e32c"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.050f501f-2aff-46e1-acf9-102cdb777bfe")
)
center_volume <- function(Cohort_v05) {
    center_vol <- Cohort_v05 %>%
                    group_by(data_partner_id) %>%
                    summarize(center_volume = n()) %>%
                    mutate(cv_quar = ntile(center_volume, 4))
    return(center_vol)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4"),
    Ptwithscores_drop_before_table2_Shared=Input(rid="ri.foundry.main.dataset.68f6ecc7-30c0-4623-bbad-a98bf8eae595")
)
covariate <- function(Ptwithscores_drop_before_table2_Shared) {  

    library(dplyr)
    library(lubridate)

    cov <- Ptwithscores_drop_before_table2_Shared %>%
            select(person_id, AKI_in_hospital, ECMO, Invasive_Ventilation, Severity_Type, Q_Score, BMI, positive_covid_test, visit_concept_name, in_death_table)
    
    center_vol <- Cohort_v05 %>%
                    group_by(data_partner_id) %>%
                    summarize(center_volume = n()) %>%
                    mutate(cv_quar = ntile(center_volume, 4))

    df <- Cohort_v05 %>%
        left_join(center_vol, by="data_partner_id") %>%
        left_join(cov, by = "person_id") %>%
        mutate( HCQ = case_when(
                    drug_hcq_v1 == 1 ~ 1,
                    is.na(drug_hcq_v1) ~ 0),
                REM = case_when(
                    drug_rem_v1 == 1 ~ 1,
                    is.na(drug_rem_v1) ~ 0),
                DEX = case_when(
                    drug_dex_v1 == 1 ~ 1,
                    is.na(drug_dex_v1) ~ 0),

                BMI_cat = case_when(
                      BMI < 18.1 ~ "2_<=18.0",
                      BMI >= 18.1 & BMI <= 25.0 ~ "1_18.1-25",
                      BMI > 25.0 & BMI <=  30.0 ~ "3_25.1-30",
                      BMI > 30.0 & BMI <=  40.0 ~ "4_30.1-40",
                      BMI > 40.0 & BMI <=  45.0 ~ "5_40.1-45",
                      BMI > 45.0  ~ "6_45.1_up",
                      is.na(BMI)|BMI=="NA" ~ "7_Missing"
                      ),
                age_group = case_when(
                            age %in% (18:34) ~ "18_to_34",
                            age %in% (35:49) ~ "35_to_49",
                            age %in% (50:64) ~ "50_to_64",
                            age %in% (65:74) ~ "65_to_74",
                            age >=75 ~ "75_plus"
                            ),
                sex = ifelse(sex=="UNKNOWN", "NO MATCHING CONCEPT", sex),
                race_v1 = case_when(
                            race %in% c('WHITE') ~ '1_WHITE',
                            race %in% c('BLACK OR AFRICAN AMERICAN') ~ '2_BLACK',
                            race %in% c('ASIAN', 'ASIAN INDIAN') ~ '3_ASIAN',
                            race %in% c('NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER', 'AMERICAN INDIAN OR ALASKA NATIVE', 'OTHER PACIFIC ISLANDER', 'OTHER') ~ '4_OTHER',
                            race %in% c('NO MATCHING CONCEPT', 'UNKNOWN', 'NA')|is.na(race) ~ '5_MISSING'
                            ),

                ethnicity_v1 = case_when(
                            ethnicity %in% c('NOT HISPANIC OR LATINO') ~ '1_NOT HISPANIC OR LATINO',
                            ethnicity %in% c('HISPANIC OR LATINO') ~ '2_HISPANIC OR LATINO',
                            ethnicity %in% c('NO MATCHING CONCEPT') ~ '3_Missing'
                            ),
                race_eth = case_when(
                            (race %in% c('WHITE') & ethnicity %in% c('NOT HISPANIC OR LATINO', 'NO MATCHING CONCEPT')) ~ '1_NH_WHITE',
                            (race %in% c('BLACK OR AFRICAN AMERICAN') & ethnicity %in% c('NOT HISPANIC OR LATINO', 'NO MATCHING CONCEPT')) ~ '2_NH_BLACK',
                            (race %in% c('ASIAN', 'ASIAN INDIAN') & ethnicity %in% c('NOT HISPANIC OR LATINO', 'NO MATCHING CONCEPT')) ~ '3_ASIAN',
                            (race %in% c('NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER', 'AMERICAN INDIAN OR ALASKA NATIVE', 'OTHER PACIFIC ISLANDER', 'OTHER') & ethnicity %in% c('NOT HISPANIC OR LATINO', 'NO MATCHING CONCEPT')) ~ '4_OTHER',
                            ethnicity %in% c('HISPANIC OR LATINO') ~ '5_HISPANIC',
                            TRUE ~ '6_Missing'
                ),
                admission_month = month(covid_admission),
                admission_month_range = case_when(
                    admission_month %in% (2:4) ~ "February-April",
                    admission_month %in% (5:7) ~ "May-July",
                    admission_month %in% (8:10) ~ "August-October"),
                
                Charlson_range = case_when(
                    CCI_INDEX == 0 ~ "0",
                    CCI_INDEX == 1 ~ "1",
                    CCI_INDEX == 2 ~ "2",
                    CCI_INDEX == 3 ~ "3",
                    CCI_INDEX >= 4 ~ "4_plus"),

                AKI_in_hospital = ifelse(is.na(AKI_in_hospital),0,1), 
                ECMO = ifelse(is.na(ECMO),0,1), 
                Invasive_Ventilation = ifelse(is.na(Invasive_Ventilation),0,1), 
                
                Severity_Type = case_when(
                    Severity_Type == "Mild" ~ "1_Mild",
                    Severity_Type == "Mild_ED" ~ "2_Mild_ED",
                    Severity_Type == "Moderate" ~ "3_Moderate",
                    Severity_Type == "Severe" ~ "4_Severe",
                    Severity_Type == "Dead_w_COVID" ~ "5_Dead_w_COVID",
                    Severity_Type == "Unaffected" ~ "6_Unaffected",
                    is.na(Severity_Type) ~ "7_Missing"),

                Q_Score_cat = case_when(
                      Q_Score < 10 ~ "1_<10",
                      Q_Score >=10 ~ "2_>=10",
                      is.na(Q_Score)|Q_Score=="NA" ~ "3_Missing"
                      ),
                Severity_v1 = case_when(
                    Severity_Type == "5_Dead_w_COVID" ~ "3_Dead_w_COVID",
                    Severity_Type != "5_Dead_w_COVID" & (Invasive_Ventilation ==1|ECMO==1) ~ "2_Severe",
                    TRUE ~ "1_Moderate")

                
               )
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.34c3c4e2-6ad6-4c40-949b-888a656862e1"),
    covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259"),
    min_SPO2_pts=Input(rid="ri.foundry.main.dataset.98edb7e0-8eac-4744-84a6-67d5a11c0a9d")
)
covariate_w_SPO2 <- function(covariate_with_steroid, min_SPO2_pts) {
    library(dplyr)
    df <- min_SPO2_pts %>%
            mutate(SPO2_flag = ifelse(SPO2<=94, 1, 0))
    df1 <- covariate_with_steroid %>%
            left_join(df, by="person_id")
    return(df1)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4"),
    steroids_pts_2=Input(rid="ri.foundry.main.dataset.238589fb-b032-4726-a8c3-441b6e80ec39")
)
covariate_with_steroid <- function(covariate, steroids_pts_2) {
    library(dplyr)
    df <- covariate %>% 
        left_join(steroids_pts_2, by="person_id") %>%
        mutate(Steroid_flag = case_when(
                    steroid == 1 ~ 1,
                    is.na(steroid) ~ 0))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7c49440e-8919-4688-9177-e7323ae353a0"),
    SPO2_new=Input(rid="ri.foundry.main.dataset.2798606e-cf97-4482-94a8-02b1bcdb9348"),
    covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
covriate_steroid_spo2 <- function(SPO2_new, covariate_with_steroid) {
    library(dplyr)
    df <- covariate_with_steroid %>%
        left_join(SPO2_new, by="person_id") %>%
        mutate(SPO2_less_than_93 = ifelse(SPO2<93, 1, 0))
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.a83f531b-3bd5-40b3-81ac-9b6fc4c93177"),
    covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
unnamed_1 <- function(covariate_with_steroid) {
    df <- covariate_with_steroid %>%
        filter(Invasive_Ventilation==1)
    return(data.frame(
        table(
            df$DEX, df$Steroid_flag
        )
    ))
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.36f1ec91-03a2-4dcb-96da-6c86ce66fd73"),
    covariate_w_SPO2=Input(rid="ri.foundry.main.dataset.34c3c4e2-6ad6-4c40-949b-888a656862e1")
)
unnamed_2 <- function(covariate_w_SPO2) {
    return(
        data.frame(
            table(covariate_w_SPO2$DEX, covariate_w_SPO2$SPO2_flag)
        )
    )
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.c2af81d2-99a0-426b-b8fa-49546c04540a"),
    covariate_w_SPO2=Input(rid="ri.foundry.main.dataset.34c3c4e2-6ad6-4c40-949b-888a656862e1")
)
unnamed_3 <- function(covariate_w_SPO2) {
    df <- covariate_w_SPO2 %>%
            filter(Invasive_Ventilation==0) %>%
            filter(SPO2_flag==1)
    return(
        data.frame(
            table(
                df$DEX
            )
        )
    )
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.3ef3c3c6-aaa6-4cbd-92fb-a11ace5e5473"),
    covriate_steroid_spo2=Input(rid="ri.foundry.main.dataset.7c49440e-8919-4688-9177-e7323ae353a0")
)
unnamed_4 <- function(covriate_steroid_spo2) {
    return(data.frame(
        table(
            covriate_steroid_spo2$Invasive_Ventilation, covriate_steroid_spo2$SPO2_less_than_93
        )
    ))

    
}

