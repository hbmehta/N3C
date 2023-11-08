

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3baee663-b8af-41d3-8b48-4801a2e33830"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    Invasive_respiratory_support=Input(rid="ri.foundry.main.dataset.4f63767e-fae0-4cf1-86cd-a1da5d4c87a4")
)
ECMO <- function(Cohort_v05, Invasive_respiratory_support) {
    library(dplyr)
    df <- Cohort_v05 %>%
            select(person_id, covid_admission, covid_discharge) %>%
            left_join(Invasive_respiratory_support %>% select(person_id, start_date, support_type), by="person_id") %>%
            filter(covid_admission-4 <= start_date & start_date <= covid_discharge+4) %>%
            filter(support_type == "ECMO") %>%
            group_by(person_id) %>%
            arrange(start_date) %>%
            slice(1L) %>%
            rename(ECMO_start_date=start_date, ECMO=support_type) %>%
            select(-covid_admission, -covid_discharge)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1b633996-582e-4a0a-bad9-df5825575d47"),
    Table_2_all_huijun=Input(rid="ri.foundry.main.dataset.dde0f3dc-8524-4f8d-9037-340e6da20801"),
    Table_2_dex=Input(rid="ri.foundry.main.dataset.13b93548-9035-43e5-b0a1-1e18b2dc73ae"),
    Table_2_hcq=Input(rid="ri.foundry.main.dataset.2ad2dac5-7e7a-459b-b336-33ec8a92feec"),
    Table_2_rem=Input(rid="ri.foundry.main.dataset.c864c328-1bf0-426d-9c13-09f6d492186f")
)
Table_1 <- function(Table_2_all_huijun, Table_2_rem, Table_2_hcq, Table_2_dex) {

    library(dplyr)
    df1 <- Table_2_all_huijun %>%
        select(covarite_cat, Patients)

    df2 <- Table_2_all_huijun %>%
        select(covarite_cat, person_count) %>%
        left_join(Table_2_hcq %>% select(covarite_cat, person_count) %>% rename(HCQ_count = person_count), by="covarite_cat") %>%
        mutate(hcq_percent = round(HCQ_count/person_count*100,1)) %>%
        select(covarite_cat, hcq_percent)

    df3 <- Table_2_all_huijun %>%
        select(covarite_cat, person_count) %>%
        left_join(Table_2_rem %>% select(covarite_cat, person_count) %>% rename(REM_count = person_count), by="covarite_cat") %>%
        mutate(rem_percent = round(REM_count/person_count*100,1)) %>%
        select(covarite_cat, rem_percent)

    df4 <- Table_2_all_huijun %>%
        select(covarite_cat, person_count) %>%
        left_join(Table_2_dex %>% select(covarite_cat, person_count) %>% rename(DEX_count = person_count), by="covarite_cat") %>%
        mutate(dex_percent = round(DEX_count/person_count*100,1)) %>%
        select(covarite_cat, dex_percent)
    
    df <- df1 %>%
        full_join(df2, by="covarite_cat") %>%
        full_join(df3, by="covarite_cat") %>%
        full_join(df4, by="covarite_cat")

    return(df)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.dde0f3dc-8524-4f8d-9037-340e6da20801"),
    covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
Table_2_all_huijun <- function(covariate_01) {

    library(dplyr)
    library(lubridate)
    center_vol <- covariate_01 %>%
                    group_by(data_partner_id) %>%
                    summarize(center_volume = n()) %>%
                    mutate(cv_quar = ntile(center_volume, 4))
    
    covariate <- covariate_01 %>%
        left_join(center_vol, by="data_partner_id")
    

    age_group <- covariate %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   
            
    BMI_group <- covariate %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent)  

    sex <- covariate %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- covariate %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- covariate %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    AKI_in_hospital <- covariate %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Invasive_Ventilation <- covariate %>%
            select(person_id, Invasive_Ventilation) %>%
            group_by(Invasive_Ventilation) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("Invasive Ventilation: ",Invasive_Ventilation,sep="")) %>%
            select(covarite_cat, person_count, percent)

    ECMO <- covariate %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- covariate %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    admission_month <- covariate %>%
            select(person_id, admission_month) %>%
            group_by(admission_month) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,1)) %>%
            mutate(covarite_cat = paste("admission_month: ",admission_month,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(age_group, sex, race_eth, Charlson_range, BMI_group, AKI_in_hospital, Invasive_Ventilation, ECMO) %>%
        mutate(Patients = paste(person_count, " (", percent,")", sep="")) %>%
        select(covarite_cat, person_count, Patients)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.13b93548-9035-43e5-b0a1-1e18b2dc73ae"),
    covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
Table_2_dex <- function(covariate_01) {

    library(dplyr)
    library(lubridate)

        df <- covariate_01 %>%
        filter(drug_dex_v1==1)

  age_group <- df %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   
  sex <- df %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)
  race_eth <- df %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)
  Charlson_range <- df %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)
  BMI <- df %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent) 
  AKI_in_hospital <- df %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)
  Invasive_Ventilation <- df %>%
            select(person_id, Invasive_Ventilation) %>%
            group_by(Invasive_Ventilation) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("Invasive Ventilation: ",Invasive_Ventilation,sep="")) %>%
            select(covarite_cat, person_count, percent)
  ECMO <- df %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

 df <- rbind(age_group, sex, race_eth, Charlson_range, BMI, AKI_in_hospital, Invasive_Ventilation, ECMO) %>%
    select(-percent)
    return(df) 
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2ad2dac5-7e7a-459b-b336-33ec8a92feec"),
    covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
Table_2_hcq <- function(covariate_01) {

    library(dplyr)
    library(lubridate)

        df <- covariate_01 %>%
        filter(drug_hcq_v1==1)

  age_group <- df %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   
  sex <- df %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)
  race_eth <- df %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)
  Charlson_range <- df %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)
  BMI <- df %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent) 
  AKI_in_hospital <- df %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)
  Invasive_Ventilation <- df %>%
            select(person_id, Invasive_Ventilation) %>%
            group_by(Invasive_Ventilation) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("Invasive Ventilation: ",Invasive_Ventilation,sep="")) %>%
            select(covarite_cat, person_count, percent)
  ECMO <- df %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

 df <- rbind(age_group, sex, race_eth, Charlson_range, BMI, AKI_in_hospital, Invasive_Ventilation, ECMO) %>%
        select(-percent)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c864c328-1bf0-426d-9c13-09f6d492186f"),
    covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
Table_2_rem <- function(covariate_01) {

    library(dplyr)
    library(lubridate)

        df <- covariate_01 %>%
        filter(drug_rem_v1==1)

  age_group <- df %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   
  sex <- df %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)
  race_eth <- df %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)
  Charlson_range <- df %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)
  BMI <- df %>%
            select(person_id, BMI_cat) %>%
            group_by(BMI_cat) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("BMI: ",BMI_cat,sep="")) %>%
            select(covarite_cat, person_count, percent) 
  AKI_in_hospital <- df %>%
            select(person_id, AKI_in_hospital) %>%
            group_by(AKI_in_hospital) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("AKI in hospital: ",AKI_in_hospital,sep="")) %>%
            select(covarite_cat, person_count, percent)
  Invasive_Ventilation <- df %>%
            select(person_id, Invasive_Ventilation) %>%
            group_by(Invasive_Ventilation) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("Invasive Ventilation: ",Invasive_Ventilation,sep="")) %>%
            select(covarite_cat, person_count, percent)
  ECMO <- df %>%
            select(person_id, ECMO) %>%
            group_by(ECMO) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = paste(round(person_count/sum(person_count)*100,1),sep="")) %>%
            mutate(covarite_cat = paste("ECMO: ",ECMO,sep="")) %>%
            select(covarite_cat, person_count, percent)

 df <- rbind(age_group, sex, race_eth, Charlson_range, BMI, AKI_in_hospital, Invasive_Ventilation, ECMO) %>%
        select(-percent)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fb60f920-b2e8-41ce-8752-2f5ce45f1245"),
    covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
bmi_mi_1 <- function(covariate_01) {
    library(mice)
    df <- covariate_01 %>%
        select(person_id, bmi)
    imp = mice(df, m=5, printFlag=FALSE, maxit = 40, seed=2525)
    return(imp)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5"),
    AKI_by_visit=Input(rid="ri.foundry.main.dataset.899c1529-d3ea-44e0-a035-510e1144e947"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    ECMO=Input(rid="ri.foundry.main.dataset.3baee663-b8af-41d3-8b48-4801a2e33830"),
    Final_bmi=Input(rid="ri.foundry.main.dataset.3b650a0b-eca5-447f-8711-2acef0efee81"),
    invasive_vent=Input(rid="ri.foundry.main.dataset.55e0fadc-6dc8-4c46-9f45-8d0cb0bdb7e6")
)
covariate_01 <- function(Cohort_v05, Final_bmi, AKI_by_visit, invasive_vent, ECMO) {
    library(dplyr)
    df <- Cohort_v05 %>%
        left_join(Final_bmi %>% select(person_id, bmi), by="person_id") %>%
        left_join(AKI_by_visit %>% select(person_id, AKI_in_hospital), by="person_id") %>%
        left_join(invasive_vent, by="person_id") %>%
        left_join(ECMO, by="person_id") %>%
        mutate(days_iv_diff = as.numeric(IV_start_date - covid_admission)) %>%
        mutate(LOS = as.numeric(covid_discharge - covid_admission)) %>%
        mutate(Invasive_Ventilation = ifelse(is.na(Invasive_Ventilation), 0, 1)) %>%
        mutate(ECMO = ifelse(is.na(ECMO), 0, 1)) %>%
        mutate(AKI_in_hospital = ifelse(is.na(AKI_in_hospital), 0, 1)) %>%
        mutate( HCQ = case_when(
                    drug_hcq_v1 == 1 ~ 1,
                    is.na(drug_hcq_v1) ~ 0),
                REM = case_when(
                    drug_rem_v1 == 1 ~ 1,
                    is.na(drug_rem_v1) ~ 0),
                DEX = case_when(
                    drug_dex_v1 == 1 ~ 1,
                    is.na(drug_dex_v1) ~ 0),
                age_group = case_when(
                            age %in% (18:34) ~ "18_to_34",
                            age %in% (35:49) ~ "35_to_49",
                            age %in% (50:64) ~ "50_to_64",
                            age %in% (65:74) ~ "65_to_74",
                            age >=75 ~ "75_plus"
                            ),
                admission_month = format(as.Date(covid_admission), "%Y-%m"),
                sex = ifelse(sex=="UNKNOWN", "NO MATCHING CONCEPT", sex),
                race_eth = case_when(
                            (race %in% c('WHITE') & ethnicity %in% c('NOT HISPANIC OR LATINO', 'NO MATCHING CONCEPT')) ~ '1_NH_WHITE',
                            (race %in% c('BLACK OR AFRICAN AMERICAN') & ethnicity %in% c('NOT HISPANIC OR LATINO', 'NO MATCHING CONCEPT')) ~ '2_NH_BLACK',
                            (race %in% c('ASIAN', 'ASIAN INDIAN') & ethnicity %in% c('NOT HISPANIC OR LATINO', 'NO MATCHING CONCEPT')) ~ '3_ASIAN',
                            (race %in% c('NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER', 'AMERICAN INDIAN OR ALASKA NATIVE', 'OTHER PACIFIC ISLANDER', 'OTHER') & ethnicity %in% c('NOT HISPANIC OR LATINO', 'NO MATCHING CONCEPT')) ~ '4_OTHER',
                            ethnicity %in% c('HISPANIC OR LATINO') ~ '5_HISPANIC',
                            TRUE ~ '6_Missing'
                ),
                BMI_cat = case_when(
                      bmi < 18.1 ~ "2_<=18.0",
                      bmi >= 18.1 & bmi <= 25.0 ~ "1_18.1-25",
                      bmi > 25.0 & bmi <=  30.0 ~ "3_25.1-30",
                      bmi > 30.0 & bmi <=  40.0 ~ "4_30.1-40",
                      bmi > 40.0 & bmi <=  45.0 ~ "5_40.1-45",
                      bmi > 45.0  ~ "6_45.1_up",
                      is.na(bmi)|bmi=="NA" ~ "7_Missing"
                      ),
                Charlson_range = case_when(
                    CCI_INDEX == 0 ~ "0",
                    CCI_INDEX == 1 ~ "1",
                    CCI_INDEX == 2 ~ "2",
                    CCI_INDEX == 3 ~ "3",
                    CCI_INDEX >= 4 ~ "4_plus")
               ) %>%
            mutate(
                Invasive_Ventilation = ifelse(person_id %in% c("8682250726509318408",
                                                                "3686642397307060337",
                                                                "3350150828323160245",
                                                                "8561603967265063125",
                                                                "3138278437600887613",
                                                                "5440978827178775341",
                                                                "3938382756262630205",
                                                                "2882774223937155901",
                                                                "4312574210961076381",
                                                                "3972483171473023805",
                                                                "6640238704279691477",
                                                                "4988653290856984789",
                                                                "7832272463940858554"), 1, Invasive_Ventilation))

    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.59f8145d-4ec8-4446-9cf8-ed91429bebea"),
    covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5"),
    steroids_pts_2=Input(rid="ri.foundry.main.dataset.9603b860-8d33-4fc1-a5f7-20794e7e0f06")
)
covariate_with_steroid <- function(steroids_pts_2, covariate_01) {
    library(dplyr)
    df <- covariate_01 %>% 
        left_join(steroids_pts_2, by="person_id") %>%
        mutate(Steroid_flag = case_when(
                    steroid == 1 ~ 1,
                    is.na(steroid) ~ 0))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.55e0fadc-6dc8-4c46-9f45-8d0cb0bdb7e6"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    Invasive_respiratory_support=Input(rid="ri.foundry.main.dataset.4f63767e-fae0-4cf1-86cd-a1da5d4c87a4")
)
invasive_vent <- function(Invasive_respiratory_support, Cohort_v05) {
    library(dplyr)
    df <- Cohort_v05 %>%
            select(person_id, covid_admission, covid_discharge) %>%
            left_join(Invasive_respiratory_support %>% select(person_id, start_date, support_type, visit_occurrence_id), by="person_id") %>%
            filter(covid_admission-4 <= start_date & start_date <= covid_discharge+4) %>%
            filter(support_type == "Invasive Ventilation") %>%
            group_by(person_id) %>%
            arrange(start_date) %>%
            slice(1L) %>%
            rename(IV_start_date=start_date, Invasive_Ventilation=support_type) %>%
            select(-covid_admission, -covid_discharge)
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.243a4db1-c0b4-4da0-a82d-b213a4ccf0a5"),
    covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
unnamed <- function(covariate_01) {
    library(dplyr)
    df <- covariate_01 
    return(
        data.frame(
            table(
                df$Invasive_Ventilation, df$ECMO
            )
        )
    )
}

