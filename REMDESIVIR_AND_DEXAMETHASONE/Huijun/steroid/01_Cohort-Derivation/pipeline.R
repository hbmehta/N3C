

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.defc513a-88d3-4cca-970e-cbcce8f9ce0c"),
    cohort_v03=Input(rid="ri.foundry.main.dataset.b74ff09e-cd21-4620-a232-d2f22e71a3cf")
)
cohort_v04 <- function(cohort_v03) {
    library(dplyr)
    library(lubridate)
    
    df <- cohort_v03 %>%
        mutate(day_of_birth = ifelse(is.na(day_of_birth), "1", day_of_birth)) %>%
        mutate(dob = as.Date(paste(year_of_birth, month_of_birth, day_of_birth,sep="-"))) %>%
        mutate(age = floor(decimal_date(covid_admission) - decimal_date(dob))) %>%
        mutate(LOS = as.numeric(covid_discharge)-as.numeric(covid_admission)) %>%
        filter(age >= 18)

    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ddef51da-ce44-490e-9833-8c1c528a7d47"),
    Ptwithscores_drop_before_table2_Shared=Input(rid="ri.foundry.main.dataset.68f6ecc7-30c0-4623-bbad-a98bf8eae595"),
    cohort_v04=Input(rid="ri.foundry.main.dataset.defc513a-88d3-4cca-970e-cbcce8f9ce0c")
)
cohort_v05 <- function(cohort_v04, Ptwithscores_drop_before_table2_Shared) {

    library(dplyr)
    library(lubridate)

    cov <- Ptwithscores_drop_before_table2_Shared %>%
            select(person_id, AKI_in_hospital, ECMO, Invasive_Ventilation, Severity_Type, BMI, visit_concept_name)

    df <- cohort_v04 %>%
        left_join(cov, by = "person_id") %>%
        mutate( REM = case_when(
                    drug_rem_v1 == 1 ~ 1,
                    is.na(drug_rem_v1) ~ 0),
                DEX = case_when(
                    drug_dex_v1 == 1 ~ 1,
                    is.na(drug_dex_v1) ~ 0),
                PRED = case_when(
                    drug_pred_v1 == 1 ~ 1,
                    is.na(drug_pred_v1) ~ 0),
                MPRED = case_when(
                    drug_mpred_v1 == 1 ~ 1,
                    is.na(drug_mpred_v1) ~ 0),

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
                Severity_v1 = case_when(
                    Severity_Type == "5_Dead_w_COVID" ~ "3_Dead_w_COVID",
                    Severity_Type != "5_Dead_w_COVID" & (Invasive_Ventilation ==1|ECMO==1) ~ "2_Severe",
                    TRUE ~ "1_Moderate")
               )
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.668ae858-f82d-4a75-aa75-93e771f0f89a"),
    cohort_v05=Input(rid="ri.foundry.main.dataset.ddef51da-ce44-490e-9833-8c1c528a7d47")
)
cohort_v06 <- function(cohort_v05) {
    library(dplyr)
    df <- cohort_v05 %>%
            filter(Invasive_Ventilation == 0)
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f38495ba-c045-4223-ad32-435359cfed04"),
    cohort_v06=Input(rid="ri.foundry.main.dataset.668ae858-f82d-4a75-aa75-93e771f0f89a")
)
cohort_v07 <- function(cohort_v06) {
    library(dplyr)
    df <- cohort_v06 %>%
            filter(DEX==1|PRED==1|MPRED==1)
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ef30f699-92a0-4833-a1c4-45a1cf8a884d"),
    pre_charlson=Input(rid="ri.foundry.main.dataset.49871a69-3b4c-4aa6-96ee-739cb2c18b55")
)
pre_charslon_R <- function(pre_charlson) {
    library(dplyr)
    df <- pre_charlson %>%
            mutate(person_id = as.character(person_id),
                   MI = as.integer(MI),
                   CHF = as.integer(CHF),
                   PVD = as.integer(PVD),
                   stroke = as.integer(stroke),
                   dementia = as.integer(dementia),
                   pulmonary = as.integer(pulmonary),
                   rheumatic = as.integer(rheumatic),
                   PUD = as.integer(PUD),
                   liver_mild = as.integer(liver_mild),
                   diabetes = as.integer(diabetes),
                   dmcx = as.integer(dmcx),
                   paralysis = as.integer(paralysis),
                   renal = as.integer(renal),
                   cancer = as.integer(cancer),
                   liversevere = as.integer(liversevere),
                   mets = as.integer(mets),
                   hiv = as.integer(hiv),
                   multiple = as.integer(multiple),
                   CCI_INDEX = as.integer(CCI_INDEX)) 

    return(df)
}

