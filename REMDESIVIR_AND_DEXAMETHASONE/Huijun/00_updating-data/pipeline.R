

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.efdf89d5-5140-439c-9b36-ba5dd53f678f"),
    cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc")
)
Figure_Number_of_patients_by_Site_and_Month <- function(cohort_v05) {
    library(lubridate)
    library(dplyr)
    library(scales)

    df <- cohort_v05 %>%
            mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) %>%
            group_by(month_year, data_partner_id) %>%
            summarize(n=n()) %>%
            mutate(rn = row_number())

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6ee2f8c0-b0fb-4439-8b49-7e751224be72"),
    cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc")
)
Number_of_Patients_by_month <- function(cohort_v05) {
    library(lubridate)
    library(dplyr)
    library(scales)
    
    df <- cohort_v05 %>%
            mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) %>%
            group_by(month_year) %>%
            summarize(n=n()) %>%
            mutate(rn = row_number())

    gg <- ggplot(df, aes(x=rn, y=n)) +
            geom_line() +
            geom_point() +
            theme_bw() +
            labs(y="Number of Patients", x="Month") +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3))) +
            scale_x_continuous(breaks=seq(1,14,1), labels=c("Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20", "Aug-20", "Sep-20", "Oct-20", "Nov-20", "Dec-20", "Jan-21", "Feb-21", "Mar-21")) +
            scale_y_continuous(labels=comma_format())
    plot(gg)
    return(df)
} 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1cbc99c5-08b1-4381-b555-03e41c0a25e8"),
    cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc")
)
Number_of_center_by_month <- function(cohort_v05) {
    library(lubridate)
    library(dplyr)
    df <- cohort_v05 %>%
            mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) %>%
            group_by(month_year) %>%
            summarize(Number_of_centers=n_distinct(data_partner_id))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.efe7471a-a8d7-4621-ad09-546b17f114d7"),
    cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc")
)
Number_of_patients_by_Site_and_Month <- function(cohort_v05) {
    library(lubridate)
    library(tidyr)
    library(dplyr)
    df <- cohort_v05 %>%
            mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) %>%
            group_by(month_year, data_partner_id) %>%
            summarize(n=n()) %>%
            pivot_wider(names_from = month_year, values_from = n)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c00cc87e-6081-4cb7-9bac-7b3fa8b92b46"),
    cohort_v03=Input(rid="ri.foundry.main.dataset.26e48fac-55e6-4026-8dcd-5e23c498e990")
)
cohort_v04 <- function(cohort_v03) {
    library(dplyr)
    library(lubridate)
    
    df <- cohort_v03 %>%
        mutate(day_of_birth = ifelse(is.na(day_of_birth), "1", day_of_birth)) %>%
        mutate(dob = as.Date(paste(year_of_birth, month_of_birth, day_of_birth,sep="-"))) %>%
        mutate(age = floor(decimal_date(covid_admission) - decimal_date(dob))) %>%
        mutate(LOS = as.numeric(covid_discharge)-as.numeric(covid_admission)) %>%
        filter(age >= 18) %>%
        mutate(one_year_prior = ymd(covid_admission) - years(1))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.37c100f5-6d57-488e-a062-d4f854033d70"),
    cohort_v04=Input(rid="ri.foundry.main.dataset.c00cc87e-6081-4cb7-9bac-7b3fa8b92b46"),
    pre_charslon_R=Input(rid="ri.foundry.main.dataset.11d0b7e9-14cd-4f07-a46a-5404d6febe7c")
)
cohort_v04_2 <- function(cohort_v04, pre_charslon_R) {
    library(dplyr)
    df <- cohort_v04 %>%
            inner_join(pre_charslon_R, by = "person_id")
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    cohort_v04_2=Input(rid="ri.foundry.main.dataset.37c100f5-6d57-488e-a062-d4f854033d70")
)
cohort_v05 <- function(cohort_v04_2) {
    library(dplyr)
    df <- cohort_v04_2 %>%
            filter(LOS >= 1) %>%
            filter(sex != "NO MATCHING CONCEPT") %>%
            filter(sex != "UNKNOWN")
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.11d0b7e9-14cd-4f07-a46a-5404d6febe7c"),
    pre_charlson=Input(rid="ri.foundry.main.dataset.16259ee7-9346-4816-b71f-139157686675")
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

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.200b30b9-5df7-4c2e-8053-8df046f607c5"),
    cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc")
)
unnamed_1 <- function(cohort_v05) {

    library(dplyr)
    library(lubridate)
    center_vol <- cohort_v05 %>%
                    group_by(data_partner_id) %>%
                    summarize(center_volume = n()) %>%
                    mutate(cv_quar = ntile(center_volume, 4))
    
    covariate <- cohort_v05 %>%
        left_join(center_vol, by="data_partner_id") %>%
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
                Charlson_range = case_when(
                    CCI_INDEX == 0 ~ "0",
                    CCI_INDEX == 1 ~ "1",
                    CCI_INDEX == 2 ~ "2",
                    CCI_INDEX == 3 ~ "3",
                    CCI_INDEX >= 4 ~ "4_plus")
               )
    

    age_group <- covariate %>%
            select(person_id, age_group) %>%
            group_by(age_group) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,2)) %>%
            mutate(covarite_cat = paste("age group: ",age_group,sep="")) %>%
            select(covarite_cat, person_count, percent)   

    sex <- covariate %>%
            select(person_id, sex) %>%
            group_by(sex) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,2)) %>%
            mutate(covarite_cat = paste("sex: ",sex,sep="")) %>%
            select(covarite_cat, person_count, percent)

    race_eth <- covariate %>%
            select(person_id, race_eth) %>%
            group_by(race_eth) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,2)) %>%
            mutate(covarite_cat = paste("race_eth: ",race_eth,sep="")) %>%
            select(covarite_cat, person_count, percent)

    Charlson_range <- covariate %>%
            select(person_id, Charlson_range) %>%
            group_by(Charlson_range) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,2)) %>%
            mutate(covarite_cat = paste("Charlson_range: ",Charlson_range,sep="")) %>%
            select(covarite_cat, person_count, percent)

    cv_quar <- covariate %>%
            select(person_id, cv_quar) %>%
            group_by(cv_quar) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,2)) %>%
            mutate(covarite_cat = paste("Center Volume Quartile: ",cv_quar,sep="")) %>%
            select(covarite_cat, person_count, percent)

    admission_month <- covariate %>%
            select(person_id, admission_month) %>%
            group_by(admission_month) %>%
            summarise(person_count = n_distinct(person_id)) %>%
            mutate(percent = round(person_count/sum(person_count)*100,2)) %>%
            mutate(covarite_cat = paste("admission_month: ",admission_month,sep="")) %>%
            select(covarite_cat, person_count, percent)

    df <- rbind(age_group, sex, race_eth, Charlson_range, cv_quar, admission_month) %>%
        mutate(Patients = paste(person_count, " (", percent,")", sep="")) %>%
        select(covarite_cat, Patients)
    return(df)
    
}

