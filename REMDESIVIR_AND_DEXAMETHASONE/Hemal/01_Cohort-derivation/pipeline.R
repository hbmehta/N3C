

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.df9b9d80-5b19-4309-ab72-71ba567513cd"),
    cohort_v03=Input(rid="ri.foundry.main.dataset.78a07753-82f8-4b7d-ba93-0256c3bcccfb")
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
    Output(rid="ri.foundry.main.dataset.050f501f-2aff-46e1-acf9-102cdb777bfe"),
    cohort_v04=Input(rid="ri.foundry.main.dataset.df9b9d80-5b19-4309-ab72-71ba567513cd")
)
cohort_v05 <- function(cohort_v04) {
    library(dplyr)
    df <- cohort_v04 %>%
            filter(LOS >= 1)
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7bebd326-f745-4186-8199-4b457557d713"),
    pre_charlson=Input(rid="ri.foundry.main.dataset.2dc76f42-af36-4b2d-ad4a-e376cd1eb7ad")
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
    Output(rid="ri.foundry.main.dataset.55618e49-30c6-4a83-a3b7-efe31bfef668"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.fd1fe4a0-93c7-4745-a602-cee3908cf624")
)
unnamed <- function(hospitalized_covid_patients) {
    library(dplyr)
    df <- hospitalized_covid_patients %>%
            mutate(LOS = as.numeric(macrovisit_end_date-macrovisit_start_date)) %>%
            filter(LOS==0) %>%
            select(-visit_source_value,-data_partner_id,-covid_diagnosis)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.99e35873-26cf-48e4-905f-7a8db2194988"),
    cohort_v04=Input(rid="ri.foundry.main.dataset.df9b9d80-5b19-4309-ab72-71ba567513cd")
)
unnamed_1 <- function(cohort_v04) {
    df <- cohort_v04 %>%
            mutate(LOS_flag = ifelse(LOS==0,0,1),
                   hcq_flag = ifelse(is.na(drug_hcq_v1),0,1),
                   rem_flag = ifelse(is.na(drug_rem_v1),0,1),
                   dex_flag = ifelse(is.na(drug_dex_v1),0,1))
    df1 <- data.frame(cbind(
        table(df$LOS_flag, df$hcq_flag),
        table(df$LOS_flag, df$rem_flag),
        table(df$LOS_flag, df$dex_flag)
    ))

    return(df1)
}

