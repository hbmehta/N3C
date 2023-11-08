

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3a25a2ee-af9d-4ef3-b49d-e4e63f661d4a"),
    Cohort_steroids=Input(rid="ri.foundry.main.dataset.13938215-1231-46f0-b2b1-11d66016e00d"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875")
)
Cohort_steroids_2 <- function(death, Cohort_steroids) {
    library(dplyr)
    df <- Cohort_steroids %>%
            left_join(death %>% select(person_id, death_date), by="person_id") %>%
            distinct() %>%
            mutate(log_cv = log(center_volume)) %>%
            mutate(death_flag = ifelse(is.na(death_date)|death_date>covid_discharge,0,1))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e7ccb8e9-a477-480b-bd4e-ca217de0a7fc"),
    Cohort_steroids_2=Input(rid="ri.foundry.main.dataset.3a25a2ee-af9d-4ef3-b49d-e4e63f661d4a")
)
dex_death <- function(Cohort_steroids_2) {
    library(dplyr)
    library(janitor)
    df <- Cohort_steroids_2 %>%
            filter(DEX==1) %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(tot_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("time_to_drug_use: ",time_to_drug_use_cat,sep="")) %>%
            select(-time_to_drug_use_cat)
    df2 <- Cohort_steroids_2 %>%
            filter(DEX==1) %>%
            filter(death_flag==1) %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(dead_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("time_to_drug_use: ",time_to_drug_use_cat,sep="")) %>%
            select(-time_to_drug_use_cat) %>%
            left_join(df, by="covarite_cat")
    df3 <- Cohort_steroids_2 %>%
            filter(DEX==1) %>%
            group_by(Drug_duration_cat) %>%
            summarize(tot_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("Drug_duration: ",Drug_duration_cat,sep="")) %>%
            select(-Drug_duration_cat)
    df4 <- Cohort_steroids_2 %>%
            filter(DEX==1) %>%
            filter(death_flag==1) %>%
            group_by(Drug_duration_cat) %>%
            summarize(dead_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("Drug_duration: ",Drug_duration_cat,sep="")) %>%
            select(-Drug_duration_cat) %>%
            left_join(df3, by="covarite_cat")
    df5 <- rbind(df2, df4)

    return(df5)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e66b5f49-b59b-4eb6-9440-ee7a397200e4"),
    Cohort_steroids_2=Input(rid="ri.foundry.main.dataset.3a25a2ee-af9d-4ef3-b49d-e4e63f661d4a")
)
mpred_death <- function(Cohort_steroids_2) {
    library(dplyr)
    library(janitor)
    df <- Cohort_steroids_2 %>%
            filter(MPRED==1) %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(tot_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("time_to_drug_use: ",time_to_drug_use_cat,sep="")) %>%
            select(-time_to_drug_use_cat)
    df2 <- Cohort_steroids_2 %>%
            filter(MPRED==1) %>%
            filter(death_flag==1) %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(dead_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("time_to_drug_use: ",time_to_drug_use_cat,sep="")) %>%
            select(-time_to_drug_use_cat) %>%
            left_join(df, by="covarite_cat")
    df3 <- Cohort_steroids_2 %>%
            filter(MPRED==1) %>%
            group_by(Drug_duration_cat) %>%
            summarize(tot_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("Drug_duration: ",Drug_duration_cat,sep="")) %>%
            select(-Drug_duration_cat)
    df4 <- Cohort_steroids_2 %>%
            filter(MPRED==1) %>%
            filter(death_flag==1) %>%
            group_by(Drug_duration_cat) %>%
            summarize(dead_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("Drug_duration: ",Drug_duration_cat,sep="")) %>%
            select(-Drug_duration_cat) %>%
            left_join(df3, by="covarite_cat")
    df5 <- rbind(df2, df4)

    return(df5)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a7145190-51b8-452e-8998-cc3a420c9b7d"),
    Cohort_steroids_2=Input(rid="ri.foundry.main.dataset.3a25a2ee-af9d-4ef3-b49d-e4e63f661d4a")
)
overall_death <- function(Cohort_steroids_2) {
    library(dplyr)
    library(janitor)
    df <- Cohort_steroids_2 %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(tot_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("time_to_drug_use: ",time_to_drug_use_cat,sep="")) %>%
            select(-time_to_drug_use_cat)
    df2 <- Cohort_steroids_2 %>%
            filter(death_flag==1) %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(dead_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("time_to_drug_use: ",time_to_drug_use_cat,sep="")) %>%
            select(-time_to_drug_use_cat) %>%
            left_join(df, by="covarite_cat")
    df3 <- Cohort_steroids_2 %>%
            group_by(Drug_duration_cat) %>%
            summarize(tot_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("Drug_duration: ",Drug_duration_cat,sep="")) %>%
            select(-Drug_duration_cat)
    df4 <- Cohort_steroids_2 %>%
            filter(death_flag==1) %>%
            group_by(Drug_duration_cat) %>%
            summarize(dead_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("Drug_duration: ",Drug_duration_cat,sep="")) %>%
            select(-Drug_duration_cat) %>%
            left_join(df3, by="covarite_cat")
    df5 <- rbind(df2, df4)

    return(df5)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.37e25d37-1b60-446c-9792-da3084aab1bd"),
    Cohort_steroids_2=Input(rid="ri.foundry.main.dataset.3a25a2ee-af9d-4ef3-b49d-e4e63f661d4a")
)
pred_death <- function(Cohort_steroids_2) {
    library(dplyr)
    library(janitor)
    df <- Cohort_steroids_2 %>%
            filter(PRED==1) %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(tot_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("time_to_drug_use: ",time_to_drug_use_cat,sep="")) %>%
            select(-time_to_drug_use_cat)
    df2 <- Cohort_steroids_2 %>%
            filter(PRED==1) %>%
            filter(death_flag==1) %>%
            group_by(time_to_drug_use_cat) %>%
            summarize(dead_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("time_to_drug_use: ",time_to_drug_use_cat,sep="")) %>%
            select(-time_to_drug_use_cat) %>%
            left_join(df, by="covarite_cat")
    df3 <- Cohort_steroids_2 %>%
            filter(PRED==1) %>%
            group_by(Drug_duration_cat) %>%
            summarize(tot_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("Drug_duration: ",Drug_duration_cat,sep="")) %>%
            select(-Drug_duration_cat)
    df4 <- Cohort_steroids_2 %>%
            filter(PRED==1) %>%
            filter(death_flag==1) %>%
            group_by(Drug_duration_cat) %>%
            summarize(dead_pat = n()) %>%
            adorn_totals() %>%
            mutate(covarite_cat = paste("Drug_duration: ",Drug_duration_cat,sep="")) %>%
            select(-Drug_duration_cat) %>%
            left_join(df3, by="covarite_cat")
    df5 <- rbind(df2, df4)

    return(df5)
    
}

