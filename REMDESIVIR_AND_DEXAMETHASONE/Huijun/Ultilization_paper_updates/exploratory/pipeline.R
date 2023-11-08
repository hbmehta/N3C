

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6a68ca3c-c105-4fe1-a137-846f910d9156"),
    Cohort_v04_2=Input(rid="ri.foundry.main.dataset.37c100f5-6d57-488e-a062-d4f854033d70")
)
LOS_0_1 <- function(Cohort_v04_2) {
    library(dplyr)
    df <- Cohort_v04_2 %>%
            filter(sex != "NO MATCHING CONCEPT") %>%
            filter(sex != "UNKNOWN")
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.beafb155-016d-4215-9e1d-494377c12719"),
    LOS_0_1=Input(rid="ri.foundry.main.dataset.6a68ca3c-c105-4fe1-a137-846f910d9156")
)
LOS_0_2 <- function(LOS_0_1) {
    library(dplyr)
    df <- explore_1 %>%
         filter(LOS < 1) 
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8bc11b1c-d19f-4c9b-834e-b40664b0eeee"),
    LOS_0_2=Input(rid="ri.foundry.main.dataset.beafb155-016d-4215-9e1d-494377c12719"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875")
)
LOS_0_3 <- function(death, LOS_0_2) {
    library(dplyr)
    death_df <- death %>%
            select(person_id, death_date) %>%
            distinct() 
    df <- LOS_0_2 %>%
        left_join(death_df, by = "person_id") %>%
        mutate(same_day_death = ifelse(death_date==covid_admission, 1, 0))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.40499269-76f6-4a4f-b942-692720bb9614"),
    all_dex_before=Input(rid="ri.foundry.main.dataset.ac78c607-863f-4a4c-9acf-8586fafb990a")
)
all_dex_30 <- function(all_dex_before) {
    library(dplyr)
    df <- all_dex_before %>%
            mutate(covid_30 = covid_admission-30) %>%
            filter(drug_exposure_start_date > covid_30) %>%
            select(person_id) %>%
            distinct()
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f7ccaf96-34cc-4dc7-b3bc-c3685bde8827"),
    all_dex_before=Input(rid="ri.foundry.main.dataset.ac78c607-863f-4a4c-9acf-8586fafb990a")
)
all_dex_60 <- function(all_dex_before) {
    library(dplyr)
    df <- all_dex_before %>%
            mutate(covid_60 = covid_admission-60) %>%
            filter(drug_exposure_start_date > covid_60) %>%
            select(person_id) %>%
            distinct()
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.821c5138-1108-4c70-af4d-b8bb3ab89830"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
any_missing <- function(Covariate_01) {
    library(dplyr)
    df <- Covariate_01 %>%
        filter(race_eth=="6_Missing" | BMI_cat=="7_Missing")
    return(df)
}

