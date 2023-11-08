

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8079ff29-9a73-482c-8e6c-8ad8d397c15d"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
DEX_REM_USE <- function(covariate) {
    
    library(dplyr)
    library(lubridate)
    df <- covariate %>%
        mutate(week_num = cut.Date(covid_admission, breaks = "1 week", labels = FALSE)) %>%
        mutate(week = ymd( "2020-02-10" ) + weeks(week_num - 1 )) %>% 
        mutate(rem_dex = ifelse(DEX==1&REM==1, 1, 0)) 
    
    table <- data.frame(
        mean = mean(df$rem_dex),
        sd = sd(df$rem_dex),
        median = median(df$rem_dex),
        twenty_five = quantile(df$rem_dex, probs=0.25),
        seventy_five = quantile(df$rem_dex, probs=0.75)
    )

    return(table)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ce0d8448-b2f2-4531-8b7d-06b0e28acf87"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
Drug_use_pt_count <- function(covariate) {
    library(dplyr)

    HCQ <- covariate %>%
        filter(drug_hcq_v1==1) %>%
        summarize(hcq = n_distinct(person_id))

    Rem <- covariate %>%
        filter(drug_rem_v1==1) %>%
        summarize(rem = n_distinct(person_id))

    Dex <- covariate %>%
        filter(drug_dex_v1==1) %>%
        summarize(dex = n_distinct(person_id))

    HCQ_alone <- covariate %>%
        filter(drug_hcq_v1==1 & is.na(drug_rem_v1) & is.na(drug_dex_v1)) %>%
        summarize(HCQ_alone = n_distinct(person_id))
    
    HCQ_rem <- covariate %>%
        filter(drug_hcq_v1==1 & drug_rem_v1 == 1 & is.na(drug_dex_v1)) %>%
        summarize(HCQ_rem = n_distinct(person_id))

    HCQ_dex <- covariate %>%
        filter(drug_hcq_v1==1 & is.na(drug_rem_v1) & drug_dex_v1 == 1) %>%
        summarize(HCQ_dex = n_distinct(person_id))

    rem_alone <- covariate %>%
        filter(is.na(drug_hcq_v1) & drug_rem_v1 == 1 & is.na(drug_dex_v1)) %>%
        summarize(rem_alone = n_distinct(person_id))

    dex_alone <- covariate %>%
        filter(is.na(drug_hcq_v1) & is.na(drug_rem_v1) & drug_dex_v1 == 1) %>%
        summarize(dex_alone = n_distinct(person_id))

    rem_dex <- covariate %>%
        filter(is.na(drug_hcq_v1) & drug_rem_v1 == 1 & drug_dex_v1 == 1) %>%
        summarize(rem_dex = n_distinct(person_id))

    all_three <- covariate %>%
        filter(drug_hcq_v1==1 & drug_rem_v1 == 1 & drug_dex_v1 == 1) %>%
        summarize(all_three = n_distinct(person_id))

    df <- data.frame(HCQ, Rem, Dex, HCQ_alone, HCQ_rem, HCQ_dex, rem_alone, dex_alone, rem_dex, all_three)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.55d6e077-fb21-4f1c-b078-95c69355928f"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
Venn <- function(covariate) {

    library(dplyr)
    library(gplots)

    x <- list(
    HCQ = covariate %>% filter(drug_hcq_v1==1) %>% select(person_id), 
    REM = covariate %>% filter(drug_rem_v1==1) %>% select(person_id), 
    DEX = covariate %>% filter(drug_dex_v1==1) %>% select(person_id)
    )

    v.table <- venn(x)

    plot(v.table)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.55fd09c0-97f8-4df6-b0ad-9399d6cce1c6"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
drug_use_by_severity <- function(covariate) {

    library(dplyr)
    library(tibble)
    df <- data.frame(
        rbind(
        table(
            covariate$HCQ, covariate$Severity_Type
        ),
        table(
            covariate$REM, covariate$Severity_Type
        ),
        table(
            covariate$DEX, covariate$Severity_Type
        )
        )
    )

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f777602e-4317-412c-8883-40609deb5993"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
mean_sd_age <- function(covariate) {
    library(dplyr)
    df <- data.frame(
        stats = c("mean_age", "sd_age"),
        number = c(mean(covariate$age, na.rm=T), sd(covariate$age, na.rm=T))
    )
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e503b180-3a5c-4768-8acc-c02f0ac72695"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
median_center_volume <- function(covariate) {
    library(dplyr)
    df <- data.frame(
        stats = c("mean_center_volume", "sd_center_volume", "median_center_volume", "p25_cv", "p75_cv"),
        number = c(mean(covariate$center_volume, na.rm=T), sd(covariate$center_volume, na.rm=T), median(covariate$center_volume, na.rm=T),
        twenty_five = quantile(covariate$center_volume, probs=0.25),
        seventy_five = quantile(covariate$center_volume, probs=0.75))
    )
    return(df)
    
}

