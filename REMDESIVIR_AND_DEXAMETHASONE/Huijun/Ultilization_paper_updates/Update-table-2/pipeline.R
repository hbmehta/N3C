

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1faec45e-9384-40dc-a45e-15800ae4f4c7"),
    Dex_no_vent_or_2=Input(rid="ri.foundry.main.dataset.d8307a0c-9000-447f-acb1-2f7d6fe5bb3a"),
    Dex_vent_or_2=Input(rid="ri.foundry.main.dataset.730250de-0ca9-4358-9523-8eb518558601"),
    Rem_2=Input(rid="ri.foundry.main.dataset.4b4f351d-daa7-47ee-bbdd-851b07166938")
)
Table_2 <- function(Dex_vent_or_2, Dex_no_vent_or_2, Rem_2) {
    library(dplyr)
    rem <- Rem_2 %>%
        mutate(OR = round(exp(estimate),2),
               Lower_CI = round(exp(estimate + qnorm(0.025)*std_error),2),
               Upper_CI = round(exp(estimate + qnorm(0.975)*std_error),2)
        ) %>%
        mutate(rem_OR = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(term, rem_OR)

    dex_vent <- Dex_vent_or_2 %>%
                select(term, OR_CI) %>%
                rename(Dex_vent_OR = OR_CI)
    dex_no_vent <- Dex_no_vent_or_2 %>%
                select(term, OR_CI) %>%
                rename(Dex_no_vent_OR = OR_CI)
    df <- rem %>%
        left_join(dex_vent, by="term") %>%
        left_join(dex_no_vent, by="term") %>%
        filter(grepl('age', term)|grepl('sex', term)|grepl('race', term)|grepl('Charlson', term)|grepl('BMI', term)|grepl('AKI', term)|grepl('ECMO', term)|grepl('Invasive', term)|grepl('cv', term))
    return(df)
}

