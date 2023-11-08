

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3b650a0b-eca5-447f-8711-2acef0efee81"),
    bmi_sql=Input(rid="ri.foundry.main.dataset.03682c93-f9d9-4ff3-9d54-167d763a5163")
)
final_bmi <- function(bmi_sql) {
    library(dplyr)
    df <- bmi_sql %>%
            select(person_id, bmi_from_site, bmi_calculated) %>%
            mutate(bmi = ifelse(is.na(bmi_from_site), bmi_calculated, bmi_from_site))
    return(df)
}

