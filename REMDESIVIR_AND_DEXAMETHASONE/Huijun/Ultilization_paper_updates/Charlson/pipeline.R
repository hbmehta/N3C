

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8a92db7a-c62a-4bd9-9694-b83edbd535b1"),
    baseline_info=Input(rid="ri.foundry.main.dataset.b7b16c7c-f6d9-4b8b-a681-102545e86fb1")
)
baseline_info_2 <- function(baseline_info) {
    library(dplyr)
    df <- baseline_info %>%
            mutate(baseline = ifelse(is.na(condition_start_date), 0, 1)) %>%
            select(person_id, baseline) %>%
            distinct() %>%
            group_by(person_id) %>%
            summarize(baseline = max(baseline))
    return(df)
}

