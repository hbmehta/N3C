

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.277f9f32-7ea6-40de-8395-5ff67a8f310f"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
ECMO_no_vent <- function(Covariate_01) {
    library(dplyr)
    df <- Covariate_01 %>%
            filter(ECMO==1 & Invasive_Ventilation==0)

    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.24646189-88c6-44b6-908a-ab7183dc42b7"),
    ECMO_OBS=Input(rid="ri.foundry.main.dataset.ccb1e3aa-fe6a-41a9-bee5-9a7499d52973"),
    ECMO_no_vent_condition=Input(rid="ri.foundry.main.dataset.9d0bd810-059c-477c-ad8a-563c6c65a4f9"),
    ECMO_no_vent_procedure=Input(rid="ri.foundry.main.dataset.83b554ff-a358-48a5-89b1-a81071878f90")
)
ECMO_no_vent_OBS <- function(ECMO_no_vent_procedure, ECMO_OBS, ECMO_no_vent_condition) {
    
    library(dplyr)
    df1 <- ECMO_OBS %>%
        select(person_id, visit_occurrence_id) %>%
        distinct() %>%
        left_join(ECMO_no_vent_procedure, by=c("person_id", "visit_occurrence_id")) %>%
        rename(condition_procedure = procedure_concept_name)
    
    df2 <- ECMO_OBS %>%
        select(person_id, visit_occurrence_id) %>%
        distinct() %>%
        left_join(ECMO_no_vent_condition, by=c("person_id", "visit_occurrence_id")) %>%
        rename(condition_procedure = condition_concept_name)

    df <- rbind(df1, df2)
    return(df)
}

