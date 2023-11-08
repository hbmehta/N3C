

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.48913daf-3b16-45c8-b2ae-114fa4c8a497"),
    Covariate_with_death=Input(rid="ri.foundry.main.dataset.e6c9c5fe-952e-403c-b05c-840476f0b30c")
)
steroid_diabetes <- function(Covariate_with_death) {
    library(dplyr)
    df <- Covariate_with_death %>%
        mutate(dex_ste = ifelse(DEX==1|steroid==1,1,0),
               dia = ifelse(diabetes==1|dmcx==1,1,0)) %>%
        select(person_id,dex_ste,dia) 
    return(df)
}

