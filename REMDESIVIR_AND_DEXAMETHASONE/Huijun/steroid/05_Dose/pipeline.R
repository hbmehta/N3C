

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.eba1b0fc-2210-4ef0-8d66-d220d84790cc"),
    all_dex=Input(rid="ri.foundry.main.dataset.5196a1f1-84bb-49c1-8211-17d35af07c05")
)
all_dex_2 <- function(all_dex) {
    library(dplyr)
    df <- all_dex %>%
        mutate(Dex_1_n_half_21 = ifelse(drug_concept_name %in% c("{21 (dexamethasone 1.5 MG Oral Tablet) } Pack", "{21 (dexamethasone 1.5 MG Oral Tablet) } Pack [DexPak TaperPak 6 Day]"), 1, 0)) %>%
        mutate(Dex_1_n_half_25 = ifelse(drug_concept_name=="{25 (dexamethasone 1.5 MG Oral Tablet) } Pack [Zcort 7 Day Taper]", 1, 0)) %>%
        mutate(Dex_1_n_half_39 = ifelse(drug_concept_name=="{39 (dexamethasone 1.5 MG Oral Tablet) } Pack", 1, 0)) %>%
        mutate(Dex_pt_517 = ifelse(drug_concept_name=="0.005 ML dexamethasone 103.4 MG/ML Injection",1,0)) %>%
        mutate(Dex_pt_001 = ifelse(grepl("0.001 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_pt_1 = ifelse(grepl("0.1 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_pt_133 = ifelse(grepl("0.133 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_pt_2 = ifelse(grepl("0.2 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_pt_25 = ifelse(grepl("0.25 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_pt_4 = ifelse(grepl("0.4 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_pt_5 = ifelse(grepl("0.5 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_pt_7 = ifelse(grepl("0.7 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_pt_75 = ifelse(grepl("0.75 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_1 = ifelse(grepl("1 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_1_n_half = ifelse(grepl("1.5 MG", drug_concept_name) & Dex_1_n_half_21==0 & Dex_1_n_half_25==0 & Dex_1_n_half_39==0, 1, 0)) %>%
        mutate(Dex_2 = ifelse(grepl("2 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_4 = ifelse(grepl("4 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_6 = ifelse(grepl("6 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_10 = ifelse(grepl("10 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_20 = ifelse(grepl("20 MG", drug_concept_name), 1, 0)) %>%
        mutate(Dex_24 = ifelse(grepl("24 MG", drug_concept_name), 1, 0)) %>%
        arrange(person_id, drug_exposure_start_date)
        
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.99aa9f74-271a-4019-8b08-24d029dae1ee"),
    all_dex_2=Input(rid="ri.foundry.main.dataset.eba1b0fc-2210-4ef0-8d66-d220d84790cc")
)
all_dex_3 <- function(all_dex_2) {
    library(dplyr)
    df <- all_dex_2 %>%
        select(-drug_concept_name, -drug_exposure_start_date, -drug_exposure_end_date) %>%
        group_by(person_id) %>%
        summarize(Dex_1_n_half_21=sum(Dex_1_n_half_21), Dex_1_n_half_25=sum(Dex_1_n_half_25), Dex_1_n_half_39=sum(Dex_1_n_half_39),
                  Dex_pt_517=sum(Dex_pt_517), Dex_pt_001=sum(Dex_pt_001), Dex_pt_1=sum(Dex_pt_1), Dex_pt_133=sum(Dex_pt_133),
                  Dex_pt_2=sum(Dex_pt_2), Dex_pt_25=sum(Dex_pt_25), Dex_pt_4=sum(Dex_pt_4), Dex_pt_5=sum(Dex_pt_5),
                  Dex_pt_7=sum(Dex_pt_7), Dex_pt_75=sum(Dex_pt_75), Dex_1=sum(Dex_1), Dex_1_n_half=sum(Dex_1_n_half),
                  Dex_2=sum(Dex_2), Dex_4=sum(Dex_4), Dex_6=sum(Dex_6), Dex_10=sum(Dex_10), Dex_20=sum(Dex_20), Dex_24=sum(Dex_24)) %>%
        mutate(dex_dose_sum = 31.5*Dex_1_n_half_21+37.5*Dex_1_n_half_25+58.5*Dex_1_n_half_39+0.517*Dex_pt_517+0.001*Dex_pt_001+
                              0.1*Dex_pt_1+0.133*Dex_pt_133+0.2*Dex_pt_2+0.25*Dex_pt_25+0.4*Dex_pt_4+0.5*Dex_pt_5+0.7*Dex_pt_7+
                              0.75*Dex_pt_75+Dex_1+1.5*Dex_1_n_half+2*Dex_2+4*Dex_4+6*Dex_6+10*Dex_10+20*Dex_20+24*Dex_24) %>%
        mutate(dex_dose_sum = ifelse(dex_dose_sum==0,NA,dex_dose_sum))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.90dad857-4ea7-463f-8e97-0c6ec1a9ad3d"),
    all_mpred=Input(rid="ri.foundry.main.dataset.22e2cdf1-7a6f-4f22-803a-47cf4ac8bb29")
)
all_mpred_2 <- function(all_mpred) {
    library(dplyr)
    df <- all_mpred %>%
        mutate(Mpred_4_21 = ifelse(drug_concept_name %in% c("{21 (methylprednisolone 4 MG Oral Tablet) } Pack", "{21 (methylprednisolone 4 MG Oral Tablet [Medrol]) } Pack [Medrol Dosepak]"), 1, 0)) %>%
        mutate(Mpred_4 = ifelse(grepl("4 MG", drug_concept_name) & Mpred_4_21==0, 1, 0)) %>%
        mutate(Mpred_8 = ifelse(grepl("8 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_16 = ifelse(grepl("16 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_20 = ifelse(grepl("20 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_32 = ifelse(grepl("32 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_40 = ifelse(grepl("40 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_50 = ifelse(grepl("50 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_62_n_half = ifelse(grepl("62.5 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_80 = ifelse(grepl("80 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_125 = ifelse(grepl("125 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_500 = ifelse(grepl("500 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_1000 = ifelse(grepl("1000 MG", drug_concept_name), 1, 0)) %>%
        mutate(Mpred_2000 = ifelse(grepl("2000 MG", drug_concept_name), 1, 0)) %>%
        arrange(person_id, drug_exposure_start_date)
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e7b7ee02-97d0-4d6c-83ae-e28a270a2ac9"),
    all_mpred_2=Input(rid="ri.foundry.main.dataset.90dad857-4ea7-463f-8e97-0c6ec1a9ad3d")
)
all_mpred_3 <- function(all_mpred_2) {
    library(dplyr)
    df <- all_mpred_2 %>%
        select(-drug_concept_name, -drug_exposure_start_date, -drug_exposure_end_date) %>%
        group_by(person_id) %>%
        summarize(Mpred_4_21=sum(Mpred_4_21), Mpred_4=sum(Mpred_4), Mpred_8=sum(Mpred_8), Mpred_16=sum(Mpred_16),
                  Mpred_20=sum(Mpred_20), Mpred_32=sum(Mpred_32), Mpred_40=sum(Mpred_40), Mpred_50=sum(Mpred_50),
                  Mpred_62_n_half=sum(Mpred_62_n_half), Mpred_80=sum(Mpred_80), Mpred_125=sum(Mpred_125),
                  Mpred_500=sum(Mpred_500), Mpred_1000=sum(Mpred_1000), Mpred_2000=sum(Mpred_2000)) %>%
        mutate(mpred_dose_sum = 84*Mpred_4_21+4*Mpred_4+8*Mpred_8+16*Mpred_16+20*Mpred_16+32*Mpred_32+
                                40*Mpred_40+50*Mpred_50+62.5*Mpred_62_n_half+80*Mpred_80+125*Mpred_125+
                                500*Mpred_500+1000*Mpred_1000+2000*Mpred_2000) %>%
        mutate(mpred_dose_sum = ifelse(mpred_dose_sum==0,NA,mpred_dose_sum))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.da78bd00-f75f-4d89-90e7-c66fc47cce09"),
    all_pred=Input(rid="ri.foundry.main.dataset.0e5a76cc-4f11-4b47-ba8c-b2d784527491")
)
all_pred_2 <- function(all_pred) { 
    library(dplyr)
    df <- all_pred %>%
        mutate(Pred_10_10 = ifelse(drug_concept_name=="{10 (Prednisone 10 MG Oral Tablet) } Pack", 1, 0)) %>%
        mutate(Pred_10_48 = ifelse(drug_concept_name %in% c("{48 (prednisone 10 MG Oral Tablet [Sterapred DS]) } Pack [Sterapred DS 12 Day Uni-Pak]", "{48 (prednisone 10 MG Oral Tablet) } Pack"), 1, 0)) %>%
        mutate(Pred_5_21 = ifelse(drug_concept_name=="{21 (prednisone 5 MG Oral Tablet) } Pack", 1, 0)) %>%
        mutate(Pred_10_25 = ifelse(drug_concept_name=="{25 (prednisone 10 MG Oral Tablet) } Pack", 1, 0)) %>%
        mutate(Pred_10_21 = ifelse(drug_concept_name %in% c("{21 (prednisone 10 MG Oral Tablet) } Pack", "{21 (prednisone 10 MG Oral Tablet [Sterapred DS]) } Pack [Sterapred DS Uni-Pak]"), 1, 0)) %>%
        mutate(Pred_5_48 = ifelse(drug_concept_name=="{48 (prednisone 5 MG Oral Tablet) } Pack", 1, 0)) %>%
        mutate(Pred_1 = ifelse(grepl("1 MG", drug_concept_name), 1, 0)) %>%
        mutate(Pred_2 = ifelse(grepl("2 MG", drug_concept_name), 1, 0)) %>%
        mutate(Pred_2_n_half = ifelse(grepl("2.5 MG", drug_concept_name), 1, 0)) %>%
        mutate(Pred_20 = ifelse(grepl("20 MG", drug_concept_name), 1, 0)) %>%
        mutate(Pred_50 = ifelse(grepl("50 MG", drug_concept_name), 1, 0)) %>%
        mutate(Pred_5 = ifelse(grepl("5 MG", drug_concept_name) & Pred_5_21==0 & Pred_5_48==0, 1, 0)) %>%
        mutate(Pred_10 = ifelse(grepl("10 MG", drug_concept_name) & Pred_10_10==0 & Pred_10_48==0 & Pred_10_25==0 & Pred_10_21==0, 1, 0)) %>%
        arrange(person_id, drug_exposure_start_date)
        
    
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f3060999-4469-4c55-835d-71edd30af977"),
    all_pred_2=Input(rid="ri.foundry.main.dataset.da78bd00-f75f-4d89-90e7-c66fc47cce09")
)
all_pred_3 <- function(all_pred_2) {
    library(dplyr)
    df <- all_pred_2 %>%
        select(-drug_concept_name, -drug_exposure_start_date, -drug_exposure_end_date) %>%
        group_by(person_id) %>%
        summarize(Pred_10_10=sum(Pred_10_10), Pred_10_48=sum(Pred_10_48), Pred_5_21=sum(Pred_5_21),
                  Pred_10_25=sum(Pred_10_25), Pred_10_21=sum(Pred_10_21), Pred_5_48=sum(Pred_5_48),
                  Pred_1=sum(Pred_1), Pred_2=sum(Pred_2), Pred_2_n_half=sum(Pred_2_n_half),
                  Pred_20=sum(Pred_20), Pred_50=sum(Pred_50), Pred_5=sum(Pred_5), Pred_10=sum(Pred_10)) %>%
        mutate(pred_dose_sum = 100*Pred_10_10+480*Pred_10_48+105*Pred_5_21+250*Pred_10_25+210*Pred_10_21+240*Pred_5_48+
                               Pred_1+2*Pred_2+2.5*Pred_2_n_half+20*Pred_20+50*Pred_50+5*Pred_5+10*Pred_10) %>%
        mutate(pred_dose_sum = ifelse(pred_dose_sum==0,NA,pred_dose_sum))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1b1a3c5a-d435-4afd-abc1-4c65cb85af98"),
    all_dex_3=Input(rid="ri.foundry.main.dataset.99aa9f74-271a-4019-8b08-24d029dae1ee")
)
dex_dist <- function(all_dex_3) {
    library(dplyr)
    df <- all_dex_3 %>%
        filter(!is.na(dex_dose_sum))
    
    return(data.frame(
        mean(df$dex_dose_sum), sd(df$dex_dose_sum)
    ))
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.730acb97-1888-4125-8832-cee0466e5f16"),
    all_mpred_3=Input(rid="ri.foundry.main.dataset.e7b7ee02-97d0-4d6c-83ae-e28a270a2ac9")
)
mpred_dist <- function(all_mpred_3) {
    library(dplyr)
    df <- all_mpred_3 %>%
        filter(!is.na(mpred_dose_sum))
    
    return(data.frame(
        mean(df$mpred_dose_sum), sd(df$mpred_dose_sum)
    ))
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.995e01d5-0c8f-4403-af9c-03b935fe0a99"),
    all_pred_3=Input(rid="ri.foundry.main.dataset.f3060999-4469-4c55-835d-71edd30af977")
)
pred_dist <- function(all_pred_3) {
    library(dplyr)
    df <- all_pred_3 %>%
        filter(!is.na(pred_dose_sum))
    
    return(data.frame(
        mean(df$pred_dose_sum), sd(df$pred_dose_sum)
    ))
}

