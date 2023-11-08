

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1bf034bc-9dfd-4f6c-9c3e-3861f3fda9cc"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875")
)
Covariate_with_death <- function(death, Covariate_with_steroid) {
    library(dplyr)
    death_table <- death %>%
        select(person_id, death_date) %>%
        filter(!is.na(death_date)) %>%
        group_by(person_id) %>%
        summarize(death_date = min(death_date))
        
    df <- Covariate_with_steroid %>%
            left_join(death_table, by="person_id")
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fcb62258-235a-42b6-b6c4-3b6678f07b88"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
Dex_pt_month <- function(covariate) {
    library(dplyr)
    library(lubridate)

    df <- covariate %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(month = month(dex_start_date))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.d6455d85-64fb-4ac7-8b85-9874e16b4e9d"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
Duration_of_DEX <- function(rem_and_dex) {
    library(ggplot2)
    library(grid)

    df <- rem_and_dex %>%
        filter(drug_dex_v1==1) %>%
        mutate(Invasive_Ventilation = ifelse(Invasive_Ventilation==1, "Ventilated", "Not Ventilated"))

    gg <- ggplot(df, aes(x=dex_duration)) +
          geom_histogram(aes(y = ..count..), binwidth = 1, fill="#1b9e77", color="white") +
          theme_bw() +
          labs(x="Duration of Dexamethasone Use (Days)", y="Number of Patients Hospitalized with COVID-19\nfrom February to October") +
          theme(axis.text.x = element_text(hjust=0.9, size=14),
                axis.text.y = element_text(size=15),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                strip.text.x = element_text(size = 16)) +
          scale_x_continuous(breaks=seq(0,25,5), limit=c(-1,25)) +
          scale_y_continuous(breaks=seq(0,4000,500), limit=c(0,4000), labels=comma_format()) +
          coord_cartesian(clip = "off") +
          facet_grid(. ~ Invasive_Ventilation)

    table <- df %>% 
                rename(`Invasive Ventilation` = Invasive_Ventilation) %>%
                group_by(`Invasive Ventilation`) %>% 
                summarize(
                    `25 percentile` = quantile(dex_duration, probs = 0.25),
                    median = median(dex_duration),
                    mean = round(mean(dex_duration),1),
                    SD = round(sd(dex_duration),1),
                    `75 percentile` = quantile(dex_duration, probs = 0.75)
                )

    mytheme <- gridExtra::ttheme_minimal(
                core = list(fg_params=list(cex = 1.1)),
                colhead = list(fg_params=list(cex = 1.1)))
    tbl <- tableGrob(table, theme = mytheme, rows = NULL)

    p <- grid.arrange(gg,
             tbl,
             nrow = 2,
             as.table = TRUE, 
             heights = c(11, 1))
    p1 <- cowplot::ggdraw(p) + 
        theme(plot.background = element_rect(fill="white", color=NA))
    
    plot(p1)    
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3d745ff6-45cd-438d-9224-e33db5aeed36"),
    Dex_pt_month=Input(rid="ri.foundry.main.dataset.fcb62258-235a-42b6-b6c4-3b6678f07b88"),
    tot_pt_month=Input(rid="ri.foundry.main.dataset.a4032b12-8ffa-4d12-8d1c-5e062c65b4c4")
)
dex_month_race_percent <- function(tot_pt_month, Dex_pt_month) {

    library(dplyr)
    
    df <- Dex_pt_month %>%
          select(person_id, month, race_eth) %>%
          group_by(month, race_eth) %>%
          summarize(dex_patient_number = n()) 

    df1 <- tot_pt_month %>%
          select(person_id, month, race_eth) %>%
          group_by(month, race_eth) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = c("month", "race_eth"), all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4)) %>%
            filter(!month %in% c(11,12)) %>%
            filter(race_eth %in% c("1_NH_WHITE", "2_NH_BLACK", "3_ASIAN", "5_HISPANIC")) %>%
            mutate(dex_percent = ifelse(is.na(dex_percent), 0, dex_percent)) %>%
            mutate(race_eth = case_when(
                            race_eth == "1_NH_WHITE" ~ "1_NH_WHITE",
                            race_eth == "2_NH_BLACK" ~ "2_NH_BLACK",
                            race_eth == "3_ASIAN" ~ "4_ASIAN",
                            race_eth == "5_HISPANIC" ~ "3_HISPANIC"))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.09724649-f4bf-493e-8d9d-483ac3a0fa3c"),
    Dex_pt_month=Input(rid="ri.foundry.main.dataset.fcb62258-235a-42b6-b6c4-3b6678f07b88"),
    tot_pt_month=Input(rid="ri.foundry.main.dataset.a4032b12-8ffa-4d12-8d1c-5e062c65b4c4")
)
dex_month_vent_percent <- function(tot_pt_month, Dex_pt_month) {

    library(dplyr)
    
    df <- Dex_pt_month %>%
          select(person_id, month, Invasive_Ventilation) %>%
          group_by(month, Invasive_Ventilation) %>%
          summarize(dex_patient_number = n()) 

    df1 <- tot_pt_month %>%
          select(person_id, month, Invasive_Ventilation) %>%
          group_by(month, Invasive_Ventilation) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = c("month", "Invasive_Ventilation"), all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4)) %>%
            filter(!month %in% c(11,12)) %>%
            mutate(dex_percent = ifelse(is.na(dex_percent), 0, dex_percent)) 

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.22f2ce16-4652-4873-ad0b-887b0eb6544c"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c"),
    tot_pt_month=Input(rid="ri.foundry.main.dataset.a4032b12-8ffa-4d12-8d1c-5e062c65b4c4")
)
dex_rem_month_percent <- function(tot_pt_month, rem_and_dex) {

    library(dplyr)
    library(lubridate)
    
    df <- rem_and_dex %>%
          filter(rem_dex==1) %>%
          mutate(month = month(dex_start_date)) %>%
          select(person_id, month) %>%
          group_by(month) %>%
          summarize(dex_rem_patient_number = n()) 

    df1 <- tot_pt_month %>%
          select(person_id, month) %>%
          group_by(month) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = "month", all = TRUE) %>%
            mutate(dex_rem_percent = round(dex_rem_patient_number/total_patient_number, 4)) %>%
            filter(!month %in% c(11,12)) %>%
            mutate(dex_rem_percent = ifelse(is.na(dex_rem_percent), 0, dex_rem_percent))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7559950b-0f88-4917-b7c8-63c9cf4f792f"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
dex_steroid <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_with_steroid %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month = month(dex_start_date))
    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7718f5fb-9d77-4ba4-bc5c-06efb2763884"),
    exclude_pts_less_2_days=Input(rid="ri.foundry.main.dataset.4f93bad5-e718-44c5-907b-64ff01c0c0f2")
)
dex_steroid_2 <- function(exclude_pts_less_2_days) {
    library(dplyr)
    library(lubridate)

    df <- exclude_pts_less_2_days %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month = month(dex_start_date))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ae9192b8-d52c-4957-9645-e292a5b7325d"),
    Covriate_steroid_spo2=Input(rid="ri.foundry.main.dataset.7c49440e-8919-4688-9177-e7323ae353a0")
)
dex_steroid_3 <- function(Covriate_steroid_spo2) {
    library(dplyr)
    library(lubridate)

    df <- Covriate_steroid_spo2 %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, Invasive_Ventilation, Steroid_flag, SPO2_less_than_93) %>%
        mutate(month = month(dex_start_date)) %>%
        mutate(SPO2 = case_when(
                            SPO2_less_than_93 == 1 ~ "1_SPO2_less_than_93",
                            SPO2_less_than_93 == 0 ~ "2_SPO2_greater_or_equal_to_93",
                            is.na(SPO2_less_than_93) ~ "3_NA"))
    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ad0d68ff-c685-4a34-b6f8-3093a09800c1"),
    dex_steroid_3=Input(rid="ri.foundry.main.dataset.ae9192b8-d52c-4957-9645-e292a5b7325d"),
    steroid_3=Input(rid="ri.foundry.main.dataset.9688b559-6f55-4b31-9984-77aca21d9540"),
    tot_3=Input(rid="ri.foundry.main.dataset.11ae13b5-0dae-4681-b8d9-fefa04a6ae4b")
)
dex_steroid_o2 <- function(tot_3, dex_steroid_3, steroid_3) {

    library(dplyr)
    
    dex <- dex_steroid_3 %>%
          filter(Invasive_Ventilation==0) %>%
          select(person_id, month, SPO2) %>%
          group_by(month, SPO2) %>%
          summarize(dex_patient_number = n()) 

    no_dex_steroid <- steroid_3 %>%
          filter(Invasive_Ventilation==0) %>%
          select(person_id, month, SPO2) %>%
          group_by(month, SPO2) %>%
          summarize(steroid_patient_number = n()) 

    tot <- tot_3 %>%
          filter(Invasive_Ventilation==0) %>%
          select(person_id, month, SPO2) %>%
          group_by(month, SPO2) %>%
          summarize(total_patient_number = n())

    df2 <- tot %>%
            left_join(dex, by = c("month","SPO2")) %>%
            left_join(no_dex_steroid, by = c("month", "SPO2")) %>%
            mutate(dex_patient_number = ifelse(is.na(dex_patient_number), 0, dex_patient_number),
                   steroid_patient_number = ifelse(is.na(steroid_patient_number), 0, steroid_patient_number)) %>%
            mutate(c_dex_percent = round(dex_patient_number/total_patient_number, 4),
                    b_steroid_percent = round(steroid_patient_number/total_patient_number, 4),
                    a_none_percent = round(1-c_dex_percent-b_steroid_percent, 4)) %>%
            filter(!month %in% c(2,3,4,5,6,11,12)) 

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8ce07f91-39c4-4915-899b-5cbf87244e92")
)
dex_steroid_race_Vent_2 <- function(dex_steroid_race_vent) {
    library(tidyverse) 
    library(dplyr)
    library(scales)

    df <- dex_steroid_race_vent %>%
            select(month, race_eth, c_dex_percent, b_steroid_percent, a_none_percent) %>%
            pivot_longer(c_dex_percent:a_none_percent, names_to = "drug_type", values_to = "percent") %>%
            mutate(race_eth = case_when(
                            race_eth == "1_NH_WHITE" ~ "1_NH_WHITE",
                            race_eth == "2_NH_BLACK" ~ "2_NH_BLACK",
                            race_eth == "3_ASIAN" ~ "4_ASIAN",
                            race_eth == "5_HISPANIC" ~ "3_HISPANIC"))
            # %>% 
            # add_row(month = 2, Invasive_Ventilation = 1, drug_type = "a_none_percent", percent = 1)
    
    race <- c("Non-Hispanic WHITE", "Non-Hispanic BLACK", "HISPANIC", "ASIAN")
    names(race) <- c("1_NH_WHITE", "2_NH_BLACK", "3_HISPANIC", "4_ASIAN")

    gg <- ggplot(df, aes(fill=drug_type, y=percent, x=month)) + 
            geom_bar(position="fill", stat="identity") +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            theme_bw() +
            theme(axis.text.x = element_text(size=11),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.key.size = unit(16, "points"),
                  legend.position="top",
                  strip.text.x = element_text(size = 16)) +
            scale_fill_manual(breaks = c("c_dex_percent", "b_steroid_percent", "a_none_percent"), 
                              labels = c("Dexamethasone", "Steroid", "None"), 
                              values = c('#1b9e77','#7570b3','#d95f02'),
                              name="Drug Use") +
            
            scale_x_continuous(breaks=seq(2,10,1), 
                                labels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), limits=c(1.5,10.5)) +
            labs(y="Drug Use among Ventilators(%)", 
                 x="Month") +
            facet_grid(. ~ race_eth, 
                         labeller = labeller(race_eth = race))

    plot(gg)
    return(df)

    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.49275e23-fcde-489c-9b11-5f7ba6fe77f6"),
    dex_steroid=Input(rid="ri.foundry.main.dataset.7559950b-0f88-4917-b7c8-63c9cf4f792f"),
    steroid=Input(rid="ri.foundry.main.dataset.6a6427f8-c111-4b14-8153-8a86db47b60c"),
    tot_steroid=Input(rid="ri.foundry.main.dataset.256b783d-8b33-434a-9842-1511759a7cf0")
)
dex_steroid_vent <- function(tot_steroid, dex_steroid, steroid) {

    library(dplyr)
    
    dex <- dex_steroid %>%
          select(person_id, month, Invasive_Ventilation) %>%
          group_by(month, Invasive_Ventilation) %>%
          summarize(dex_patient_number = n()) 

    no_dex_steroid <- steroid %>%
          select(person_id, month, Invasive_Ventilation) %>%
          group_by(month, Invasive_Ventilation) %>%
          summarize(steroid_patient_number = n()) 

    tot <- tot_steroid %>%
          select(person_id, month, Invasive_Ventilation) %>%
          group_by(month, Invasive_Ventilation) %>%
          summarize(total_patient_number = n())

    df2 <- tot %>%
            left_join(dex, by = c("month", "Invasive_Ventilation")) %>%
            left_join(no_dex_steroid, by = c("month", "Invasive_Ventilation")) %>%
            mutate(dex_patient_number = ifelse(is.na(dex_patient_number), 0, dex_patient_number),
                   steroid_patient_number = ifelse(is.na(steroid_patient_number), 0, steroid_patient_number)) %>%
            mutate(c_dex_percent = round(dex_patient_number/total_patient_number, 4),
                    b_steroid_percent = round(steroid_patient_number/total_patient_number, 4),
                    a_none_percent = round(1-c_dex_percent-b_steroid_percent, 4)) %>%
            filter(!month %in% c(11,12)) 

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a9457db2-b8d0-4bd0-b7ef-5e123f5e675f"),
    dex_steroid_vent=Input(rid="ri.foundry.main.dataset.49275e23-fcde-489c-9b11-5f7ba6fe77f6")
)
dex_steroid_vent_2 <- function(dex_steroid_vent) {
    library(tidyverse) 
    library(dplyr)
    library(scales)

    df <- dex_steroid_vent %>%
            select(month, Invasive_Ventilation, c_dex_percent, b_steroid_percent, a_none_percent) %>%
            pivot_longer(c_dex_percent:a_none_percent, names_to = "drug_type", values_to = "percent") %>%
            mutate(Invasive_Ventilation = ifelse(Invasive_Ventilation==1, "Ventilator", "Not Ventilator"))
            # %>% 
            # add_row(month = 2, Invasive_Ventilation = 1, drug_type = "a_none_percent", percent = 1)
    

    gg <- ggplot(df, aes(fill=drug_type, y=percent, x=month)) + 
            geom_bar(position="fill", stat="identity") +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            theme_bw() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.key.size = unit(16, "points"),
                  legend.position="top",
                  strip.text.x = element_text(size = 16)) +
            scale_fill_manual(breaks = c("c_dex_percent", "b_steroid_percent", "a_none_percent"), 
                              labels = c("Dexamethasone", "Steroid", "None"), 
                              values = c('#1b9e77','#7570b3','#d95f02'),
                              name="Drug Use") +
            
            scale_x_continuous(breaks=seq(2,10,1), 
                                labels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), limits=c(1.5,10.5)) +
            labs(y="Drug Use (%)", 
                 x="Month") +
            facet_grid(. ~ Invasive_Ventilation)

    plot(gg)
    return(df)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.25aed264-58b9-4792-977e-2289f3e7b2a6"),
    dex_steroid_2=Input(rid="ri.foundry.main.dataset.7718f5fb-9d77-4ba4-bc5c-06efb2763884"),
    steroid_2=Input(rid="ri.foundry.main.dataset.26097278-1913-4e5c-96a9-b7fef322f787"),
    tot_2=Input(rid="ri.foundry.main.dataset.bdede965-69dd-4467-aada-b789254860d8")
)
dex_steroid_vent_2day <- function(tot_2, dex_steroid_2, steroid_2) {

    library(dplyr)
    
    dex <- dex_steroid_2 %>%
          filter(Invasive_Ventilation==1) %>%
          select(person_id, month) %>%
          group_by(month) %>%
          summarize(dex_patient_number = n()) 

    no_dex_steroid <- steroid_2 %>%
          filter(Invasive_Ventilation==1) %>%
          select(person_id, month) %>%
          group_by(month) %>%
          summarize(steroid_patient_number = n()) 

    tot <- tot_2 %>%
          filter(Invasive_Ventilation==1) %>%
          select(person_id, month) %>%
          group_by(month) %>%
          summarize(total_patient_number = n())

    df2 <- tot %>%
            left_join(dex, by = "month") %>%
            left_join(no_dex_steroid, by = "month") %>%
            mutate(dex_patient_number = ifelse(is.na(dex_patient_number), 0, dex_patient_number),
                   steroid_patient_number = ifelse(is.na(steroid_patient_number), 0, steroid_patient_number)) %>%
            mutate(c_dex_percent = round(dex_patient_number/total_patient_number, 4),
                    b_steroid_percent = round(steroid_patient_number/total_patient_number, 4),
                    a_none_percent = round(1-c_dex_percent-b_steroid_percent, 4)) %>%
            filter(!month %in% c(2,3,4,5,6,11,12)) 

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.2938ca02-44be-468d-b704-ac1901df804d"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
diabetes_dex <- function(Covariate_with_steroid) {
    df <- Covariate_with_steroid %>%
        select(person_id, diabetes, dmcx, DEX, Invasive_Ventilation) %>%
        mutate(dia = ifelse(diabetes==1|dmcx==1,1,0)) %>%
        filter(Invasive_Ventilation==0)
    return(
        data.frame(
            table(
                df$dia, df$DEX
            )
        )
    )
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4f93bad5-e718-44c5-907b-64ff01c0c0f2"),
    Covariate_with_death=Input(rid="ri.foundry.main.dataset.1bf034bc-9dfd-4f6c-9c3e-3861f3fda9cc")
)
# Sensitivity analysis – exclude pts who dies/discharged within 2 days – cross-tab of DEX*ventilation
exclude_pts_less_2_days <- function(Covariate_with_death) {
    library(dplyr)
    df <- Covariate_with_death %>%
        mutate(diff1 = as.numeric(death_date)-as.numeric(covid_admission), 
               diff2 = as.numeric(covid_discharge)-as.numeric(covid_admission),
               diff = pmin(diff1, diff2, na.rm=T)) %>%
        filter(diff > 2)

    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
rem_and_dex <- function(covariate) {
    library(dplyr)
    library(lubridate)
    df <- covariate %>%
        mutate(
            rem_dex = ifelse(drug_dex_v1==1&drug_rem_v1==1&month(dex_start_date)==month(rem_start_date),1,0),
            time_to_dex_use = as.numeric(dex_start_date - covid_admission),
            dex_duration = ifelse(is.na(dex_end_date)|dex_end_date<dex_start_date, 0,as.numeric(dex_end_date-dex_start_date))
            )
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6ff52adb-31a1-4055-ba08-0455d7738b10"),
    rem_pt_month=Input(rid="ri.foundry.main.dataset.e64dd515-edb1-4a6c-97c5-feba91f4236d"),
    tot_pt_month=Input(rid="ri.foundry.main.dataset.a4032b12-8ffa-4d12-8d1c-5e062c65b4c4")
)
rem_month_race_percent <- function(rem_pt_month, tot_pt_month) {

    library(dplyr)
    
    df <- rem_pt_month %>%
          select(person_id, month, race_eth) %>%
          group_by(month, race_eth) %>%
          summarize(rem_patient_number = n()) 

    df1 <- tot_pt_month %>%
          select(person_id, month, race_eth) %>%
          group_by(month, race_eth) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = c("month", "race_eth"), all = TRUE) %>%
            mutate(rem_percent = round(rem_patient_number/total_patient_number, 4)) %>%
            filter(!month %in% c(2,11,12)) %>%
            filter(race_eth %in% c("1_NH_WHITE", "2_NH_BLACK", "3_ASIAN", "5_HISPANIC"))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e64dd515-edb1-4a6c-97c5-feba91f4236d"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
rem_pt_month <- function(covariate) {
    library(dplyr)
    library(lubridate)

    df <- covariate %>%
        filter(drug_rem_v1==1) %>%
        select(person_id, rem_start_date, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(month = month(rem_start_date))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6a6427f8-c111-4b14-8153-8a86db47b60c"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
steroid <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_with_steroid %>%
        filter(is.na(drug_dex_v1)) %>%
        filter(Steroid_flag==1) %>%
        select(person_id, steroid_start_date, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month = month(steroid_start_date))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.26097278-1913-4e5c-96a9-b7fef322f787"),
    exclude_pts_less_2_days=Input(rid="ri.foundry.main.dataset.4f93bad5-e718-44c5-907b-64ff01c0c0f2")
)
steroid_2 <- function(exclude_pts_less_2_days) {
    library(dplyr)
    library(lubridate)

    df <- exclude_pts_less_2_days %>%
        filter(is.na(drug_dex_v1)) %>%
        filter(Steroid_flag==1) %>%
        select(person_id, steroid_start_date, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month = month(steroid_start_date))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9688b559-6f55-4b31-9984-77aca21d9540"),
    Covriate_steroid_spo2=Input(rid="ri.foundry.main.dataset.7c49440e-8919-4688-9177-e7323ae353a0")
)
steroid_3 <- function(Covriate_steroid_spo2) {
    library(dplyr)
    library(lubridate)

    df <- Covriate_steroid_spo2 %>%
        filter(is.na(drug_dex_v1)) %>%
        filter(Steroid_flag==1) %>%
        select(person_id, steroid_start_date, Invasive_Ventilation, SPO2_less_than_93) %>%
        mutate(month = month(steroid_start_date)) %>%
        mutate(SPO2 = case_when(
                            SPO2_less_than_93 == 1 ~ "1_SPO2_less_than_93",
                            SPO2_less_than_93 == 0 ~ "2_SPO2_greater_or_equal_to_93",
                            is.na(SPO2_less_than_93) ~ "3_NA"))
    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.dce445c9-daa0-4a88-8fd2-4c5d58db7a7f"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
time_to_first_use_dex <- function(rem_and_dex) {
    library(ggplot2)
    library(grid)
    library(gridExtra)
    library(scales)
    library(dplyr)

    df <- rem_and_dex %>%
        filter(drug_dex_v1==1) %>%
        mutate(Invasive_Ventilation = ifelse(Invasive_Ventilation==1, "Ventilated", "Not Ventilated"))
    
    gg <- ggplot(df, aes(x=time_to_dex_use)) +
          geom_histogram(aes(y = ..count..), binwidth = 1,fill="#1b9e77", color="white") +
          theme_bw() +
          labs(x="Time to First Dexamethasone Use (Days)", y="Number of Patients Hospitalized with COVID-19\nfrom February to October") +
          theme(axis.text.x = element_text(hjust=0.9, size=14),
                axis.text.y = element_text(size=15),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                strip.text.x = element_text(size = 16)) +
          scale_x_continuous(limit=c(-1,25)) +
          scale_y_continuous(breaks=seq(0,6000,500), limit=c(0,6000), labels=comma_format()) +
          coord_cartesian(clip = "off") +
          facet_grid(. ~ Invasive_Ventilation)

    table <- df %>% 
                rename(`Invasive Ventilation` = Invasive_Ventilation) %>%
                group_by(`Invasive Ventilation`) %>% 
                summarize(
                    `25 percentile` = quantile(time_to_dex_use, probs = 0.25),
                    median = median(time_to_dex_use),
                    mean = round(mean(time_to_dex_use),1),
                    SD = round(sd(time_to_dex_use),1),
                    `75 percentile` = quantile(time_to_dex_use, probs = 0.75)
                )

    mytheme <- gridExtra::ttheme_minimal(
                core = list(fg_params=list(cex = 1.1)),
                colhead = list(fg_params=list(cex = 1.1)))
    tbl <- tableGrob(table, theme = mytheme, rows = NULL)

    p <- grid.arrange(gg,
             tbl,
             nrow = 2,
             as.table = TRUE, 
             heights = c(11, 1))
    p1 <- cowplot::ggdraw(p) + 
        theme(plot.background = element_rect(fill="white", color=NA))
    
    plot(p1)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.bdede965-69dd-4467-aada-b789254860d8"),
    exclude_pts_less_2_days=Input(rid="ri.foundry.main.dataset.4f93bad5-e718-44c5-907b-64ff01c0c0f2")
)
tot_2 <- function(exclude_pts_less_2_days) {
    library(dplyr)
    library(lubridate)
    tot <- exclude_pts_less_2_days %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month = month(covid_admission)) 
    return(tot)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.11ae13b5-0dae-4681-b8d9-fefa04a6ae4b"),
    Covriate_steroid_spo2=Input(rid="ri.foundry.main.dataset.7c49440e-8919-4688-9177-e7323ae353a0")
)
tot_3 <- function(Covriate_steroid_spo2) {
    library(dplyr)
    library(lubridate)
    tot <- Covriate_steroid_spo2 %>% 
        select(person_id, covid_admission, Invasive_Ventilation, Steroid_flag, SPO2_less_than_93) %>%
        mutate(month = month(covid_admission)) %>%
        mutate(SPO2 = case_when(
                            SPO2_less_than_93 == 1 ~ "1_SPO2_less_than_93",
                            SPO2_less_than_93 == 0 ~ "2_SPO2_greater_or_equal_to_93",
                            is.na(SPO2_less_than_93) ~ "3_NA"))
    return(tot)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a4032b12-8ffa-4d12-8d1c-5e062c65b4c4"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
tot_pt_month <- function(covariate) {
    library(dplyr)
    library(lubridate)
    df <- covariate %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(month = month(covid_admission)) 
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.256b783d-8b33-434a-9842-1511759a7cf0"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
tot_steroid <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)
    tot <- Covariate_with_steroid %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month = month(covid_admission)) 
    return(tot)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.b30c1cb7-8be4-450f-8271-5eb41beafb65"),
    dex_month_race_percent=Input(rid="ri.foundry.main.dataset.3d745ff6-45cd-438d-9224-e33db5aeed36")
)
trend_dex_race <- function(dex_month_race_percent) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(dex_month_race_percent, aes(x=month, y=dex_percent)) +
            geom_line(aes(color=factor(race_eth)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(race_eth), shape=factor(race_eth)), size = 2, stroke = 1.8, fill = "white", show.legend = TRUE) +
            
            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=13, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.88),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.8, "cm"),
                  legend.key.size = unit(15, "points")) +
            scale_x_continuous(breaks=seq(2,10,1), labels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), limits=c(2,10)) +
            scale_y_continuous(breaks=seq(0,0.5,0.1),limits=c(0,0.5), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Dexamethasone (%)", 
                 x="Time (Months)") +
            scale_colour_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK", "3_HISPANIC", "4_ASIAN"), 
                                labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian"), 
                                values = c('#1b9e77','#7570b3','#d95f02', "#e7298a"),
                                name="Race/Ethnicity") +
            scale_shape_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK", "3_HISPANIC", "4_ASIAN"), 
                                labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian"), 
                                values = c(21, 22, 24, 23),
                                name="Race/Ethnicity") 
    
    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.63d0c25a-777b-4483-a969-1e097ded61dd"),
    dex_month_vent_percent=Input(rid="ri.foundry.main.dataset.09724649-f4bf-493e-8d9d-483ac3a0fa3c")
)
trend_dex_vent <- function(dex_month_vent_percent) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(dex_month_vent_percent, aes(x=month, y=dex_percent)) +
            geom_line(aes(color=factor(Invasive_Ventilation)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(Invasive_Ventilation), shape=factor(Invasive_Ventilation)), size = 4, stroke = 1.8, fill = "white", show.legend = TRUE) +
            
            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=13, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.88),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.8, "cm"),
                  legend.key.size = unit(15, "points")) +
            scale_x_continuous(breaks=seq(2,10,1), labels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), limits=c(2,10)) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Dexamethasone (%)", 
                 x="Time (Months)") +
            scale_colour_manual(breaks = c("1", "0"), 
                                labels = c("Ventilation", "No Ventilation"), 
                                values = c('#1b9e77','#7570b3'),
                                name="Invasive Ventilation") +
            scale_shape_manual(breaks = c("1", "0"), 
                                labels = c("Ventilation", "No Ventilation"), 
                                values = c(21, 22),
                                name="Invasive Ventilation") 
    
    plot(gg)
    return(NULL)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.a3d7e887-fcb8-488e-8367-54328c685f01"),
    rem_month_race_percent=Input(rid="ri.foundry.main.dataset.6ff52adb-31a1-4055-ba08-0455d7738b10")
)
trend_rem_race <- function(rem_month_race_percent) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(rem_month_race_percent, aes(x=month, y=rem_percent)) +
            geom_line(aes(color=factor(race_eth)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(race_eth), shape=factor(race_eth)), size = 2, stroke = 1.8, fill = "white", show.legend = TRUE) +
            
            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=13, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.88),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.8, "cm"),
                  legend.key.size = unit(15, "points")) +
            scale_x_continuous(breaks=seq(3,10,1), labels=c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), limits=c(3,10)) +
            scale_y_continuous(breaks=seq(0,0.3,0.1),limits=c(0,0.3), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Remdesivir (%)", 
                 x="Time (Months)") +
            scale_colour_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK", "3_ASIAN", "5_HISPANIC"), 
                                labels = c("Non-Hispanic White", "Non-Hispanic Black", "Asian", "Hispanics"), 
                                values = c('#1b9e77','#7570b3','#d95f02', '#e7298a'),
                                name="Race/Ethnicity") +
            scale_shape_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK", "3_ASIAN", "5_HISPANIC"), 
                                labels = c("Non-Hispanic White", "Non-Hispanic Black", "Asian", "Hispanics"), 
                                values = c(21, 22, 24, 23),
                                name="Race/Ethnicity") 
    
    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.8ec23dff-8f96-4ccc-b1e7-33c5e69ff347"),
    dex_rem_month_percent=Input(rid="ri.vector.main.execute.22f2ce16-4652-4873-ad0b-887b0eb6544c")
)
unnamed <- function(dex_rem_month_percent) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(dex_rem_month_percent, aes(x=month, y=dex_rem_percent)) +
            geom_line(size = 1.5, show.legend = FALSE, color = "#1b9e77") +
            geom_point(size = 4, stroke = 1.8, fill = "white", show.legend = TRUE, color = "#1b9e77", shape = 21) +
            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3))) +
            scale_x_continuous(breaks=seq(2,10,1), labels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"), limits=c(2,10)) +
            scale_y_continuous(breaks=seq(0,0.3,0.05),limits=c(0,0.3), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized with COVID-19\nReceivingBoth Dexamethasone and Remdesivir (%)", 
                 x="Time (Months)")
    plot(gg)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.90d16bc4-cb90-4b7f-bbfb-8895b385a6d5"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
unnamed_1 <- function(rem_and_dex) {
    df <- rem_and_dex %>%
        filter(drug_dex_v1==1) %>%
        mutate(month = month(dex_start_date)) %>%
        group_by(month) %>%
        summarize(dex_duration_mean = round(mean(dex_duration, na.rm=T),1), time_to_dex_use_mean = round(mean(time_to_dex_use, na.rm=T),1),
                  dex_duration_median = median(dex_duration, na.rm=T), time_to_dex_use_median = median(time_to_dex_use, na.rm=T),
                  dex_duration_twentyfive = quantile(dex_duration, probs = 0.25), time_to_dex_use_twentyfive = quantile(time_to_dex_use, probs = 0.25),
                  dex_duration_seventyfive = quantile(dex_duration, probs = 0.75), time_to_dex_use_seventyfive = quantile(dex_duration, probs = 0.75),
                  dex_duration_sd = round(sd(dex_duration, na.rm=T),1), time_to_dex_use_sd = round(sd(time_to_dex_use, na.rm=T),1))%>%
        mutate(dex_duration_mean_sd = paste(dex_duration_mean, " (", dex_duration_sd, ")", sep=""),
               dex_duration_median_IQR = paste(dex_duration_median, " (", dex_duration_twentyfive, ", ", dex_duration_seventyfive, ")", sep=""), 
               time_to_dex_use_mean_sd = paste(time_to_dex_use_mean, " (", time_to_dex_use_sd, ")", sep=""),
               time_to_dex_use_median_IQR = paste(time_to_dex_use_median, " (", time_to_dex_use_twentyfive, ", ", time_to_dex_use_seventyfive, ")", sep=""))
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9a3fc608-a663-42ec-b9c0-590961c2ffe9"),
    dex_steroid_vent=Input(rid="ri.foundry.main.dataset.49275e23-fcde-489c-9b11-5f7ba6fe77f6")
)
unnamed_10 <- function(dex_steroid_vent) {
    library(tidyverse) 
    library(dplyr)
    library(scales)

    df <- dex_steroid_vent %>%
            select(month, Invasive_Ventilation, c_dex_percent, b_steroid_percent, a_none_percent) %>%
            pivot_longer(c_dex_percent:a_none_percent, names_to = "drug_type", values_to = "percent") %>%
            filter(Invasive_Ventilation==1)
    

    gg <- ggplot(df, aes(fill=drug_type, y=percent, x=month)) + 
            geom_bar(position="fill", stat="identity") +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            theme_bw() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.key.size = unit(16, "points"),
                  legend.position="top",
                  strip.text.x = element_text(size = 16)) +
            scale_fill_manual(breaks = c("c_dex_percent", "b_steroid_percent", "a_none_percent"), 
                              labels = c("Dexamethasone", "Steroid", "None"), 
                              values = c('#1b9e77','#7570b3','#d95f02'),
                              name="Drug Use") +
            
            scale_x_continuous(breaks=seq(7,10,1), 
                                labels=c("Jul", "Aug", "Sep", "Oct"), limits=c(6.5,10.5)) +
            labs(y="Drug Use (%)", 
                 x="Month") 

    plot(gg)
    return(df)

    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.8128e7b6-9888-4ed0-a9d9-193afb4d808d"),
    dex_steroid_o2=Input(rid="ri.foundry.main.dataset.ad0d68ff-c685-4a34-b6f8-3093a09800c1")
)
unnamed_11 <- function(dex_steroid_o2) {
    library(tidyverse) 
    library(dplyr)
    library(scales)

    df <- dex_steroid_o2 %>%
            select(month, c_dex_percent, b_steroid_percent, a_none_percent, SPO2) %>%
            pivot_longer(c_dex_percent:a_none_percent, names_to = "drug_type", values_to = "percent") %>%
        mutate(SPO2 = case_when(
                            SPO2 == "1_SPO2_less_than_93" ~ "SPO2 < 93",
                            SPO2 == "2_SPO2_greater_or_equal_to_93" ~ "SPO2 >= 93",
                            SPO2 == "3_NA" ~ "Missing"))

    gg <- ggplot(df, aes(fill=drug_type, y=percent, x=month)) + 
            geom_bar(position="fill", stat="identity") +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            theme_bw() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.key.size = unit(16, "points"),
                  legend.position="top",
                  strip.text.x = element_text(size = 16)) +
            scale_fill_manual(breaks = c("c_dex_percent", "b_steroid_percent", "a_none_percent"), 
                              labels = c("Dexamethasone", "Steroid", "None"), 
                              values = c('#1b9e77','#7570b3','#d95f02'),
                              name="Drug Use") +
            
            scale_x_continuous(breaks=seq(7,10,1), 
                                labels=c("Jul", "Aug", "Sep", "Oct"), limits=c(6.5,10.5)) +
            labs(y="Drug Use (%)", 
                 x="Month") +
            facet_grid(. ~ SPO2)

    plot(gg)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cfde7233-8212-4394-a87d-8a2d751dd44a"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
unnamed_2 <- function(rem_and_dex) {
    df <- rem_and_dex %>%
        filter(drug_dex_v1==1) %>%
        group_by(race_eth) %>%
        summarize(dex_duration_mean = round(mean(dex_duration, na.rm=T),1), time_to_dex_use_mean = round(mean(time_to_dex_use, na.rm=T),1),
                  dex_duration_median = median(dex_duration, na.rm=T), time_to_dex_use_median = median(time_to_dex_use, na.rm=T),
                  dex_duration_twentyfive = quantile(dex_duration, probs = 0.25), time_to_dex_use_twentyfive = quantile(time_to_dex_use, probs = 0.25),
                  dex_duration_seventyfive = quantile(dex_duration, probs = 0.75), time_to_dex_use_seventyfive = quantile(dex_duration, probs = 0.75),
                  dex_duration_sd = round(sd(dex_duration, na.rm=T),1), time_to_dex_use_sd = round(sd(time_to_dex_use, na.rm=T),1)) %>%
        mutate(dex_duration_mean_sd = paste(dex_duration_mean, " (", dex_duration_sd, ")", sep=""),
               dex_duration_median_IQR = paste(dex_duration_median, " (", dex_duration_twentyfive, ", ", dex_duration_seventyfive, ")", sep=""), 
               time_to_dex_use_mean_sd = paste(time_to_dex_use_mean, " (", time_to_dex_use_sd, ")", sep=""),
               time_to_dex_use_median_IQR = paste(time_to_dex_use_median, " (", time_to_dex_use_twentyfive, ", ", time_to_dex_use_seventyfive, ")", sep=""))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.398fcfad-e242-45a3-8a43-b05baced5b64"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
library(dplyr)

unnamed_3 <- function(rem_and_dex) {
    df <- rem_and_dex %>%
        filter(drug_dex_v1==1) %>%
        group_by(data_partner_id) %>%
        summarize(dex_duration_mean = round(mean(dex_duration, na.rm=T),1), time_to_dex_use_mean = round(mean(time_to_dex_use, na.rm=T),1),
                  dex_duration_median = median(dex_duration, na.rm=T), time_to_dex_use_median = median(time_to_dex_use, na.rm=T),
                  dex_duration_twentyfive = quantile(dex_duration, probs = 0.25), time_to_dex_use_twentyfive = quantile(time_to_dex_use, probs = 0.25),
                  dex_duration_seventyfive = quantile(dex_duration, probs = 0.75), time_to_dex_use_seventyfive = quantile(dex_duration, probs = 0.75),
                  dex_duration_sd = round(sd(dex_duration, na.rm=T),1), time_to_dex_use_sd = round(sd(time_to_dex_use, na.rm=T),1)) %>%
        mutate(dex_duration_mean_sd = paste(dex_duration_mean, " (", dex_duration_sd, ")", sep=""),
               dex_duration_median_IQR = paste(dex_duration_median, " (", dex_duration_twentyfive, ", ", dex_duration_seventyfive, ")", sep=""), 
               time_to_dex_use_mean_sd = paste(time_to_dex_use_mean, " (", time_to_dex_use_sd, ")", sep=""),
               time_to_dex_use_median_IQR = paste(time_to_dex_use_median, " (", time_to_dex_use_twentyfive, ", ", time_to_dex_use_seventyfive, ")", sep=""))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.109d7ed0-a7fe-4ae4-a5a0-9e96e7dcaf96"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
unnamed_4 <- function(rem_and_dex) {
    
    library(ggplot2)
    library(scales)
    library(dplyr)
    df <- rem_and_dex %>%
            filter(drug_dex_v1==1)

    gg <- ggplot(df, aes(x=as.factor(data_partner_id), y=time_to_dex_use, group=as.factor(data_partner_id))) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(fill='#1b9e77') + 
            theme(axis.text.x = element_text(size = rel(1.5)),
                  axis.text.y = element_text(size = rel(1.5)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50"),
                  legend.position = "none") +
            labs(y="Time to first Dexamethasone Use\nin Individuals Hospitalized with COVID-19 (days)",x="Center") +
            scale_x_discrete(labels=seq(1,32,1))
        
        plot(gg)
        return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.a35ab947-844e-437e-b17d-7f48321375f2"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
unnamed_5 <- function(rem_and_dex) {
    
    library(ggplot2)
    library(scales)
    library(dplyr)
    df <- rem_and_dex %>%
            filter(drug_dex_v1==1)

    gg <- ggplot(df, aes(x=as.factor(data_partner_id), y=dex_duration, group=as.factor(data_partner_id))) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(fill='#1b9e77') + 
            theme(axis.text.x = element_text(size = rel(1.5)),
                  axis.text.y = element_text(size = rel(1.5)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50"),
                  legend.position = "none") +
            labs(y="Dexamethasone Duration\nin Individuals Hospitalized with COVID-19 (days)",x="Center") +
            scale_x_discrete(labels=seq(1,32,1))
        
        plot(gg)
        return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.92f716d0-27a5-458a-b81e-adc0919931ea"),
    Dex_pt_month=Input(rid="ri.foundry.main.dataset.fcb62258-235a-42b6-b6c4-3b6678f07b88"),
    tot_pt_month=Input(rid="ri.foundry.main.dataset.a4032b12-8ffa-4d12-8d1c-5e062c65b4c4")
)
unnamed_6 <- function(Dex_pt_month, tot_pt_month) {

    library(dplyr)
    
    df <- Dex_pt_month %>%
          select(person_id, month, data_partner_id) %>%
          group_by(month, data_partner_id) %>%
          summarize(rem_patient_number = n()) 

    df1 <- tot_pt_month %>%
          select(person_id, month, data_partner_id) %>%
          group_by(month, data_partner_id) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = c("month", "data_partner_id"), all = TRUE) %>%
            mutate(rem_percent = round(rem_patient_number/total_patient_number, 4)) %>%
            filter(!month %in% c(2,11,12))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.832234e2-1966-443f-89ec-852f1c7e4d68"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
unnamed_7 <- function(rem_and_dex) {

    df <- rem_and_dex %>%
        filter(drug_dex_v1==1) %>%
        filter(race_eth %in% c("1_NH_WHITE", "2_NH_BLACK", "3_ASIAN", "5_HISPANIC")) %>%
        mutate(race_eth = case_when(
                            race_eth == "1_NH_WHITE" ~ "1_NH_WHITE",
                            race_eth == "2_NH_BLACK" ~ "2_NH_BLACK",
                            race_eth == "3_ASIAN" ~ "4_ASIAN",
                            race_eth == "5_HISPANIC" ~ "3_HISPANIC")) 
        
    gg <- ggplot(df, aes(x=race_eth, y=time_to_dex_use)) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(aes(fill = factor(race_eth))) +
            theme_bw() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=13, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.88),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.8, "cm"),
                  legend.key.size = unit(15, "points")) +
            scale_x_discrete(labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian")) +
            scale_y_continuous(breaks=seq(0,20,5),limits=c(0,20)) +
            labs(y="Time to First Dexamethasone Use\nin Individuals Hospitalized with COVID-19 (days)", 
                 x="Race/Ethnicity") +
            scale_fill_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK", "3_HISPANIC", "4_ASIAN"), 
                                labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian"), 
                                values = c('#1b9e77','#7570b3','#d95f02', "#e7298a"),
                                name="Race/Ethnicity")
    plot(gg)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.f4272f3b-9e64-409c-a081-fdf941e7b516"),
    dex_steroid_vent_2day=Input(rid="ri.foundry.main.dataset.25aed264-58b9-4792-977e-2289f3e7b2a6")
)
unnamed_8 <- function(dex_steroid_vent_2day) {
    library(tidyverse) 
    library(dplyr)
    library(scales)

    df <- dex_steroid_vent_2day %>%
            select(month, c_dex_percent, b_steroid_percent, a_none_percent) %>%
            pivot_longer(c_dex_percent:a_none_percent, names_to = "drug_type", values_to = "percent") 
    

    gg <- ggplot(df, aes(fill=drug_type, y=percent, x=month)) + 
            geom_bar(position="fill", stat="identity") +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            theme_bw() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.key.size = unit(16, "points"),
                  legend.position="top",
                  strip.text.x = element_text(size = 16)) +
            scale_fill_manual(breaks = c("c_dex_percent", "b_steroid_percent", "a_none_percent"), 
                              labels = c("Dexamethasone", "Steroid", "None"), 
                              values = c('#1b9e77','#7570b3','#d95f02'),
                              name="Drug Use") +
            
            scale_x_continuous(breaks=seq(7,10,1), 
                                labels=c("Jul", "Aug", "Sep", "Oct"), limits=c(6.5,10.5)) +
            labs(y="Drug Use (%)", 
                 x="Month") 

    plot(gg)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.46d04cca-d2fa-4765-be39-cdb4e94ea756"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.9f388cd5-eeb3-4498-ba96-0d9d8471838c")
)
unnamed_9 <- function(rem_and_dex) {

    df <- rem_and_dex %>%
        filter(drug_dex_v1==1) %>%
        filter(race_eth %in% c("1_NH_WHITE", "2_NH_BLACK", "3_ASIAN", "5_HISPANIC")) %>%
        mutate(race_eth = case_when(
                            race_eth == "1_NH_WHITE" ~ "1_NH_WHITE",
                            race_eth == "2_NH_BLACK" ~ "2_NH_BLACK",
                            race_eth == "3_ASIAN" ~ "4_ASIAN",
                            race_eth == "5_HISPANIC" ~ "3_HISPANIC")) 
        
    gg <- ggplot(df, aes(x=race_eth, y=dex_duration)) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(aes(fill = factor(race_eth))) +
            theme_bw() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=13, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.88),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.8, "cm"),
                  legend.key.size = unit(15, "points")) +
            scale_x_discrete(labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian")) +
            scale_y_continuous(breaks=seq(0,50,10),limits=c(0,50)) +
            labs(y="Dexamethasone Duration\nin Individuals Hospitalized with COVID-19 (days)", 
                 x="Race/Ethnicity") +
            scale_fill_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK", "3_HISPANIC", "4_ASIAN"), 
                                labels = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian"), 
                                values = c('#1b9e77','#7570b3','#d95f02', "#e7298a"),
                                name="Race/Ethnicity")
    plot(gg)
    return(NULL)
    
}

