

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5ccb8aa4-5bb7-41c5-85ed-373d695a2522"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
dex <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_01 %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(month_year = format(as.Date(dex_start_date), "%Y-%m")) %>%
        filter(month_year >= "2020-07")
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.07571352-79c1-4eec-a8ae-38defedb57d8"),
    overuse_in_non_vent=Input(rid="ri.foundry.main.dataset.ff3a0539-95b0-4a99-988a-53d71cc976f5")
)
fig3_overuse_in_non_vent <- function(overuse_in_non_vent) {
    library(ggplot2)
    
    p <- ggplot(overuse_in_non_vent, aes(row_num, dex_percent)) + 
            geom_line(color='#d95f02', size = 1.5) +
            geom_point(size = 4, stroke = 1.8, fill = "white", color='#d95f02', shape=21) +
            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.position="none") +
            xlab("Site Rank") + 
            ylab("Percentage of dexamethasone use") + 
            scale_y_continuous(breaks=seq(0,1,0.25),labels=scales::percent, limit=c(0,1)) + 
            scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40,43)) +
            geom_hline(yintercept=0.4491, linetype="dashed", color = '#7570b3', size=1)
    plot(p)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.6774cde9-1a52-47fb-ae21-42442ebb1ed2"),
    overuse_in_non_vent=Input(rid="ri.foundry.main.dataset.ff3a0539-95b0-4a99-988a-53d71cc976f5")
)
median_non_vent <- function(overuse_in_non_vent) {
    return(
        data.frame(
            median(overuse_in_non_vent$dex_percent)
        )
    )
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.60a47e03-4554-4030-a2d6-fc6dc6e11370"),
    underuse_in_vent=Input(rid="ri.foundry.main.dataset.62b4f3c1-beac-4407-8009-dc7439b11b2f")
)
median_vent <- function(underuse_in_vent) {
    return(
        data.frame(
            median(underuse_in_vent$dex_percent)
        )
    )
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ff3a0539-95b0-4a99-988a-53d71cc976f5"),
    dex=Input(rid="ri.foundry.main.dataset.5ccb8aa4-5bb7-41c5-85ed-373d695a2522"),
    tot=Input(rid="ri.foundry.main.dataset.68411cf3-8cba-44bf-9630-f05bd56142e0")
)
overuse_in_non_vent <- function(tot, dex) {

    library(dplyr)
    
    dex_2 <- dex %>%
          filter(Invasive_Ventilation==0) %>%
          group_by(data_partner_id) %>%
          summarize(dex_patient_number = n()) 

    tot_2 <- tot %>%
          filter(Invasive_Ventilation==0) %>%
          group_by(data_partner_id) %>%
          summarize(total_patient_number = n())

    df2 <- tot_2 %>%
            left_join(dex_2, by = c("data_partner_id")) %>%
            mutate(dex_patient_number=ifelse(is.na(dex_patient_number),0,dex_patient_number)) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4)) %>%
            arrange(dex_percent) %>%
            mutate(row_num = as.numeric(rownames(tot_2)))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.68411cf3-8cba-44bf-9630-f05bd56142e0"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
tot <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    tot <- Covariate_01 %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation) %>%
        mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) %>%
        filter(month_year >= "2020-07")
    return(tot)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.62b4f3c1-beac-4407-8009-dc7439b11b2f"),
    dex=Input(rid="ri.foundry.main.dataset.5ccb8aa4-5bb7-41c5-85ed-373d695a2522"),
    tot=Input(rid="ri.foundry.main.dataset.68411cf3-8cba-44bf-9630-f05bd56142e0")
)
underuse_in_vent <- function(tot, dex) {

    library(dplyr)
    
    dex_2 <- dex %>%
          filter(Invasive_Ventilation==1) %>%
          group_by(data_partner_id) %>%
          summarize(dex_patient_number = n()) 

    tot_2 <- tot %>%
          filter(Invasive_Ventilation==1) %>%
          group_by(data_partner_id) %>%
          summarize(total_patient_number = n())

    df2 <- tot_2 %>%
            left_join(dex_2, by = c("data_partner_id")) %>%
            mutate(dex_patient_number=ifelse(is.na(dex_patient_number),0,dex_patient_number)) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4)) %>%
            arrange(dex_percent) %>%
            mutate(row_num = as.numeric(rownames(tot_2)))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.6fee33a7-60a2-48a4-ac9b-814a8f302221"),
    underuse_in_vent=Input(rid="ri.foundry.main.dataset.62b4f3c1-beac-4407-8009-dc7439b11b2f")
)
underuse_in_vent_fig <- function(underuse_in_vent) {
    library(ggplot2)
    
    p <- ggplot(underuse_in_vent, aes(row_num, dex_percent)) + 
            geom_line(color='#d95f02', size = 1.5) +
            geom_point(size = 4, stroke = 1.8, fill = "white", color='#d95f02', shape=21) +
            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.position="none") +
            xlab("Site Rank") + 
            ylab("Percentage of dexamethasone use") + 
            scale_y_continuous(labels=scales::percent)+ 
            scale_x_continuous(breaks=c(0,5,10,15,20,25,30,35,40)) +
            geom_hline(yintercept=0.797, linetype="dashed", color = '#7570b3', size=1)
    plot(p)
    return(NULL)
}

