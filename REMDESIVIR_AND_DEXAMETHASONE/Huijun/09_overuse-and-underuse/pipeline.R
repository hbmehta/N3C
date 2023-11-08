

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e2f63323-efa3-4291-aab8-30a6d765fc09"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
dex <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_with_steroid %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(month = month(dex_start_date)) %>%
        filter(month %in% c(7,8,9,10))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.51e0ccc3-b505-4087-8718-16a8bad4f290"),
    overuse_in_non_vent=Input(rid="ri.foundry.main.dataset.3850cc39-4a7b-49c8-b4ff-ee33eec46cf3"),
    underuse_in_vent=Input(rid="ri.foundry.main.dataset.a0e24906-2cec-4b32-992d-a7f135297ed9")
)
dot_plot <- function(overuse_in_non_vent, underuse_in_vent) {
    library(dplyr)
    df <- underuse_in_vent %>%
            select(data_partner_id, dex_percent) %>%
            rename(dex_percent_vent = dex_percent) %>%
            left_join(overuse_in_non_vent %>% select(data_partner_id, dex_percent) %>% rename(dex_percent_no_vent=dex_percent), by="data_partner_id")
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.7fe20b2c-993b-4e0a-b1f2-c142ef9675ac"),
    dot_plot=Input(rid="ri.vector.main.execute.51e0ccc3-b505-4087-8718-16a8bad4f290")
)
dot_plot_2 <- function(dot_plot) {
    library(ggplot2)
    
    p <- ggplot(dot_plot, aes(dex_percent_vent, dex_percent_no_vent)) + 
            geom_point(size = 4, stroke = 1.8, fill = "white", color='#1b9e77', shape=21) +
            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.position="none") +
            xlab("Percentage of dexamethasone use in Ventilators") + 
            ylab("Percentage of dexamethasone use in Non-Ventilators") + 
            scale_y_continuous(labels=scales::percent)+ 
            scale_x_continuous(labels=scales::percent) +
            geom_hline(yintercept=0.3708, linetype="dashed", color = '#7570b3', size=1) +
            geom_vline(xintercept=0.7778, linetype="dashed", color = '#7570b3', size=1)
    plot(p)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3850cc39-4a7b-49c8-b4ff-ee33eec46cf3"),
    dex=Input(rid="ri.foundry.main.dataset.e2f63323-efa3-4291-aab8-30a6d765fc09"),
    tot=Input(rid="ri.foundry.main.dataset.18c2c13b-7d48-470a-9ef5-ee82f5626009")
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
    Output(rid="ri.vector.main.execute.41e3ea2a-f88c-46e8-b5c1-a20a95943c1c"),
    overuse_in_non_vent=Input(rid="ri.foundry.main.dataset.3850cc39-4a7b-49c8-b4ff-ee33eec46cf3")
)
overuse_in_non_vent_fig <- function(overuse_in_non_vent) {
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
            scale_x_continuous(breaks=c(0,5,10,15,20,25,30,33)) +
            geom_hline(yintercept=0.3708, linetype="dashed", color = '#7570b3', size=1)
    plot(p)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.18c2c13b-7d48-470a-9ef5-ee82f5626009"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
tot <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)
    tot <- Covariate_with_steroid %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation) %>%
        mutate(month = month(covid_admission)) %>%
        filter(month %in% c(7,8,9,10))
    return(tot)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a0e24906-2cec-4b32-992d-a7f135297ed9"),
    dex=Input(rid="ri.foundry.main.dataset.e2f63323-efa3-4291-aab8-30a6d765fc09"),
    tot=Input(rid="ri.foundry.main.dataset.18c2c13b-7d48-470a-9ef5-ee82f5626009")
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
    Output(rid="ri.vector.main.execute.9bb874d7-e8e4-4a6d-be61-ae9c844c2dec"),
    underuse_in_vent=Input(rid="ri.foundry.main.dataset.a0e24906-2cec-4b32-992d-a7f135297ed9")
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
            scale_x_continuous(breaks=c(0,5,10,15,20,25,27)) +
            geom_hline(yintercept=0.7778, linetype="dashed", color = '#7570b3', size=1)
    plot(p)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e7bf8d10-93ff-4e71-bc48-ef6c3470c84f"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
unnamed <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)
    df <- Covariate_with_steroid %>%
        mutate(month=month(covid_admission)) %>%
        filter(month %in% c(7,8,9,10))
    return(df)
}

