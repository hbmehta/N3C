

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d159346f-02e9-4d61-9e11-ee18800813e3"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
DEX_pt <- function(covariate_v02) {
    library(dplyr)
    library(lubridate)

    df <- covariate_v02 %>%
        filter(drug_dex_v1==1) %>% 
        mutate(end_date = as.Date(ifelse(is.na(dex_end_date), dex_start_date, dex_end_date), origin="1970-01-01")) %>%
        select(person_id, data_partner_id, dex_start_date, end_date, Invasive_Ventilation, race_eth) %>%
        distinct() %>%
        mutate(bi_week_num = cut.Date(dex_start_date, breaks = "2 weeks", labels = FALSE)) %>%
        mutate(bi_week = ymd( "2020-03-09" ) + 2*weeks(bi_week_num - 1 )) %>%
        filter(dex_start_date < '2020-11-01') %>%
        mutate(dex_end_week_num = cut.Date(end_date, breaks = "2 weeks", labels = FALSE)) %>%
        mutate(dex_end_week = ymd( "2020-03-09" ) + 2*weeks(dex_end_week_num - 1 ))
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.39cba43f-2de8-4ae7-bc45-498f29205136"),
    DEX_pt=Input(rid="ri.foundry.main.dataset.d159346f-02e9-4d61-9e11-ee18800813e3")
)
DEX_pt_2 <- function(DEX_pt) {
    library(dplyr)
    df <- DEX_pt %>%
        select(bi_week) %>%
        distinct() %>%
        arrange(bi_week) %>%
        rename(week_index = bi_week)

    df1 <- merge(DEX_pt, df) %>%
            filter(week_index >= bi_week,
                   week_index <= dex_end_week) %>%
            as.data.frame()
    return(df1)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1bca3e49-3a92-49c3-881b-3b76f8160a25"),
    DEX_pt_2=Input(rid="ri.foundry.main.dataset.39cba43f-2de8-4ae7-bc45-498f29205136"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b84e3567-5e76-4743-8236-3f7f011d728b")
)
DEX_pt_3 <- function(DEX_pt_2, total_pt_2) {

    library(dplyr)

    df <- DEX_pt_2 %>%
          select(person_id, data_partner_id, bi_week) %>%
          group_by(bi_week, data_partner_id) %>%
          summarize(dex_patient_number = n()) 

    df1 <- total_pt_2 %>%
          select(person_id, data_partner_id, bi_week) %>%
          group_by(bi_week, data_partner_id) %>%
          summarize(total_patient_number = n()) 

    df2 <- merge(df1, df, by = c("bi_week", "data_partner_id"), all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4)) %>%
            mutate(dex_percent = ifelse(is.na(dex_percent), 0, dex_percent))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5dcfd8ba-b76e-4592-b416-5587fcdf780b"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
HCQ_pt <- function(covariate_v02) {
    library(dplyr)
    library(lubridate)

    df <- covariate_v02 %>%
        filter(drug_hcq_v1==1) %>% 
        mutate(end_date = as.Date(ifelse(is.na(hcq_end_date), hcq_start_date, hcq_end_date), origin="1970-01-01")) %>%
        select(person_id, data_partner_id, hcq_start_date, end_date) %>%
        distinct() %>%
        mutate(bi_week_num = cut.Date(hcq_start_date, breaks = "2 weeks", labels = FALSE)) %>%
        mutate(bi_week = ymd( "2020-03-09" ) + 2*weeks(bi_week_num - 1 )) %>%
        filter(hcq_start_date < '2020-11-01')%>%
        mutate(hcq_end_week_num = cut.Date(end_date, breaks = "2 weeks", labels = FALSE)) %>%
        mutate(hcq_end_week = ymd( "2020-03-09" ) + 2*weeks(hcq_end_week_num - 1 ))
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.10b3d630-924d-45f4-89e9-297730bdd9b6"),
    HCQ_pt=Input(rid="ri.foundry.main.dataset.5dcfd8ba-b76e-4592-b416-5587fcdf780b")
)
HCQ_pt_2 <- function(HCQ_pt) {
    library(dplyr)
    df <- HCQ_pt %>%
        select(bi_week) %>%
        distinct() %>%
        arrange(bi_week) %>%
        rename(week_index = bi_week)

    df1 <- merge(HCQ_pt, df) %>%
            filter(week_index >= bi_week,
                   week_index <= hcq_end_week) %>%
            as.data.frame()
    return(df1)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.6d177b02-853e-4a29-9417-e49e75a4aada"),
    HCQ_pt_2=Input(rid="ri.foundry.main.dataset.10b3d630-924d-45f4-89e9-297730bdd9b6"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b84e3567-5e76-4743-8236-3f7f011d728b")
)
HCQ_pt_3 <- function(total_pt_2, HCQ_pt_2) {

    library(dplyr)

    df <- HCQ_pt_2 %>%
          select(person_id, data_partner_id, bi_week) %>%
          group_by(bi_week, data_partner_id) %>%
          summarize(hcq_patient_number = n()) 

    df1 <- total_pt_2 %>%
          select(person_id, data_partner_id, bi_week) %>%
          group_by(bi_week, data_partner_id) %>%
          summarize(total_patient_number = n()) 

    df2 <- merge(df1, df, by = c("bi_week", "data_partner_id"), all = TRUE) %>%
            mutate(hcq_percent = round(hcq_patient_number/total_patient_number, 4)) %>%
            mutate(hcq_percent = ifelse(is.na(hcq_percent), 0, hcq_percent))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2dca4f32-ffe4-4263-9a62-e1ec1b1b1e62"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
REM_pt <- function(covariate_v02) {
    library(dplyr)
    library(lubridate)

    df <- covariate_v02 %>%
        filter(drug_rem_v1==1) %>% 
        mutate(end_date = as.Date(ifelse(is.na(rem_end_date), rem_start_date, rem_end_date), origin="1970-01-01")) %>%
        select(person_id, data_partner_id, rem_start_date, end_date) %>%
        distinct() %>%
        mutate(week_num = cut.Date(rem_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(bi_week_num = case_when(
          week_num ==1 ~ 0, week_num %in% c(2,3) ~ 1, week_num %in% c(4,5) ~ 2, week_num %in% c(6,7) ~ 3,
          week_num %in% c(8,9) ~ 4, week_num %in% c(10,11) ~ 5, week_num %in% c(12,13) ~ 6, week_num %in% c(14,15) ~ 7,
          week_num %in% c(16,17) ~ 8, week_num %in% c(18,19) ~ 9, week_num %in% c(20,21) ~ 10, week_num %in% c(22,23) ~ 11,
          week_num %in% c(24,25) ~ 12, week_num %in% c(26,27) ~ 13, week_num %in% c(28,29) ~ 14, week_num %in% c(30,31)~15,
          week_num %in% c(32,33) ~ 16, week_num %in% c(34,35) ~ 17, week_num %in% c(36,37) ~ 18, week_num %in% c(38,39)~19,
          week_num %in% c(40,41) ~ 20, week_num ==42 ~ 21
        )) %>%
        mutate(bi_week = ymd( "2020-03-09" ) + 2*weeks(bi_week_num)) %>%
        filter(rem_start_date < '2020-11-01') %>%
        mutate(rem_end_week_num = cut.Date(end_date, breaks = "1 week", labels = FALSE))  %>%
        mutate(bi_end_week_num = case_when(
          rem_end_week_num ==1 ~ 0, rem_end_week_num %in% c(2,3) ~ 1, rem_end_week_num %in% c(4,5) ~ 2, rem_end_week_num %in% c(6,7) ~ 3,
          rem_end_week_num %in% c(8,9) ~ 4, rem_end_week_num %in% c(10,11) ~ 5, rem_end_week_num %in% c(12,13) ~ 6, rem_end_week_num %in% c(14,15) ~ 7,
          rem_end_week_num %in% c(16,17) ~ 8, rem_end_week_num %in% c(18,19) ~ 9, rem_end_week_num %in% c(20,21) ~ 10, rem_end_week_num %in% c(22,23) ~ 11,
          rem_end_week_num %in% c(24,25) ~ 12, rem_end_week_num %in% c(26,27) ~ 13, rem_end_week_num %in% c(28,29) ~ 14, rem_end_week_num %in% c(30,31)~15,
          rem_end_week_num %in% c(32,33) ~ 16, rem_end_week_num %in% c(34,35) ~ 17, rem_end_week_num %in% c(36,37) ~ 18, rem_end_week_num %in% c(38,39)~19,
          rem_end_week_num %in% c(40,41) ~ 20, rem_end_week_num ==42 ~ 21
        )) %>%
        mutate(rem_end_week = ymd( "2020-03-09" ) + 2*weeks(bi_end_week_num))
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ed4a0c22-78ce-4bac-b364-d9183ec164a8"),
    REM_pt=Input(rid="ri.foundry.main.dataset.2dca4f32-ffe4-4263-9a62-e1ec1b1b1e62")
)
REM_pt_2 <- function(REM_pt) {
    library(dplyr)
    df <- REM_pt %>%
        select(bi_week) %>%
        distinct() %>%
        arrange(bi_week) %>%
        rename(week_index = bi_week)

    df1 <- merge(REM_pt, df) %>%
            filter(week_index >= bi_week,
                   week_index <= rem_end_week) %>%
            as.data.frame()
    return(df1)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6528fa1e-ae68-4bc2-b932-81a474c4b7fa"),
    REM_pt_2=Input(rid="ri.foundry.main.dataset.ed4a0c22-78ce-4bac-b364-d9183ec164a8"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b84e3567-5e76-4743-8236-3f7f011d728b")
)
REM_pt_3 <- function(REM_pt_2, total_pt_2) {

    library(dplyr)

    df <- REM_pt_2 %>%
          select(person_id, data_partner_id, bi_week) %>%
          group_by(bi_week, data_partner_id) %>%
          summarize(rem_patient_number = n()) 

    df1 <- total_pt_2 %>%
          select(person_id, data_partner_id, bi_week) %>%
          group_by(bi_week, data_partner_id) %>%
          summarize(total_patient_number = n()) 

    df2 <- merge(df1, df, by = c("bi_week", "data_partner_id"), all = TRUE) %>%
            mutate(rem_percent = round(rem_patient_number/total_patient_number, 4)) %>%
            mutate(rem_percent = ifelse(is.na(rem_percent), 0, rem_percent))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ee4b416b-43da-4414-99ba-b55f6f55ed42"),
    DEX_pt_3=Input(rid="ri.foundry.main.dataset.1bca3e49-3a92-49c3-881b-3b76f8160a25"),
    HCQ_pt_3=Input(rid="ri.vector.main.execute.6d177b02-853e-4a29-9417-e49e75a4aada"),
    REM_pt_3=Input(rid="ri.foundry.main.dataset.6528fa1e-ae68-4bc2-b932-81a474c4b7fa")
)
combined <- function(HCQ_pt_3, REM_pt_3, DEX_pt_3) {
    library(dplyr)
    df1 <- HCQ_pt_3 %>%
            select(bi_week, hcq_percent) %>%
            rename(percent = hcq_percent) %>%
            mutate(drug = "1_HCQ")

    df2 <- REM_pt_3 %>%
            select(bi_week, rem_percent) %>%
            rename(percent = rem_percent) %>%
            mutate(drug = "2_REM")

    df3 <- DEX_pt_3 %>%
            select(bi_week, dex_percent) %>%
            rename(percent = dex_percent) %>%
            mutate(drug = "3_DEX")
    
    df <- rbind(df1, df2, df3)

    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.df3a2a3e-1bbd-48f0-a175-22d67715afd5"),
    HCQ_pt_3=Input(rid="ri.vector.main.execute.6d177b02-853e-4a29-9417-e49e75a4aada")
)
hcq_boxplot_bi_week_plot <- function(HCQ_pt_3) {
    
    library(ggplot2)
    library(scales)
    library(grid)
    library(gridExtra)
    library(tibble)
    library(janitor)
    library(cowplot)
    
    gg <- ggplot(HCQ_pt_3, aes(x=bi_week, y=hcq_percent, group=bi_week)) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(fill='skyblue2') + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-02-10"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = date_format("%m/%d")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L))+
            theme(axis.text.x = element_text(angle=45, hjust=1, size = rel(1.2)),
                  axis.text.y = element_text(size = rel(1.2)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50")) +
            labs(y="Hydroxychloroquine(%)",x="Calender time, biweekly")
    plot(gg)
    return(NULL)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a7d1f7ed-9955-43c6-a2b5-b6398975736d"),
    vent=Input(rid="ri.foundry.main.dataset.68c10a14-8c63-4df3-a95e-1b87313b89df")
)
sensitivity_analys_centers <- function(vent) {
    library(dplyr)
    df <- vent %>%
            filter(bi_week %in% c(as.Date('2020-06-29'), as.Date('2020-07-13'), as.Date('2020-07-27'), as.Date('2020-08-10'), as.Date('2020-08-24'), as.Date('2020-09-07'), as.Date('2020-09-21'), as.Date('2020-10-05'), as.Date('2020-10-19'))) %>%
            group_by(data_partner_id, Invasive_Ventilation) %>%
            summarize(n=n()) %>%
            filter(n==9)
            
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0c1848d3-27cf-42e8-aa46-b8b8507acef8"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
total_pt <- function(covariate_v02) {
    library(dplyr)
    library(lubridate)
    df <- covariate_v02 %>% 
        select(person_id, covid_admission, covid_discharge, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(bi_week_num = cut.Date(covid_admission, breaks = "2 weeks", labels = FALSE)) %>%
        mutate(bi_week = ymd( "2020-02-10" ) + 2*weeks(bi_week_num-1)) %>%
        mutate(disch_week_num = cut.Date(covid_discharge, breaks = "1 week", labels = FALSE)) %>%
        mutate(bi_disch_week_num = case_when(
          disch_week_num ==1 ~ 0, disch_week_num %in% c(2,3) ~ 1, disch_week_num %in% c(4,5) ~ 2, disch_week_num %in% c(6,7) ~ 3,
          disch_week_num %in% c(8,9) ~ 4, disch_week_num %in% c(10,11) ~ 5, disch_week_num %in% c(12,13) ~ 6, disch_week_num %in% c(14,15) ~ 7,
          disch_week_num %in% c(16,17) ~ 8, disch_week_num %in% c(18,19) ~ 9, disch_week_num %in% c(20,21) ~ 10, disch_week_num %in% c(22,23) ~ 11,
          disch_week_num %in% c(24,25) ~ 12, disch_week_num %in% c(26,27) ~ 13, disch_week_num %in% c(28,29) ~ 14, disch_week_num %in% c(30,31)~15,
          disch_week_num %in% c(32,33) ~ 16, disch_week_num %in% c(34,35) ~ 17, disch_week_num %in% c(36,37) ~ 18, disch_week_num %in% c(38,39)~19,
          disch_week_num %in% c(40,41) ~ 20, disch_week_num ==42 ~ 21
        )) %>%
        mutate(bi_disch_week = ymd( "2020-02-24" ) + 2*weeks(bi_disch_week_num))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b84e3567-5e76-4743-8236-3f7f011d728b"),
    total_pt=Input(rid="ri.foundry.main.dataset.0c1848d3-27cf-42e8-aa46-b8b8507acef8")
)
total_pt_2 <- function(total_pt) {
    library(dplyr)
    df <- total_pt %>%
        select(bi_week) %>%
        distinct() %>%
        arrange(bi_week) %>%
        rename(week_index = bi_week)

    df1 <- merge(total_pt, df) %>%
            filter(week_index >= bi_week,
                   week_index <= bi_disch_week) %>%
            as.data.frame()
    return(df1)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e2f03919-68f7-4f00-ab0e-e31e95421b8b"),
    combined=Input(rid="ri.foundry.main.dataset.ee4b416b-43da-4414-99ba-b55f6f55ed42")
)
unnamed <- function(combined) {
    
    library(ggplot2)
    library(scales)
     
    gg <- ggplot(combined, aes(x=bi_week, y=percent, group=bi_week)) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(fill='skyblue2') + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-01-20"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = date_format("%m/%d")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L))+
            theme(axis.text.x = element_text(angle=45, hjust=1, size = rel(1.2)),
                  axis.text.y = element_text(size = rel(1.2)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50")) +
            labs(y="Drug(%)",x="Calender time, biweekly") +
            facet_wrap(~drug)
    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.ef3b5fcf-b50e-47f9-8adf-e497dcab2ce9"),
    combined=Input(rid="ri.foundry.main.dataset.ee4b416b-43da-4414-99ba-b55f6f55ed42")
)
unnamed_1 <- function(combined) {
    
    library(ggplot2)
    library(scales)
    library(janitor)
    library(dplyr)
    library(grid)
    library(gridExtra)
    library(tibble)
     
    gg <- ggplot(combined, aes(x=bi_week, y=percent, fill=drug, group=interaction(bi_week, drug))) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot() + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-02-10"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = c("Biweekly 1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L))+
            theme(axis.text.x = element_text(hjust=1, size = rel(1.2)),
                  axis.text.y = element_text(size = rel(1.2)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50"),
                  legend.key = element_blank(),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.position = c(0.12,0.91),
                  legend.key.size = unit(16, "points"),
                  legend.key.height = unit(0.8, "cm")) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Product (%)",x="Time (biweekly)", fill="Drug") +
            scale_fill_manual(breaks = c("1_HCQ", "2_REM", "3_DEX"), 
                              labels = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                              values = c('#1b9e77','#7570b3','#d95f02'),
                              name="Drug")

    table <- as.tibble(t(combined %>% 
                filter(drug == "1_HCQ") %>%
                select(bi_week) %>% 
                mutate(Date = format(bi_week, "%m/%d")) %>%
                select(-bi_week) %>%
                group_by(Date) %>% 
                mutate(`Number of Centers` = n()) %>% 
                distinct() %>%
                rownames_to_column("Biweekly Number")), rownames=" ") %>%
                row_to_names(row_number=1)

    mytheme <- gridExtra::ttheme_minimal(
                core = list(fg_params=list(cex = 0.9)),
                colhead = list(fg_params=list(cex = 0.9)))
    tbl <- tableGrob(table, theme = mytheme, rows = NULL)

    p <- grid.arrange(gg,
             tbl,
             nrow = 2,
             as.table = TRUE, 
             heights = c(10, 1))
    p1 <- cowplot::ggdraw(p) + 
        theme(plot.background = element_rect(fill="white", color=NA))
    
    plot(p1)
    
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.0d764bd6-523d-4e1b-b6b1-b6315f251141"),
    DEX_pt_3=Input(rid="ri.foundry.main.dataset.1bca3e49-3a92-49c3-881b-3b76f8160a25")
)
unnamed_11 <- function(DEX_pt_3) {
    
    library(ggplot2)
    library(scales)

    gg <- ggplot(DEX_pt_3, aes(x=bi_week, y=dex_percent, group=bi_week)) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(fill='skyblue2') + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-01-20"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = date_format("%m/%d")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L))+
            theme(axis.text.x = element_text(angle=45, hjust=1, vjust=1, size = rel(1.2)),
                  axis.text.y = element_text(size = rel(1.2)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50")) +
            labs(y="Dexamethasone(%)",x="Calender time, biweekly")
    plot(gg)
    return(NULL)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.ffb13f9e-0a82-4efc-be26-997607e31e50"),
    vent_race=Input(rid="ri.foundry.main.dataset.037a27e9-6542-41c4-a913-062436d3038c")
)
unnamed_2 <- function(vent_race) {
    
    library(ggplot2)
    library(scales)
    library(dplyr)
     
    gg <- ggplot(vent_race, aes(x=bi_week, y=dex_percent, fill=race_eth, group=interaction(bi_week, race_eth))) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot() + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-02-24"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = c("Biweekly 1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L))+
            theme(axis.text.x = element_text(hjust=1, size = rel(1.2)),
                  axis.text.y = element_text(size = rel(1.2)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50"),
                  legend.key = element_blank(),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.position = c(0.12,0.91),
                  legend.key.size = unit(16, "points"),
                  legend.key.height = unit(0.8, "cm")) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Dexamethasone (%)",x="Time (biweekly)", fill="Race/Ethnicity") +
            scale_fill_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK", "3_ASIAN", "5_HISPANIC"), 
                              labels = c("Non-Hispanic White", "Non-Hispanic Black", "Asian", "Hispanics"), 
                              values = c('#1b9e77','#7570b3','#d95f02', '#e7298a'),
                              name="Race/Ethnicity")
        
        plot(gg)
        return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.51c7e919-2d36-41b2-8184-3f2b1cb99c68"),
    DEX_pt=Input(rid="ri.foundry.main.dataset.d159346f-02e9-4d61-9e11-ee18800813e3")
)
unnamed_3 <- function(DEX_pt) {
    df <- DEX_pt %>%
        mutate(drug_dur = as.numeric(end_date - dex_start_date))
    table <- df %>%
        group_by(Invasive_Ventilation) %>%
        summarize(mean=mean(drug_dur), sd=sd(drug_dur))
    return(table)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ae72c923-d2e7-4d0b-acf9-10ca985afae1"),
    vent=Input(rid="ri.foundry.main.dataset.68c10a14-8c63-4df3-a95e-1b87313b89df")
)
unnamed_4 <- function(vent) {
    
    library(ggplot2)
    library(scales)
    library(janitor)
    library(tidyr)
    library(dplyr)
    library(grid)
    library(gridExtra)
    library(tibble)
    
    gg <- ggplot(vent, aes(x=bi_week, y=dex_percent, fill=Invasive_Ventilation, group=interaction(bi_week, Invasive_Ventilation))) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot() + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-02-10"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = c("Biweekly 1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18", "19")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L))+
            theme(axis.text.x = element_text(hjust=1, size = rel(1.2)),
                  axis.text.y = element_text(size = rel(1.2)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50"),
                  legend.key = element_blank(),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.position = c(0.15,0.91),
                  legend.key.size = unit(16, "points"),
                  legend.key.height = unit(0.8, "cm")) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Dexamethasone (%)",x="Time (biweekly)", fill="Invasive Ventilation") +
            scale_fill_manual(breaks = c("0", "1"), 
                              labels = c("No Ventilation", "Ventilation"), 
                              values = c('#d95f02', '#1b9e77'),
                              name="Invasive Ventilation")

table <- as.tibble(t(vent %>% 
                select(bi_week, Invasive_Ventilation) %>% 
                mutate(Invasive_Ventilation = ifelse(Invasive_Ventilation==1, "Number of Centers, Ventilation", "Number of Centers, No Ventilation")) %>%
                mutate(Date = format(bi_week, "%m/%d")) %>%
                select(-bi_week) %>%
                group_by(Date, Invasive_Ventilation) %>% 
                mutate(`Number of Centers` = n()) %>% 
                distinct() %>%
                pivot_wider(names_from = Invasive_Ventilation, values_from = `Number of Centers`) %>%
                mutate(`Number of Centers, Ventilation` = ifelse(is.na(`Number of Centers, Ventilation`), 0, `Number of Centers, Ventilation`)) %>%
                rownames_to_column("Biweekly Number")) , rownames=" ") %>%
                row_to_names(row_number=1)

    mytheme <- gridExtra::ttheme_minimal(
                core = list(fg_params=list(cex = 0.85)),
                colhead = list(fg_params=list(cex = 0.85)))
    tbl <- tableGrob(table, theme = mytheme, rows = NULL)

    p <- grid.arrange(gg,
             tbl,
             nrow = 2,
             as.table = TRUE, 
             heights = c(8, 1))
    p1 <- cowplot::ggdraw(p) + 
        theme(plot.background = element_rect(fill="white", color=NA))
    
    plot(p1)
    
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9466ec49-de5f-4725-a85d-7ffa0b6df99f"),
    REM_pt_3=Input(rid="ri.foundry.main.dataset.6528fa1e-ae68-4bc2-b932-81a474c4b7fa")
)
unnamed_5 <- function(REM_pt_3) {
    
    library(ggplot2)
    library(scales)
     
    gg <- ggplot(REM_pt_3, aes(x=bi_week, y=rem_percent, group=bi_week)) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot(fill='skyblue2') + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-02-10"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = date_format("%m/%d")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L))+
            theme(axis.text.x = element_text(angle=45, hjust=1, size = rel(1.2)),
                  axis.text.y = element_text(size = rel(1.2)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50")) +
            labs(y="Remdesivir(%)",x="Calender time, biweekly")
    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.ac1f8f3a-dfee-45de-9387-b15aa702eee2"),
    vent=Input(rid="ri.foundry.main.dataset.68c10a14-8c63-4df3-a95e-1b87313b89df")
)
unnamed_6 <- function(vent) {
    
    library(ggplot2)
    library(scales)
    library(janitor)
    library(tidyr)
    library(dplyr)
    library(grid)
    library(gridExtra)
    library(tibble)
     
    df <- vent %>% 
            filter(data_partner_id %in% c("181", "376", "439", "526", "698", "726", "793", "828", "1003")) %>%
            filter(bi_week %in% c(as.Date('2020-06-29'), as.Date('2020-07-13'), as.Date('2020-07-27'), as.Date('2020-08-10'), as.Date('2020-08-24'), as.Date('2020-09-07'), as.Date('2020-09-21'), as.Date('2020-10-05'), as.Date('2020-10-19')))

    gg <- ggplot(df, aes(x=bi_week, y=dex_percent, fill=Invasive_Ventilation, group=interaction(bi_week, Invasive_Ventilation))) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot() + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-06-29"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = c("Biweekly 1","2","3","4","5","6","7","8","9")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L))+
            theme(axis.text.x = element_text(hjust=1, size = rel(1.2)),
                  axis.text.y = element_text(size = rel(1.2)),
                  panel.grid.major.y = element_line(size=.1, color="grey"),
                  axis.title.y = element_text(size = rel(1.5), face="bold", vjust=1, margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = rel(1.5), face="bold", vjust=0.5, margin = margin(t = 20, r = 3, b = 5, l = 3)))+
            theme(panel.background = element_rect(fill = "white"),
                  axis.line = element_line(colour = "grey50"),
                  legend.key = element_blank(),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.position = c(0.15,0.91),
                  legend.key.size = unit(16, "points"),
                  legend.key.height = unit(0.8, "cm")) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Dexamethasone (%)",x="Time (biweekly)", fill="Invasive Ventilation") +
            scale_fill_manual(breaks = c("0", "1"), 
                              labels = c("No Ventilation", "Ventilation"), 
                              values = c('#d95f02', '#1b9e77'),
                              name="Invasive Ventilation")

table <- as.tibble(t(df %>% 
                select(bi_week, Invasive_Ventilation) %>% 
                mutate(Invasive_Ventilation = ifelse(Invasive_Ventilation==1, "Number of Centers, Ventilation", "Number of Centers, No Ventilation")) %>%
                mutate(Date = format(bi_week, "%m/%d")) %>%
                select(-bi_week) %>%
                group_by(Date, Invasive_Ventilation) %>% 
                mutate(`Number of Centers` = n()) %>% 
                distinct() %>%
                pivot_wider(names_from = Invasive_Ventilation, values_from = `Number of Centers`) %>%
                mutate(`Number of Centers, Ventilation` = ifelse(is.na(`Number of Centers, Ventilation`), 0, `Number of Centers, Ventilation`)) %>%
                rownames_to_column("Biweekly Number")) , rownames=" ") %>%
                row_to_names(row_number=1)

    mytheme <- gridExtra::ttheme_minimal(
                core = list(fg_params=list(cex = 0.85)),
                colhead = list(fg_params=list(cex = 0.85)))
    tbl <- tableGrob(table, theme = mytheme, rows = NULL)

    p <- grid.arrange(gg,
             tbl,
             nrow = 2,
             as.table = TRUE, 
             heights = c(8, 1))
    p1 <- cowplot::ggdraw(p) + 
        theme(plot.background = element_rect(fill="white", color=NA))
    
    plot(p1)
    
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.68c10a14-8c63-4df3-a95e-1b87313b89df"),
    DEX_pt_2=Input(rid="ri.foundry.main.dataset.39cba43f-2de8-4ae7-bc45-498f29205136"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b84e3567-5e76-4743-8236-3f7f011d728b")
)
vent <- function(total_pt_2, DEX_pt_2) {

    library(dplyr)

    df <- DEX_pt_2 %>%
          select(person_id, data_partner_id, bi_week, Invasive_Ventilation) %>%
          group_by(bi_week, data_partner_id, Invasive_Ventilation) %>%
          summarize(dex_patient_number = n()) 

    df1 <- total_pt_2 %>%
          select(person_id, data_partner_id, bi_week, Invasive_Ventilation) %>%
          group_by(bi_week, data_partner_id, Invasive_Ventilation) %>%
          summarize(total_patient_number = n()) 

    df2 <- merge(df1, df, by = c("bi_week", "data_partner_id", "Invasive_Ventilation"), all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4)) %>%
            mutate(dex_percent = ifelse(is.na(dex_percent), 0, dex_percent)) 

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.037a27e9-6542-41c4-a913-062436d3038c"),
    DEX_pt_2=Input(rid="ri.foundry.main.dataset.39cba43f-2de8-4ae7-bc45-498f29205136"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b84e3567-5e76-4743-8236-3f7f011d728b")
)
vent_race <- function(total_pt_2, DEX_pt_2) {

    library(dplyr)

    df <- DEX_pt_2 %>%
          filter(Invasive_Ventilation==1) %>%
          select(person_id, data_partner_id, bi_week, race_eth) %>%
          group_by(bi_week, data_partner_id, race_eth) %>%
          summarize(dex_patient_number = n()) 

    df1 <- total_pt_2 %>%
          filter(Invasive_Ventilation==1) %>%
          select(person_id, data_partner_id, bi_week, race_eth) %>%
          group_by(bi_week, data_partner_id, race_eth) %>%
          summarize(total_patient_number = n()) 

    df2 <- merge(df1, df, by = c("bi_week", "data_partner_id", "race_eth"), all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4)) %>%
            mutate(dex_percent = ifelse(is.na(dex_percent), 0, dex_percent)) %>%
            filter(race_eth %in% c("1_NH_WHITE", "2_NH_BLACK", "3_ASIAN", "5_HISPANIC"))

    return(df2)
    
}

