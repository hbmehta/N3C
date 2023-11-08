

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2d942d6d-c87d-47cd-aa1f-677e0f0f1f20"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
DEX_pt <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_01 %>%
        filter(drug_dex_v1==1) %>% 
        mutate(end_date = as.Date(ifelse(is.na(dex_end_date), dex_start_date, dex_end_date), origin="1970-01-01")) %>%
        select(person_id, data_partner_id, Invasive_Ventilation, dex_start_date, end_date, Invasive_Ventilation, race_eth) %>%
        distinct() %>%
        mutate(week_num = cut.Date(dex_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(bi_week_num = case_when(
          week_num ==1 ~ 0, week_num %in% c(2,3) ~ 1, week_num %in% c(4,5) ~ 2, week_num %in% c(6,7) ~ 3,
          week_num %in% c(8,9) ~ 4, week_num %in% c(10,11) ~ 5, week_num %in% c(12,13) ~ 6, week_num %in% c(14,15) ~ 7,
          week_num %in% c(16,17) ~ 8, week_num %in% c(18,19) ~ 9, week_num %in% c(20,21) ~ 10, week_num %in% c(22,23) ~ 11,
          week_num %in% c(24,25) ~ 12, week_num %in% c(26,27) ~ 13, week_num %in% c(28,29) ~ 14, week_num %in% c(30,31)~15,
          week_num %in% c(32,33) ~ 16, week_num %in% c(34,35) ~ 17, week_num %in% c(36,37) ~ 18, week_num %in% c(38,39)~19,
          week_num %in% c(40,41) ~ 20, week_num %in% c(42,43) ~ 21, week_num %in% c(44,45) ~ 22, week_num %in% c(46,47)~23,
          week_num %in% c(48,49) ~ 24, week_num %in% c(50,51) ~ 25, week_num %in% c(52,53) ~ 26
        )) %>%
        mutate(bi_week = ymd( "2020-02-24" ) + 2*weeks(bi_week_num)) %>%
        filter(dex_start_date < '2021-03-01') %>%
        mutate(dex_end_week_num = cut.Date(end_date, breaks = "1 week", labels = FALSE))  %>%
        mutate(bi_end_week_num = case_when(
          dex_end_week_num ==1 ~ 0, dex_end_week_num %in% c(2,3) ~ 1, dex_end_week_num %in% c(4,5) ~ 2, dex_end_week_num %in% c(6,7) ~ 3,
          dex_end_week_num %in% c(8,9) ~ 4, dex_end_week_num %in% c(10,11) ~ 5, dex_end_week_num %in% c(12,13) ~ 6, dex_end_week_num %in% c(14,15) ~ 7,
          dex_end_week_num %in% c(16,17) ~ 8, dex_end_week_num %in% c(18,19) ~ 9, dex_end_week_num %in% c(20,21) ~ 10, dex_end_week_num %in% c(22,23) ~ 11,
          dex_end_week_num %in% c(24,25) ~ 12, dex_end_week_num %in% c(26,27) ~ 13, dex_end_week_num %in% c(28,29) ~ 14, dex_end_week_num %in% c(30,31)~15,
          dex_end_week_num %in% c(32,33) ~ 16, dex_end_week_num %in% c(34,35) ~ 17, dex_end_week_num %in% c(36,37) ~ 18, dex_end_week_num %in% c(38,39)~19,
          dex_end_week_num %in% c(40,41) ~ 20, dex_end_week_num %in% c(42,43) ~ 21, dex_end_week_num %in% c(44,45) ~ 22, dex_end_week_num %in% c(46,47)~23,
          dex_end_week_num %in% c(48,49) ~ 24, dex_end_week_num %in% c(50,51) ~ 25, dex_end_week_num %in% c(52,53) ~ 26, dex_end_week_num %in% c(54,55) ~ 27, dex_end_week_num %in% c(56,57) ~ 28, dex_end_week_num %in% c(58,59) ~ 29
        )) %>%
        mutate(dex_end_week = ymd( "2020-02-24" ) + 2*weeks(bi_end_week_num))
    
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.204359c3-de0f-4ff8-816c-4cf7701fda34"),
    DEX_pt=Input(rid="ri.foundry.main.dataset.2d942d6d-c87d-47cd-aa1f-677e0f0f1f20")
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
    Output(rid="ri.foundry.main.dataset.805625a0-e86a-4225-a301-ab9f84bc564d"),
    DEX_pt_2=Input(rid="ri.foundry.main.dataset.204359c3-de0f-4ff8-816c-4cf7701fda34"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b0e22a3d-5f79-45a5-ba3d-bf2b468858e3")
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
    Output(rid="ri.foundry.main.dataset.0956edd6-6a12-43e7-8f23-acef1903e009"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
HCQ_pt <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_01 %>%
        filter(drug_hcq_v1==1) %>% 
        mutate(end_date = as.Date(ifelse(is.na(hcq_end_date), hcq_start_date, hcq_end_date), origin="1970-01-01")) %>%
        select(person_id, data_partner_id, Invasive_Ventilation, hcq_start_date, end_date) %>%
        distinct() %>%
        mutate(bi_week_num = cut.Date(hcq_start_date, breaks = "2 weeks", labels = FALSE)) %>%
        mutate(bi_week = ymd( "2020-03-09" ) + 2*weeks(bi_week_num - 1 )) %>%
        filter(hcq_start_date < '2021-03-01')%>%
        mutate(hcq_end_week_num = cut.Date(end_date, breaks = "2 weeks", labels = FALSE)) %>%
        mutate(hcq_end_week = ymd( "2020-03-09" ) + 2*weeks(hcq_end_week_num - 1 ))
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4c2b9fa4-0879-4d07-94c2-027d24539830"),
    HCQ_pt=Input(rid="ri.foundry.main.dataset.0956edd6-6a12-43e7-8f23-acef1903e009")
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
    Output(rid="ri.foundry.main.dataset.d0af71a9-5b61-42d9-ab57-6a04e5c5936f"),
    HCQ_pt_2=Input(rid="ri.foundry.main.dataset.4c2b9fa4-0879-4d07-94c2-027d24539830"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b0e22a3d-5f79-45a5-ba3d-bf2b468858e3")
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
    Output(rid="ri.foundry.main.dataset.4970c357-57a0-45a3-ac3a-7bdb6edff227"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
REM_pt <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_01 %>%
        filter(drug_rem_v1==1) %>% 
        mutate(end_date = as.Date(ifelse(is.na(rem_end_date), rem_start_date, rem_end_date), origin="1970-01-01")) %>%
        select(person_id, data_partner_id, Invasive_Ventilation, rem_start_date, end_date) %>%
        distinct() %>%
        mutate(week_num = cut.Date(rem_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(bi_week_num = case_when(
          week_num ==1 ~ 0, week_num %in% c(2,3) ~ 1, week_num %in% c(4,5) ~ 2, week_num %in% c(6,7) ~ 3,
          week_num %in% c(8,9) ~ 4, week_num %in% c(10,11) ~ 5, week_num %in% c(12,13) ~ 6, week_num %in% c(14,15) ~ 7,
          week_num %in% c(16,17) ~ 8, week_num %in% c(18,19) ~ 9, week_num %in% c(20,21) ~ 10, week_num %in% c(22,23) ~ 11,
          week_num %in% c(24,25) ~ 12, week_num %in% c(26,27) ~ 13, week_num %in% c(28,29) ~ 14, week_num %in% c(30,31)~15,
          week_num %in% c(32,33) ~ 16, week_num %in% c(34,35) ~ 17, week_num %in% c(36,37) ~ 18, week_num %in% c(38,39)~19,
          week_num %in% c(40,41) ~ 20, week_num %in% c(42,43) ~ 21, week_num %in% c(44,45) ~ 22, week_num %in% c(46,47)~23,
          week_num %in% c(48,49) ~ 24, week_num %in% c(50,51) ~ 25
        )) %>%
        mutate(bi_week = ymd( "2020-03-09" ) + 2*weeks(bi_week_num)) %>%
        filter(rem_start_date < '2021-03-01') %>%
        mutate(rem_end_week_num = cut.Date(end_date, breaks = "1 week", labels = FALSE))  %>%
        mutate(bi_end_week_num = case_when(
          rem_end_week_num ==1 ~ 0, rem_end_week_num %in% c(2,3) ~ 1, rem_end_week_num %in% c(4,5) ~ 2, rem_end_week_num %in% c(6,7) ~ 3,
          rem_end_week_num %in% c(8,9) ~ 4, rem_end_week_num %in% c(10,11) ~ 5, rem_end_week_num %in% c(12,13) ~ 6, rem_end_week_num %in% c(14,15) ~ 7,
          rem_end_week_num %in% c(16,17) ~ 8, rem_end_week_num %in% c(18,19) ~ 9, rem_end_week_num %in% c(20,21) ~ 10, rem_end_week_num %in% c(22,23) ~ 11,
          rem_end_week_num %in% c(24,25) ~ 12, rem_end_week_num %in% c(26,27) ~ 13, rem_end_week_num %in% c(28,29) ~ 14, rem_end_week_num %in% c(30,31)~15,
          rem_end_week_num %in% c(32,33) ~ 16, rem_end_week_num %in% c(34,35) ~ 17, rem_end_week_num %in% c(36,37) ~ 18, rem_end_week_num %in% c(38,39)~19,
          rem_end_week_num %in% c(40,41) ~ 20, rem_end_week_num %in% c(42,43) ~ 21, rem_end_week_num %in% c(44,45) ~ 22, rem_end_week_num %in% c(46,47)~23,
          rem_end_week_num %in% c(48,49) ~ 24, rem_end_week_num %in% c(50,51) ~ 25
        )) %>%
        mutate(rem_end_week = ymd( "2020-03-09" ) + 2*weeks(bi_end_week_num))
    
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.19f21590-d871-4910-84cd-9f1f58245e87"),
    REM_pt=Input(rid="ri.foundry.main.dataset.4970c357-57a0-45a3-ac3a-7bdb6edff227")
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
    Output(rid="ri.foundry.main.dataset.b4290cb5-e9a8-493b-9d98-211c6e6d1852"),
    REM_pt_2=Input(rid="ri.foundry.main.dataset.19f21590-d871-4910-84cd-9f1f58245e87"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b0e22a3d-5f79-45a5-ba3d-bf2b468858e3")
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
    Output(rid="ri.foundry.main.dataset.4646de5c-9327-4381-a632-2b3d5b7d2782"),
    DEX_pt_3=Input(rid="ri.foundry.main.dataset.805625a0-e86a-4225-a301-ab9f84bc564d"),
    HCQ_pt_3=Input(rid="ri.foundry.main.dataset.d0af71a9-5b61-42d9-ab57-6a04e5c5936f"),
    REM_pt_3=Input(rid="ri.foundry.main.dataset.b4290cb5-e9a8-493b-9d98-211c6e6d1852")
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
    Output(rid="ri.vector.main.execute.eb0993d1-adc9-4432-814c-f5f31a5ca5a5"),
    vent=Input(rid="ri.foundry.main.dataset.0c5be87e-6e15-4ac7-989d-f790ad1c4efd")
)
efig5b <- function(vent) {
    
    library(ggplot2)
    library(scales)
    library(janitor)
    library(tidyr)
    library(dplyr)
    library(grid)
    library(gridExtra)
    library(tibble)
     
    df <- vent %>% 
            filter(data_partner_id %in% c("181","198","217","260","264","376","411","439","526","569","688","698","726","785","793","819","828","960")) %>%
            filter(bi_week %in% c(as.Date('2020-06-29'), as.Date('2020-07-13'), as.Date('2020-07-27'), as.Date('2020-08-10'), as.Date('2020-08-24'), as.Date('2020-09-07'), as.Date('2020-09-21'), as.Date('2020-10-05'), as.Date('2020-10-19'), as.Date('2020-11-02'), as.Date('2020-11-16'), as.Date('2020-11-30'), as.Date('2020-12-14'), as.Date('2020-12-28'), as.Date('2021-01-11'), as.Date('2021-01-25'), as.Date('2021-02-08'), as.Date('2021-02-22'), as.Date('2021-03-08')))

    gg <- ggplot(df, aes(x=bi_week, y=dex_percent, fill=as.factor(Invasive_Ventilation), group=interaction(bi_week, as.factor(Invasive_Ventilation)))) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot() + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-06-29"), 
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
    Output(rid="ri.vector.main.execute.90822873-8a55-4213-97fc-a75c6c86c39c"),
    vent=Input(rid="ri.foundry.main.dataset.0c5be87e-6e15-4ac7-989d-f790ad1c4efd")
)
efig_5a <- function(vent) {
    
    library(ggplot2)
    library(scales)
    library(janitor)
    library(tidyr)
    library(dplyr)
    library(grid)
    library(gridExtra)
    library(tibble)
    
    gg <- ggplot(vent, aes(x=bi_week, y=dex_percent, fill=as.factor(Invasive_Ventilation), group=interaction(bi_week, as.factor(Invasive_Ventilation)))) + 
            stat_boxplot(geom ='errorbar') + 
            geom_boxplot() + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-01-27"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = c("Biweekly 1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18", "19","20","21","22","23","24","25","26","27","28","29")) +
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
                
    tbl <- tableGrob(table, theme = ttheme_minimal(base_size = 6.5), rows = NULL)

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
    Output(rid="ri.vector.main.execute.838f8332-5785-4102-964c-5103ba3c6d7f"),
    combined=Input(rid="ri.foundry.main.dataset.4646de5c-9327-4381-a632-2b3d5b7d2782")
)
figure_2 <- function(combined) {
    
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
                         breaks = function(bi_week) seq.Date(from = as.Date("2020-01-27"), 
                                                 to = max(bi_week), 
                                                 by = "14 days"),
                         labels = c("Biweekly 1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29")) +
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

    tbl <- tableGrob(table, theme = ttheme_minimal(base_size = 6.5), rows = NULL)

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
    Output(rid="ri.foundry.main.dataset.93a7a82b-7207-4211-a0bf-6d5085f1152b"),
    vent=Input(rid="ri.foundry.main.dataset.0c5be87e-6e15-4ac7-989d-f790ad1c4efd")
)
sensitivity_analys_centers <- function(vent) {
    library(dplyr)
    df <- vent %>%
            filter(bi_week %in% c(as.Date('2020-06-29'), as.Date('2020-07-13'), as.Date('2020-07-27'), as.Date('2020-08-10'), as.Date('2020-08-24'), as.Date('2020-09-07'), as.Date('2020-09-21'), as.Date('2020-10-05'), as.Date('2020-10-19'), as.Date('2020-11-02'), as.Date('2020-11-16'), as.Date('2020-11-30'), as.Date('2020-12-14'), as.Date('2020-12-28'), as.Date('2021-01-11'), as.Date('2021-01-25'), as.Date('2021-02-08'), as.Date('2021-02-22'), as.Date('2021-03-08'))) %>%
            group_by(data_partner_id, Invasive_Ventilation) %>%
            summarize(n=n()) %>%
            filter(n==18)
            
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3637de20-a038-44a9-a529-e1e2014dbeb0"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
total_pt <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    df <- Covariate_01 %>% 
        select(person_id, covid_admission, covid_discharge, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(bi_week_num = cut.Date(covid_admission, breaks = "2 weeks", labels = FALSE)) %>%
        mutate(bi_week = ymd( "2020-01-27" ) + 2*weeks(bi_week_num-1))  %>%
        mutate(disch_week_num = cut.Date(covid_discharge, breaks = "1 week", labels = FALSE)) %>%
        mutate(bi_disch_week_num = case_when(
          disch_week_num ==1 ~ 0, disch_week_num %in% c(2,3) ~ 1, disch_week_num %in% c(4,5) ~ 2, disch_week_num %in% c(6,7) ~ 3,
          disch_week_num %in% c(8,9) ~ 4, disch_week_num %in% c(10,11) ~ 5, disch_week_num %in% c(12,13) ~ 6, disch_week_num %in% c(14,15) ~ 7,
          disch_week_num %in% c(16,17) ~ 8, disch_week_num %in% c(18,19) ~ 9, disch_week_num %in% c(20,21) ~ 10, disch_week_num %in% c(22,23) ~ 11,
          disch_week_num %in% c(24,25) ~ 12, disch_week_num %in% c(26,27) ~ 13, disch_week_num %in% c(28,29) ~ 14, disch_week_num %in% c(30,31)~15,
          disch_week_num %in% c(32,33) ~ 16, disch_week_num %in% c(34,35) ~ 17, disch_week_num %in% c(36,37) ~ 18, disch_week_num %in% c(38,39)~19,
          disch_week_num %in% c(40,41) ~ 20, disch_week_num %in% c(42,43) ~ 21, disch_week_num %in% c(44,45) ~ 22, disch_week_num %in% c(46,47) ~ 23,
          disch_week_num %in% c(48,49) ~ 24, disch_week_num %in% c(50,51) ~ 25, disch_week_num %in% c(52,53) ~ 26, disch_week_num %in% c(54,55)~27,
          disch_week_num %in% c(56,57) ~ 28, disch_week_num %in% c(58,59) ~ 29, disch_week_num %in% c(60,61) ~ 30, disch_week_num %in% c(62,63)~31,
          disch_week_num %in% c(64,65) ~ 32,
        )) %>%
        mutate(bi_disch_week = ymd( "2020-01-27" ) + 2*weeks(bi_disch_week_num))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b0e22a3d-5f79-45a5-ba3d-bf2b468858e3"),
    total_pt=Input(rid="ri.foundry.main.dataset.3637de20-a038-44a9-a529-e1e2014dbeb0")
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
    Output(rid="ri.foundry.main.dataset.0c5be87e-6e15-4ac7-989d-f790ad1c4efd"),
    DEX_pt_2=Input(rid="ri.foundry.main.dataset.204359c3-de0f-4ff8-816c-4cf7701fda34"),
    total_pt_2=Input(rid="ri.foundry.main.dataset.b0e22a3d-5f79-45a5-ba3d-bf2b468858e3")
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

