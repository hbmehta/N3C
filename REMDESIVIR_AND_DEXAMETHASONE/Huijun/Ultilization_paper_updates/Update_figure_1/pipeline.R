

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.efdb0066-bc16-4cea-9710-7cda3cece301"),
    dex_week_final=Input(rid="ri.foundry.main.dataset.0931d8cc-ce40-4c58-b197-6726e1013581"),
    hcq_week_final=Input(rid="ri.foundry.main.dataset.0255b060-309a-42cf-88ba-54f06b4d642a"),
    rem_week_final=Input(rid="ri.foundry.main.dataset.ac6e68f4-0d90-48a1-abe7-cf4479745338")
)
Figure_1_sum <- function(hcq_week_final, rem_week_final, dex_week_final) {
    
    library(dplyr)
    library(lubridate)
    
    df1 <- rem_week_final %>%
            select(week_index, rem_percent) %>%
            mutate(drug_type = "Remdesivir",
                   percent = rem_percent) %>%
            select(week_index, percent, drug_type)

    df2 <- hcq_week_final %>%
            select(week_index, hcq_percent) %>%
            mutate(drug_type = "Hydroxychloroquine",
                   percent = hcq_percent) %>%
            select(week_index, percent, drug_type)

    df3 <- dex_week_final %>%
            select(week_index, dex_percent) %>%
            mutate(drug_type = "Dexamethasone",
                   percent = dex_percent) %>%
            select(week_index, percent, drug_type)
    df <- rbind(df1, df2, df3) %>%
            mutate(month_year = format(as.Date(week_index), "%Y-%m")) %>%
            mutate(percent = ifelse(is.na(percent), 0 , percent))
    return(df) 
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.736363de-0333-40a8-bdcc-220c9502f5bd"),
    dex_week=Input(rid="ri.vector.main.execute.dc24afc1-6b31-4cce-9a8a-072fb3a01533")
)
dex_pts_by_week <- function(dex_week) {
    library(dplyr)
    df <- dex_week %>%
        select(start_week) %>%
        distinct() %>%
        arrange(start_week) %>%
        rename(week_index = start_week)

    df1 <- merge(dex_week, df) %>%
            filter(week_index >= start_week,
                   week_index <= end_week) %>%
            as.data.frame()
    return(df1)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.dc24afc1-6b31-4cce-9a8a-072fb3a01533"),
    hos_dex=Input(rid="ri.vector.main.execute.3f235ebf-a1d8-467d-80fe-724d5dc1ab13")
)
dex_week <- function(hos_dex) {
    library(dplyr)
    library(lubridate)

    df <- hos_dex %>% 
        select(person_id, data_partner_id, dex_start_date, dex_end_date, Invasive_Ventilation, race_eth) %>%
        distinct() %>%
        mutate(start_week_num = cut.Date(dex_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(start_week = ymd( "2020-03-02" ) + weeks(start_week_num - 1 )) %>%
        filter(dex_start_date < '2021-02-28') %>%
        mutate(end_week_num = cut.Date(dex_end_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(end_week = ymd( "2020-03-02" ) + weeks(end_week_num - 1 )) %>%
        select(-start_week_num, -end_week_num) %>%
        mutate(week_LOS = as.numeric(end_week)-as.numeric(start_week))

    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0931d8cc-ce40-4c58-b197-6726e1013581"),
    dex_pts_by_week=Input(rid="ri.foundry.main.dataset.736363de-0333-40a8-bdcc-220c9502f5bd"),
    weeks=Input(rid="ri.foundry.main.dataset.4416a09a-f88d-4a04-bae5-8a20aab2ae3b")
)
dex_week_final <- function(weeks, dex_pts_by_week) {

    library(dplyr)
    
    df <- dex_pts_by_week %>%
          select(person_id, week_index) %>%
          group_by(week_index) %>%
          summarize(dex_patient_number = n()) 

    df1 <- weeks %>%
          select(person_id, week_index) %>%
          group_by(week_index) %>%
          summarize(total_patient_number = n()) 

#    df2 <- df %>% 
#        full_join(df1, by = "week_index") %>%
#        mutate(dex_percent = round(dex_patient_number/total_patient_number, 4))

    df2 <- merge(df1, df, by = "week_index", all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.bca56315-c1fa-4695-ad56-14d01e32e3db"),
    Figure_1_sum=Input(rid="ri.foundry.main.dataset.efdb0066-bc16-4cea-9710-7cda3cece301")
)
fig_1_100 <- function(Figure_1_sum) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(Figure_1_sum, aes(x=week_index, y=percent)) +

            annotate("segment", x =  as.Date("2020-03-20"), xend =  as.Date("2020-03-20"), y = 0.385, yend = 0.50, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-03-08"), xmax = as.Date("2020-04-01"), ymin = 0.50, ymax = 0.58, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-03-20"), y = 0.54, label = "HCQ\nEUA issued\nMarch 20", size=3) + 

            annotate("segment", x =  as.Date("2020-05-12"), xend =  as.Date("2020-05-12"), y = 0.035, yend = 0.496, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-04-21"), xmax = as.Date("2020-06-03"), ymin = 0.496, ymax = 0.624, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-05-12"), y = 0.56, label = "Federal COVID-19\ntreatment guideline\nrecommended\nagainst HCQ use\nMay 12", size=3) + 

            annotate("segment", x = as.Date("2020-04-24"), xend = as.Date("2020-04-24"), y = 0.11, yend = 0.356, color="black", size=0.4)  +
            annotate("rect", xmin = as.Date("2020-04-11"), xmax = as.Date("2020-05-07"), ymin = 0.356, ymax = 0.434, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-04-24"), y = 0.395, label = "HCQ caution\nissued\nApril 24", size=3) + 

            annotate("segment", x =  as.Date("2020-06-16"), xend =  as.Date("2020-06-16"), y = 0.15, yend = 0.64, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-05-26"), xmax = as.Date("2020-07-07"), ymin = 0.64, ymax = 0.72, alpha = 1.0, size= 1.0, fill = "white", color="#d95f02") +
            annotate("text", x = as.Date("2020-06-16"), y = 0.68, label = "DEX RECOVERY trial\npress release\nJune 16", size=3) + 

            annotate("segment", x =  as.Date("2020-06-15"), xend =  as.Date("2020-06-15"), y = 0.022, yend = 0.395, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-05-30"), xmax = as.Date("2020-06-21"), ymin = 0.395, ymax = 0.474, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-06-10"), y = 0.435, label = "HCQ EUA\nrevoked\nJune 15", size=3) + 

            annotate("segment", x =  as.Date("2020-05-01"), xend =  as.Date("2020-05-01"), y = 0.018, yend = 0.169, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-04-21"), xmax = as.Date("2020-05-11"), ymin = 0.169, ymax = 0.247, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-05-01"), y = 0.208, label = "REM EUA\nissued\nMay 1", size=3) + 

            annotate("segment", x =  as.Date("2020-05-22"), xend =  as.Date("2020-05-22"), y = 0.085, yend = 0.259, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-05-09"), xmax = as.Date("2020-06-16"), ymin = 0.259, ymax = 0.381, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-05-28"), y = 0.32, label = "Preliminary results\n(Beigel et al,\nMay 22) and\n(Goldman et al,\nMay 27)", size=3) + 

            annotate("segment", x =  as.Date("2020-10-22"), xend =  as.Date("2020-10-22"), y = 0.28, yend = 0.306, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-09-29"), xmax = as.Date("2020-10-29"), ymin = 0.306, ymax = 0.374, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-10-14"), y = 0.34, label = "FDA approved\nRemdesivir\nOctober 22", size=3) +

            geom_line(aes(color=factor(drug_type)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(drug_type), shape=factor(drug_type)), size = 4, stroke = 1.8, fill = "white", show.legend = TRUE) +

            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=13, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.91),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.8, "cm"),
                  legend.key.size = unit(15, "points")) +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Product (%)", 
                 x="Time (Weeks)") +
            scale_colour_manual(breaks = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                labels = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                values = c('#1b9e77','#7570b3','#d95f02'),
                                name="Drug") +
            scale_shape_manual(breaks = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                labels = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                values = c(21, 22, 24),
                                name="Drug") +
            
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = as.Date('2020-01-27'), 
                                                 to = max(week_index), 
                                                 by = "7 days"),
                         labels = c('Week','1', '2','3\n\nFeb \n2020','4',
                                    '1','2','3\n\nMar \n2020','4',
                                    '1','2','3\n\nApr \n2020','4','5',
                                    '1','2','3\n\nMay \n2020','4',
                                    '1','2','3\n\nJun \n2020','4',
                                    '1','2','3\n\nJul \n2020','4','5',
                                    '1','2','3\n\nAug \n2020','4',
                                    '1','2','3\n\nSep \n2020','4',
                                    '1','2','3\n\nOct \n2020','4','5',
                                    '1','2','3\n\nNov \n2020','4',
                                    '1','2','3\n\nDec \n2020','4',
                                    '1','2','3\n\nJan \n2021','4','5',
                                    '1','2','3\n\nFeb \n2021','4',''),
                         limits = c(as.Date('2020-01-27'),as.Date('2021-03-01')))

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.2c3a1d04-195c-4842-8566-2f905c75546a"),
    Figure_1_sum=Input(rid="ri.foundry.main.dataset.efdb0066-bc16-4cea-9710-7cda3cece301")
)
figure_1 <- function(Figure_1_sum) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(Figure_1_sum, aes(x=week_index, y=percent)) +

            annotate("segment", x =  as.Date("2020-03-20"), xend =  as.Date("2020-03-20"), y = 0.385, yend = 0.45, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-03-09"), xmax = as.Date("2020-03-31"), ymin = 0.45, ymax = 0.51, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-03-20"), y = 0.48, label = "HCQ\nEUA issued\nMarch 20", size=3) + 

            annotate("segment", x =  as.Date("2020-05-12"), xend =  as.Date("2020-05-12"), y = 0.035, yend = 0.396, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-04-23"), xmax = as.Date("2020-06-01"), ymin = 0.396, ymax = 0.484, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-05-12"), y = 0.44, label = "Federal COVID-19\ntreatment guideline\nrecommended\nagainst HCQ use\nMay 12", size=3) + 

            annotate("segment", x = as.Date("2020-04-24"), xend = as.Date("2020-04-24"), y = 0.11, yend = 0.306, color="black", size=0.4)  +
            annotate("rect", xmin = as.Date("2020-04-11"), xmax = as.Date("2020-05-07"), ymin = 0.306, ymax = 0.364, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-04-24"), y = 0.335, label = "HCQ caution\nissued\nApril 24", size=3) + 

            annotate("segment", x =  as.Date("2020-06-16"), xend =  as.Date("2020-06-16"), y = 0.15, yend = 0.50, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-05-28"), xmax = as.Date("2020-07-05"), ymin = 0.50, ymax = 0.56, alpha = 1.0, size= 1.0, fill = "white", color="#d95f02") +
            annotate("text", x = as.Date("2020-06-16"), y = 0.53, label = "DEX RECOVERY trial\npress release\nJune 16", size=3) + 

            annotate("segment", x =  as.Date("2020-06-15"), xend =  as.Date("2020-06-15"), y = 0.022, yend = 0.325, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-06-01"), xmax = as.Date("2020-06-19"), ymin = 0.325, ymax = 0.384, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-06-10"), y = 0.355, label = "HCQ EUA\nrevoked\nJune 15", size=3) + 

            annotate("segment", x =  as.Date("2020-05-01"), xend =  as.Date("2020-05-01"), y = 0.018, yend = 0.165, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-04-22"), xmax = as.Date("2020-05-10"), ymin = 0.165, ymax = 0.223, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-05-01"), y = 0.194, label = "REM EUA\nissued\nMay 1", size=3) + 

            annotate("segment", x =  as.Date("2020-05-22"), xend =  as.Date("2020-05-22"), y = 0.085, yend = 0.229, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-05-09"), xmax = as.Date("2020-06-14"), ymin = 0.229, ymax = 0.311, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-05-27"), y = 0.27, label = "Preliminary results\n(Beigel et al,\nMay 22) and\n(Goldman et al,\nMay 27)", size=3) + 

            annotate("segment", x =  as.Date("2020-10-22"), xend =  as.Date("2020-10-22"), y = 0.28, yend = 0.311, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-09-30"), xmax = as.Date("2020-10-28"), ymin = 0.311, ymax = 0.369, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-10-14"), y = 0.34, label = "FDA approved\nRemdesivir\nOctober 22", size=3) +

            geom_line(aes(color=factor(drug_type)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(drug_type), shape=factor(drug_type)), size = 4, stroke = 1.8, fill = "white", show.legend = TRUE) +

            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=13, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.91),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.8, "cm"),
                  legend.key.size = unit(15, "points")) +
            scale_y_continuous(breaks=seq(0,0.71,0.05),limits=c(0,0.71), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Product (%)", 
                 x="Time (Weeks)") +
            scale_colour_manual(breaks = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                labels = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                values = c('#1b9e77','#7570b3','#d95f02'),
                                name="Drug") +
            scale_shape_manual(breaks = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                labels = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                values = c(21, 22, 24),
                                name="Drug") +
            
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = as.Date('2020-03-02'), 
                                                 to = max(week_index), 
                                                 by = "7 days"),
                         labels = c('Week 1','2','3\n\nMar \n2020','4',
                                    '1','2','3\n\nApr \n2020','4','5',
                                    '1','2','3\n\nMay \n2020','4',
                                    '1','2','3\n\nJun \n2020','4',
                                    '1','2','3\n\nJul \n2020','4','5',
                                    '1','2','3\n\nAug \n2020','4',
                                    '1','2','3\n\nSep \n2020','4',
                                    '1','2','3\n\nOct \n2020','4','5',
                                    '1','2','3\n\nNov \n2020','4',
                                    '1','2','3\n\nDec \n2020','4',
                                    '1','2','3\n\nJan \n2021','4','5',
                                    '1','2','3\n\nFeb \n2021','4',''),
                         limits = c(as.Date('2020-03-02'),as.Date('2021-03-01')))

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fe2083de-add2-4b57-87ce-81586b4955e5"),
    hcq_week=Input(rid="ri.vector.main.execute.4067a050-49b3-4fee-869b-040c2aae4f78")
)
hcq_pts_by_week <- function(hcq_week) {
    library(dplyr)
    df <- hcq_week %>%
        select(start_week) %>%
        distinct() %>%
        arrange(start_week) %>%
        rename(week_index = start_week)

    df1 <- merge(hcq_week, df) %>%
            filter(week_index >= start_week,
                   week_index <= end_week) %>%
            as.data.frame()
    return(df1)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.4067a050-49b3-4fee-869b-040c2aae4f78"),
    hos_hcq=Input(rid="ri.vector.main.execute.7943275f-edfb-4c3a-a2ef-79e9b6204ace")
)
hcq_week <- function(hos_hcq) {
    library(dplyr)
    library(lubridate)

    df <- hos_hcq %>% 
        select(person_id, data_partner_id, hcq_start_date, hcq_end_date, Invasive_Ventilation, race_eth) %>%
        distinct() %>%
        mutate(start_week_num = cut.Date(hcq_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(start_week = ymd( "2020-03-09" ) + weeks(start_week_num - 1 )) %>%
        filter(hcq_start_date < '2021-02-28') %>%
        mutate(end_week_num = cut.Date(hcq_end_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(end_week = ymd( "2020-03-09" ) + weeks(end_week_num - 1 )) %>%
        select(-start_week_num, -end_week_num) %>%
        mutate(week_LOS = as.numeric(end_week)-as.numeric(start_week))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0255b060-309a-42cf-88ba-54f06b4d642a"),
    hcq_pts_by_week=Input(rid="ri.foundry.main.dataset.fe2083de-add2-4b57-87ce-81586b4955e5"),
    weeks=Input(rid="ri.foundry.main.dataset.4416a09a-f88d-4a04-bae5-8a20aab2ae3b")
)
hcq_week_final <- function(weeks, hcq_pts_by_week) {

    library(dplyr)
    
    df <- hcq_pts_by_week %>%
          select(person_id, week_index) %>%
          group_by(week_index) %>%
          summarize(hcq_patient_number = n()) 

    df1 <- weeks %>%
          select(person_id, week_index) %>%
          group_by(week_index) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = "week_index", all = TRUE) %>%
            mutate(hcq_percent = round(hcq_patient_number/total_patient_number, 4))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.3f235ebf-a1d8-467d-80fe-724d5dc1ab13"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
hos_dex <- function(Covariate_01) {
    library(dplyr)
    df <- Covariate_01 %>%
        filter(drug_dex_v1==1) %>%
        mutate(dex_end_date = ifelse(is.na(dex_end_date), dex_start_date, dex_end_date)) %>%
        mutate(dex_end_date = as.Date(dex_end_date, origin="1970-01-01"))
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.7943275f-edfb-4c3a-a2ef-79e9b6204ace"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
hos_hcq <- function(Covariate_01) {
    library(dplyr)
    df <- Covariate_01 %>%
        filter(drug_hcq_v1==1) %>%
        mutate(hcq_end_date = ifelse(is.na(hcq_end_date), hcq_start_date, hcq_end_date)) %>%
        mutate(hcq_end_date = as.Date(hcq_end_date, origin="1970-01-01"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e6287d0b-ad16-4087-993c-4029cd418eed"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
hos_rem <- function(Covariate_01) {
    library(dplyr)
    df <- Covariate_01 %>%
        filter(drug_rem_v1==1) %>%
        mutate(rem_end_date = ifelse(is.na(rem_end_date), rem_start_date, rem_end_date)) %>%
        mutate(rem_end_date = as.Date(rem_end_date, origin="1970-01-01"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4e83fd5e-0bfe-4f31-813d-2beeacc9bea4"),
    rem_week=Input(rid="ri.vector.main.execute.9aa01150-449b-4716-a927-3e83dadb5618")
)
rem_pts_by_week <- function(rem_week) {
    library(dplyr)
    df <- rem_week %>%
        select(start_week) %>%
        distinct() %>%
        arrange(start_week) %>%
        rename(week_index = start_week)

    df1 <- merge(rem_week, df) %>%
            filter(week_index >= start_week,
                   week_index <= end_week) %>%
            as.data.frame()
    return(df1)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9aa01150-449b-4716-a927-3e83dadb5618"),
    hos_rem=Input(rid="ri.vector.main.execute.e6287d0b-ad16-4087-993c-4029cd418eed")
)
rem_week <- function(hos_rem) {
    library(dplyr)
    library(lubridate)

    df <- hos_rem %>% 
        select(person_id, data_partner_id, rem_start_date, rem_end_date, Invasive_Ventilation, race_eth) %>%
        distinct() %>%
        mutate(start_week_num = cut.Date(rem_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(start_week = ymd( "2020-03-16" ) + weeks(start_week_num - 1 )) %>%
        filter(rem_start_date < '2021-02-28') %>%
        mutate(end_week_num = cut.Date(rem_end_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(end_week = ymd( "2020-03-23" ) + weeks(end_week_num - 1 )) %>%
        select(-start_week_num, -end_week_num) %>%
        mutate(week_LOS = as.numeric(end_week)-as.numeric(start_week))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ac6e68f4-0d90-48a1-abe7-cf4479745338"),
    rem_pts_by_week=Input(rid="ri.foundry.main.dataset.4e83fd5e-0bfe-4f31-813d-2beeacc9bea4"),
    weeks=Input(rid="ri.foundry.main.dataset.4416a09a-f88d-4a04-bae5-8a20aab2ae3b")
)
rem_week_final <- function(weeks, rem_pts_by_week) {

    library(dplyr)
    
    df <- rem_pts_by_week %>%
          select(person_id, week_index) %>%
          group_by(week_index) %>%
          summarize(rem_patient_number = n()) 

    df1 <- weeks %>%
          select(person_id, week_index) %>%
          group_by(week_index) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = "week_index", all = TRUE) %>%
            mutate(rem_percent = round(rem_patient_number/total_patient_number, 4))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.68db8e1e-bd7d-4740-9457-cd82ab373f3f"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
tot_pt <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    df <- Covariate_01 %>% 
        select(person_id, covid_admission, covid_discharge, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(admit_week_num = cut.Date(covid_admission, breaks = "1 week", labels = FALSE)) %>%
        mutate(admit_week = ymd( "2020-01-27" ) + weeks(admit_week_num - 1 )) %>%
        mutate(disch_week_num = cut.Date(covid_discharge, breaks = "1 week", labels = FALSE)) %>%
        mutate(disch_week = ymd( "2020-02-03" ) + weeks(disch_week_num - 1 )) %>%
        select(-admit_week_num, -disch_week_num) %>%
        mutate(week_LOS = as.numeric(disch_week)-as.numeric(admit_week))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4416a09a-f88d-4a04-bae5-8a20aab2ae3b"),
    tot_pt=Input(rid="ri.vector.main.execute.68db8e1e-bd7d-4740-9457-cd82ab373f3f")
)
weeks <- function(tot_pt) {
    library(dplyr)
    library(tidyr)
    df <- tot_pt %>%
        select(admit_week) %>%
        distinct() %>%
        arrange(admit_week) %>%
        rename(week_index = admit_week)

    df1 <- tot_pt %>%
            crossing(df) %>%
            filter(week_index >= admit_week,
                   week_index <= disch_week)
    return(df1)
    
}

