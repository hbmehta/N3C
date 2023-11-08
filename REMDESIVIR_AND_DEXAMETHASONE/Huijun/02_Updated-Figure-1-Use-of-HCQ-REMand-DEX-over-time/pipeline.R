

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c0275d8c-577c-4e9a-a85b-6a7c8ab5e642"),
    dex_week_final=Input(rid="ri.foundry.main.dataset.2ef12e6a-8417-474e-8e93-ffab4ecea177"),
    hcq_week_final=Input(rid="ri.foundry.main.dataset.7173bade-2706-429b-b6c0-bc73fa83042b"),
    rem_week_final=Input(rid="ri.foundry.main.dataset.ab737ab1-0cf1-486b-92ee-d5dc04e8f94d")
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
            mutate(month = month(week_index, label = TRUE, abbr = TRUE))
    return(df) 
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.41297c2a-0565-41ac-aecb-3b0b8c39fcb4"),
    dex_week=Input(rid="ri.foundry.main.dataset.83f8b97c-f56b-44f3-bb6f-229449278502")
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
    Output(rid="ri.vector.main.execute.1eaa8b6c-39cc-4435-87c8-72450ddbe7ef"),
    dex_week_final=Input(rid="ri.foundry.main.dataset.2ef12e6a-8417-474e-8e93-ffab4ecea177")
)
dex_trend <- function(dex_week_final) {
    library(ggplot2)

    gg <- ggplot(dex_week_final, aes(x=week_index, y=dex_percent)) +
            geom_line(color="#636363", size = 1.5) +
            geom_point(size = 3, shape = 21, stroke = 2, fill = "white") +
            ggtitle("Use of Dexamethasone Over Time") + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = min(week_index), 
                                                 to = max(week_index), 
                                                 by = "7 days"))+ 
            theme_bw() +
            theme(axis.text.x = element_text(angle=45, hjust=1, size=12),
                  axis.text.y = element_text(size=12),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=14, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.9),
                  axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.9, "cm"),
                  legend.key.size = unit(20, "points")) + 
            scale_y_continuous(breaks=seq(0,0.40,0.05),limits=c(0,0.40), labels=scales::percent)+
            labs(y="Dexamethasone %", x="Time (weeks)")+

            annotate("rect", xmin = as.Date("2020-02-11"), xmax = as.Date("2020-03-10"), ymin = 0.05, ymax = 0.10, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-02-25"), y = 0.075, label = "DEX Correspondence\nfrom Shang et al\nin Lancet\nFebruary 11", size=3) + 
            annotate("segment", x =  as.Date("2020-02-11"), xend =  as.Date("2020-02-11"), y = 0, yend = 0.05, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-06-16"), xmax = as.Date("2020-07-12"), ymin = 0.355, ymax = 0.395, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-06-29"), y = 0.375, label = "DEX RECOVERY trial\npress release\nJune 16", size=3) + 
            annotate("segment", x =  as.Date("2020-06-16"), xend =  as.Date("2020-06-16"), y = 0, yend = 0.355, color="black", size=0.3)

    plot(gg)
    return(NULL)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.83f8b97c-f56b-44f3-bb6f-229449278502"),
    hos_dex=Input(rid="ri.foundry.main.dataset.2554fd83-e15b-464e-a993-c8a27d103499")
)
dex_week <- function(hos_dex) {
    library(dplyr)
    library(lubridate)

    df <- hos_dex %>% 
        select(person_id, data_partner_id, dex_start_date, dex_end_date, Invasive_Ventilation, race_eth) %>%
        distinct() %>%
        mutate(start_week_num = cut.Date(dex_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(start_week = ymd( "2020-03-09" ) + weeks(start_week_num - 1 )) %>%
        filter(dex_start_date < '2020-11-01') %>%
        mutate(end_week_num = cut.Date(dex_end_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(end_week = ymd( "2020-03-09" ) + weeks(end_week_num - 1 )) %>%
        select(-start_week_num, -end_week_num) %>%
        mutate(week_LOS = as.numeric(end_week)-as.numeric(start_week))

    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2ef12e6a-8417-474e-8e93-ffab4ecea177"),
    dex_pts_by_week=Input(rid="ri.foundry.main.dataset.41297c2a-0565-41ac-aecb-3b0b8c39fcb4"),
    weeks=Input(rid="ri.foundry.main.dataset.7599395a-f97c-425a-9dfa-be8913425f52")
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

    df2 <- merge(df1, df, by = "week_index", all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.22e7229e-4f9c-4c5e-807f-c174bdb3d020"),
    dex_pts_by_week=Input(rid="ri.foundry.main.dataset.41297c2a-0565-41ac-aecb-3b0b8c39fcb4"),
    weeks=Input(rid="ri.foundry.main.dataset.7599395a-f97c-425a-9dfa-be8913425f52")
)
dex_week_final_race <- function(weeks, dex_pts_by_week) {

    library(dplyr)
    
    df <- dex_pts_by_week %>%
          select(person_id, week_index, race_eth) %>%
          group_by(week_index, race_eth) %>%
          summarize(dex_patient_number = n()) 

    df1 <- weeks %>%
          select(person_id, week_index, race_eth) %>%
          group_by(week_index, race_eth) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = c("week_index", "race_eth"), all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.93f05613-ffb0-49c0-a217-47b8d458fc29"),
    dex_pts_by_week=Input(rid="ri.foundry.main.dataset.41297c2a-0565-41ac-aecb-3b0b8c39fcb4"),
    weeks=Input(rid="ri.foundry.main.dataset.7599395a-f97c-425a-9dfa-be8913425f52")
)
dex_week_final_vent <- function(dex_pts_by_week, weeks) {

    library(dplyr)
    
    df <- dex_pts_by_week %>%
          select(person_id, week_index, Invasive_Ventilation) %>%
          group_by(week_index, Invasive_Ventilation) %>%
          summarize(dex_patient_number = n()) 

    df1 <- weeks %>%
          select(person_id, week_index, Invasive_Ventilation) %>%
          group_by(week_index, Invasive_Ventilation) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = c("week_index", "Invasive_Ventilation"), all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.1d19d2f3-936f-43ff-8067-5f51edad420c"),
    Figure_1_sum=Input(rid="ri.foundry.main.dataset.c0275d8c-577c-4e9a-a85b-6a7c8ab5e642")
)
figure_3_trends <- function(Figure_1_sum) {
    library(dplyr)
    library(ggplot2)
    library(scales)

        gg <- ggplot(Figure_1_sum, aes(x=week_index, y=percent)) +
            geom_line(aes(color=factor(drug_type)), size = 1.5, show.legend = TRUE) +
            geom_point(aes(color = factor(drug_type)), size = 3, shape = 21, stroke = 2, fill = "white", show.legend = FALSE) +
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = min(week_index), 
                                                 to = max(week_index), 
                                                 by = "7 days"),
                         labels = date_format("%m/%d")) + 
            theme_bw() +
            theme(axis.text.x = element_text(angle=45, hjust=1, size=12),
                  axis.text.y = element_text(size=12),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=14, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.9),
                  axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.9, "cm"),
                  legend.key.size = unit(20, "points")) +
            scale_y_continuous(breaks=seq(0,0.51,0.05),limits=c(0,0.51), labels=scales::percent) +
            labs(y="Proportion of Patients Utilizing Each Drug (%)", color = "Drug",
                 x="Time (Weeks)") +
            scale_colour_manual(breaks = c("Dexamethasone", "Hydroxychloroquine", "Remdesivir"), 
                                labels = c("Dexamethasone", "Hydroxychloroquine", "Remdesivir"), 
                                values = c('#e41a1c','#377eb8','#4daf4a')) +

            annotate("rect", xmin = as.Date("2020-03-12"), xmax = as.Date("2020-03-28"), ymin = 0.39, ymax = 0.43, alpha = 1.0, fill = "white", color="#377eb8") +
            annotate("text", x = as.Date("2020-03-20"), y = 0.41, label = "HCQ\nEUA issued\nMarch 20", size=3) + 
            annotate("segment", x =  as.Date("2020-03-20"), xend =  as.Date("2020-03-20"), y = 0, yend = 0.39, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-04-29"), xmax = as.Date("2020-05-25"), ymin = 0.32, ymax = 0.38, alpha = 1.0, fill = "white", color="#377eb8") +
            annotate("text", x = as.Date("2020-05-12"), y = 0.35, label = "Federal COVID-19\ntreatment guideline\nrecommended\nagainst HCQ use\nMay 12", size=3) + 
            annotate("segment", x =  as.Date("2020-05-12"), xend =  as.Date("2020-05-12"), y = 0, yend = 0.32, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-04-15"), xmax = as.Date("2020-05-03"), ymin = 0.248, ymax = 0.282, alpha = 1.0, fill = "white", color="#377eb8") +
            annotate("text", x = as.Date("2020-04-24"), y = 0.265, label = "HCQ caution\nissued\nApril 24", size=3) + 
            annotate("segment", x =  as.Date("2020-04-24"), xend =  as.Date("2020-04-24"), y = 0, yend = 0.248, color="black", size=0.3)  +

            annotate("rect", xmin = as.Date("2020-06-16"), xmax = as.Date("2020-07-12"), ymin = 0.395, ymax = 0.435, alpha = 1.0, fill = "white", color="#e41a1c") +
            annotate("text", x = as.Date("2020-06-29"), y = 0.415, label = "DEX RECOVERY trial\npress release\nJune 16", size=3) + 
            annotate("segment", x =  as.Date("2020-06-16"), xend =  as.Date("2020-06-16"), y = 0, yend = 0.395, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-06-03"), xmax = as.Date("2020-06-29"), ymin = 0.32, ymax = 0.38, alpha = 1.0, fill = "white", color="#377eb8") +
            annotate("text", x = as.Date("2020-06-16"), y = 0.35, label = "Federal COVID-19\ntreatment guideline\nrecommended\nagainst HCQ use\nJune 16", size=3) + 
            annotate("segment", x =  as.Date("2020-06-16"), xend =  as.Date("2020-06-16"), y = 0, yend = 0.32, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-06-03"), xmax = as.Date("2020-06-17"), ymin = 0.278, ymax = 0.312, alpha = 1.0, fill = "white", color="#377eb8") +
            annotate("text", x = as.Date("2020-06-10"), y = 0.295, label = "HCQ EUA\nrevoked\nJune 15", size=3) + 
            annotate("segment", x =  as.Date("2020-06-15"), xend =  as.Date("2020-06-15"), y = 0, yend = 0.278, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-04-25"), xmax = as.Date("2020-05-07"), ymin = 0.208, ymax = 0.242, alpha = 1.0, fill = "white", color="#4daf4a") +
            annotate("text", x = as.Date("2020-05-01"), y = 0.225, label = "REM EUA\nissued\nMay 1", size=3) + 
            annotate("segment", x =  as.Date("2020-05-01"), xend =  as.Date("2020-05-01"), y = 0, yend = 0.208, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-05-14"), xmax = as.Date("2020-06-09"), ymin = 0.208, ymax = 0.262, alpha = 1.0, fill = "white", color="#4daf4a") +
            annotate("text", x = as.Date("2020-05-27"), y = 0.235, label = "Preliminary results\n(Beigel et al,\nMay 22) and\n(Goldman et al,\nMay 27)", size=3) + 
            annotate("segment", x =  as.Date("2020-05-22"), xend =  as.Date("2020-05-22"), y = 0, yend = 0.208, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-04-21"), xmax = as.Date("2020-05-31"), ymin = 0.12, ymax = 0.20, alpha = 1.0, fill = "white", color="#4daf4a") +
            annotate("text", x = as.Date("2020-05-11"), y = 0.16, 
                     label = "Wang et al publish\nnegative findings on REM\nGilead issues press relase\nof positive findings\nDr.Fauci calls the development\n\"very optimistic\"\nApril 29", 
                     size=3) + 
            annotate("segment", x =  as.Date("2020-04-29"), xend =  as.Date("2020-04-29"), y = 0, yend = 0.12, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-10-04"), xmax = as.Date("2020-10-24"), ymin = 0.178, ymax = 0.212, alpha = 1.0, fill = "white", color="#4daf4a") +
            annotate("text", x = as.Date("2020-10-14"), y = 0.195, label = "FDA approved\nRemdesivir\nOctober 22", size=3) + 
            annotate("segment", x =  as.Date("2020-10-22"), xend =  as.Date("2020-10-22"), y = 0, yend = 0.178, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-02-11"), xmax = as.Date("2020-03-10"), ymin = 0.195, ymax = 0.245, alpha = 1.0, fill = "white", color="#e41a1c") +
            annotate("text", x = as.Date("2020-02-25"), y = 0.22, label = "DEX Correspondence\nfrom Shang et al\nin Lancet\nFebruary 11", size=3) + 
            annotate("segment", x =  as.Date("2020-02-11"), xend =  as.Date("2020-02-11"), y = 0, yend = 0.195, color="black", size=0.3)

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6c2de237-1ffb-4aac-88da-07a701c197dd"),
    hcq_week=Input(rid="ri.foundry.main.dataset.b7f7f447-fc67-47e6-99e4-936344646427")
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
    Output(rid="ri.vector.main.execute.fe49261f-53fe-4c27-9721-fbfbe2ec69ca"),
    hcq_week_final=Input(rid="ri.foundry.main.dataset.7173bade-2706-429b-b6c0-bc73fa83042b")
)
hcq_trend <- function(hcq_week_final) {
    library(dplyr)
    library(ggplot2)

    gg <- ggplot(hcq_week_final, aes(x=week_index, y=hcq_percent)) +
            geom_line(color="#636363", size = 1.5) +
            geom_point(size = 3, shape = 21, stroke = 2, fill = "white") +
            ggtitle("Use of Hydroxychloroquine Over Time") + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = min(week_index), 
                                                 to = max(week_index), 
                                                 by = "7 days")) + 
            theme_bw() +
            theme(axis.text.x = element_text(angle=45, hjust=1, size=12),
                  axis.text.y = element_text(size=12),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=14, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.9),
                  axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.9, "cm"),
                  legend.key.size = unit(20, "points")) + 
            scale_y_continuous(breaks=seq(0,0.45,0.05),limits=c(0,0.45), labels=scales::percent)+
            labs(y="Hydroxychloroquine %", x="Time (weeks)") +

            annotate("rect", xmin = as.Date("2020-03-12"), xmax = as.Date("2020-03-28"), ymin = 0.39, ymax = 0.43, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-03-20"), y = 0.41, label = "HCQ\nEUA issued\nMarch 20", size=3) + 
            annotate("segment", x =  as.Date("2020-03-20"), xend =  as.Date("2020-03-20"), y = 0, yend = 0.39, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-04-29"), xmax = as.Date("2020-05-25"), ymin = 0.32, ymax = 0.38, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-05-12"), y = 0.35, label = "Federal COVID-19\ntreatment guideline\nrecommended\nagainst HCQ use\nMay 12", size=3) + 
            annotate("segment", x =  as.Date("2020-05-12"), xend =  as.Date("2020-05-12"), y = 0, yend = 0.32, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-04-15"), xmax = as.Date("2020-05-03"), ymin = 0.248, ymax = 0.282, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-04-24"), y = 0.265, label = "HCQ caution\nissued\nApril 24", size=3) + 
            annotate("segment", x =  as.Date("2020-04-24"), xend =  as.Date("2020-04-24"), y = 0, yend = 0.248, color="black", size=0.3)  +

            annotate("rect", xmin = as.Date("2020-06-03"), xmax = as.Date("2020-06-29"), ymin = 0.32, ymax = 0.38, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-06-16"), y = 0.35, label = "Federal COVID-19\ntreatment guideline\nrecommended\nagainst HCQ use\nJune 16", size=3) + 
            annotate("segment", x =  as.Date("2020-06-16"), xend =  as.Date("2020-06-16"), y = 0, yend = 0.32, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-06-03"), xmax = as.Date("2020-06-17"), ymin = 0.278, ymax = 0.312, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-06-10"), y = 0.295, label = "HCQ EUA\nrevoked\nJune 15", size=3) + 
            annotate("segment", x =  as.Date("2020-06-15"), xend =  as.Date("2020-06-15"), y = 0, yend = 0.278, color="black", size=0.3)

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b7f7f447-fc67-47e6-99e4-936344646427"),
    hos_hcq=Input(rid="ri.foundry.main.dataset.237e0fec-df1e-4680-848a-9fff1aac546c")
)
hcq_week <- function(hos_hcq) {
    library(dplyr)
    library(lubridate)

    df <- hos_hcq %>% 
        select(person_id, data_partner_id, hcq_start_date, hcq_end_date, Invasive_Ventilation, race_eth) %>%
        distinct() %>%
        mutate(start_week_num = cut.Date(hcq_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(start_week = ymd( "2020-03-09" ) + weeks(start_week_num - 1 )) %>%
        filter(hcq_start_date < '2020-11-01') %>%
        mutate(end_week_num = cut.Date(hcq_end_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(end_week = ymd( "2020-03-09" ) + weeks(end_week_num - 1 )) %>%
        select(-start_week_num, -end_week_num) %>%
        mutate(week_LOS = as.numeric(end_week)-as.numeric(start_week))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7173bade-2706-429b-b6c0-bc73fa83042b"),
    hcq_pts_by_week=Input(rid="ri.foundry.main.dataset.6c2de237-1ffb-4aac-88da-07a701c197dd"),
    weeks=Input(rid="ri.foundry.main.dataset.7599395a-f97c-425a-9dfa-be8913425f52")
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
    Output(rid="ri.foundry.main.dataset.2554fd83-e15b-464e-a993-c8a27d103499"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
hos_dex <- function(covariate) {
    library(dplyr)
    df <- covariate %>%
        filter(drug_dex_v1==1) %>%
        mutate(dex_end_date = ifelse(is.na(dex_end_date), dex_start_date, dex_end_date)) %>%
        mutate(dex_end_date = as.Date(dex_end_date, origin="1970-01-01")) %>%
        mutate(dex_end_date = ifelse(dex_end_date==as.Date("1900-01-01"), dex_start_date, dex_end_date)) %>%
        mutate(dex_end_date = as.Date(dex_end_date, origin="1970-01-01"))
    return(df)
    as.Date
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.237e0fec-df1e-4680-848a-9fff1aac546c"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
hos_hcq <- function(covariate) {
    library(dplyr)
    df <- covariate %>%
        filter(drug_hcq_v1==1) %>%
        mutate(hcq_end_date = ifelse(is.na(hcq_end_date), hcq_start_date, hcq_end_date)) %>%
        mutate(hcq_end_date = as.Date(hcq_end_date, origin="1970-01-01"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d4b16147-bc61-411b-ad8e-da88aac3f677"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
hos_rem <- function(covariate) {
    library(dplyr)
    df <- covariate %>%
        filter(drug_rem_v1==1) %>%
        mutate(rem_end_date = ifelse(is.na(rem_end_date), rem_start_date, rem_end_date)) %>%
        mutate(rem_end_date = as.Date(rem_end_date, origin="1970-01-01"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.32aaa389-6e41-4e9a-8c66-56247f41dc7e"),
    rem_week=Input(rid="ri.foundry.main.dataset.bb9030fd-c095-40c2-b18c-ed0a27f78375")
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
    Output(rid="ri.vector.main.execute.780be63c-5dbd-4dbf-abc3-77e51432dddc"),
    rem_week_final=Input(rid="ri.foundry.main.dataset.ab737ab1-0cf1-486b-92ee-d5dc04e8f94d")
)
rem_trend <- function(rem_week_final) {
    library(ggplot2)

    gg <- ggplot(rem_week_final, aes(x=week_index, y=rem_percent)) +
            geom_line(color="#636363", size = 1.5) +
            geom_point(size = 3, shape = 21, stroke = 2, fill = "white") +
            ggtitle("Use of Remdesivir Over Time") + 
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = min(week_index), 
                                                 to = max(week_index), 
                                                 by = "7 days")) + 
            theme_bw() +
            theme(axis.text.x = element_text(angle=45, hjust=1, size=12),
                  axis.text.y = element_text(size=12),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.title = element_text(colour="black", size=14, face="bold"), legend.text = element_text(size = 13),
                  legend.position = c(0.10,0.9),
                  axis.title.y = element_text(size = 14, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 14, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.key.height = unit(0.9, "cm"),
                  legend.key.size = unit(20, "points")) + 
            scale_y_continuous(breaks=seq(0,0.30,0.05),limits=c(0,0.30), labels=scales::percent)+
            labs(y="Remdesivir %", x="Time (weeks)") +
            
            annotate("rect", xmin = as.Date("2020-04-25"), xmax = as.Date("2020-05-07"), ymin = 0.21, ymax = 0.24, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-05-01"), y = 0.225, label = "REM EUA\nissued\nMay 1", size=3) + 
            annotate("segment", x =  as.Date("2020-05-01"), xend =  as.Date("2020-05-01"), y = 0, yend = 0.21, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-05-14"), xmax = as.Date("2020-06-09"), ymin = 0.215, ymax = 0.255, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-05-27"), y = 0.235, label = "Preliminary results\n(Beigel et al,\nMay 22) and\n(Goldman et al,\nMay 27)", size=3) + 
            annotate("segment", x =  as.Date("2020-05-22"), xend =  as.Date("2020-05-22"), y = 0, yend = 0.215, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-04-21"), xmax = as.Date("2020-05-31"), ymin = 0.135, ymax = 0.185, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-05-11"), y = 0.16, 
                     label = "Wang et al publish\nnegative findings on REM\nGilead issues press relase\nof positive findings\nDr.Fauci calls the development\n\"very optimistic\"\nApril 29", 
                     size=3) + 
            annotate("segment", x =  as.Date("2020-04-29"), xend =  as.Date("2020-04-29"), y = 0, yend = 0.135, color="black", size=0.3) +

            annotate("rect", xmin = as.Date("2020-10-04"), xmax = as.Date("2020-10-24"), ymin = 0.182, ymax = 0.208, alpha = 1.0, fill = "white", color="#636363") +
            annotate("text", x = as.Date("2020-10-14"), y = 0.195, label = "FDA approved\nRemdesivir\nOctober 22", size=3) + 
            annotate("segment", x =  as.Date("2020-10-22"), xend =  as.Date("2020-10-22"), y = 0, yend = 0.182, color="black", size=0.3)

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.bb9030fd-c095-40c2-b18c-ed0a27f78375"),
    hos_rem=Input(rid="ri.foundry.main.dataset.d4b16147-bc61-411b-ad8e-da88aac3f677")
)
rem_week <- function(hos_rem) {
    library(dplyr)
    library(lubridate)

    df <- hos_rem %>% 
        select(person_id, data_partner_id, rem_start_date, rem_end_date, Invasive_Ventilation, race_eth) %>%
        distinct() %>%
        mutate(start_week_num = cut.Date(rem_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(start_week = ymd( "2020-03-16" ) + weeks(start_week_num - 1 )) %>%
        filter(rem_start_date < '2020-11-01') %>%
        mutate(end_week_num = cut.Date(rem_end_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(end_week = ymd( "2020-03-16" ) + weeks(end_week_num - 1 )) %>%
        select(-start_week_num, -end_week_num) %>%
        mutate(week_LOS = as.numeric(end_week)-as.numeric(start_week))

    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ab737ab1-0cf1-486b-92ee-d5dc04e8f94d"),
    rem_pts_by_week=Input(rid="ri.foundry.main.dataset.32aaa389-6e41-4e9a-8c66-56247f41dc7e"),
    weeks=Input(rid="ri.foundry.main.dataset.7599395a-f97c-425a-9dfa-be8913425f52")
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
    Output(rid="ri.foundry.main.dataset.cd8b50c3-de3c-4296-a3d6-620c1cab8442"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4")
)
tot_pt <- function(covariate) {
    library(dplyr)
    library(lubridate)
    df <- covariate %>% 
        select(person_id, covid_admission, covid_discharge, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(admit_week_num = cut.Date(covid_admission, breaks = "1 week", labels = FALSE)) %>%
        mutate(admit_week = ymd( "2020-02-10" ) + weeks(admit_week_num - 1 )) %>%
        mutate(disch_week_num = cut.Date(covid_discharge, breaks = "1 week", labels = FALSE)) %>%
        mutate(disch_week = ymd( "2020-03-02" ) + weeks(disch_week_num - 1 )) %>%
        select(-admit_week_num, -disch_week_num) %>%
        mutate(week_LOS = as.numeric(disch_week)-as.numeric(admit_week))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.c0fbf051-18f6-44ed-b5ea-bb9028f7ae73"),
    Figure_1_sum=Input(rid="ri.foundry.main.dataset.c0275d8c-577c-4e9a-a85b-6a7c8ab5e642")
)
unnamed <- function(Figure_1_sum) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(Figure_1_sum, aes(x=week_index, y=percent)) +

            annotate("segment", x =  as.Date("2020-03-20"), xend =  as.Date("2020-03-20"), y = 0.345, yend = 0.39, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-03-12"), xmax = as.Date("2020-03-28"), ymin = 0.39, ymax = 0.43, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-03-20"), y = 0.41, label = "HCQ\nEUA issued\nMarch 20", size=3) + 

            annotate("segment", x =  as.Date("2020-05-12"), xend =  as.Date("2020-05-12"), y = 0.035, yend = 0.319, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-04-29"), xmax = as.Date("2020-05-25"), ymin = 0.319, ymax = 0.381, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-05-12"), y = 0.35, label = "Federal COVID-19\ntreatment guideline\nrecommended\nagainst HCQ use\nMay 12", size=3) + 

            annotate("segment", x =  as.Date("2020-04-24"), xend =  as.Date("2020-04-24"), y = 0.08, yend = 0.246, color="black", size=0.4)  +
            annotate("rect", xmin = as.Date("2020-04-15"), xmax = as.Date("2020-05-03"), ymin = 0.246, ymax = 0.284, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-04-24"), y = 0.265, label = "HCQ caution\nissued\nApril 24", size=3) + 

            annotate("segment", x =  as.Date("2020-06-16"), xend =  as.Date("2020-06-16"), y = 0.165, yend = 0.395, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-06-03"), xmax = as.Date("2020-06-29"), ymin = 0.395, ymax = 0.435, alpha = 1.0, size= 1.0, fill = "white", color="#d95f02") +
            annotate("text", x = as.Date("2020-06-16"), y = 0.415, label = "DEX RECOVERY trial\npress release\nJune 16", size=3) + 

            annotate("segment", x =  as.Date("2020-06-15"), xend =  as.Date("2020-06-15"), y = 0.022, yend = 0.275, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-06-03"), xmax = as.Date("2020-06-17"), ymin = 0.275, ymax = 0.314, alpha = 1.0, size= 1.0, fill = "white", color="#1b9e77") +
            annotate("text", x = as.Date("2020-06-10"), y = 0.295, label = "HCQ EUA\nrevoked\nJune 15", size=3) + 

            annotate("segment", x =  as.Date("2020-05-01"), xend =  as.Date("2020-05-01"), y = 0.02, yend = 0.20, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-04-25"), xmax = as.Date("2020-05-07"), ymin = 0.20, ymax = 0.238, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-05-01"), y = 0.219, label = "REM EUA\nissued\nMay 1", size=3) + 

            annotate("segment", x =  as.Date("2020-05-22"), xend =  as.Date("2020-05-22"), y = 0.1, yend = 0.204, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-05-14"), xmax = as.Date("2020-06-09"), ymin = 0.204, ymax = 0.266, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-05-27"), y = 0.235, label = "Preliminary results\n(Beigel et al,\nMay 22) and\n(Goldman et al,\nMay 27)", size=3) + 

            annotate("segment", x =  as.Date("2020-10-22"), xend =  as.Date("2020-10-22"), y = 0.168, yend = 0.176, color="black", size=0.4) +
            annotate("rect", xmin = as.Date("2020-10-04"), xmax = as.Date("2020-10-24"), ymin = 0.176, ymax = 0.214, alpha = 1.0, size= 1.0, fill = "white", color="#7570b3") +
            annotate("text", x = as.Date("2020-10-14"), y = 0.195, label = "FDA approved\nRemdesivir\nOctober 22", size=3) +

            geom_line(aes(color=factor(drug_type)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(drug_type), shape=factor(drug_type)), size = 4, stroke = 1.8, fill = "white", show.legend = TRUE) +
            
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = as.Date('2020-03-02'), 
                                                 to = max(week_index), 
                                                 by = "7 days"),
                         labels = c('Week 1','2','3\n\nMar','4',
                                    '1','2','3\n\nApr','4','5',
                                    '1','2','3\n\nMay','4',
                                    '1','2','3\n\nJun','4',
                                    '1','2','3\n\nJul','4','5',
                                    '1','2','3\n\nAug','4',
                                    '1','2','3\n\nSep','4',
                                    '1','2','3\n\nOct','4','5'),
                         limits = c(as.Date('2020-03-02'),as.Date('2020-10-26'))) + 
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
            scale_y_continuous(breaks=seq(0,0.51,0.05),limits=c(0,0.51), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Product (%)", 
                 x="Time (Weeks)") +
            scale_colour_manual(breaks = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                labels = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                values = c('#1b9e77','#7570b3','#d95f02'),
                                name="Drug") +
            scale_shape_manual(breaks = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                labels = c("Hydroxychloroquine", "Remdesivir", "Dexamethasone"), 
                                values = c(21, 22, 24),
                                name="Drug") 

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.defa74d5-d158-4957-9e9b-5672cb7c4b97"),
    dex_week_final=Input(rid="ri.foundry.main.dataset.2ef12e6a-8417-474e-8e93-ffab4ecea177")
)
unnamed_1 <- function(dex_week_final) {
    library(ggplot2)

    df_dex <- dex_week_final %>% 
        select(week_index, dex_percent) %>%
        mutate(dex_percent=ifelse(is.na(dex_percent),0,dex_percent),
               type = "2_DEX") %>%
        rename(percent=dex_percent)
    df_non <- df_dex %>% 
        select(week_index, percent) %>%
        mutate(percent = 1-percent,
               type="1_Non_Dex")
    df <- rbind(df_non, df_dex)

    gg <- ggplot(df, aes(fill=type, y=percent, x=week_index)) + 
            geom_bar(position="fill", stat="identity") +
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            theme_bw() +
            theme(axis.text.x = element_text(hjust=0.9, size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3)),
                  legend.title = element_text(colour="black", size=20, face="bold"), legend.text = element_text(size = 16),
                  legend.key.size = unit(16, "points")) +
            scale_fill_manual(breaks = c("1_Non_Dex", "2_DEX"), 
                              labels = c("Dexamethasone Non-use", "Dexamethasone Use"), 
                              values = c('#d95f02', '#1b9e77'),
                              name="Overall Patients") +
            
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = as.Date('2020-03-02'), 
                                                 to = max(week_index), 
                                                 by = "7 days"),
                         labels = c('Week 1','2','3\n\nMar','4',
                                    '1','2','3\n\nApr','4','5',
                                    '1','2','3\n\nMay','4',
                                    '1','2','3\n\nJun','4',
                                    '1','2','3\n\nJul','4','5',
                                    '1','2','3\n\nAug','4',
                                    '1','2','3\n\nSep','4',
                                    '1','2','3\n\nOct','4','5'),
                         limits = c(as.Date('2020-03-02'),as.Date('2020-10-26')))+
            labs(y="Dexamethasone (%)", 
                 x="Time (Weeks)")

    plot(gg)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e938bb28-87d3-4ec5-8cc3-9053caad2fd9"),
    dex_week_final_vent=Input(rid="ri.foundry.main.dataset.93f05613-ffb0-49c0-a217-47b8d458fc29")
)
unnamed_2 <- function(dex_week_final_vent) {
    library(ggplot2)
    library(scales)
    df_dex <- dex_week_final_vent %>% 
        select(week_index, dex_percent, Invasive_Ventilation) %>%
        mutate(dex_percent=ifelse(is.na(dex_percent),0,dex_percent),
               type = "2_DEX") %>%
        rename(percent=dex_percent)
    df_non <- df_dex %>% 
        select(week_index, percent, Invasive_Ventilation) %>%
        mutate(percent = 1-percent,
               type="1_Non_Dex")
    df <- rbind(df_non, df_dex) %>%
        mutate(Invasive_Ventilation = ifelse(Invasive_Ventilation==1, "Vent", "No Vent"))

    gg <- ggplot(df, aes(fill=type, y=percent, x=week_index)) + 
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
                  legend.key.size = unit(16, "points")) +
            scale_fill_manual(breaks = c("1_Non_Dex", "2_DEX"), 
                              labels = c("Dexamethasone Non-use", "Dexamethasone Use"), 
                              values = c('#d95f02', '#1b9e77'),
                              name="Overall Patients") +
            
            scale_x_date(expand = expansion(add = 0),
                         breaks = "1 month",
                         labels = date_format("%b"),
                         limits = c(as.Date('2020-03-02'),as.Date('2020-10-26')))+
            labs(y="Dexamethasone (%)", 
                 x="Time") +
            facet_grid(. ~ Invasive_Ventilation)

    plot(gg)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.ff7a7e6a-b479-430e-bd08-f7a2c800cb23"),
    dex_week_final=Input(rid="ri.foundry.main.dataset.2ef12e6a-8417-474e-8e93-ffab4ecea177"),
    dex_week_final_vent=Input(rid="ri.foundry.main.dataset.93f05613-ffb0-49c0-a217-47b8d458fc29")
)
unnamed_3 <- function(dex_week_final_vent, dex_week_final) {

    df1 <- dex_week_final %>%
            select(week_index, dex_percent) %>%
            mutate(Invasive_Ventilation = "Overall")
    df2 <- dex_week_final_vent %>%
            select(week_index, dex_percent, Invasive_Ventilation)
    df <- rbind(df1, df2)
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.28755af4-1832-4e4f-a9c6-d4aa0359def1"),
    dex_week_final_race=Input(rid="ri.vector.main.execute.22e7229e-4f9c-4c5e-807f-c174bdb3d020")
)
unnamed_4 <- function(dex_week_final_race) {
    library(ggplot2)

    df_dex <- dex_week_final_race %>% 
        select(week_index, dex_percent, race_eth) %>%
        mutate(dex_percent=ifelse(is.na(dex_percent),0,dex_percent),
               type = "2_DEX") %>%
        rename(percent=dex_percent)
    df_non <- df_dex %>% 
        select(week_index, percent, race_eth) %>%
        mutate(percent = 1-percent,
               type="1_Non_Dex")
    df <- rbind(df_non, df_dex)

    gg <- ggplot(df, aes(fill=type, y=percent, x=week_index)) + 
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
                  legend.key.size = unit(16, "points")) +
            scale_fill_manual(breaks = c("1_Non_Dex", "2_DEX"), 
                              labels = c("Dexamethasone Non-use", "Dexamethasone Use"), 
                              values = c('#d95f02', '#1b9e77'),
                              name="Overall Patients") +
            
            scale_x_date(expand = expansion(add = 0),
                         breaks = "1 month",
                         labels = date_format("%b"),
                         limits = c(as.Date('2020-03-02'),as.Date('2020-10-26')))+

            labs(y="Dexamethasone (%)", 
                 x="Time") + 
            facet_wrap(. ~ race_eth)

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.b63c2394-0b1d-4121-acb0-524d50aff3c9"),
    unnamed_3=Input(rid="ri.vector.main.execute.ff7a7e6a-b479-430e-bd08-f7a2c800cb23")
)
unnamed_5 <- function(unnamed_3) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(unnamed_3, aes(x=week_index, y=dex_percent)) +
            geom_line(aes(color=factor(Invasive_Ventilation)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(Invasive_Ventilation), shape=factor(Invasive_Ventilation)), size = 4, stroke = 1.8, fill = "white", show.legend = TRUE) +
            
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = as.Date('2020-03-02'), 
                                                 to = max(week_index), 
                                                 by = "7 days"),
                         labels = c('Week 1','2','3\n\nMar','4',
                                    '1','2','3\n\nApr','4','5',
                                    '1','2','3\n\nMay','4',
                                    '1','2','3\n\nJun','4',
                                    '1','2','3\n\nJul','4','5',
                                    '1','2','3\n\nAug','4',
                                    '1','2','3\n\nSep','4',
                                    '1','2','3\n\nOct','4','5'),
                         limits = c(as.Date('2020-03-02'),as.Date('2020-10-26'))) + 
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
            scale_y_continuous(breaks=seq(0,0.51,0.05),limits=c(0,0.51), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Dexamethasone (%)", 
                 x="Time (Weeks)") +
            scale_colour_manual(breaks = c("Overall", "1", "0"), 
                                labels = c("Overall", "Ventilation", "No Ventilation"), 
                                values = c('#1b9e77','#7570b3','#d95f02'),
                                name="Invasive Ventilation") +
            scale_shape_manual(breaks = c("Overall", "1", "0"), 
                                labels = c("Overall", "Ventilation", "No Ventilation"), 
                                values = c(21, 22, 24),
                                name="Invasive Ventilation") 
    
    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.12a89418-a96c-4322-a717-7d91c824461e"),
    dex_week_final=Input(rid="ri.foundry.main.dataset.2ef12e6a-8417-474e-8e93-ffab4ecea177"),
    dex_week_final_race=Input(rid="ri.vector.main.execute.22e7229e-4f9c-4c5e-807f-c174bdb3d020")
)
unnamed_6 <- function(dex_week_final, dex_week_final_race) {

    df1 <- dex_week_final %>%
            select(week_index, dex_percent) %>%
            mutate(race_eth = "Overall")
    df2 <- dex_week_final_race %>%
            select(week_index, dex_percent, race_eth) %>%
            filter(race_eth %in% c("1_NH_WHITE", "2_NH_BLACK"))
    df <- rbind(df1, df2)
    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.7298ffbd-9736-4c6c-adf3-3c9a0df48aa4"),
    unnamed_6=Input(rid="ri.vector.main.execute.12a89418-a96c-4322-a717-7d91c824461e")
)
unnamed_7 <- function(unnamed_6) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(unnamed_6, aes(x=week_index, y=dex_percent)) +
            geom_line(aes(color=factor(race_eth)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(race_eth), shape=factor(race_eth)), size = 4, stroke = 1.8, fill = "white", show.legend = TRUE) +
            
            scale_x_date(expand = expansion(add = 0),
                         breaks = function(week_index) seq.Date(from = as.Date('2020-03-02'), 
                                                 to = max(week_index), 
                                                 by = "7 days"),
                         labels = c('Week 1','2','3\n\nMar','4',
                                    '1','2','3\n\nApr','4','5',
                                    '1','2','3\n\nMay','4',
                                    '1','2','3\n\nJun','4',
                                    '1','2','3\n\nJul','4','5',
                                    '1','2','3\n\nAug','4',
                                    '1','2','3\n\nSep','4',
                                    '1','2','3\n\nOct','4','5'),
                         limits = c(as.Date('2020-03-02'),as.Date('2020-10-26'))) + 
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
            scale_y_continuous(breaks=seq(0,0.6,0.1),limits=c(0,0.6), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Dexamethasone (%)", 
                 x="Time (Weeks)") +
            scale_colour_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK"), 
                                labels = c("Non-Hispanic White", "Non-Hispanic Black"), 
                                values = c('#1b9e77','#7570b3'),
                                name="Race/Ethnicity") +
            scale_shape_manual(breaks = c("1_NH_WHITE", "2_NH_BLACK"), 
                                labels = c("Non-Hispanic White", "Non-Hispanic Black"), 
                                values = c(21, 22),
                                name="Race/Ethnicity") 
    
    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7599395a-f97c-425a-9dfa-be8913425f52"),
    tot_pt=Input(rid="ri.foundry.main.dataset.cd8b50c3-de3c-4296-a3d6-620c1cab8442")
)
weeks <- function(tot_pt) {
    library(dplyr)
    df <- tot_pt %>%
        select(admit_week) %>%
        distinct() %>%
        arrange(admit_week) %>%
        rename(week_index = admit_week)

    df1 <- merge(tot_pt, df) %>%
            filter(week_index >= admit_week,
                   week_index <= disch_week) %>%
            as.data.frame()
    return(df1)
    
}

