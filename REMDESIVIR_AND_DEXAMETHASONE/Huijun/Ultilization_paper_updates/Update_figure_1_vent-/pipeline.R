

@transform_pandas(
    Output(rid="ri.vector.main.execute.af132c2a-ab25-4c12-9ef5-843f7f48660d"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
Dex_pt_month <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_01 %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(month_year = format(as.Date(dex_start_date), "%Y-%m"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1321ee1c-9846-4495-96b0-2b4e181d7287"),
    Dex_pt_month=Input(rid="ri.vector.main.execute.af132c2a-ab25-4c12-9ef5-843f7f48660d"),
    tot_pt_month=Input(rid="ri.vector.main.execute.cbf17b24-f6c7-4cc4-b3af-43aa6de647c1")
)
dex_month_vent_percent <- function(tot_pt_month, Dex_pt_month) {

    library(dplyr)
    
    df <- Dex_pt_month %>%
          select(person_id, month_year, Invasive_Ventilation) %>%
          group_by(month_year, Invasive_Ventilation) %>%
          summarize(dex_patient_number = n()) 

    df1 <- tot_pt_month %>%
          select(person_id, month_year, Invasive_Ventilation) %>%
          group_by(month_year, Invasive_Ventilation) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = c("month_year", "Invasive_Ventilation"), all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4)) %>%
            mutate(dex_percent = ifelse(is.na(dex_percent), 0, dex_percent)) %>%
            filter(month_year != "2021-03")

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.90af76cd-c080-4b5e-9558-81c88797a203"),
    dex_week=Input(rid="ri.vector.main.execute.9581bfd0-52fe-4cad-a7dc-c5689a275c21")
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
    Output(rid="ri.vector.main.execute.a438808c-d678-4079-9d46-63a7f2c6c871"),
    dex_week_2=Input(rid="ri.vector.main.execute.8b421ada-3caf-40fb-ac71-70b5e2c712ff")
)
dex_pts_by_week_2 <- function(dex_week_2) {
    library(dplyr)
    df <- dex_week_2 %>%
        select(start_month) %>%
        distinct() %>%
        arrange(start_month) %>%
        rename(month_index = start_month)

    df1 <- merge(dex_week_2, df) %>%
            filter(month_index >= start_month,
                   month_index <= end_month) %>%
            as.data.frame()
    return(df1)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.9581bfd0-52fe-4cad-a7dc-c5689a275c21"),
    hos_dex=Input(rid="ri.vector.main.execute.5d82c63e-563b-495c-9b63-810b3d37c406")
)
dex_week <- function(hos_dex) {
    library(dplyr)
    library(lubridate)

    df <- hos_dex %>% 
        filter(Invasive_Ventilation==1) %>%
        select(person_id, data_partner_id, dex_start_date, dex_end_date, race_eth) %>%
        distinct() %>%
        mutate(start_week_num = cut.Date(dex_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(start_week = ymd( "2020-03-02" ) + weeks(start_week_num - 1 )) %>%
        filter(dex_start_date < '2021-02-28') %>%
        mutate(end_week_num = cut.Date(dex_end_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(end_week = ymd("2020-03-02") + weeks(end_week_num - 1 )) %>%
        select(-start_week_num, -end_week_num) %>%
        mutate(week_LOS = as.numeric(end_week)-as.numeric(start_week))

    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.8b421ada-3caf-40fb-ac71-70b5e2c712ff"),
    hos_dex_2=Input(rid="ri.vector.main.execute.77b6cb78-ae20-4fda-a63a-954ebfbd3dd9")
)
dex_week_2 <- function(hos_dex_2) {
    library(dplyr)
    library(lubridate)

    df <- hos_dex_2 %>% 
        filter(Invasive_Ventilation==1) %>%
        select(person_id, data_partner_id, dex_start_date, dex_end_date, race_eth) %>%
        distinct() %>%
        mutate(start_month = format(as.Date(dex_start_date), "%Y-%m")) %>%
        mutate(end_month = format(as.Date(dex_end_date), "%Y-%m"))

    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d7efd5d1-f524-4c22-9545-0ceb9f9d6e16"),
    dex_pts_by_week=Input(rid="ri.foundry.main.dataset.90af76cd-c080-4b5e-9558-81c88797a203"),
    weeks=Input(rid="ri.foundry.main.dataset.af89e23f-c915-4670-a048-75798e5116a1")
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
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4))%>%
            filter(week_index < as.Date("2021-03-01"))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.b194401c-9231-4f61-89e4-1a5ab5f1b758"),
    dex_pts_by_week_2=Input(rid="ri.vector.main.execute.a438808c-d678-4079-9d46-63a7f2c6c871"),
    weeks_2=Input(rid="ri.vector.main.execute.298f7aa8-0e72-4cca-bc05-2ca92afc28b1")
)
dex_week_final_2 <- function(weeks_2, dex_pts_by_week_2) {

    library(dplyr)
    
    df <- dex_pts_by_week_2 %>%
          select(person_id, month_index) %>%
          group_by(month_index) %>%
          summarize(dex_patient_number = n()) 

    df1 <- weeks_2 %>%
          select(person_id, month_index) %>%
          group_by(month_index) %>%
          summarize(total_patient_number = n()) 

    df2 <- merge(df1, df, by = "month_index", all = TRUE) %>%
            mutate(dex_percent = round(dex_patient_number/total_patient_number, 4))%>%
            filter(month_index != "2021-03") %>%
            mutate(Invasive_Ventilation=1)

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.0b497dfd-4b31-4c82-aca8-e0bc43f799c4"),
    dex_week_final=Input(rid="ri.foundry.main.dataset.d7efd5d1-f524-4c22-9545-0ceb9f9d6e16")
)
figure_1_vent <- function(dex_week_final) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(dex_week_final, aes(x=week_index, y=dex_percent)) +

            geom_line(size = 1.5, show.legend = FALSE, color='#d95f02') +
            geom_point(size = 4, stroke = 1.8, fill = "white", show.legend = TRUE, color='#d95f02', shape=24) +

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
            scale_y_continuous(breaks=seq(0,0.75,0.05),limits=c(0,0.75), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Product (%)", 
                 x="Time (Weeks)") +
            
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
                                    '1','2','3\n\nOct','4','5',
                                    '1','2','3\n\nNov','4',
                                    '1','2','3\n\nDec','4',
                                    '1','2','3\n\nJan','4','5',
                                    '1','2','3\n\nFeb','4',''),
                         limits = c(as.Date('2020-03-02'),as.Date('2021-03-01')))
            

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.dc7812a3-dc52-433a-8f3d-2e520c4ffb93"),
    dex_week_final_2=Input(rid="ri.vector.main.execute.b194401c-9231-4f61-89e4-1a5ab5f1b758")
)
figure_1_vent_2 <- function(dex_week_final_2) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(dex_week_final_2, aes(x=month_index, y=dex_percent, group=Invasive_Ventilation)) +

            geom_line(size = 1.5, show.legend = FALSE, color='#d95f02') +
            geom_point(size = 4, stroke = 1.8, fill = "white", show.legend = TRUE, color='#d95f02', shape=24) +

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
            scale_y_continuous(breaks=seq(0,0.75,0.05),limits=c(0,0.75), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Product (%)", 
                 x="Time (Weeks)") 
            

    plot(gg)
    return(NULL)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.5d82c63e-563b-495c-9b63-810b3d37c406"),
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
    Output(rid="ri.vector.main.execute.77b6cb78-ae20-4fda-a63a-954ebfbd3dd9"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
hos_dex_2 <- function(Covariate_01) {
    library(dplyr)
    df <- Covariate_01 %>%
        filter(drug_dex_v1==1) %>%
        mutate(dex_end_date = ifelse(is.na(dex_end_date), dex_start_date, dex_end_date)) %>%
        mutate(dex_end_date = as.Date(dex_end_date, origin="1970-01-01"))
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.efb0b66e-1deb-43b8-9971-56c025807167"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
tot_pt <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    df <- Covariate_01 %>% 
        filter(Invasive_Ventilation==1) %>%
        select(person_id, covid_admission, covid_discharge, data_partner_id, race_eth, IV_start_date) %>%
        mutate(admit_week_num = cut.Date(IV_start_date, breaks = "1 week", labels = FALSE)) %>%
        mutate(admit_week = ymd( "2020-03-02" ) + weeks(admit_week_num - 1 )) %>%
        mutate(disch_week_num = cut.Date(covid_discharge, breaks = "1 week", labels = FALSE)) %>%
        mutate(disch_week = ymd( "2020-03-09" ) + weeks(disch_week_num - 1 )) %>%
        select(-admit_week_num, -disch_week_num) %>%
        mutate(week_LOS = as.numeric(disch_week)-as.numeric(admit_week))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.32aafe2a-17bc-4e9f-b061-563524ec91c5"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
tot_pt_2 <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    df <- Covariate_01 %>% 
        filter(Invasive_Ventilation==1) %>%
        select(person_id, covid_admission, covid_discharge, data_partner_id, race_eth, IV_start_date) %>%
        mutate(month_year_start = format(as.Date(IV_start_date), "%Y-%m")) %>%
        mutate(month_year_end = format(as.Date(covid_discharge), "%Y-%m"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.cbf17b24-f6c7-4cc4-b3af-43aa6de647c1"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
tot_pt_month <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    df <- Covariate_01 %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation, race_eth, IV_start_date) %>%
        mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) 
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.77339e53-046c-4339-b3fe-8386bf7961ab"),
    dex_month_vent_percent=Input(rid="ri.foundry.main.dataset.1321ee1c-9846-4495-96b0-2b4e181d7287")
)
trend_dex_vent <- function(dex_month_vent_percent) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(dex_month_vent_percent, aes(x=month_year, y=dex_percent, group=Invasive_Ventilation)) +
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
    Output(rid="ri.vector.main.execute.9512f1b6-04b6-4fac-badc-22f97f8eef2c"),
    dex_month_vent_percent=Input(rid="ri.foundry.main.dataset.1321ee1c-9846-4495-96b0-2b4e181d7287")
)
trend_dex_vent_2 <- function(dex_month_vent_percent) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        
    df <- dex_month_vent_percent %>%
        filter(Invasive_Ventilation==1)

        gg <- ggplot(df, aes(x=month_year, y=dex_percent, group=Invasive_Ventilation)) +
            geom_line(size = 1.5, show.legend = FALSE, color='#d95f02') +
            geom_point(size = 4, stroke = 1.8, fill = "white", show.legend = TRUE, color='#d95f02', shape=24) +
            
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
            scale_y_continuous(breaks=seq(0,1,0.1),limits=c(0,1), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized\nwith COVID-19 Receiving Dexamethasone (%)", 
                 x="Time (Months)")
    
    plot(gg)
    return(NULL)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.af89e23f-c915-4670-a048-75798e5116a1"),
    tot_pt=Input(rid="ri.vector.main.execute.efb0b66e-1deb-43b8-9971-56c025807167")
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

@transform_pandas(
    Output(rid="ri.vector.main.execute.298f7aa8-0e72-4cca-bc05-2ca92afc28b1"),
    tot_pt_2=Input(rid="ri.vector.main.execute.32aafe2a-17bc-4e9f-b061-563524ec91c5")
)
weeks_2 <- function(tot_pt_2) {
    library(dplyr)
    library(tidyr)
    df <- tot_pt_2 %>%
        select(month_year_start) %>%
        distinct() %>%
        arrange(month_year_start) %>%
        rename(month_index = month_year_start)

    df1 <- tot_pt_2 %>%
            crossing(df) %>%
            filter(month_index >= month_year_start,
                   month_index <= month_year_end)
    return(df1)
    
}

