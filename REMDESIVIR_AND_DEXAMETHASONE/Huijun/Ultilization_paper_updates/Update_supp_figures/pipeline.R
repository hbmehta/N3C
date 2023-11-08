

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e6c9c5fe-952e-403c-b05c-840476f0b30c"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.59f8145d-4ec8-4446-9cf8-ed91429bebea"),
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
    Output(rid="ri.foundry.main.dataset.af0fe6ec-a6a8-47b8-8742-dac3b1eb9773"),
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
    Output(rid="ri.foundry.main.dataset.3f31ab73-8f57-4955-8777-e26b1c161fe2"),
    Covariate_with_death=Input(rid="ri.foundry.main.dataset.e6c9c5fe-952e-403c-b05c-840476f0b30c")
)
afterJulVent <- function(Covariate_with_death) {
    library(dplyr)
    df <- Covariate_with_death %>%
        mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) %>%
        #filter(month_year >= "2020-07" & month_year <= "2021-02") %>%
        #filter(Invasive_Ventilation==1) %>%
        filter(DEX==1 | steroid==1)
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.59e1b4b0-c619-46d1-a7ee-a2aba4bb29dd"),
    Dex_pt_month=Input(rid="ri.foundry.main.dataset.af0fe6ec-a6a8-47b8-8742-dac3b1eb9773"),
    tot_pt_month=Input(rid="ri.foundry.main.dataset.c6aa9789-9c04-4cf7-a1e3-555d7e3f60d1")
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
            mutate(dex_percent = ifelse(is.na(dex_percent), 0, dex_percent))  %>%
            filter(month_year != "2021-03")

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.ad81363a-98f1-4ef2-b8cc-07faeb6184ae"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34"),
    tot_pt_month=Input(rid="ri.foundry.main.dataset.c6aa9789-9c04-4cf7-a1e3-555d7e3f60d1")
)
dex_rem_month_percent <- function(tot_pt_month, rem_and_dex) {

    library(dplyr)
    library(lubridate)
    
    df <- rem_and_dex %>%
          filter(rem_dex==1) %>%
          mutate(month_year = format(as.Date(dex_start_date), "%Y-%m")) %>%
          select(person_id, month_year) %>%
          group_by(month_year) %>%
          summarize(dex_rem_patient_number = n()) 

    df1 <- tot_pt_month %>%
          select(person_id, month_year) %>%
          group_by(month_year) %>%
          summarize(total_patient_number = n())

    df2 <- merge(df1, df, by = "month_year", all = TRUE) %>%
            mutate(dex_rem_percent = round(dex_rem_patient_number/total_patient_number, 4)) %>%
            mutate(dex_rem_percent = ifelse(is.na(dex_rem_percent), 0, dex_rem_percent)) %>%
            filter(month_year != "2021-03")

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.17d82a8b-8787-4b75-89a4-ad48fc05a732"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.59f8145d-4ec8-4446-9cf8-ed91429bebea")
)
dex_steroid <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_with_steroid %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month_year = format(as.Date(dex_start_date), "%Y-%m"))
    return(df)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ce51ce64-cc13-4085-bf47-30f3f15d33e9"),
    exclude_pts_less_2_days=Input(rid="ri.foundry.main.dataset.f075e738-05e8-4f1b-8b4d-079a8c761a35")
)
dex_steroid_2 <- function(exclude_pts_less_2_days) {
    library(dplyr)
    library(lubridate)

    df <- exclude_pts_less_2_days %>%
        filter(drug_dex_v1==1) %>%
        select(person_id, dex_start_date, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month_year = format(as.Date(dex_start_date), "%Y-%m"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.df8df10c-56df-4864-998a-ac71ca5c7ccc"),
    dex_steroid=Input(rid="ri.foundry.main.dataset.17d82a8b-8787-4b75-89a4-ad48fc05a732"),
    steroid=Input(rid="ri.foundry.main.dataset.7fa5b36a-aa9b-4674-b912-77e3e2e5fa75"),
    tot_steroid=Input(rid="ri.foundry.main.dataset.a0271057-e789-4e6c-9a41-532e272e71f8")
)
dex_steroid_vent <- function(tot_steroid, dex_steroid, steroid) {

    library(dplyr)
    
    dex <- dex_steroid %>%
          select(person_id, month_year, Invasive_Ventilation) %>%
          group_by(month_year, Invasive_Ventilation) %>%
          summarize(dex_patient_number = n()) 

    no_dex_steroid <- steroid %>%
          select(person_id, month_year, Invasive_Ventilation) %>%
          group_by(month_year, Invasive_Ventilation) %>%
          summarize(steroid_patient_number = n()) 

    tot <- tot_steroid %>%
          select(person_id, month_year, Invasive_Ventilation) %>%
          group_by(month_year, Invasive_Ventilation) %>%
          summarize(total_patient_number = n())

    df2 <- tot %>%
            left_join(dex, by = c("month_year", "Invasive_Ventilation")) %>%
            left_join(no_dex_steroid, by = c("month_year", "Invasive_Ventilation")) %>%
            mutate(dex_patient_number = ifelse(is.na(dex_patient_number), 0, dex_patient_number),
                   steroid_patient_number = ifelse(is.na(steroid_patient_number), 0, steroid_patient_number)) %>%
            mutate(c_dex_percent = round(dex_patient_number/total_patient_number, 4),
                    b_steroid_percent = round(steroid_patient_number/total_patient_number, 4),
                    a_none_percent = round(1-c_dex_percent-b_steroid_percent, 4)) %>%
            filter(!month_year %in% c("2021-03","2020-02","2020-03","2020-04","2020-05","2020-06"))

    return(df2)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.052afafa-c3e4-4020-91f6-1821e6025381"),
    dex_steroid_2=Input(rid="ri.foundry.main.dataset.ce51ce64-cc13-4085-bf47-30f3f15d33e9"),
    steroid_2=Input(rid="ri.foundry.main.dataset.9d6b412b-aeec-466a-a446-49e183e2496a"),
    tot_2=Input(rid="ri.foundry.main.dataset.34e49d8d-b40e-444d-82ed-02f84ca65ef3")
)
dex_steroid_vent_2day <- function(tot_2, dex_steroid_2, steroid_2) {

    library(dplyr)
    
    dex <- dex_steroid_2 %>%
          filter(Invasive_Ventilation==1) %>%
          select(person_id, month_year) %>%
          group_by(month_year) %>%
          summarize(dex_patient_number = n()) 

    no_dex_steroid <- steroid_2 %>%
          filter(Invasive_Ventilation==1) %>%
          select(person_id, month_year) %>%
          group_by(month_year) %>%
          summarize(steroid_patient_number = n()) 

    tot <- tot_2 %>%
          filter(Invasive_Ventilation==1) %>%
          select(person_id, month_year) %>%
          group_by(month_year) %>%
          summarize(total_patient_number = n())

    df2 <- tot %>%
            left_join(dex, by = "month_year") %>%
            left_join(no_dex_steroid, by = "month_year") %>%
            mutate(dex_patient_number = ifelse(is.na(dex_patient_number), 0, dex_patient_number),
                   steroid_patient_number = ifelse(is.na(steroid_patient_number), 0, steroid_patient_number)) %>%
            mutate(c_dex_percent = round(dex_patient_number/total_patient_number, 4),
                    b_steroid_percent = round(steroid_patient_number/total_patient_number, 4),
                    a_none_percent = round(1-c_dex_percent-b_steroid_percent, 4)) %>%
            filter(!month_year %in% c("2021-03","2020-02","2020-03","2020-04","2020-05","2020-06"))

    return(df2)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.f7d90287-9491-489d-b53e-af09418001eb"),
    dex_rem_month_percent=Input(rid="ri.vector.main.execute.ad81363a-98f1-4ef2-b8cc-07faeb6184ae")
)
efig2 <- function(dex_rem_month_percent) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        
    df <- dex_rem_month_percent %>%
        mutate(dum=1)

        gg <- ggplot(df, aes(x=month_year, y=dex_rem_percent, group=dum)) +
            geom_line(size = 1.5, show.legend = FALSE, color = "#1b9e77") +
            geom_point(size = 4, stroke = 1.8, fill = "white", show.legend = TRUE, color = "#1b9e77", shape = 21) +
            theme_bw() +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=15),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  axis.title.y = element_text(size = 16, face = "bold", margin = margin(t = 3, r = 20, b = 3, l = 5)),
                  axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 20, r = 3, b = 5, l = 3))) +
            scale_x_discrete(labels=c("Feb2020", "Mar2020", "Apr2020", "May2020", "Jun2020", "Jul2020", "Aug2020", "Sep2020", "Oct2020", "Nov2020", "Dec2020", "Jan2021", "Feb2021")) +
            scale_y_continuous(breaks=seq(0,0.3,0.05),limits=c(0,0.3), labels=scales::label_percent(accuracy = 1L)) +
            labs(y="Percent of Individuals Hospitalized with COVID-19\nReceivingBoth Dexamethasone and Remdesivir (%)", 
                 x="Time (Months)")
    plot(gg)
    return(NULL)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.adcf7352-a4fb-46d4-b1a1-45bb1399372c"),
    dex_month_vent_percent=Input(rid="ri.foundry.main.dataset.59e1b4b0-c619-46d1-a7ee-a2aba4bb29dd")
)
efig3 <- function(dex_month_vent_percent) {
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(grid)
        

        gg <- ggplot(dex_month_vent_percent, aes(x=month_year, y=dex_percent, group=factor(Invasive_Ventilation))) +
            geom_line(aes(color=factor(Invasive_Ventilation)), size = 1.5, show.legend = FALSE) +
            geom_point(aes(color = factor(Invasive_Ventilation), shape=factor(Invasive_Ventilation)), size = 4, stroke = 1.8, fill = "white", show.legend = TRUE) +
            
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
            scale_x_discrete(labels=c("Feb2020", "Mar2020", "Apr2020", "May2020", "Jun2020", "Jul2020", "Aug2020", "Sep2020", "Oct2020", "Nov2020", "Dec2020", "Jan2021", "Feb2021")) +
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
    Output(rid="ri.vector.main.execute.ead72411-e63b-4711-b6de-a69d5bb3c2e9"),
    dex_steroid_vent=Input(rid="ri.foundry.main.dataset.df8df10c-56df-4864-998a-ac71ca5c7ccc")
)
efig4a <- function(dex_steroid_vent) {
    library(tidyverse) 
    library(dplyr)
    library(scales)

    df <- dex_steroid_vent %>%
            select(month_year, Invasive_Ventilation, c_dex_percent, b_steroid_percent, a_none_percent) %>%
            pivot_longer(c_dex_percent:a_none_percent, names_to = "drug_type", values_to = "percent") %>%
            filter(Invasive_Ventilation==1)
    

    gg <- ggplot(df, aes(fill=drug_type, y=percent, x=month_year)) + 
            geom_bar(position="fill", stat="identity") +
            scale_x_discrete(labels=c("Jul2020", "Aug2020", "Sep2020", "Oct2020", "Nov2020", "Dec2020", "Jan2021", "Feb2021")) +
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
            
            labs(y="Drug Use (%)", 
                 x="Month") 

    plot(gg)
    return(df)

    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.fbca2223-23b7-46d8-800b-0f13bcd7b473"),
    dex_steroid_vent_2day=Input(rid="ri.foundry.main.dataset.052afafa-c3e4-4020-91f6-1821e6025381")
)
efig4b <- function(dex_steroid_vent_2day) {
    library(tidyverse) 
    library(dplyr)
    library(scales)

    df <- dex_steroid_vent_2day %>%
            select(month_year, c_dex_percent, b_steroid_percent, a_none_percent) %>%
            pivot_longer(c_dex_percent:a_none_percent, names_to = "drug_type", values_to = "percent") 
    

    gg <- ggplot(df, aes(fill=drug_type, y=percent, x=month_year)) + 
            geom_bar(position="fill", stat="identity") +
            scale_x_discrete(labels=c("Jul2020", "Aug2020", "Sep2020", "Oct2020", "Nov2020", "Dec2020", "Jan2021", "Feb2021")) +
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
            labs(y="Drug Use (%)", 
                 x="Month") 

    plot(gg)
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.c6fe09b3-a562-4f88-bf9d-024a62c7e6a4"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
)
efig6a <- function(rem_and_dex) {
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
          scale_y_continuous(breaks=seq(0,30000,1000), limit=c(0,30000), labels=comma_format()) +
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
    Output(rid="ri.vector.main.execute.c96cab03-0225-422e-9cc1-92f34ab05cdb"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
)
efig6b <- function(rem_and_dex) {
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
          scale_x_continuous(breaks=c(1,5,10,15,20,25), limit=c(0,25)) +
          scale_y_continuous(breaks=seq(0,10000,500), limit=c(0,10000), labels=comma_format()) +
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
    Output(rid="ri.vector.main.execute.ee2924e3-c48c-45ed-bb24-74bb5b37e257"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
)
efig7a <- function(rem_and_dex) {

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
    Output(rid="ri.vector.main.execute.85abf2e9-1840-471a-bf59-efddffc97fc4"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
)
efig7b <- function(rem_and_dex) {

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

@transform_pandas(
    Output(rid="ri.vector.main.execute.25ffbd9f-663e-4a32-a7eb-e7bf4dc8becf"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
)
efig8a <- function(rem_and_dex) {
    
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
            scale_x_discrete(labels=seq(1,43,1))
        
        plot(gg)
        return(df)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.98019b06-5d86-4582-b1a0-5d4f538b87d1"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
)
efig8b <- function(rem_and_dex) {
    
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
            scale_x_discrete(labels=seq(1,43,1))
        
        plot(gg)
        return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f075e738-05e8-4f1b-8b4d-079a8c761a35"),
    Covariate_with_death=Input(rid="ri.foundry.main.dataset.e6c9c5fe-952e-403c-b05c-840476f0b30c")
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
    Output(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
rem_and_dex <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    df <- Covariate_01 %>%
        mutate(
            rem_dex = ifelse(drug_dex_v1==1&drug_rem_v1==1&format(as.Date(rem_start_date), "%Y-%m")==format(as.Date(dex_start_date), "%Y-%m"),1,0),
            time_to_dex_use = as.numeric(dex_start_date - covid_admission),
            dex_duration = ifelse(is.na(dex_end_date)|dex_end_date<dex_start_date, 1,as.numeric(dex_end_date-dex_start_date))
            )
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7fa5b36a-aa9b-4674-b912-77e3e2e5fa75"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.59f8145d-4ec8-4446-9cf8-ed91429bebea")
)
steroid <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)

    df <- Covariate_with_steroid %>%
        filter(is.na(drug_dex_v1)) %>%
        filter(Steroid_flag==1) %>%
        select(person_id, steroid_start_date, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month_year = format(as.Date(steroid_start_date), "%Y-%m"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9d6b412b-aeec-466a-a446-49e183e2496a"),
    exclude_pts_less_2_days=Input(rid="ri.foundry.main.dataset.f075e738-05e8-4f1b-8b4d-079a8c761a35")
)
steroid_2 <- function(exclude_pts_less_2_days) {
    library(dplyr)
    library(lubridate)

    df <- exclude_pts_less_2_days %>%
        filter(is.na(drug_dex_v1)) %>%
        filter(Steroid_flag==1) %>%
        select(person_id, steroid_start_date, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month_year = format(as.Date(steroid_start_date), "%Y-%m"))
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.34e49d8d-b40e-444d-82ed-02f84ca65ef3"),
    exclude_pts_less_2_days=Input(rid="ri.foundry.main.dataset.f075e738-05e8-4f1b-8b4d-079a8c761a35")
)
tot_2 <- function(exclude_pts_less_2_days) {
    library(dplyr)
    library(lubridate)
    tot <- exclude_pts_less_2_days %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) 
    return(tot)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c6aa9789-9c04-4cf7-a1e3-555d7e3f60d1"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5")
)
tot_pt_month <- function(Covariate_01) {
    library(dplyr)
    library(lubridate)
    df <- Covariate_01 %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation, race_eth) %>%
        mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) 
    return(df)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a0271057-e789-4e6c-9a41-532e272e71f8"),
    Covariate_with_steroid=Input(rid="ri.foundry.main.dataset.59f8145d-4ec8-4446-9cf8-ed91429bebea")
)
tot_steroid <- function(Covariate_with_steroid) {
    library(dplyr)
    library(lubridate)
    tot <- Covariate_with_steroid %>% 
        select(person_id, covid_admission, data_partner_id, Invasive_Ventilation, race_eth, Steroid_flag) %>%
        mutate(month_year = format(as.Date(covid_admission), "%Y-%m")) 
    return(tot)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.7f703c88-93e0-4263-bf94-351a1a8d75ec"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
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
    Output(rid="ri.vector.main.execute.f28fab8a-39b9-4701-9d78-e4af48b88c9f"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
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
    Output(rid="ri.vector.main.execute.51ddeeb4-e30f-4bc8-a4b3-975180ea7a1d"),
    rem_and_dex=Input(rid="ri.foundry.main.dataset.7528220e-8d3b-4fba-8818-8c9757890a34")
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

