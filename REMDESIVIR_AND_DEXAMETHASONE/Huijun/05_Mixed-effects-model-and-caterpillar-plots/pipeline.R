

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d4d7ef10-7e1d-4c7e-b677-fe02649a2c14"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
DEX_MEM <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr)
    library(tibble)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + race_eth*cv_quar + race_eth*Invasive_Ventilation + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate + qnorm(0.5)*Std..Error),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6e8abce3-5f30-448c-b113-50d0445b99eb"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
DEX_mem_no_interaction <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr)
    library(tibble)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate + qnorm(0.5)*Std..Error),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0d40f4d9-b921-4a6c-a909-6d7839ee9b70"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
DEX_mem_no_interaction_Vent_0 <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr)
    library(tibble)
    df <- covariate_v02 %>%
        filter(Invasive_Ventilation==0)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + cv_quar + (1|data_partner_id), 
                    data = df, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate + qnorm(0.5)*Std..Error),3),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),3),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),3)
        ) %>%
        mutate(Estimate = round(Estimate,3),
               Std..Error = round(Std..Error,3),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0f4b136b-9258-485e-aea2-34e5150a1840"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
DEX_mem_no_interaction_Vent_1 <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr)
    library(tibble)
    df <- covariate_v02 %>%
        filter(Invasive_Ventilation==1)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + cv_quar + (1|data_partner_id), 
                    data = df, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate + qnorm(0.5)*Std..Error),3),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),3),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),3)
        ) %>%
        mutate(Estimate = round(Estimate,3),
               Std..Error = round(Std..Error,3),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.aacbdc2b-8e8c-4521-90c4-8eaca1dadf09"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
DEX_pred <- function(covariate_v02) {
    library(lme4)
    library(dplyr)
    library(tibble)

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + race_eth*cv_quar + race_eth*Invasive_Ventilation + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    DEX_pred <- data.frame(dex_pred = predict(dex_mem, covariate_v02 , type = "response", allow.new.levels=TRUE))

    DEX_pred_all <- cbind(covariate_v02, DEX_pred)  %>%
                    select(person_id, data_partner_id, dex_pred)
    return(DEX_pred_all)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.16853816-f329-409d-9474-4623198a86a2"),
    HCQ_pred=Input(rid="ri.foundry.main.dataset.814b1aad-4d1d-4b24-92e1-ebfe937448bb"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
Figure_2a_hcq <- function(covariate_v02, HCQ_pred) {
    library(dplyr)
    library(tibble)
    library(ggplot2)

    Site <- HCQ_pred %>% 
            group_by(data_partner_id) %>%
            summarize(E_mean=round(mean(hcq_pred,na.rm=T),4), E_se=round(sd(hcq_pred,na.rm=T),4)) %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(O_mean= round(mean(HCQ, na.rm=T),4)),
                by="data_partner_id") %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(O_sd= round(sd(HCQ, na.rm=T),4)),
                by="data_partner_id"
                ) %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(group_number= n()),
                by="data_partner_id"
                ) %>%
            mutate(Adjusted_rate_mean = round(mean(covariate_v02$HCQ, na.rm=T)*O_mean/E_mean,4),
                    Adjusted_rate_se = round((mean(covariate_v02$HCQ, na.rm=T)*O_sd/E_mean)/sqrt(group_number),4))

    Site_1 <- Site %>%
            arrange(Adjusted_rate_mean) %>%
            mutate(row_num = as.numeric(rownames(Site)))

    p <- ggplot(Site_1, aes(row_num, Adjusted_rate_mean)) + 
            geom_point(color="darkgray") +
            geom_line(color="darkgray") +
            xlab("Site Rank") + 
            ylab("Adjusted % HCQ use") + 
            theme(legend.position="none") + 
            geom_errorbar(aes(ymin=Adjusted_rate_mean-1.96*Adjusted_rate_se, ymax=Adjusted_rate_mean+1.96*Adjusted_rate_se), colour="skyblue") + 
            scale_y_continuous(labels=scales::percent)
    plot(p)
    return(Site)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e284e9c4-ffe5-47d5-a316-f1616b9402ce"),
    REM_pred=Input(rid="ri.foundry.main.dataset.648c7032-093a-4d2e-8654-0b9c4008f216"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
Figure_2b_rem <- function(covariate_v02, REM_pred) {
    library(dplyr)
    library(tibble)
    library(ggplot2)

    Site <- REM_pred %>% 
            group_by(data_partner_id) %>%
            summarize(E_mean=round(mean(rem_pred,na.rm=T),4), E_se=round(sd(rem_pred,na.rm=T),4)) %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(O_mean= round(mean(REM, na.rm=T),4)),
                by="data_partner_id") %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(O_sd= round(sd(REM, na.rm=T),4)),
                by="data_partner_id"
                ) %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(group_number= n()),
                by="data_partner_id"
                ) %>%
            mutate(Adjusted_rate_mean = round(mean(covariate_v02$REM, na.rm=T)*O_mean/E_mean,4),
                    Adjusted_rate_se = round((mean(covariate_v02$REM, na.rm=T)*O_sd/E_mean)/sqrt(group_number),4))
            
    Site_1 <- Site %>%
            arrange(Adjusted_rate_mean) %>%
            mutate(row_num = as.numeric(rownames(Site)))

    p <- ggplot(Site_1, aes(row_num, Adjusted_rate_mean)) + 
            geom_point(color="darkgray") +
            geom_line(color="darkgray") +
            xlab("Site Rank") + 
            ylab("Adjusted % REM use") + 
            theme(legend.position="none") + 
            geom_errorbar(aes(ymin=Adjusted_rate_mean-1.96*Adjusted_rate_se, ymax=Adjusted_rate_mean+1.96*Adjusted_rate_se), colour="skyblue") + 
            scale_y_continuous(labels=scales::percent)

    plot(p)
    return(Site)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.21fd8a4b-618d-4f38-be5d-e3bbb86f965f"),
    DEX_pred=Input(rid="ri.foundry.main.dataset.aacbdc2b-8e8c-4521-90c4-8eaca1dadf09"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
Figure_2c_dex <- function(covariate_v02, DEX_pred) {
    library(dplyr)
    library(tibble)
    library(ggplot2)

    Site <- DEX_pred %>% 
            group_by(data_partner_id) %>%
            summarize(E_mean=round(mean(dex_pred,na.rm=T),4), E_se=round(sd(dex_pred,na.rm=T),4)) %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(O_mean= round(mean(DEX, na.rm=T),4)),
                by="data_partner_id") %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(O_sd= round(sd(DEX, na.rm=T),4)),
                by="data_partner_id"
                ) %>%
            left_join(
                covariate_v02 %>% group_by(data_partner_id) %>% summarize(group_number= n()),
                by="data_partner_id"
                ) %>%
            mutate(Adjusted_rate_mean = round(mean(covariate_v02$DEX, na.rm=T)*O_mean/E_mean,4),
                    Adjusted_rate_se = round((mean(covariate_v02$DEX, na.rm=T)*O_sd/E_mean)/sqrt(group_number),4))
            
    Site_1 <- Site %>%
            arrange(Adjusted_rate_mean) %>%
            mutate(row_num = as.numeric(rownames(Site)))

    p <- ggplot(Site_1, aes(row_num, Adjusted_rate_mean)) + 
            geom_point(color="darkgray") +
            geom_line(color="darkgray") +
            xlab("Site Rank") + 
            ylab("Adjusted % DEX use") + 
            theme(legend.position="none") + 
            geom_errorbar(aes(ymin=Adjusted_rate_mean-1.96*Adjusted_rate_se, ymax=Adjusted_rate_mean+1.96*Adjusted_rate_se), colour="skyblue") + 
            scale_y_continuous(labels=scales::percent) 
    plot(p)
    return(Site)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.644b2fc7-d5cb-4a83-86d1-0e5340420b03"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
HCQ_MEM <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr)

    library(tibble)
    hcq_mem <- glmer(HCQ ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(hcq_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)

}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.814b1aad-4d1d-4b24-92e1-ebfe937448bb"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
HCQ_pred <- function(covariate_v02) {
    library(lme4)
    library(dplyr)
    library(tibble)

    hcq_mem <- glmer(HCQ ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    HCQ_pred <- data.frame(hcq_pred = predict(hcq_mem, covariate_v02 , type = "response", allow.new.levels=TRUE))

    HCQ_pred_all <- cbind(covariate_v02, HCQ_pred)  %>%
                    select(person_id, data_partner_id, hcq_pred)
    return(HCQ_pred_all)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.eb12fb85-e118-46f0-9b27-72f4976378b6"),
    stddev_for_each_model=Input(rid="ri.foundry.main.dataset.6bdc25ae-eb9f-4b1a-9e05-b29b60c28a44")
)
ICC_calc <- function(stddev_for_each_model) {
    library(dplyr)
    df <- stddev_for_each_model %>%
        mutate(ICC_NULL = round(sd_NULL^2/(sd_NULL^2+(pi^2 / 3)),4),
               ICC_MEM = round(sd_MEM^2/(sd_MEM^2+(pi^2 / 3)),4))
    return(df)
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.0c337071-c9cd-4636-9424-1f82a9c853bb"),
    sd_dex_vent_no_vent=Input(rid="ri.foundry.main.dataset.edb6ab65-c782-4b22-b508-e952873d2883")
)
ICC_calc_vent <- function(sd_dex_vent_no_vent) {
    library(dplyr)
    df <- sd_dex_vent_no_vent %>%
        mutate(ICC_NULL = round(sd_NULL^2/(sd_NULL^2+(pi^2 / 3)),4),
               ICC_MEM = round(sd_MEM^2/(sd_MEM^2+(pi^2 / 3)),4))
    return(df)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.552f6bd6-81b8-4d97-91bb-301bf104eb42"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
REM_MEM <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr)
    library(tibble)

    rem_mem <- glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(rem_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    OR <- summary %>%
        mutate(OR = round(exp(Estimate + qnorm(0.5)*Std..Error),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               Std..Error = round(Std..Error,2),
               OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, OR_CI)

    return(OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.648c7032-093a-4d2e-8654-0b9c4008f216"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
REM_pred <- function(covariate_v02) {
    library(lme4)
    library(dplyr)
    library(tibble)
    df <- covariate_v02 %>%
        mutate(week_v1 = ifelse(week=="2020-03-09", "1_2020-03-09", week)) %>%
        mutate(week_v1 = as.factor(week_v1))   

    rem_mem <- glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + (1|data_partner_id), 
                    data = df, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    REM_pred <- data.frame(rem_pred = predict(rem_mem, df , type = "response", allow.new.levels=TRUE))

    REM_pred_all <- cbind(df, REM_pred)  %>%
                    select(person_id, data_partner_id, rem_pred)
    return(REM_pred_all)
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.30ffd633-4641-4f89-8853-b38bd570910e"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
dex_mem_race_cv_interaction <- function(covariate_v02) {
    library(lme4)
    library(dplyr)
    library(tibble)

    cv_1 <- covariate_v02 %>% filter(cv_quar==1) 
    cv_2 <- covariate_v02 %>% filter(cv_quar==2) 
    cv_3 <- covariate_v02 %>% filter(cv_quar==3) 
    cv_4 <- covariate_v02 %>% filter(cv_quar==4)

    m_cv_1 <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + race_eth*Invasive_Ventilation + (1|data_partner_id), data = cv_1, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    m_cv_2 <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + race_eth*Invasive_Ventilation + (1|data_partner_id), data = cv_2, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    m_cv_3 <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + race_eth*Invasive_Ventilation + (1|data_partner_id), data = cv_3, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    m_cv_4 <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + race_eth*Invasive_Ventilation + (1|data_partner_id), data = cv_4, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    
    summary_1 <- data.frame(summary(m_cv_1)$coefficients) %>%
            rownames_to_column(var = "variables")
    summary_2 <- data.frame(summary(m_cv_2)$coefficients) %>%
            rownames_to_column(var = "variables")
    summary_3 <- data.frame(summary(m_cv_3)$coefficients) %>%
            rownames_to_column(var = "variables")
    summary_4 <- data.frame(summary(m_cv_4)$coefficients) %>%
            rownames_to_column(var = "variables")

    OR_1 <- summary_1 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(cv_1_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, cv_1_OR_CI)
    OR_2 <- summary_2 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(cv_2_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, cv_2_OR_CI)

    OR_3 <- summary_3 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(cv_3_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, cv_3_OR_CI)

    OR_4 <- summary_4 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(cv_4_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, cv_4_OR_CI)
    
    final_OR <- OR_1 %>%
            left_join(OR_2, by="variables") %>%
            left_join(OR_3, by="variables") %>%
            left_join(OR_4, by="variables") 

    return(final_OR)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1f1a1fe4-8ef0-4c37-be00-b7f1ac282af0"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
dex_mem_race_vent_interaction <- function(covariate_v02) {
    library(lme4)
    library(dplyr)
    library(tibble)

    vent_0 <- covariate_v02 %>% filter(Invasive_Ventilation==0)
    vent_1 <- covariate_v02 %>% filter(Invasive_Ventilation==1)

    m_vent_0 <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + cv_quar + race_eth*cv_quar + (1|data_partner_id), data = vent_0, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    m_vent_1 <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + cv_quar + race_eth*cv_quar + (1|data_partner_id), data = vent_1, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    
    summary_0 <- data.frame(summary(m_vent_0)$coefficients) %>%
            rownames_to_column(var = "variables")
    summary_1 <- data.frame(summary(m_vent_1)$coefficients) %>%
            rownames_to_column(var = "variables")

    OR_0 <- summary_0 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(vent_0_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, vent_0_OR_CI)

    OR_1 <- summary_1 %>%
        mutate(OR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(vent_1_OR_CI = paste(OR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, vent_1_OR_CI)
    
    final_OR <- OR_0 %>%
            left_join(OR_1, by="variables")

    return(final_OR)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.e09f230a-c8b7-4904-b8b2-13c1c475be4d"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
dex_poisson_mlm <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr)
    library(tibble)

    df <- covariate_v02 %>%
            mutate(log_cv = log(center_volume))

    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = df, family = poisson(link="log"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(dex_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    RR <- summary %>%
        mutate(RR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               RR_CI = paste(RR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, RR_CI)

    return(RR)
    
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.62ec5da8-0d46-4793-8c5f-a7fd2e656d1d"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
hcq_poisson_mlm <- function(covariate_v02) {
    
    
    library(lme4)
    library(dplyr)
    library(tibble)

    df <- covariate_v02 %>%
            mutate(log_cv = log(center_volume))

    hcq_mem <- glmer(HCQ ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = df, family = poisson(link="log"), control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(hcq_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    RR <- summary %>%
        mutate(RR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               RR_CI = paste(RR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, RR_CI)

    return(RR)
    
}

@transform_pandas(
    Output(rid="ri.vector.main.execute.44ab63d5-9daa-49a3-826e-5f9b6314c5ce"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
rem_poisson_mlm <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr)
    library(tibble)
    
    df <- covariate_v02 %>%
            mutate(log_cv = log(center_volume))

    rem_mem <- glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + log_cv + (1|data_partner_id), data = df, family = "poisson", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    summary <- data.frame(summary(rem_mem)$coefficients) %>%
            rownames_to_column(var = "variables")
            
    RR <- summary %>%
        mutate(RR = round(exp(Estimate),2),
               Lower_CI = round(exp(Estimate + qnorm(0.025)*Std..Error),2),
               Upper_CI = round(exp(Estimate + qnorm(0.975)*Std..Error),2)
        ) %>%
        mutate(Estimate = round(Estimate,2),
               RR_CI = paste(RR, " (", Lower_CI, ", ", Upper_CI, ")", sep="")) %>%
        select(variables, Estimate, Std..Error, RR_CI)

    return(RR)

    
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.edb6ab65-c782-4b22-b508-e952873d2883"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
sd_dex_vent_no_vent <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr) 

    vent <- covariate_v02 %>%
            filter(Invasive_Ventilation==1) %>%
            mutate(log_cv = log(center_volume))
    no_vent <- covariate_v02 %>%
            filter(Invasive_Ventilation==0) %>%
            mutate(log_cv = log(center_volume))

    dex_vent_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex_no_vent_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = no_vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    

    dex_vent_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                    data = vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)                  
    dex_no_vent_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + week + log_cv + (1|data_partner_id), 
                    data = no_vent, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    sd <- data.frame(
        Vent = c("Vent", "No_Vent"),
        sd_NULL = c(
            attr(summary(dex_vent_null)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_no_vent_null)$varcor$data_partner_id, "stddev")
        ),
        sd_MEM = c(
            attr(summary(dex_vent_mem)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_no_vent_mem)$varcor$data_partner_id, "stddev")
        )
    )
    return(sd)
    
}

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6bdc25ae-eb9f-4b1a-9e05-b29b60c28a44"),
    covariate_v02=Input(rid="ri.foundry.main.dataset.62172c2c-691f-4e49-ac0d-c21d063ad879")
)
stddev_for_each_model <- function(covariate_v02) {
    
    library(lme4)
    library(dplyr) 

    hcq_null <- glmer(HCQ ~ (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    rem_null <- glmer(REM ~ (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    dex_null <- glmer(DEX ~ (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    

    hcq_mem <- glmer(HCQ ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)
    rem_mem <- glmer(REM ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)                  
    dex_mem <- glmer(DEX ~ age_group + sex + race_eth + Charlson_range + BMI_cat + AKI_in_hospital + ECMO + Invasive_Ventilation + week + cv_quar + race_eth*cv_quar + race_eth*Invasive_Ventilation + (1|data_partner_id), 
                    data = covariate_v02, family = "binomial", control = glmerControl(optimizer = "bobyqa"), nAGQ = 10)

    sd <- data.frame(
        Drug = c("HCQ", "REM", "DEX"),
        sd_NULL = c(
            attr(summary(hcq_null)$varcor$data_partner_id, "stddev"),
            attr(summary(rem_null)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_null)$varcor$data_partner_id, "stddev")
        ),
        sd_MEM = c(
            attr(summary(hcq_mem)$varcor$data_partner_id, "stddev"),
            attr(summary(rem_mem)$varcor$data_partner_id, "stddev"),
            attr(summary(dex_mem)$varcor$data_partner_id, "stddev")
        )
    )
    return(sd)
    
}

