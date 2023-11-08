

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4f69af7f-a23b-49fb-a760-8e60885adb5a"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d")
)
SELECT 
distinct
person_id, sex , race , ethnicity , age , age_group , data_partner_id ,
case when datediff(drug_exposure_start_date,visit_start_date) = 0 then 1 else 0 end as 0_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 1 then 1 else 0 end as 1_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 2 then 1 else 0 end as 2_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 3 then 1 else 0 end as 3_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 4 then 1 else 0 end as 4_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 5 then 1 else 0 end as 5_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 6 then 1 else 0 end as 6_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 7 then 1 else 0 end as 7_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 8 then 1 else 0 end as 8_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 9 then 1 else 0 end as 9_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 10 then 1 else 0 end as 10_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 11 then 1 else 0 end as 11_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 12 then 1 else 0 end as 12_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 13 then 1 else 0 end as 13_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 14 then 1 else 0 end as 14_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 15 then 1 else 0 end as 15_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 16 then 1 else 0 end as 16_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 17 then 1 else 0 end as 17_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 18 then 1 else 0 end as 18_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 19 then 1 else 0 end as 19_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 20 then 1 else 0 end as 20_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 21 then 1 else 0 end as 21_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 22 then 1 else 0 end as 22_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 23 then 1 else 0 end as 23_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 24 then 1 else 0 end as 24_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 25 then 1 else 0 end as 25_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 26 then 1 else 0 end as 26_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 27 then 1 else 0 end as 27_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 28 then 1 else 0 end as 28_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 29 then 1 else 0 end as 29_days ,
case when datediff(drug_exposure_start_date,visit_start_date) = 30 then 1 else 0 end as 30_days ,
case when datediff(drug_exposure_start_date,visit_start_date) >= 31 then 1 else 0 end as 31_days
FROM hospitalized_covid_pts_on_hcq
order by 1 asc

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.dfb9c31e-e8bb-43b5-b6f6-b3e4d82eb575"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d")
)
SELECT
distinct 
person_id, sex , race , ethnicity , age , age_group , data_partner_id , 
case
 when datediff(drug_exposure_start_date,visit_start_date) = 0 then "0_days" 
 when datediff(drug_exposure_start_date,visit_start_date) = 1 then "1_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 2 then "2_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 3 then "3_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 4 then "4_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 5 then "5_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 6 then "6_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 7 then "7_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 8 then "8_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 9 then "9_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 10 then "10_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 11 then "11_days" 
 when datediff(drug_exposure_start_date,visit_start_date) = 12 then "12_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 13 then "13_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 14 then "14_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 15 then "15_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 16 then "16_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 17 then "17_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 18 then "18_days" 
 when datediff(drug_exposure_start_date,visit_start_date) = 19 then "19_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 20 then "20_days"
 when datediff(drug_exposure_start_date,visit_start_date) = 21 then "21_days" 
 when datediff(drug_exposure_start_date,visit_start_date) = 22 then "22_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 23 then "23_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 24 then "24_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 25 then "25_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 26 then "26_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 27 then "27_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 28 then "28_days" 
 when datediff(drug_exposure_start_date,visit_start_date) = 29 then "29_days"  
 when datediff(drug_exposure_start_date,visit_start_date) = 30 then "30_days"
 when datediff(drug_exposure_start_date,visit_start_date) >= 31 then "31_or_more_days" 
 else null  
end as utilization
FROM hospitalized_covid_pts_on_hcq

