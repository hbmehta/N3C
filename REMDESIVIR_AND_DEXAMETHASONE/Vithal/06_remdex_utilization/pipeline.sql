

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b2bb03c9-f474-499b-a8ae-c9ef3674c8cc"),
    hospitalized_covid_pts_on_rem_dex=Input(rid="ri.foundry.main.dataset.8c80c0de-047f-4e19-9c1a-43381546a822")
)
SELECT 
distinct
person_id, sex , race , ethnicity , age , age_group , data_partner_id , drug_flag ,
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
FROM hospitalized_covid_pts_on_rem_dex

@transform_pandas(
    Output(rid="ri.vector.main.execute.2417745e-0581-451b-9130-6c0ed797d07f"),
    hospitalized_covid_pts_on_rem_dex=Input(rid="ri.foundry.main.dataset.8c80c0de-047f-4e19-9c1a-43381546a822")
)
SELECT *
FROM hospitalized_covid_pts_on_rem_dex

