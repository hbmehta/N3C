

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3b159696-4aad-492f-bbbd-bdc19aa83568"),
    measurement=Input(rid="ri.foundry.main.dataset.d6054221-ee0c-4858-97de-22292458fa19")
)
SELECT measurement_concept_id, person_id, measurement_date, value_as_number
FROM measurement
where measurement_concept_id in ('3011367','3016502','3021901','3039426','36305237','37398626','40483579','40762500','40762504','40762506','40762508','42869607','44810017','4013965','4096101','4196147',
'36203184','40483539','40762503','40762507','42869598','42869603','42869608','44810015','2314047','2314049','4310328','36304254','40762499','40762501','3014295','40762502''40762505','44810014','2314048',
'3013502','4020553','36685445','36685446','44810016')
--Take concept ids from oxygen concepts

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2798606e-cf97-4482-94a8-02b1bcdb9348"),
    harmonized_qn_meas_by_day_w_scores=Input(rid="ri.foundry.main.dataset.da48cf15-4cf6-4946-9ffc-cd80223a2be5")
)
SELECT Alias, person_id, min(daily_measurement) as SPO2
FROM harmonized_qn_meas_by_day_w_scores
where lcase(Alias) like '%spo2%'
group by Alias, person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4b74f282-f742-454c-8b7e-760516d29e2e"),
    SPO2=Input(rid="ri.foundry.main.dataset.3b159696-4aad-492f-bbbd-bdc19aa83568"),
    covariate_with_steroid=Input(rid="ri.foundry.main.dataset.c50c9079-d90b-4908-ab98-2c4f876b1259")
)
SELECT o.*
FROM covariate_with_steroid c
inner join SPO2 o
    on c.person_id = o.person_id
    and o.measurement_date between c.covid_admission and c.covid_discharge
    and o.value_as_number is not NULL

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.98edb7e0-8eac-4744-84a6-67d5a11c0a9d"),
    SPO2_pts=Input(rid="ri.foundry.main.dataset.4b74f282-f742-454c-8b7e-760516d29e2e")
)
select person_id, min(value_as_number) as SPO2 
from SPO2_pts 
group by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.feb73320-6463-4239-934d-23b2096a5375"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
SELECT *
FROM concept_set_members
where codeset_id in ('33582023')

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.97f16dec-3a3b-4287-b085-e54a9fe4a853"),
    concept=Input(rid="ri.foundry.main.dataset.5cb3c4a3-327a-47bf-a8bf-daf0cafe6772")
)
SELECT *
FROM concept
WHERE /*steroids*/ 
lcase(concept_name) like '%prednisone%'
or lcase(concept_name) like '%methylprednisolone%'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ec9bf944-3f02-42b9-b0d2-b316b9479ff0"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    steroid_concept=Input(rid="ri.foundry.main.dataset.97f16dec-3a3b-4287-b085-e54a9fe4a853")
)
SELECT d.*, i.concept_name, i.domain_id, i.vocabulary_id, 
    case
        when ( lcase(concept_name) like '%prednisone%'
            or lcase(concept_name) like '%methylprednisolone%') then (1)
            end as prednisone

FROM drug_exposure d
inner join steroid_concept i
    on d.drug_concept_id = i.concept_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1c5708ab-e8fa-4bed-a7a4-c22fcda971db"),
    covariate=Input(rid="ri.foundry.main.dataset.ffd5f2a4-c65d-4f7b-8707-ac1a56c496f4"),
    steroid_exposures=Input(rid="ri.foundry.main.dataset.ec9bf944-3f02-42b9-b0d2-b316b9479ff0")
)
SELECT c.person_id, i.drug_concept_name, i.drug_exposure_start_date, c.covid_admission, c.covid_discharge, i.drug_exposure_end_date, i.prednisone
FROM covariate c 
left outer join steroid_exposures i
    on c.person_id = i.person_id
    and i.drug_exposure_start_date between c.covid_admission and c.covid_discharge

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.238589fb-b032-4726-a8c3-441b6e80ec39"),
    steroids_pts=Input(rid="ri.foundry.main.dataset.1c5708ab-e8fa-4bed-a7a4-c22fcda971db")
)
SELECT person_id, min (drug_exposure_start_date) as steroid_start_date, max (prednisone) as steroid, max(drug_exposure_end_date) as steroid_end_date
FROM steroids_pts
group by person_id

