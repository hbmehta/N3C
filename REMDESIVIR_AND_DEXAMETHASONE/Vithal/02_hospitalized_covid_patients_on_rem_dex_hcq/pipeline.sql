

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.92831814-115a-4485-a55d-298ea5a7d314"),
    covid_patients_with_earliest_diagnosis=Input(rid="ri.foundry.main.dataset.89cb609d-42a7-4959-a6fa-5e52bc2cbb5e"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91"),
    payer_plan_period=Input(rid="ri.foundry.main.dataset.2210599f-758d-467e-82ae-9c350422b2ab")
)

SELECT
drug , payer_concept_name  , patient_count as N , ( (patient_count/(SUM(patient_count) over()) ) * 100 ) as percent_n
from
(
SELECT 
distinct
'dexamethasone' as drug ,
case when payer_concept_name is null then 'Unknown' else payer_concept_name end as payer_concept_name  , count(distinct c.person_id) as patient_count
FROM 
hospitalized_covid_pts_on_dexamethasone c
left outer join payer_plan_period m on m.person_id = c.person_id
group by payer_concept_name , drug
)
union

SELECT
drug , payer_concept_name  , patient_count as N , ( (patient_count/(SUM(patient_count) over()) ) * 100 ) as percent_n
from
(
SELECT 
distinct
'remdesivir' as drug ,
case when payer_concept_name is null then 'Unknown' else payer_concept_name end as payer_concept_name  , count(distinct c.person_id) as patient_count
FROM 
hospitalized_covid_pts_on_remedesivir c
left outer join payer_plan_period m on m.person_id = c.person_id
group by payer_concept_name , drug
)
union

SELECT
drug , payer_concept_name  , patient_count as N , ( (patient_count/(SUM(patient_count) over()) ) * 100 ) as percent_n
from
(
SELECT 
distinct
'hcq' as drug ,
case when payer_concept_name is null then 'Unknown' else payer_concept_name end as payer_concept_name  , count(distinct c.person_id) as patient_count
FROM 
hospitalized_covid_pts_on_hcq c
left outer join payer_plan_period m on m.person_id = c.person_id
group by payer_concept_name , drug
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3490820e-9098-4898-8cb3-2aae8714ed46"),
    covid_patients_with_earliest_diagnosis=Input(rid="ri.foundry.main.dataset.89cb609d-42a7-4959-a6fa-5e52bc2cbb5e"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d"),
    hospitalized_patients=Input(rid="ri.foundry.main.dataset.9704f008-a8a7-49b4-a757-098b4c47ef19")
)

SELECT 
 'Patients diagnosed with COVID-19' Steps  , count( distinct person_id ) N 
FROM 
covid_patients_with_earliest_diagnosis

union

SELECT 
 'All hospitalized patients' Steps  , count( distinct person_id ) N   
FROM 
hospitalized_patients

union

SELECT 
 'Patients hospitalized with COVID-19' Steps  , count( distinct person_id ) N  
FROM 
hospitalized_covid_patients 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f749a845-6255-4c5c-9681-02d6dea7d1ff"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d")
)
SELECT
Age , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
age_group as Age , count(distinct person_id) N
FROM hospitalized_covid_patients
group by age_group
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.98268344-9e60-4456-8bcd-be917f481bd6"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d")
)
SELECT
data_partner_id , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
data_partner_id , count(distinct person_id) N
FROM hospitalized_covid_patients
group by data_partner_id
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1292e410-215b-4a68-9c46-67e3dbc598fc"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d")
)
SELECT
ethnicity , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
ethnicity , count(distinct person_id) N
FROM hospitalized_covid_patients
group by ethnicity
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.78c4fd68-67a1-492b-8941-41da72317d0e"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d")
)
SELECT
race ,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
case when ( race is null ) then 'Missing' else race end as race , count(distinct person_id) N
FROM hospitalized_covid_patients
group by race
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3a64bddd-a860-4327-b42a-aa857afc9374"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d")
)
SELECT
case 
when sex in ('MALE') then 'Male' 
when sex in ('FEMALE') then 'Female'
else 'Missing' 
end as Sex,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
sex , count(distinct person_id) N
FROM hospitalized_covid_patients
group by sex
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.89cb609d-42a7-4959-a6fa-5e52bc2cbb5e"),
    covid_19_positive_patients=Input(rid="ri.foundry.main.dataset.0f56b126-a1e5-4ab9-8edc-c49d2a11eb28")
)
SELECT 
distinct
a.person_id , a.data_partner_id , a.date
FROM covid_19_positive_patients a
where
a.date = (select min(b.date) from covid_19_positive_patients b where b.person_id = a.person_id)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f442983a-cb91-40da-ac20-a4a2c005b975"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91"),
    manifest=Input(rid="ri.foundry.main.dataset.b1e99f7f-5dcd-4503-985a-bbb28edc8f6f")
)

SELECT
drug , cdm_name , patient_count as N , ( (patient_count/(SUM(patient_count) over()) ) * 100 ) as percent_n
from
(
SELECT 
distinct
'dexamethasone' as drug ,
case when cdm_name is null then 'Unknown' else cdm_name end as cdm_name , count(distinct person_id) as patient_count
FROM 
hospitalized_covid_pts_on_dexamethasone c
left outer join manifest m on m.data_partner_id = c.data_partner_id
group by cdm_name , drug
)
union
SELECT
drug , cdm_name , patient_count as N , ( (patient_count/(SUM(patient_count) over()) ) * 100 ) as percent_n
from
(
SELECT 
distinct
'hcq' as drug ,
case when cdm_name is null then 'Unknown' else cdm_name end as cdm_name , count(distinct person_id) as patient_count
FROM 
hospitalized_covid_pts_on_hcq c
left outer join manifest m on m.data_partner_id = c.data_partner_id
group by cdm_name , drug
)
union
SELECT
drug , cdm_name , patient_count as N , ( (patient_count/(SUM(patient_count) over()) ) * 100 ) as percent_n
from
(
SELECT 
distinct
'remdesivir' as drug ,
case when cdm_name is null then 'Unknown' else cdm_name end as cdm_name , count(distinct person_id) as patient_count
FROM 
hospitalized_covid_pts_on_remedesivir c
left outer join manifest m on m.data_partner_id = c.data_partner_id
group by cdm_name , drug
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5712db1b-4f84-4126-bfa9-02af07300b98"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb")
)
SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , 'cohort' as statistic_subgroup , 'cohort' as statistic_group 
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1

union

SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , age_group as statistic_subgroup , 'age_group' as statistic_group
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.age_group

union

SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , sex as statistic_subgroup , 'sex' as statistic_group 
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.sex

union

SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , race as statistic_subgroup , 'race' as statistic_group
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.race

union

SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , data_partner_id as statistic_subgroup , 'data_partner_id' as statistic_group
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.71278a45-9394-4987-9af9-0aa12f570862"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb")
)
SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , 'cohort' as statistic_subgroup , 'cohort' as statistic_group 
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1

union

SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , age_group as statistic_subgroup , 'age_group' as statistic_group
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.age_group

union

SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , sex as statistic_subgroup , 'sex' as statistic_group 
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.sex

union

SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , race as statistic_subgroup , 'race' as statistic_group
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.race

union

SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , data_partner_id as statistic_subgroup , 'data_partner_id' as statistic_group
FROM hospitalized_covid_pts_on_dexamethasone a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d"),
    covid_patients_with_earliest_diagnosis=Input(rid="ri.foundry.main.dataset.89cb609d-42a7-4959-a6fa-5e52bc2cbb5e"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"),
    hospitalized_patients=Input(rid="ri.foundry.main.dataset.9704f008-a8a7-49b4-a757-098b4c47ef19"),
    person=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT
a.* ,
case
     when ( a.age < 18 ) then ('0-17 Years')
     when ( a.age between 18 and 34 ) then ('18-34 Years')
     when ( a.age between 35 and 49 ) then ('35-49 Years')
     when ( a.age between 50 and 64 ) then ('50-64 Years')
     when ( a.age between 65 and 74 ) then ('65-74 Years')
     when ( a.age >= 75 ) then ('>=75 Years')
     else ('Missing')
end as age_group 
from
(
SELECT 
a.person_id , a.date as date_of_earliest_covid_diagnosis , a.data_partner_id ,
b.visit_occurrence_id ,  b.visit_start_date , b.visit_end_date , b.visit_concept_name , b.admitting_source_concept_name admitted_from , b.discharge_to_concept_name discharged_to
, p.year_of_birth  , p.month_of_birth, p.day_of_birth , upper(p.gender_concept_name) as sex , upper(p.race_concept_name) as race , upper(p.ethnicity_concept_name) as ethnicity , d.death_date , 
( 
  ( year(a.date) - p.year_of_birth )
   - ( case 
        when (month(a.date) < coalesce(p.month_of_birth,1)) then 1
        when (month(a.date) = coalesce(p.month_of_birth,1) and day(a.date)<coalesce(p.day_of_birth,1)) then 1
        else 0
       end
   )   
) 
as age
FROM 
covid_patients_with_earliest_diagnosis a
inner join hospitalized_patients b on b.person_id = a.person_id and a.date between b.visit_start_date and b.visit_end_date
inner join person p on p.person_id = b.person_id
left outer join death d on d.person_id = p.person_id
where a.data_partner_id not in (224, 787, 967, 1003)
) a

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.70351956-21f1-406d-9dda-61de8910cf93"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d"),
    hospitalized_covid_pts_on_remdex_with_flags=Input(rid="ri.foundry.main.dataset.28e16470-eedd-48b1-8a5a-ab3dee6b73ae"),
    person=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT 
distinct
h.person_id , upper(p.gender_concept_name) as sex , upper(p.race_concept_name) as race , upper(p.ethnicity_concept_name) as ethnicity , 'N' REMDESIVIR , 'N' DEXAMETHASONE
FROM 
hospitalized_covid_patients h 
inner join person p on p.person_id = h.person_id
WHERE 
h.person_id not in (select person_id from hospitalized_covid_pts_on_remdex_with_flags)

union

SELECT
distinct
f.person_id ,f.sex , f.race , f.ethnicity , f.REMDESIVIR , f.DEXAMETHASONE
from 
hospitalized_covid_patients h 
inner join hospitalized_covid_pts_on_remdex_with_flags f on f.person_id = h.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d"),
    person=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT
a.* ,
case
     when ( a.age < 18 ) then ('0-17 Years')
     when ( a.age between 18 and 34 ) then ('18-34 Years')
     when ( a.age between 35 and 49 ) then ('35-49 Years')
     when ( a.age between 50 and 64 ) then ('50-64 Years')
     when ( a.age between 65 and 74 ) then ('65-74 Years')
     when ( a.age >= 75 ) then ('>=75 Years')
     else ('Missing')
end as age_group 
from
(
SELECT 
a.person_id , a.date_of_earliest_covid_diagnosis , a.data_partner_id , a.visit_occurrence_id , a.visit_concept_name , a.visit_start_date , a.visit_end_date
, d.drug_concept_name , d.drug_exposure_start_date , d.drug_exposure_start_datetime , d.drug_exposure_end_date , d.drug_exposure_end_datetime
, min(d.drug_exposure_start_datetime ) over(partition by a.person_id ) datetime_of_first_exposure
, min(to_timestamp(a.visit_start_date)) over(partition by a.person_id ) datetime_of_first_covid_hospitalization
, rank() over(partition by a.person_id order by a.visit_occurrence_id asc , d.drug_exposure_start_datetime asc ) as rank_by_visit_drug_exp_start_date_time
, ( unix_timestamp(min(d.drug_exposure_start_datetime ) over(partition by a.person_id )) - unix_timestamp(min(to_timestamp(a.visit_start_date)) over(partition by a.person_id )) ) / 60 as time_from_admission_to_first_exposure_minutes
, datediff( (min(d.drug_exposure_start_date) over(partition by a.person_id )) , (min(a.visit_start_date) over(partition by a.person_id )) ) as time_from_admission_to_first_exposure_days 
, datediff(coalesce( (max(d.drug_exposure_end_date) over(partition by a.person_id )) , ( max(a.visit_end_date) over(partition by a.person_id)) ),(min(d.drug_exposure_start_date ) over(partition by a.person_id ))) as duration_of_exposure_days
, p.year_of_birth  , p.month_of_birth, p.day_of_birth , upper(p.gender_concept_name) as sex , upper(p.race_concept_name) as race , upper(p.ethnicity_concept_name) as ethnicity , dth.death_date , 
( 
  ( year(a.date_of_earliest_covid_diagnosis) - p.year_of_birth )
   - ( case 
        when (month(a.date_of_earliest_covid_diagnosis) < coalesce(p.month_of_birth,1)) then 1
        when (month(a.date_of_earliest_covid_diagnosis) = coalesce(p.month_of_birth,1) and day(a.date_of_earliest_covid_diagnosis)<coalesce(p.day_of_birth,1)) then 1
        else 0
       end
   )   
) 
as age 
FROM 
hospitalized_covid_patients a
inner join drug_exposure d on d.person_id = a.person_id and a.visit_occurrence_id = d.visit_occurrence_id 
                            and d.drug_exposure_start_date between a.visit_start_date and a.visit_end_date
--                          and a.visit_start_date <= d.drug_exposure_end_date and a.visit_end_date >= d.drug_exposure_start_date    
inner join concept_set_members cs on upper( cs.concept_set_name ) = 'CS_PEPI_RM_DEX' and upper(cs.is_most_recent_version) = 'TRUE' and cs.concept_id = d.drug_concept_id 
                                  and upper(d.drug_concept_name) not like '%REMDESIVIR%'

inner join person p on p.person_id = a.person_id
left outer join death dth on dth.person_id = a.person_id

) a

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d"),
    person=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT
a.* ,
case
     when ( a.age < 18 ) then ('0-17 Years')
     when ( a.age between 18 and 34 ) then ('18-34 Years')
     when ( a.age between 35 and 49 ) then ('35-49 Years')
     when ( a.age between 50 and 64 ) then ('50-64 Years')
     when ( a.age between 65 and 74 ) then ('65-74 Years')
     when ( a.age >= 75 ) then ('>=75 Years')
     else ('Missing')
end as age_group 
from
(
SELECT 
a.person_id , a.date_of_earliest_covid_diagnosis , a.data_partner_id , a.visit_occurrence_id ,  a.visit_concept_name , a.visit_start_date , a.visit_end_date
, d.drug_concept_name , d.drug_exposure_start_date , d.drug_exposure_start_datetime , d.drug_exposure_end_date , d.drug_exposure_end_datetime
, min(d.drug_exposure_start_datetime ) over(partition by a.person_id ) datetime_of_first_exposure
, min(to_timestamp(a.visit_start_date)) over(partition by a.person_id ) datetime_of_first_covid_hospitalization
, rank() over(partition by a.person_id order by a.visit_occurrence_id asc , d.drug_exposure_start_datetime asc ) as rank_by_visit_drug_exp_start_date_time
, ( unix_timestamp(min(d.drug_exposure_start_datetime ) over(partition by a.person_id )) - unix_timestamp(min(to_timestamp(a.visit_start_date)) over(partition by a.person_id )) ) / 60 as time_from_admission_to_first_exposure_minutes
, datediff( (min(d.drug_exposure_start_date) over(partition by a.person_id )),(min(a.visit_start_date) over(partition by a.person_id )) ) as time_from_admission_to_first_exposure_days
, datediff(coalesce( (max(d.drug_exposure_end_date) over(partition by a.person_id )) , ( max(a.visit_end_date) over(partition by a.person_id)) ),(min(d.drug_exposure_start_date ) over(partition by a.person_id ))) as duration_of_exposure_days
, p.year_of_birth  , p.month_of_birth, p.day_of_birth , upper(p.gender_concept_name) as sex , upper(p.race_concept_name) as race , upper(p.ethnicity_concept_name) as ethnicity , dth.death_date , 
( 
  ( year(a.date_of_earliest_covid_diagnosis) - p.year_of_birth )
   - ( case 
        when (month(a.date_of_earliest_covid_diagnosis) < coalesce(p.month_of_birth,1)) then 1
        when (month(a.date_of_earliest_covid_diagnosis) = coalesce(p.month_of_birth,1) and day(a.date_of_earliest_covid_diagnosis)<coalesce(p.day_of_birth,1)) then 1
        else 0
       end
   )   
) 
as age 
FROM 
hospitalized_covid_patients a
inner join drug_exposure d on d.person_id = a.person_id and a.visit_occurrence_id = d.visit_occurrence_id 
                          and d.drug_exposure_start_date between a.visit_start_date and a.visit_end_date 
--                        and a.visit_start_date <= d.drug_exposure_end_date and a.visit_end_date >= d.drug_exposure_start_date    
inner join concept_set_members cs on upper( cs.concept_set_name ) = 'PEPI_DRUG_HYDROXYCHLOROQUINE' and upper(cs.is_most_recent_version) = 'TRUE' and cs.concept_id = d.drug_concept_id 
inner join person p on p.person_id = a.person_id
left outer join death dth on dth.person_id = a.person_id
) a

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.8c80c0de-047f-4e19-9c1a-43381546a822"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
 -- this SELECT statement will provide all the patients info who are taking remdesivir  along with dexamethasone during same hospital visit
select 
r.* , 'R' as drug_flag
from hospitalized_covid_pts_on_remedesivir r
where r.person_id in 
(
    -- this SELECT statement finds all patients who are taking remdesivir and dexamethasone during same hospital visit
SELECT 
a.person_id
FROM
hospitalized_covid_pts_on_remedesivir a
where a.person_id in ( select b.person_id from hospitalized_covid_pts_on_dexamethasone b where b.visit_occurrence_id = a.visit_occurrence_id)
)

union

 -- this SELECT statement will provide all the patients info who are taking dexamethasone along with remdesivir during same hospital visit
select 
d.* , 'D' as drug_flag
from hospitalized_covid_pts_on_dexamethasone d
where d.person_id in 
(
    -- this SELECT statement finds all patients who are taking remdesivir and dexamethasone during same hospital visit
SELECT 
a.person_id
FROM
hospitalized_covid_pts_on_dexamethasone a
where a.person_id in ( select b.person_id from hospitalized_covid_pts_on_remedesivir b where b.visit_occurrence_id = a.visit_occurrence_id)
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.28e16470-eedd-48b1-8a5a-ab3dee6b73ae"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    hospitalized_covid_pts_on_rem_dex=Input(rid="ri.foundry.main.dataset.8c80c0de-047f-4e19-9c1a-43381546a822"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)

SELECT 
distinct
a.person_id , a.sex , a.race , a.ethnicity , 'Y' AS REMDESIVIR , 'N' AS DEXAMETHASONE
FROM 
hospitalized_covid_pts_on_remedesivir a
WHERE
a.person_id not in ( select b.person_id from hospitalized_covid_pts_on_rem_dex b )

UNION

SELECT 
distinct
a.person_id , a.sex , a.race , a.ethnicity , 'N' AS REMDESIVIR , 'Y' AS DEXAMETHASONE
FROM 
hospitalized_covid_pts_on_dexamethasone a
WHERE
a.person_id not in ( select b.person_id from hospitalized_covid_pts_on_rem_dex b )

UNION

SELECT 
distinct
a.person_id , a.sex , a.race , a.ethnicity , 'Y' AS REMDESIVIR , 'Y' AS DEXAMETHASONE
FROM 
hospitalized_covid_pts_on_rem_dex a

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    death=Input(rid="ri.foundry.main.dataset.d8cc2ad4-215e-4b5d-bc80-80ffb3454875"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d"),
    person=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT
a.* ,
case
     when ( a.age < 18 ) then ('0-17 Years')
     when ( a.age between 18 and 34 ) then ('18-34 Years')
     when ( a.age between 35 and 49 ) then ('35-49 Years')
     when ( a.age between 50 and 64 ) then ('50-64 Years')
     when ( a.age between 65 and 74 ) then ('65-74 Years')
     when ( a.age >= 75 ) then ('>=75 Years')
     else ('Missing')
end as age_group 
from
(
SELECT 
a.person_id , a.date_of_earliest_covid_diagnosis , a.data_partner_id , a.visit_occurrence_id ,  a.visit_concept_name , a.visit_start_date , a.visit_end_date
, d.drug_concept_name , d.drug_exposure_start_date , d.drug_exposure_start_datetime , d.drug_exposure_end_date , d.drug_exposure_end_datetime
, min(d.drug_exposure_start_datetime ) over(partition by a.person_id ) datetime_of_first_exposure
, min(to_timestamp(a.visit_start_date)) over(partition by a.person_id ) datetime_of_first_covid_hospitalization
, rank() over(partition by a.person_id order by a.visit_occurrence_id asc , d.drug_exposure_start_datetime asc ) as rank_by_visit_drug_exp_start_date_time
, ( unix_timestamp(min(d.drug_exposure_start_datetime ) over(partition by a.person_id )) - unix_timestamp(min(to_timestamp(a.visit_start_date)) over(partition by a.person_id )) ) / 60 as time_from_admission_to_first_exposure_minutes
, datediff( (min(d.drug_exposure_start_date) over(partition by a.person_id )),(min(a.visit_start_date) over(partition by a.person_id )) ) as time_from_admission_to_first_exposure_days
, datediff(coalesce( (max(d.drug_exposure_end_date) over(partition by a.person_id )) , ( max(a.visit_end_date) over(partition by a.person_id)) ),(min(d.drug_exposure_start_date ) over(partition by a.person_id ))) as duration_of_exposure_days
, p.year_of_birth  , p.month_of_birth, p.day_of_birth , upper(p.gender_concept_name) as sex , upper(p.race_concept_name) as race , upper(p.ethnicity_concept_name) as ethnicity , dth.death_date , 
( 
  ( year(a.date_of_earliest_covid_diagnosis) - p.year_of_birth )
   - ( case 
        when (month(a.date_of_earliest_covid_diagnosis) < coalesce(p.month_of_birth,1)) then 1
        when (month(a.date_of_earliest_covid_diagnosis) = coalesce(p.month_of_birth,1) and day(a.date_of_earliest_covid_diagnosis)<coalesce(p.day_of_birth,1)) then 1
        else 0
       end
   )   
) 
as age 
FROM 
hospitalized_covid_patients a
inner join drug_exposure d on d.person_id = a.person_id and a.visit_occurrence_id = d.visit_occurrence_id 
                          and d.drug_exposure_start_date between a.visit_start_date and a.visit_end_date 
--                        and a.visit_start_date <= d.drug_exposure_end_date and a.visit_end_date >= d.drug_exposure_start_date    
inner join concept_set_members cs on upper( cs.concept_set_name ) = 'CS_PEPI_RM_DEX' and upper(cs.is_most_recent_version) = 'TRUE' and cs.concept_id = d.drug_concept_id 
                                  and upper(d.drug_concept_name) like '%REMDESIVIR%'
inner join person p on p.person_id = a.person_id
left outer join death dth on dth.person_id = a.person_id
) a

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b479eee2-1e77-4c73-a8ce-0338c3816b7c"),
    covid_patients_with_earliest_diagnosis=Input(rid="ri.foundry.main.dataset.89cb609d-42a7-4959-a6fa-5e52bc2cbb5e"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    hospitalized_covid_pts_on_rem_dex=Input(rid="ri.foundry.main.dataset.8c80c0de-047f-4e19-9c1a-43381546a822"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91"),
    hospitalized_patients=Input(rid="ri.foundry.main.dataset.9704f008-a8a7-49b4-a757-098b4c47ef19")
)
SELECT 
count( distinct person_id ) Total , 'Total_Hospitalized_Pts' Patient_Type  
FROM 
hospitalized_patients

union

SELECT 
count( distinct person_id ) Total , 'Total_Covid_Positive_pts' Patient_Type  
FROM 
covid_patients_with_earliest_diagnosis

union

SELECT 
count( distinct person_id ) Total,  'Total_hospitalized_Covid_Positive_pts' Patient_Type
FROM 
hospitalized_covid_patients 

union

SELECT 
count( distinct a.person_id ) Total ,  'Total_hospitalized_Covid_Positive_pts_on_remdesivir' Patient_Type
FROM 
hospitalized_covid_pts_on_remedesivir a
-- where a.rank_by_visit_drug_exp_start_date_time = 1

union

SELECT 
count( distinct a.person_id ) total ,  'Total_hospitalized_Covid_Positive_pts_on_dexamethasone' Patient_Type
FROM 
hospitalized_covid_pts_on_dexamethasone a
-- where a.rank_by_visit_drug_exp_start_date_time = 1

union

SELECT 
count( distinct person_id ) Total, 'Total_hospitalized_Covid_Positive_pts_on_remdesivir_and_dexamethasone' Patient_Type
FROM 
hospitalized_covid_pts_on_rem_dex

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9704f008-a8a7-49b4-a757-098b4c47ef19"),
    visit_occurrence=Input(rid="ri.foundry.main.dataset.911d0bb2-c56e-46bd-af4f-8d9611183bb7")
)
SELECT 
*
FROM visit_occurrence v
where v.visit_concept_id in ('262','8717','9201', '9203', '32037', '581379') 
                            

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.faa94d11-9d7c-46fd-8ef3-00224b1f0bad"),
    covid_patients_with_earliest_diagnosis=Input(rid="ri.foundry.main.dataset.89cb609d-42a7-4959-a6fa-5e52bc2cbb5e"),
    microvisit_to_macrovisit_lds=Input(rid="ri.foundry.main.dataset.5af2c604-51e0-4afa-b1ae-1e5fa2f4b905")
)
SELECT v.*
FROM microvisit_to_macrovisit_lds v
inner join covid_patients_with_earliest_diagnosis c
    on c.person_id = v.person_id
and DATEDIFF (c.date, v.visit_start_date) between -21 and 5
where v.visit_concept_id in ('262','8717','9201', '9203', '32037', '581379')

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2734cfb6-4fa6-44e0-be64-b49d15fda75e"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , 'cohort' as statistic_subgroup , 'cohort' as statistic_group 
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1

union

SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , age_group as statistic_subgroup , 'age_group' as statistic_group
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.age_group

union

SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , sex as statistic_subgroup , 'sex' as statistic_group 
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.sex

union

SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , race as statistic_subgroup , 'race' as statistic_group
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.race

union

SELECT 
min(a.duration_of_exposure_days) as min_duration , max(a.duration_of_exposure_days) as max_duration , mean(a.duration_of_exposure_days) as mean_duration , percentile(a.duration_of_exposure_days,0.25) as 25th_percentile,
percentile(a.duration_of_exposure_days,0.75) as 75th_percentile,  stddev(a.duration_of_exposure_days) as SD_duration , data_partner_id as statistic_subgroup , 'data_partner_id' as statistic_group
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.301e1696-f848-44f9-b4c5-389f6fd21c92"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , 'cohort' as statistic_subgroup , 'cohort' as statistic_group 
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1

union

SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , age_group as statistic_subgroup , 'age_group' as statistic_group
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.age_group

union

SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , sex as statistic_subgroup , 'sex' as statistic_group 
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.sex

union

SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , race as statistic_subgroup , 'race' as statistic_group
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.race

union

SELECT 
min(a.time_from_admission_to_first_exposure_days) as min_of_first_exp_time , max(a.time_from_admission_to_first_exposure_days) as max_of_first_exp_time , mean(a.time_from_admission_to_first_exposure_days) as mean_of_first_exp_time , percentile(a.time_from_admission_to_first_exposure_days,0.25) as 25th_percentile,
percentile(a.time_from_admission_to_first_exposure_days,0.75) as 75th_percentile,  stddev(a.time_from_admission_to_first_exposure_days) as SD_of_first_exp_time , data_partner_id as statistic_subgroup , 'data_partner_id' as statistic_group
FROM hospitalized_covid_pts_on_remedesivir a
where
a.rank_by_visit_drug_exp_start_date_time = 1
group by a.data_partner_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e1481e2b-8afa-493a-b50a-c622424c1328"),
    hospitlization_micromacro=Input(rid="ri.foundry.main.dataset.faa94d11-9d7c-46fd-8ef3-00224b1f0bad")
)
SELECT min(visit_start_date) as covid_admission, max(visit_end_date) as covid_discharge, person_id
FROM hospitlization_micromacro
    GROUP BY person_id

