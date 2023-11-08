

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.490550b8-d86a-4fa5-97e4-53b1c848a384"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    drugs_of_interest=Input(rid="ri.foundry.main.dataset.6b2810c8-f383-4b6d-bf87-3162a23cd668")
)
---Keeping rem, dex, hcq drug records (at any time)
SELECT d.*, i.concept_name, i.domain_id, i.vocabulary_id,
    case 
        when (lcase(concept_name) like '%remdesivir%' or lcase(concept_name) like '%venklury%')          then (1)
    end as drug_rem ,
    case
        when (lcase(concept_name) like '%dexamethasone%')       then (1)
    end as drug_dex , 
    case
        when (lcase(concept_name) like '%hydroxychloroquine%')  then (1)
    end as drug_hcq
FROM drug_exposure d
inner join drugs_of_interest i
    on d.drug_concept_id = i.concept_id

---Here we are only left with domain_id
---We pick up RxNorm, RxNorm Extension, and also a few HCPCS (because doxorubicin and irinotecan are injected) --> ok to leave and will not limit to RxNorm

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9df52d46-6226-416d-afa0-9edb8231d8c7"),
    All_rem_dex_hcq=Input(rid="ri.foundry.main.dataset.490550b8-d86a-4fa5-97e4-53b1c848a384"),
    covid_nearest_max_hospitalization=Input(rid="ri.foundry.main.dataset.3899499c-0771-445e-bdb2-9c43b08f1f72")
)
---PURPOSE: HCQ, REM and DEX during admission
SELECT c.person_id, i.drug_concept_name, i.drug_exposure_start_date, c.covid_admission, c.covid_discharge, i.drug_exposure_end_date, i.drug_rem, i.drug_dex, i.drug_hcq 
FROM covid_nearest_max_hospitalization c 
left outer join All_rem_dex_hcq i
    on c.person_id = i.person_id
    and i.drug_exposure_start_date between c.covid_admission and c.covid_discharge

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4f9e9258-bb57-48df-a938-03aeb7905767"),
    covid_nearest_max_hospitalization=Input(rid="ri.foundry.main.dataset.3899499c-0771-445e-bdb2-9c43b08f1f72"),
    dex_records_pt=Input(rid="ri.foundry.main.dataset.29c4177e-1c42-45e9-920a-fc3fd2ffcaff"),
    hcq_records_pt=Input(rid="ri.foundry.main.dataset.92033ce3-0745-43e7-a90d-3868aae40729"),
    rem_records_pt=Input(rid="ri.foundry.main.dataset.73e837f6-4b28-4975-840c-6e76667f1ced")
)
SELECT a.*, b.hcq_start_date, b.hcq_end_date, b.drug_hcq_v1, c.rem_start_date, c.rem_end_date, c.drug_rem_v1, d.dex_start_date, d.dex_end_date, d.drug_dex_v1
FROM covid_nearest_max_hospitalization a
left join hcq_records_pt b
    on a.person_id = b.person_id
left join rem_records_pt c
    on a.person_id = c.person_id
left join dex_records_pt d
    on a.person_id = d.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1e4ba85c-8440-477a-b2bc-ec6f50981d52"),
    cohort_v00=Input(rid="ri.foundry.main.dataset.4f9e9258-bb57-48df-a938-03aeb7905767"),
    person=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT a.*, p.year_of_birth, p.month_of_birth, p.day_of_birth , upper(p.gender_concept_name) as sex , upper(p.race_concept_name) as race , upper(p.ethnicity_concept_name) as ethnicity, p.data_partner_id
FROM cohort_v00 a
inner join person p
    on a.person_id=p.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.37c1bd33-5886-432d-8c8f-ebbdb3aba938"),
    cohort_v01=Input(rid="ri.foundry.main.dataset.1e4ba85c-8440-477a-b2bc-ec6f50981d52")
)
SELECT *
FROM cohort_v01 
where covid_admission >= '2020-02-01'  and covid_admission <= '2021-02-28'

-- b.MI, b.CHF, b.PVD, b.stroke, b.dementia, b.pulmonary, b.rheumatic, b.PUD, b.liver_mild, b.diabetes, b.dmcx, b.paralysis, b.renal, b.cancer, b.liversevere, b.mets, b.hiv, b.multiple, b.CCI_INDEX

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.26e48fac-55e6-4026-8dcd-5e23c498e990"),
    cohort_v02=Input(rid="ri.foundry.main.dataset.37c1bd33-5886-432d-8c8f-ebbdb3aba938")
)
Select *
from cohort_v02
where data_partner_id not in ('808','325') 

-- need to define age, 
-- exclude site 808, 787, 1003

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c94a2354-8bb9-4692-ba24-6d0a14ac96f4"),
    covid_nearest_hospitalization=Input(rid="ri.foundry.main.dataset.546452a8-7ab1-4900-bc14-5f9f58ec39f7")
)
select distinct person_id, covid_admission, covid_discharge, visit_occurrence_id
    from covid_nearest_hospitalization inner join (select person_id as ps_id, max(covid_admission) as max_covid_admission from covid_nearest_hospitalization group by ps_id) mn
    on (covid_nearest_hospitalization.covid_admission = mn.max_covid_admission and covid_nearest_hospitalization.person_id = mn.ps_id)
    order by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.546452a8-7ab1-4900-bc14-5f9f58ec39f7"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.41bb01fb-b080-47fc-b77f-d5af6fc1515f")
)
--Purpose: to keep the first ever and last ever hospitalization date
--Doing this because some have multiple consecutive, and some have multiple non-consecutive but in a short time frame (1-3 days apart)
--Created by Kayte Andersen on December 3, 2020

select distinct person_id, date_of_first_covid_diagnosis, visit_occurrence_id, macrovisit_start_date as covid_admission, macrovisit_end_date as covid_discharge
    from hospitalized_covid_patients inner join (select person_id as ps_id, min(diff_covid_hosp) as min_diff from hospitalized_covid_patients group by ps_id) mn
    on (hospitalized_covid_patients.diff_covid_hosp = mn.min_diff and hospitalized_covid_patients.person_id = mn.ps_id)
    order by person_id

--SELECT min(macrovisit_start_date) as covid_admission, max(macrovisit_end_date) as covid_discharge, person_id
  --  FROM hospitalized_covid_patients
  --  GROUP BY person_id

/* not doing this: we are taking min max rather than the first one in the dataset
SELECT *,ROW_NUMBER()OVER(PARTITION BY person_id ORDER BY visit_start_date) AS unique
FROM hospitalized_covid_patients a 
    WHERE visit_start_date = 
        (
            -- Taking the first hospitalization
            SELECT MIN(visit_start_date)
            FROM hospitalized_covid_patients
            WHERE person_id = a.person_id
        )
--NOTE! There are duplicates for the same person on the same day, which will need to be removed in the next step
*/

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3899499c-0771-445e-bdb2-9c43b08f1f72"),
    covid_nearest_hospitalization=Input(rid="ri.foundry.main.dataset.546452a8-7ab1-4900-bc14-5f9f58ec39f7")
)
select distinct person_id, covid_admission, covid_discharge
    from covid_nearest_hospitalization inner join (select person_id as ps_id, max(covid_admission) as max_covid_admission from covid_nearest_hospitalization group by ps_id) mn
    on (covid_nearest_hospitalization.covid_admission = mn.max_covid_admission and covid_nearest_hospitalization.person_id = mn.ps_id)
    order by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5e5bfde6-a336-46ef-920a-5779eee7ee79"),
    All_rem_dex_hcq_hospital=Input(rid="ri.foundry.main.dataset.9df52d46-6226-416d-afa0-9edb8231d8c7")
)
SELECT *
FROM All_rem_dex_hcq_hospital
where drug_dex = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.29c4177e-1c42-45e9-920a-fc3fd2ffcaff"),
    dex_records=Input(rid="ri.foundry.main.dataset.5e5bfde6-a336-46ef-920a-5779eee7ee79")
)
SELECT person_id, min (drug_exposure_start_date) as dex_start_date, max (drug_dex) as drug_dex_v1, max(drug_exposure_end_date) as dex_end_date
FROM dex_records
group by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6b2810c8-f383-4b6d-bf87-3162a23cd668"),
    concept=Input(rid="ri.foundry.main.dataset.5cb3c4a3-327a-47bf-a8bf-daf0cafe6772")
)
---rem, dex and hcq
SELECT *
FROM concept
WHERE
lcase(concept_name) like '%remdesivir%'
or lcase(concept_name) like '%dexamethasone%'
or lcase(concept_name) like '%hydroxychloroquine%'

--PEPI_DRUG_HYDROXYCHLOROQUINE
--CS_PEPI_RM_DEX
SELECT *
FROM 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.1dc228e7-a715-4372-811a-4ecf87e2e0c5"),
    All_rem_dex_hcq_hospital=Input(rid="ri.foundry.main.dataset.9df52d46-6226-416d-afa0-9edb8231d8c7")
)
SELECT *
FROM All_rem_dex_hcq_hospital
where drug_hcq = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.92033ce3-0745-43e7-a90d-3868aae40729"),
    hcq_records=Input(rid="ri.foundry.main.dataset.1dc228e7-a715-4372-811a-4ecf87e2e0c5")
)
SELECT person_id, min (drug_exposure_start_date) as hcq_start_date, max (drug_hcq) as drug_hcq_v1, max(drug_exposure_end_date) as hcq_end_date
FROM hcq_records
group by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.41bb01fb-b080-47fc-b77f-d5af6fc1515f"),
    Covid_19_positive_patients=Input(rid="ri.foundry.main.dataset.94861a8e-00bb-4410-ad92-5d49db2c7a3f"),
    inpatient=Input(rid="ri.foundry.main.dataset.ee814525-d7a3-4e24-9146-bbff65bde164"),
    unnamed=Input(rid="ri.foundry.main.dataset.ccc5a2f1-e32a-4150-8ea7-4d705793b783")
)
--Purpose: to merge the COVID+ table with hospitalized table
--Creator: Kayte modified from Hemal's code in REM/DEX "003_COVID hospitalized pts" on November 11, 2020
SELECT distinct c.*, v.macrovisit_start_date, v.macrovisit_end_date, v.visit_concept_name, v.visit_source_value, v.macrovisit_id, v.visit_occurrence_id, abs(DATEDIFF (c.date_of_first_covid_diagnosis, v.macrovisit_start_date)) as diff_covid_hosp
FROM inpatient v 
--Inner join, because we only want the hospitalized COVID+, not people who are hospitalized without COVID or outpatient COVID
inner join unnamed c
    on c.person_id = v.person_id 
--Restricting to hospitalizations within 21 from COVID diagnosis up until 5 days after hospitalization (as per Brian Garibaldi)
    and DATEDIFF (c.date_of_first_covid_diagnosis, v.macrovisit_start_date) between -21 and 5

--Note these are not distinct hospitalizations
--Scenario 1: someone diagnosed 5/12, has 3 hospitalizations recorded (5/13-5/14, 5/14-5/21, 5/15-5/21) --> consider them continuous and take earliest and latest date?
--Scenario 2: someone diagnosed 8/26/2019, hospitalized 8/26/2019 (no end date) and also 9/10/2019 (no end date)  --> IDK, taking the first and dropping second seems wrong because the readmission would indicate some sort of discharge but we don't have that information?
--Scenario 3: someone diagnosed 3/3/20201, hospitalized 3/3-3/4, then 3/7-3/8, then 3/10-3/10, then 3/12-3/14, then 3/18-3/22, then 3/24-3/24 --> consider them continuous and take earliest and latest date?

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ee814525-d7a3-4e24-9146-bbff65bde164"),
    microvisit_to_macrovisit_lds=Input(rid="ri.foundry.main.dataset.5af2c604-51e0-4afa-b1ae-1e5fa2f4b905")
)
SELECT *
FROM microvisit_to_macrovisit_lds
where macrovisit_id IS NOT NULL

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.16259ee7-9346-4816-b71f-139157686675"),
    cohort_v04=Input(rid="ri.foundry.main.dataset.c00cc87e-6081-4cb7-9bac-7b3fa8b92b46"),
    concept_set_members_1=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    condition_occurrence_1=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86")
)
SELECT
distinct
x.* ,
(x.MI*1 + x.CHF*1 + x.PVD*1 + x.stroke*1 + x.dementia*1 + x.pulmonary*1 + x.rheumatic*1 + x.PUD*1 + x.liver_mild*1 + x.diabetes*1 + x.dmcx*2 + x.paralysis*2 + x.renal*2 + x.cancer*2 
 + x.liversevere*3 + x.mets*6 + x.hiv*6) CCI_INDEX
FROM
(
SELECT
distinct
    person_id, 
    sum(case when comorbidity = 'MI' then 1 else 0 end) MI ,
    sum(case when comorbidity = 'CHF' then 1 else 0 end) CHF ,
    sum(case when comorbidity = 'PVD' then 1 else 0 end) PVD ,
    sum(case when comorbidity = 'Stroke' then 1 else 0 end) stroke ,
    sum(case when comorbidity = 'Dementia' then 1 else 0 end) dementia ,
    sum(case when comorbidity = 'Pulmonary' then 1 else 0 end) pulmonary ,
    sum(case when comorbidity = 'Rheumatic' then 1 else 0 end) rheumatic ,
    sum(case when comorbidity = 'PUD' then 1 else 0 end) PUD ,
    sum(case when comorbidity = 'LiverMild' then 1 else 0 end) liver_mild ,
    sum(case when comorbidity = 'DM' then 1 else 0 end) diabetes ,
    sum(case when comorbidity = 'DMcx' then 1 else 0 end) dmcx ,
    sum(case when comorbidity = 'Paralysis' then 1 else 0 end) paralysis ,
    sum(case when comorbidity = 'Renal' then 1 else 0 end) renal ,
    sum(case when comorbidity = 'Cancer' then 1 else 0 end) cancer ,
    sum(case when comorbidity = 'LiverSevere' then 1 else 0 end) liversevere ,
    sum(case when comorbidity = 'Mets' then 1 else 0 end) mets ,   
    sum(case when comorbidity = 'HIV' then 1 else 0 end) hiv,    
    case when count(*) > 1 then 1 else 0 end multiple
FROM (
SELECT 
distinct
cp.person_id ,
replace(cs.concept_set_name, 'Charlson - ','') comorbidity 
FROM 
cohort_v04 cp
left outer join condition_occurrence_1 co on ( cp.person_id = co.person_id and co.condition_start_date <= cp.covid_discharge and cp.one_year_prior <= co.condition_start_date )
left outer join concept_set_members_1 cs on ( cs.concept_id = co.condition_source_concept_id or cs.concept_id = co.condition_concept_id )
and cs.is_most_recent_version = true
    and cs.codeset_id in ( 535274723, 359043664, 78746470, 719585646, 403438288, 73549360, 494981955, 248333963, 378462283, 259495957, 489555336, 510748896, 514953976, 376881697, 
    220495690, 765004404, 652711186
    )
) t
group by t.person_id
) x

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4cb8cd68-c196-455c-a3f0-4a62af9309bc"),
    All_rem_dex_hcq_hospital=Input(rid="ri.foundry.main.dataset.9df52d46-6226-416d-afa0-9edb8231d8c7")
)
SELECT *
FROM All_rem_dex_hcq_hospital
where drug_rem = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.73e837f6-4b28-4975-840c-6e76667f1ced"),
    rem_records=Input(rid="ri.foundry.main.dataset.4cb8cd68-c196-455c-a3f0-4a62af9309bc")
)
SELECT person_id, min (drug_exposure_start_date) as rem_start_date, max (drug_rem) as drug_rem_v1, max(drug_exposure_end_date) as rem_end_date
FROM rem_records
group by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ccc5a2f1-e32a-4150-8ea7-4d705793b783"),
    Covid_19_positive_patients=Input(rid="ri.foundry.main.dataset.94861a8e-00bb-4410-ad92-5d49db2c7a3f")
)
SELECT *
FROM Covid_19_positive_patients
where date_of_first_covid_diagnosis <= '2021-03-05'

