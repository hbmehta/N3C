

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f8e342eb-2b34-4bf9-898c-73d1d1e5ca4b"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    drugs_of_interest=Input(rid="ri.foundry.main.dataset.528c83fb-72c9-4d5e-b402-2f73b6613c4c")
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
    Output(rid="ri.foundry.main.dataset.4f0de965-fd21-4b59-99ef-871eafca5d3e"),
    All_rem_dex_hcq=Input(rid="ri.foundry.main.dataset.f8e342eb-2b34-4bf9-898c-73d1d1e5ca4b"),
    covid_nearest_max_hospitalization=Input(rid="ri.foundry.main.dataset.0cde7bff-a60e-4a57-98e4-8e1682b1ba8a")
)
---PURPOSE: HCQ, REM and DEX during admission
SELECT c.person_id, i.drug_concept_name, i.drug_exposure_start_date, c.covid_admission, c.covid_discharge, i.drug_exposure_end_date, i.drug_rem, i.drug_dex, i.drug_hcq 
FROM covid_nearest_max_hospitalization c 
left outer join All_rem_dex_hcq i
    on c.person_id = i.person_id
    and i.drug_exposure_start_date between c.covid_admission and c.covid_discharge

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5b26ab0d-68b6-4a20-97f8-3843599da8f3"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    drugs_of_interest_v2=Input(rid="ri.foundry.main.dataset.b6c572f7-494c-4f98-a447-31983a16a6f3")
)
---Keeping rem, dex, hcq drug records (at any time)
SELECT d.*
FROM drug_exposure d
inner join drugs_of_interest_v2 i
    on d.drug_concept_id = i.concept_id

---Here we are only left with domain_id
---We pick up RxNorm, RxNorm Extension, and also a few HCPCS (because doxorubicin and irinotecan are injected) --> ok to leave and will not limit to RxNorm

--3,493,563 rows

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d5bb250f-bcef-4a13-9671-fbd52e10c8ca"),
    visit_occurrence=Input(rid="ri.foundry.main.dataset.911d0bb2-c56e-46bd-af4f-8d9611183bb7")
)
---List from Ben Amor (https://unite.nih.gov/workspace/report/ri.report.main.report.87d07942-85d0-4e97-b472-4530593d6a8d, Safe Harbor --> Documentation --> Finding COVID visits)
SELECT *
FROM visit_occurrence v
WHERE v.visit_concept_id in ('262','8717','9201', '9203', '32037', '581379')
---These are all hospitalizations, not just COVID ones

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a5c92b6e-ae92-4682-b476-82c92bf18f4d"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    tylenol_concept=Input(rid="ri.foundry.main.dataset.abc62eb5-27c3-4232-9741-475d6090f0fd")
)
---Keeping tylenol
SELECT d.*, i.concept_name, i.domain_id, i.vocabulary_id,
    case 
        when (lcase(concept_name) like '%tylenol%' or lcase(concept_name) like '%acetaminophen%')          then (1)
    end as drug_tylenol 
FROM drug_exposure d
inner join tylenol_concept i
    on d.drug_concept_id = i.concept_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.94b70084-4c79-4abc-bb35-4f30e5b39e52"),
    all_tylenol=Input(rid="ri.foundry.main.dataset.a5c92b6e-ae92-4682-b476-82c92bf18f4d"),
    covid_nearest_max_hospitalization=Input(rid="ri.foundry.main.dataset.0cde7bff-a60e-4a57-98e4-8e1682b1ba8a")
)
---PURPOSE: HCQ, REM and DEX during admission
SELECT c.person_id, i.drug_concept_name, i.drug_exposure_start_date, c.covid_admission, c.covid_discharge, i.drug_exposure_end_date, i.drug_tylenol
FROM covid_nearest_max_hospitalization c 
left outer join all_tylenol i
    on c.person_id = i.person_id
    and i.drug_exposure_start_date between c.covid_admission and c.covid_discharge

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.75550c6a-b5fc-4df5-82d1-409441e1d7d0"),
    covid_nearest_max_hospitalization=Input(rid="ri.foundry.main.dataset.0cde7bff-a60e-4a57-98e4-8e1682b1ba8a"),
    dex_records_pt=Input(rid="ri.foundry.main.dataset.5a56856d-7480-41ea-9977-63d17867c097"),
    hcq_records_pt=Input(rid="ri.foundry.main.dataset.7eb2c42c-0685-4e90-bd24-ed145e7e1d5f"),
    rem_records_pt=Input(rid="ri.foundry.main.dataset.6e09c0de-8e79-4fc7-8d92-7f51a1b14737")
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
    Output(rid="ri.foundry.main.dataset.8555c32d-a61c-45b9-8310-ca0c643dffbe"),
    cohort_v00=Input(rid="ri.foundry.main.dataset.75550c6a-b5fc-4df5-82d1-409441e1d7d0"),
    person=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT a.*, p.year_of_birth, p.month_of_birth, p.day_of_birth , upper(p.gender_concept_name) as sex , upper(p.race_concept_name) as race , upper(p.ethnicity_concept_name) as ethnicity,          p.data_partner_id
FROM cohort_v00 a
inner join person p
    on a.person_id=p.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7c2095e8-9bbf-4e86-a032-e85948e38275"),
    cohort_v01=Input(rid="ri.foundry.main.dataset.8555c32d-a61c-45b9-8310-ca0c643dffbe"),
    pre_charslon_R=Input(rid="ri.foundry.main.dataset.7bebd326-f745-4186-8199-4b457557d713")
)
SELECT a.*, b.MI, b.CHF, b.PVD, b.stroke, b.dementia, b.pulmonary, b.rheumatic, b.PUD, b.liver_mild, b.diabetes, b.dmcx, b.paralysis, b.renal, b.cancer, b.liversevere, b.mets, b.hiv, b.multiple, b.CCI_INDEX
FROM cohort_v01 a
inner join pre_charslon_R b
    on a.person_id=b.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.78a07753-82f8-4b7d-ba93-0256c3bcccfb"),
    cohort_v02=Input(rid="ri.foundry.main.dataset.7c2095e8-9bbf-4e86-a032-e85948e38275")
)
Select *
from cohort_v02
where data_partner_id not in ('808','325') 
and covid_admission >= '2020-02-01' and covid_admission < '2020-11-01'

-- need to define age, 
-- exclude site 808, 787, 1003

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f5045d7c-7b11-4a5c-aad7-2961932a94a4"),
    cohort_v02=Input(rid="ri.foundry.main.dataset.7c2095e8-9bbf-4e86-a032-e85948e38275")
)
Select *
from cohort_v02
where  covid_admission >= '2020-02-01' and covid_admission < '2020-11-01'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.754cb5b5-5db7-44a2-be43-9aa5e3cab0a2"),
    cohort_v03_trial01=Input(rid="ri.foundry.main.dataset.f5045d7c-7b11-4a5c-aad7-2961932a94a4")
)
Select *
from cohort_v03_trial01
where data_partner_id not in ('808','325') 

-- need to define age, 
-- exclude site 808, 787, 1003

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.dd30973c-5aa1-4722-8fe8-ab20eef79448"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.fd1fe4a0-93c7-4745-a602-cee3908cf624")
)
--Purpose: to keep the first ever and last ever hospitalization date
--Doing this because some have multiple consecutive, and some have multiple non-consecutive but in a short time frame (1-3 days apart)
--Created by Kayte Andersen on December 3, 2020

select distinct person_id, date_of_first_covid_diagnosis, macrovisit_start_date as covid_admission, macrovisit_end_date as covid_discharge
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
    Output(rid="ri.foundry.main.dataset.0cde7bff-a60e-4a57-98e4-8e1682b1ba8a"),
    covid_nearest_hospitalization=Input(rid="ri.foundry.main.dataset.dd30973c-5aa1-4722-8fe8-ab20eef79448")
)
select distinct person_id, covid_admission, covid_discharge
    from covid_nearest_hospitalization inner join (select person_id as ps_id, max(covid_admission) as max_covid_admission from covid_nearest_hospitalization group by ps_id) mn
    on (covid_nearest_hospitalization.covid_admission = mn.max_covid_admission and covid_nearest_hospitalization.person_id = mn.ps_id)
    order by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.33aed96a-1356-4b7c-a82e-6f5f1fdaa215"),
    All_rem_dex_hcq_hospital=Input(rid="ri.foundry.main.dataset.4f0de965-fd21-4b59-99ef-871eafca5d3e")
)
SELECT *
FROM All_rem_dex_hcq_hospital
where drug_dex = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5a56856d-7480-41ea-9977-63d17867c097"),
    dex_records=Input(rid="ri.foundry.main.dataset.33aed96a-1356-4b7c-a82e-6f5f1fdaa215")
)
SELECT person_id, min (drug_exposure_start_date) as dex_start_date, max (drug_dex) as drug_dex_v1, max(drug_exposure_end_date) as dex_end_date
FROM dex_records
group by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.528c83fb-72c9-4d5e-b402-2f73b6613c4c"),
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

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b6c572f7-494c-4f98-a447-31983a16a6f3"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
---rem, dex and hcq
SELECT distinct concept_id, concept_name
 FROM concept_set_members
 WHERE
 (upper(concept_set_name) like '%PEPI_DRUG_HYDROXYCHLOROQUINE%'
 or upper(concept_set_name) like '%CS_PEPI_RM_DEX%') and (upper(is_most_recent_version) = 'TRUE')

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f787fae7-1f29-44b2-b866-3e1de27afc8c"),
    All_rem_dex_hcq_hospital=Input(rid="ri.foundry.main.dataset.4f0de965-fd21-4b59-99ef-871eafca5d3e")
)
SELECT *
FROM All_rem_dex_hcq_hospital
where drug_hcq = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.7eb2c42c-0685-4e90-bd24-ed145e7e1d5f"),
    hcq_records=Input(rid="ri.foundry.main.dataset.f787fae7-1f29-44b2-b866-3e1de27afc8c")
)
SELECT person_id, min (drug_exposure_start_date) as hcq_start_date, max (drug_hcq) as drug_hcq_v1, max(drug_exposure_end_date) as hcq_end_date
FROM hcq_records
group by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fd1fe4a0-93c7-4745-a602-cee3908cf624"),
    Covid_19_positive_patients=Input(rid="ri.foundry.main.dataset.c7ffdbf5-2218-4295-b53f-4fa348339eef"),
    inpatient=Input(rid="ri.foundry.main.dataset.f2735291-2eb0-43cd-a6d4-46145f6c2c9e")
)
--Purpose: to merge the COVID+ table with hospitalized table
--Creator: Kayte modified from Hemal's code in REM/DEX "003_COVID hospitalized pts" on November 11, 2020
SELECT distinct c.*, v.macrovisit_start_date, v.macrovisit_end_date, v.visit_concept_name, v.visit_source_value, v.macrovisit_id, abs(DATEDIFF (c.date_of_first_covid_diagnosis, v.macrovisit_start_date)) as diff_covid_hosp
FROM inpatient v 
--Inner join, because we only want the hospitalized COVID+, not people who are hospitalized without COVID or outpatient COVID
inner join Covid_19_positive_patients c
    on c.person_id = v.person_id 
--Restricting to hospitalizations within 21 from COVID diagnosis up until 5 days after hospitalization (as per Brian Garibaldi)
    and DATEDIFF (c.date_of_first_covid_diagnosis, v.macrovisit_start_date) between -21 and 5

--Note these are not distinct hospitalizations
--Scenario 1: someone diagnosed 5/12, has 3 hospitalizations recorded (5/13-5/14, 5/14-5/21, 5/15-5/21) --> consider them continuous and take earliest and latest date?
--Scenario 2: someone diagnosed 8/26/2019, hospitalized 8/26/2019 (no end date) and also 9/10/2019 (no end date)  --> IDK, taking the first and dropping second seems wrong because the readmission would indicate some sort of discharge but we don't have that information?
--Scenario 3: someone diagnosed 3/3/20201, hospitalized 3/3-3/4, then 3/7-3/8, then 3/10-3/10, then 3/12-3/14, then 3/18-3/22, then 3/24-3/24 --> consider them continuous and take earliest and latest date?

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f2735291-2eb0-43cd-a6d4-46145f6c2c9e"),
    microvisit_to_macrovisit_lds=Input(rid="ri.foundry.main.dataset.5af2c604-51e0-4afa-b1ae-1e5fa2f4b905")
)
SELECT *
FROM microvisit_to_macrovisit_lds
where macrovisit_id IS NOT NULL

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.2dc76f42-af36-4b2d-ad4a-e376cd1eb7ad"),
    cohort_v01=Input(rid="ri.foundry.main.dataset.8555c32d-a61c-45b9-8310-ca0c643dffbe"),
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
cohort_v01 cp
left outer join condition_occurrence_1 co on cp.person_id = co.person_id and co.condition_start_date < cp.covid_admission
left outer join concept_set_members_1 cs on ( cs.concept_id = co.condition_source_concept_id or cs.concept_id = co.condition_concept_id )
and cs.is_most_recent_version = true
    and cs.codeset_id in ( 535274723, 359043664, 78746470, 719585646, 403438288, 73549360, 494981955, 248333963, 378462283, 259495957, 489555336, 510748896, 514953976, 376881697, 
    220495690, 765004404, 652711186
    )
) t
group by t.person_id
) x

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c91750b8-cf70-4711-9ef2-925ec4026d9b"),
    All_rem_dex_hcq_hospital=Input(rid="ri.foundry.main.dataset.4f0de965-fd21-4b59-99ef-871eafca5d3e")
)
SELECT *
FROM All_rem_dex_hcq_hospital
where drug_rem = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6e09c0de-8e79-4fc7-8d92-7f51a1b14737"),
    rem_records=Input(rid="ri.foundry.main.dataset.c91750b8-cf70-4711-9ef2-925ec4026d9b")
)
SELECT person_id, min (drug_exposure_start_date) as rem_start_date, max (drug_rem) as drug_rem_v1, max(drug_exposure_end_date) as rem_end_date
FROM rem_records
group by person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.66dc688b-dca8-4503-9e88-53c103ffa424"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef")
)
SELECT *
FROM drug_exposure
where lcase(drug_concept_name) like '%remdesivir%' or lcase(drug_concept_name) like '%venklury%'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.abc62eb5-27c3-4232-9741-475d6090f0fd"),
    concept=Input(rid="ri.foundry.main.dataset.5cb3c4a3-327a-47bf-a8bf-daf0cafe6772")
)
---rem, dex and hcq
SELECT *
FROM concept
WHERE
lcase(concept_name) like '%tylenol%' or lcase(concept_name) like '%acetaminophen%'
--PEPI_DRUG_HYDROXYCHLOROQUINE
--CS_PEPI_RM_DEX

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6c832e22-b4d2-4782-9d04-b012af335e93"),
    all_tylenol_hospital=Input(rid="ri.foundry.main.dataset.94b70084-4c79-4abc-bb35-4f30e5b39e52")
)
SELECT *
FROM all_tylenol_hospital
where drug_tylenol = 1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a3e300ca-7e19-436a-ac18-310e118d78e9"),
    tylenol_records=Input(rid="ri.foundry.main.dataset.6c832e22-b4d2-4782-9d04-b012af335e93")
)
SELECT person_id, min (drug_exposure_start_date) as hcq_start_date, max (drug_tylenol) as drug_tylenol_v1
FROM tylenol_records
group by person_id

