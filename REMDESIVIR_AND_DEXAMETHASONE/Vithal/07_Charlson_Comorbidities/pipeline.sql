

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f6269588-ef14-43eb-8375-4df10d828bfb"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86"),
    covid_patients_with_earliest_diagnosis=Input(rid="ri.foundry.main.dataset.89cb609d-42a7-4959-a6fa-5e52bc2cbb5e")
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
covid_patients_with_earliest_diagnosis cp
left outer join condition_occurrence co on cp.person_id = co.person_id and co.condition_start_date < cp.date
left outer join concept_set_members cs on ( cs.concept_id = co.condition_source_concept_id or cs.concept_id = co.condition_concept_id )
and cs.is_most_recent_version = true
    and cs.codeset_id in ( 535274723, 359043664, 78746470, 719585646, 403438288, 73549360, 494981955, 248333963, 378462283, 259495957, 489555336, 510748896, 514953976, 376881697, 
    220495690, 765004404, 652711186
    )
) t
group by t.person_id
) x

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c60c58ee-061d-4f77-90a5-29f825d3489c"),
    covid_patient_preexisting_comorbidities=Input(rid="ri.foundry.main.dataset.f6269588-ef14-43eb-8375-4df10d828bfb"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb")
)
SELECT
CCI_INDEX ,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
CCI_INDEX, count(distinct person_id) N
from
(
SELECT 
h.person_id , 
case 
when c.CCI_INDEX < 4 then c.CCI_INDEX 
when c.CCI_INDEX >= 4 then '4+' 
else 'Missing' 
end as CCI_INDEX
FROM 
hospitalized_covid_pts_on_dexamethasone h
left outer join covid_patient_preexisting_comorbidities c on c.person_id = h.person_id
)
group by CCI_INDEX
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ae58914d-ff93-408e-9bf8-e800519b6014"),
    covid_patient_preexisting_comorbidities=Input(rid="ri.foundry.main.dataset.f6269588-ef14-43eb-8375-4df10d828bfb"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d")
)
SELECT
CCI_INDEX ,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
CCI_INDEX, count(distinct person_id) N
from
(
SELECT 
h.person_id , 
case 
when c.CCI_INDEX < 4 then c.CCI_INDEX 
when c.CCI_INDEX >= 4 then '4+' 
else 'Missing' 
end as CCI_INDEX
FROM 
hospitalized_covid_pts_on_hcq h
left outer join covid_patient_preexisting_comorbidities c on c.person_id = h.person_id
)
group by CCI_INDEX
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.718c53d2-9d5e-4f62-afcf-104a0a5c7905"),
    covid_patient_preexisting_comorbidities=Input(rid="ri.foundry.main.dataset.f6269588-ef14-43eb-8375-4df10d828bfb"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
SELECT
CCI_INDEX ,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
CCI_INDEX, count(distinct person_id) N
from
(
SELECT 
h.person_id , 
case 
when c.CCI_INDEX < 4 then c.CCI_INDEX 
when c.CCI_INDEX >= 4 then '4+' 
else 'Missing' 
end as CCI_INDEX
FROM 
hospitalized_covid_pts_on_remedesivir h
left outer join covid_patient_preexisting_comorbidities c on c.person_id = h.person_id
)
group by CCI_INDEX
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c8ed6555-00bf-4038-ade1-f918712cb9d7"),
    covid_patient_preexisting_comorbidities=Input(rid="ri.foundry.main.dataset.f6269588-ef14-43eb-8375-4df10d828bfb"),
    hospitalized_covid_patients=Input(rid="ri.foundry.main.dataset.d4209982-609f-4ef0-9966-b6e002103c4d")
)
SELECT
CCI_INDEX ,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
CCI_INDEX, count(distinct person_id) N
from
(
SELECT 
h.person_id , 
case 
when c.CCI_INDEX < 4 then c.CCI_INDEX 
when c.CCI_INDEX >= 4 then '4+' 
else 'Missing' 
end as CCI_INDEX
FROM 
hospitalized_covid_patients h
left outer join covid_patient_preexisting_comorbidities c on c.person_id = h.person_id
)
group by CCI_INDEX
)

