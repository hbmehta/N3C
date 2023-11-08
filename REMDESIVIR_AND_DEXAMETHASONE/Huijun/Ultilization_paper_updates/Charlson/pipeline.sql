

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a54c6ebb-ce93-4802-ac47-674a6ceca380"),
    Charlson_2=Input(rid="ri.foundry.main.dataset.40a51930-f98f-4d55-b5bb-2f31adb0dbbc"),
    Charlson_3=Input(rid="ri.foundry.main.dataset.6db7ba29-d534-497c-bf3a-c47d9f28ff17"),
    baseline_info_2=Input(rid="ri.foundry.main.dataset.8a92db7a-c62a-4bd9-9694-b83edbd535b1"),
    charlson_1=Input(rid="ri.foundry.main.dataset.509a91a5-913c-4f97-abd7-450d20670510")
)
SELECT a.person_id, a.CCI_INDEX_1, b.CCI_INDEX_2, c.CCI_INDEX_3, d.baseline
FROM charlson_1 a
inner join Charlson_2 b
inner join Charlson_3 c
inner join baseline_info_2 d
on a.person_id = b.person_id and a.person_id = c.person_id and a.person_id = d.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.40a51930-f98f-4d55-b5bb-2f31adb0dbbc"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86")
)
SELECT
distinct
x.* ,
(x.MI*1 + x.CHF*1 + x.PVD*1 + x.stroke*1 + x.dementia*1 + x.pulmonary*1 + x.rheumatic*1 + x.PUD*1 + x.liver_mild*1 + x.diabetes*1 + x.dmcx*2 + x.paralysis*2 + x.renal*2 + x.cancer*2 
 + x.liversevere*3 + x.mets*6 + x.hiv*6) CCI_INDEX_2
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
Cohort_v05 cp
left outer join condition_occurrence co on ( cp.person_id = co.person_id and co.condition_start_date <= cp.covid_discharge )
left outer join concept_set_members cs on ( cs.concept_id = co.condition_source_concept_id or cs.concept_id = co.condition_concept_id )
and cs.is_most_recent_version = true
    and cs.codeset_id in ( 535274723, 359043664, 78746470, 719585646, 403438288, 73549360, 494981955, 248333963, 378462283, 259495957, 489555336, 510748896, 514953976, 376881697, 
    220495690, 765004404, 652711186
    )
) t
group by t.person_id
) x

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6db7ba29-d534-497c-bf3a-c47d9f28ff17"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86")
)
SELECT
distinct
x.* ,
(x.MI*1 + x.CHF*1 + x.PVD*1 + x.stroke*1 + x.dementia*1 + x.pulmonary*1 + x.rheumatic*1 + x.PUD*1 + x.liver_mild*1 + x.diabetes*1 + x.dmcx*2 + x.paralysis*2 + x.renal*2 + x.cancer*2 
 + x.liversevere*3 + x.mets*6 + x.hiv*6) CCI_INDEX_3
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
Cohort_v05 cp
left outer join condition_occurrence co on ( cp.person_id = co.person_id and co.condition_start_date <= cp.covid_discharge and cp.one_year_prior <= co.condition_start_date )
left outer join concept_set_members cs on ( cs.concept_id = co.condition_source_concept_id or cs.concept_id = co.condition_concept_id )
and cs.is_most_recent_version = true
    and cs.codeset_id in ( 535274723, 359043664, 78746470, 719585646, 403438288, 73549360, 494981955, 248333963, 378462283, 259495957, 489555336, 510748896, 514953976, 376881697, 
    220495690, 765004404, 652711186
    )
) t
group by t.person_id
) x

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b7b16c7c-f6d9-4b8b-a681-102545e86fb1"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86")
)
SELECT cp.person_id, cp.covid_admission, co.condition_start_date
FROM Cohort_v05 cp
left  join condition_occurrence co 
on ( cp.person_id = co.person_id and co.condition_start_date < cp.covid_admission and cp.one_year_prior < co.condition_start_date)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.509a91a5-913c-4f97-abd7-450d20670510"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86")
)
SELECT
distinct
x.* ,
(x.MI*1 + x.CHF*1 + x.PVD*1 + x.stroke*1 + x.dementia*1 + x.pulmonary*1 + x.rheumatic*1 + x.PUD*1 + x.liver_mild*1 + x.diabetes*1 + x.dmcx*2 + x.paralysis*2 + x.renal*2 + x.cancer*2 
 + x.liversevere*3 + x.mets*6 + x.hiv*6) CCI_INDEX_1
FROM
(
SELECT
distinct
    person_id, 
    sum(case when comorbidity = 'MI' then (1) else (0) end) MI ,
    sum(case when comorbidity = 'CHF' then (1) else (0) end) CHF ,
    sum(case when comorbidity = 'PVD' then (1) else (0) end) PVD ,
    sum(case when comorbidity = 'Stroke' then (1) else (0) end) stroke ,
    sum(case when comorbidity = 'Dementia' then (1) else (0) end) dementia ,
    sum(case when comorbidity = 'Pulmonary' then (1) else (0) end) pulmonary ,
    sum(case when comorbidity = 'Rheumatic' then (1) else (0) end) rheumatic ,
    sum(case when comorbidity = 'PUD' then (1) else (0) end) PUD ,
    sum(case when comorbidity = 'LiverMild' then (1) else (0) end) liver_mild ,
    sum(case when comorbidity = 'DM' then (1) else (0) end) diabetes ,
    sum(case when comorbidity = 'DMcx' then (1) else (0) end) dmcx ,
    sum(case when comorbidity = 'Paralysis' then (1) else (0) end) paralysis ,
    sum(case when comorbidity = 'Renal' then (1) else (0) end) renal ,
    sum(case when comorbidity = 'Cancer' then (1) else (0) end) cancer ,
    sum(case when comorbidity = 'LiverSevere' then (1) else (0) end) liversevere ,
    sum(case when comorbidity = 'Mets' then (1) else (0) end) mets ,   
    sum(case when comorbidity = 'HIV' then (1) else (0) end) hiv,    
    case when count(*) > 1 then (1) else (0) end multiple
FROM (
SELECT 
distinct
cp.person_id ,
replace(cs.concept_set_name, 'Charlson - ','') comorbidity 
FROM 
Cohort_v05 cp
left outer join condition_occurrence co on ( cp.person_id = co.person_id and cp.covid_admission <= co.condition_start_date and co.condition_start_date <= cp.covid_discharge )
left outer join concept_set_members cs on ( cs.concept_id = co.condition_source_concept_id or cs.concept_id = co.condition_concept_id )
and cs.is_most_recent_version = true
    and cs.codeset_id in ( 535274723, 359043664, 78746470, 719585646, 403438288, 73549360, 494981955, 248333963, 378462283, 259495957, 489555336, 510748896, 514953976, 376881697, 
    220495690, 765004404, 652711186
    )
) t
group by t.person_id
) x

