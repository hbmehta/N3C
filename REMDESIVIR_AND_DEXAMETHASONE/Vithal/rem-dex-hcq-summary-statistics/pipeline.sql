

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a7c345dc-5de5-4cb9-9347-f7e2d7294a54"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
select
q.Usage_Type , q.N , ( q.N/( sum(q.N) over() )  )* 100 as percentage
from
(
Select 
'1. All Patients on only DEX and not on (HCQ-REM)' as Usage_Type , count(distinct d.person_id) as N
from
hospitalized_covid_pts_on_dexamethasone d
where 
d.person_id not in
( select h.person_id from hospitalized_covid_pts_on_hcq h ) 
and d.person_id not in 
( select r.person_id from hospitalized_covid_pts_on_remedesivir r )

union

Select 
'2. All Patients on DEX and HCQ (not on REM)' as Usage_Type , count(distinct d.person_id) as N
from
hospitalized_covid_pts_on_dexamethasone d
where 
d.person_id in
( select h.person_id from hospitalized_covid_pts_on_hcq h )
and d.person_id not in 
( select r.person_id from hospitalized_covid_pts_on_remedesivir r ) 

union

Select 
'3. All Patients on DEX and REM (not on HCQ)' as Usage_Type , count(distinct d.person_id) as N
from
hospitalized_covid_pts_on_dexamethasone d
where 
d.person_id not in
( select h.person_id from hospitalized_covid_pts_on_hcq h ) 
and d.person_id in 
( select r.person_id from hospitalized_covid_pts_on_remedesivir r ) 

union

Select 
'4. All Patients on DEX and HCQ and REM' as Usage_Type , count(distinct d.person_id) as N
from
hospitalized_covid_pts_on_dexamethasone d
where 
d.person_id in
( select h.person_id from hospitalized_covid_pts_on_hcq h ) 
and d.person_id in 
( select r.person_id from hospitalized_covid_pts_on_remedesivir r ) 

) q
group by q.Usage_Type , q.N

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.fef4067b-f8ed-44fa-8dc6-deaa359fe8d6"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
select
q.Usage_Type , q.N , ( q.N/( sum(q.N) over() )  )* 100 as percentage
from
(
Select 
'1. All Patients on only HCQ and not on (REM-DEX)' as Usage_Type , count(distinct h.person_id) as N
from
hospitalized_covid_pts_on_hcq h
where 
h.person_id not in
( select r.person_id from hospitalized_covid_pts_on_remedesivir r ) -- where r.visit_occurrence_id = h.visit_occurrence_id )
and h.person_id not in 
( select d.person_id from hospitalized_covid_pts_on_dexamethasone d ) -- where d.visit_occurrence_id = h.visit_occurrence_id )

union

Select 
'2. All Patients on HCQ and REM (not on DEX)' as Usage_Type , count(distinct h.person_id) as N
from
hospitalized_covid_pts_on_hcq h
where 
h.person_id in
( select r.person_id from hospitalized_covid_pts_on_remedesivir r ) -- where r.visit_occurrence_id = h.visit_occurrence_id )
and h.person_id not in 
( select d.person_id from hospitalized_covid_pts_on_dexamethasone d ) -- where d.visit_occurrence_id = h.visit_occurrence_id )

union

Select 
'3. All Patients on HCQ and DEX (not on REM)' as Usage_Type , count(distinct h.person_id) as N
from
hospitalized_covid_pts_on_hcq h
where 
h.person_id not in
( select r.person_id from hospitalized_covid_pts_on_remedesivir r ) -- where r.visit_occurrence_id = h.visit_occurrence_id )
and h.person_id in 
( select d.person_id from hospitalized_covid_pts_on_dexamethasone d ) -- where d.visit_occurrence_id = h.visit_occurrence_id )

union

Select 
'4. All Patients on HCQ and REM and DEX' as Usage_Type , count(distinct h.person_id) as N
from
hospitalized_covid_pts_on_hcq h
where 
h.person_id in
( select r.person_id from hospitalized_covid_pts_on_remedesivir r ) -- where r.visit_occurrence_id = h.visit_occurrence_id )
and h.person_id in 
( select d.person_id from hospitalized_covid_pts_on_dexamethasone d ) -- where d.visit_occurrence_id = h.visit_occurrence_id )

) q
group by q.Usage_Type , q.N

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.cf0cc919-86af-42dd-99a1-3a516a4d2d48"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
select
q.Usage_Type , q.N , ( q.N/( sum(q.N) over() )  )* 100 as percentage
from
(
Select 
'1. All Patients on only REM and not on (HCQ-DEX)' as Usage_Type , count(distinct r.person_id) as N
from
hospitalized_covid_pts_on_remedesivir r
where 
r.person_id not in
( select h.person_id from hospitalized_covid_pts_on_hcq h ) 
and r.person_id not in 
( select d.person_id from hospitalized_covid_pts_on_dexamethasone d )

union

Select 
'2. All Patients on REM and HCQ (not on DEX)' as Usage_Type , count(distinct r.person_id) as N
from
hospitalized_covid_pts_on_remedesivir r
where 
r.person_id in
( select h.person_id from hospitalized_covid_pts_on_hcq h )
and r.person_id not in 
( select d.person_id from hospitalized_covid_pts_on_dexamethasone d ) 

union

Select 
'3. All Patients on REM and DEX (not on HCQ)' as Usage_Type , count(distinct r.person_id) as N
from
hospitalized_covid_pts_on_remedesivir r
where 
r.person_id not in
( select r.person_id from hospitalized_covid_pts_on_hcq r ) 
and r.person_id in 
( select d.person_id from hospitalized_covid_pts_on_dexamethasone d ) 

union

Select 
'4. All Patients on REM and HCQ and DEX' as Usage_Type , count(distinct r.person_id) as N
from
hospitalized_covid_pts_on_remedesivir r
where 
r.person_id in
( select h.person_id from hospitalized_covid_pts_on_hcq h ) 
and r.person_id in 
( select d.person_id from hospitalized_covid_pts_on_dexamethasone d ) 

) q
group by q.Usage_Type , q.N

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.544236d8-668c-45b3-8f76-c6f7e23d07b8"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb")
)
SELECT
Age , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
age_group as Age , count(distinct person_id) N
FROM hospitalized_covid_pts_on_dexamethasone
group by age_group
) 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.45cb7bf5-6b86-4947-b1a0-4b1b22f8d97a"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d")
)
SELECT
Age , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
age_group as Age , count(distinct person_id) N
FROM hospitalized_covid_pts_on_hcq
group by age_group
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.adc25f5c-d5a8-4790-be15-88cf3c015f99"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
SELECT
Age , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
age_group as Age , count(distinct person_id) N
FROM hospitalized_covid_pts_on_remedesivir
group by age_group
) 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a268afff-5c39-4782-8c70-f6cd83916e3b"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb")
)
SELECT
data_partner_id , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
data_partner_id , count(distinct person_id) N
FROM hospitalized_covid_pts_on_dexamethasone
group by data_partner_id
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.aacd0791-325e-40fb-a69b-e9e519bdb56f"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d")
)
SELECT
data_partner_id , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
data_partner_id , count(distinct person_id) N
FROM hospitalized_covid_pts_on_hcq
group by data_partner_id
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6b258e4c-38e7-40cc-b359-6b671729b558"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
SELECT
data_partner_id , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
data_partner_id , count(distinct person_id) N
FROM hospitalized_covid_pts_on_remedesivir
group by data_partner_id
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.4def47ee-bc52-4d45-905a-4aae119d78a6"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb")
)
SELECT
ethnicity , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
ethnicity , count(distinct person_id) N
FROM hospitalized_covid_pts_on_dexamethasone
group by ethnicity
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.b14f6376-2601-4bbf-b299-79a529d0ce1b"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d")
)
SELECT
ethnicity , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
ethnicity , count(distinct person_id) N
FROM hospitalized_covid_pts_on_hcq
group by ethnicity
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.143db800-491d-4377-b616-e09c870cbb60"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
SELECT
ethnicity , N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
ethnicity , count(distinct person_id) N
FROM hospitalized_covid_pts_on_remedesivir
group by ethnicity
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d9a2ab26-75e8-43f8-917c-b418c698b22b"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb")
)
SELECT
race ,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
case when ( race is null ) then 'Missing' else race end as race , count(distinct person_id) N
FROM hospitalized_covid_pts_on_dexamethasone
group by race
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ba125140-2da6-405a-9921-29a2c6b53d28"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d")
)
SELECT
race ,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
case when ( race is null ) then 'Missing' else race end as race , count(distinct person_id) N
FROM hospitalized_covid_pts_on_hcq
group by race
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c8e524db-8294-4b00-bbc6-111a8b5a0917"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
)
SELECT
race ,
 N , ( (N/(SUM(N) over()) ) * 100 ) as percentage
FROM
(
SELECT 
case when ( race is null ) then 'Missing' else race end as race , count(distinct person_id) N
FROM hospitalized_covid_pts_on_remedesivir
group by race
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.80bf9ae6-fd12-4a90-98bb-3ac9a41f74ad"),
    hospitalized_covid_pts_on_dexamethasone=Input(rid="ri.foundry.main.dataset.13dcc9d6-f5f1-4e8f-a818-3a9efcc089bb")
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
FROM hospitalized_covid_pts_on_dexamethasone
group by sex
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.acf5d1f9-c7a4-4202-98e2-6ef2f4f590b5"),
    hospitalized_covid_pts_on_hcq=Input(rid="ri.foundry.main.dataset.aa2c5f70-6abe-4334-934e-705521f9814d")
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
FROM hospitalized_covid_pts_on_hcq
group by sex
)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9b2d9af5-d525-42b0-ab65-de12ec0a5e7f"),
    hospitalized_covid_pts_on_remedesivir=Input(rid="ri.foundry.main.dataset.ea43c2db-0f42-4649-82ba-e43c95b2bd91")
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
FROM hospitalized_covid_pts_on_remedesivir
group by sex
)

