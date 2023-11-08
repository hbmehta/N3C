

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.919a3f84-ddd4-4a66-aa3a-089e21a7fdcb"),
    cohort_table=Input(rid="ri.foundry.main.dataset.996e1eda-6349-4d38-9815-08a03dae7d64"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6")
)
SELECT c.*
    ,row_number() over (partition by c.person_id order by datediff( c.covid_admission, c.measurement_date) ASC) as weight_rn
FROM cohort_table c
    join concept_set_members cm 
    on (c.measurement_concept_id = cm.concept_id and cm.codeset_id = 776390058) -- Weight codeset
where c.measurement_date <= c.covid_admission and
    c.harmonized_value_as_number is not null and 
    (c.harmonized_value_as_number >= 5 and c.harmonized_value_as_number <= 300)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.03682c93-f9d9-4ff3-9d54-167d763a5163"),
    first_compute_bmi=Input(rid="ri.foundry.main.dataset.89ca5800-869f-43cb-a8ae-d515b211d21d"),
    first_site_bmi=Input(rid="ri.foundry.main.dataset.5c335a93-4990-498b-8680-98b06b099aa9")
)
select b.*,
    s.bmi_from_site, s.bmi_date_from_site, s.days_since_bmi_measurement
from first_compute_bmi b left join first_site_bmi s on (b.person_id = s.person_id)
order by b.person_id, b.measurement_date

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.996e1eda-6349-4d38-9815-08a03dae7d64"),
    Cohort_v05=Input(rid="ri.foundry.main.dataset.1da11e06-e633-445b-9cdf-80f4f2de9abc"),
    measurement=Input(rid="ri.foundry.main.dataset.d6054221-ee0c-4858-97de-22292458fa19")
)
select c.covid_admission, m.*
from Cohort_v05 c 
left join measurement m 
on c.person_id = m.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.35fd9b56-8b63-47c2-becd-8c87bc9cb56d"),
    all_weights=Input(rid="ri.foundry.main.dataset.919a3f84-ddd4-4a66-aa3a-089e21a7fdcb"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    measurement=Input(rid="ri.foundry.main.dataset.d6054221-ee0c-4858-97de-22292458fa19"),
    person=Input(rid="ri.foundry.main.dataset.50cae11a-4afb-457d-99d4-55b4bc2cbe66")
)
SELECT w.*, h.harmonized_value_as_number height_harmonized_value_as_number, h.measurement_date height_measurement_date,
           h.value_as_number height_value_as_number, h.unit_concept_name height_unit_concept_name, h.measurement_concept_id height_measurement_concept_id,
           h.measurement_id height_measurement_id,
   case when w.harmonized_value_as_number is not null and h.harmonized_value_as_number is not null then w.harmonized_value_as_number / (h.harmonized_value_as_number * h.harmonized_value_as_number) else null end bmi_calculated,
   -datediff( h.measurement_date, w.measurement_date) days_since_height_measurement,
   row_number() over (partition by w.person_id order by abs(datediff( h.measurement_date, w.measurement_date)) ASC) as bmi_rn
FROM all_weights w left join measurement h on (w.person_id = h.person_id)    
     join concept_set_members cm on (cm.concept_id = h.measurement_concept_id and cm.codeset_id = 754731201) -- Height codeset
     left join person p on (p.person_id = w.person_id)
where weight_rn = 1 and
-- Find nearest height measurment that is within 1 week in the future and the person's 18th birthday
   (h.measurement_concept_id = cm.concept_id and datediff( h.measurement_date, w.measurement_date) < 7) and (year(h.measurement_date) >= p.year_of_birth + 18)
-- Make sure Height is between 0.6m and 2.43m (2ft-8ft)
   and h.harmonized_value_as_number is not null and (h.harmonized_value_as_number >= 0.6 and h.harmonized_value_as_number <= 2.43)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.89ca5800-869f-43cb-a8ae-d515b211d21d"),
    compute_bmi=Input(rid="ri.foundry.main.dataset.35fd9b56-8b63-47c2-becd-8c87bc9cb56d")
)
select * 
    from compute_bmi
    where bmi_rn = 1  

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5c335a93-4990-498b-8680-98b06b099aa9"),
    site_bmi=Input(rid="ri.foundry.main.dataset.d46c872f-8149-44db-91c8-3ca35155ba7a")
)
select * 
from site_bmi
where site_bmi_rn = 1 

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.d46c872f-8149-44db-91c8-3ca35155ba7a"),
    all_weights=Input(rid="ri.foundry.main.dataset.919a3f84-ddd4-4a66-aa3a-089e21a7fdcb"),
    concept_set_members=Input(rid="ri.foundry.main.dataset.e670c5ad-42ca-46a2-ae55-e917e3e161b6"),
    measurement=Input(rid="ri.foundry.main.dataset.d6054221-ee0c-4858-97de-22292458fa19")
)
SELECT m.person_id, 
    m.measurement_id bmi_from_site_measurement_id,
    m.harmonized_value_as_number bmi_from_site, m.measurement_date as bmi_date_from_site,
datediff( m.measurement_date, w.measurement_date) days_since_bmi_measurement,
row_number() over (partition by w.person_id order by abs(datediff( w.measurement_date, m.measurement_date)) ASC) as site_bmi_rn
FROM all_weights w join measurement m on (w.person_id = m.person_id)
    join concept_set_members cm on (cm.concept_id = m.measurement_concept_id and cm.codeset_id = 65622096) -- BMI codeset
where m.harmonized_value_as_number is not null and m.harmonized_value_as_number > 0

