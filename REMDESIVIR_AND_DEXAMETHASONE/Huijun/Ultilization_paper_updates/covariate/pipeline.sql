

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.6d1bf022-3ebe-4a69-8921-eda3e41b7a19"),
    concept=Input(rid="ri.foundry.main.dataset.5cb3c4a3-327a-47bf-a8bf-daf0cafe6772")
)
SELECT *
FROM concept
WHERE /*steroids*/ 
lcase(concept_name) like '%prednisone%'
or lcase(concept_name) like '%methylprednisolone%'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.55df18c1-2b76-4a6d-84e4-6b5d673d12cd"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.ec252b05-8f82-4f7f-a227-b3bb9bc578ef"),
    steroid_concept=Input(rid="ri.foundry.main.dataset.6d1bf022-3ebe-4a69-8921-eda3e41b7a19")
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
    Output(rid="ri.foundry.main.dataset.f2a7e787-04ef-441c-9ed9-f376e480a915"),
    covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5"),
    steroid_exposures=Input(rid="ri.foundry.main.dataset.55df18c1-2b76-4a6d-84e4-6b5d673d12cd")
)
SELECT c.person_id, i.drug_concept_name, i.drug_exposure_start_date, c.covid_admission, c.covid_discharge, i.drug_exposure_end_date, i.prednisone
FROM covariate_01 c 
left outer join steroid_exposures i
    on c.person_id = i.person_id
    and i.drug_exposure_start_date between c.covid_admission and c.covid_discharge

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9603b860-8d33-4fc1-a5f7-20794e7e0f06"),
    steroids_pts=Input(rid="ri.foundry.main.dataset.f2a7e787-04ef-441c-9ed9-f376e480a915")
)
SELECT person_id, min (drug_exposure_start_date) as steroid_start_date, max (prednisone) as steroid, max(drug_exposure_end_date) as steroid_end_date
FROM steroids_pts
group by person_id

