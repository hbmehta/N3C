

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.5196a1f1-84bb-49c1-8211-17d35af07c05"),
    all_drug_pts=Input(rid="ri.foundry.main.dataset.f501f012-0472-49f2-a82c-f38faa649510")
)
SELECT *
FROM all_drug_pts
WHERE lcase(drug_concept_name) like '%dexamethasone%'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f501f012-0472-49f2-a82c-f38faa649510"),
    All_drug=Input(rid="ri.foundry.main.dataset.d3187e3b-49dc-4613-8a62-5874156c83a5"),
    Cohort_v07=Input(rid="ri.foundry.main.dataset.f38495ba-c045-4223-ad32-435359cfed04")
)
SELECT c.person_id, i.drug_concept_name, i.drug_exposure_start_date, i.drug_exposure_end_date
FROM Cohort_v07 c 
left outer join All_drug i
    on c.person_id = i.person_id
    and i.drug_exposure_start_date between c.covid_admission and c.covid_discharge

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.22e2cdf1-7a6f-4f22-803a-47cf4ac8bb29"),
    all_drug_pts=Input(rid="ri.foundry.main.dataset.f501f012-0472-49f2-a82c-f38faa649510")
)
SELECT *
FROM all_drug_pts
WHERE lcase(drug_concept_name) like '%methylprednisolone%'

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.0e5a76cc-4f11-4b47-ba8c-b2d784527491"),
    all_drug_pts=Input(rid="ri.foundry.main.dataset.f501f012-0472-49f2-a82c-f38faa649510")
)
SELECT *
FROM all_drug_pts
WHERE lcase(drug_concept_name) like '%prednisone%'

