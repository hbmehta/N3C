

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3a882bf1-6839-403a-a310-1995e1acf5e5"),
    All_rem_dex_hcq=Input(rid="ri.foundry.main.dataset.490550b8-d86a-4fa5-97e4-53b1c848a384")
)
SELECT *
FROM All_rem_dex_hcq
where drug_dex=1

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ac78c607-863f-4a4c-9acf-8586fafb990a"),
    All_dex=Input(rid="ri.foundry.main.dataset.3a882bf1-6839-403a-a310-1995e1acf5e5"),
    drug_use_outpat_1=Input(rid="ri.foundry.main.dataset.3df7323b-7f4a-4c94-8e73-1b02938d938c")
)
SELECT a.person_id, a.covid_admission, b.drug_exposure_start_date
FROM drug_use_outpat_1 a
inner join All_dex b
on a.person_id = b.person_id
where a.covid_admission > b.drug_exposure_start_date

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.3df7323b-7f4a-4c94-8e73-1b02938d938c"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5"),
    outpat_ids=Input(rid="ri.foundry.main.dataset.93d1cf03-37f6-43a5-ae65-9634c232e2cf")
)
SELECT distinct a.*
FROM Covariate_01 a 
inner join outpat_ids b
on a.person_id = b.person_id
where b.measurement_date > a.one_year_prior and b.measurement_date < a.covid_admission

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.93d1cf03-37f6-43a5-ae65-9634c232e2cf"),
    measurements_to_microvisits=Input(rid="ri.foundry.main.dataset.152dc81a-e193-4701-88f6-becba8bed3d2")
)
SELECT distinct person_id, measurement_date
FROM measurements_to_microvisits
where macrovisit_id IS NULL

