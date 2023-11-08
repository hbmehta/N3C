

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.583a6cca-f843-49d7-a6b0-da22ece9c302"),
    ECMO_OBS=Input(rid="ri.foundry.main.dataset.ccb1e3aa-fe6a-41a9-bee5-9a7499d52973"),
    ECMO_no_vent_procedure=Input(rid="ri.foundry.main.dataset.83b554ff-a358-48a5-89b1-a81071878f90")
)
SELECT distinct person_id
FROM ECMO_no_vent_procedure
where person_id not in (select distinct person_id from ECMO_OBS)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.58262840-cf9e-43b1-8bf6-2e9f7f1cd779"),
    Covariate_01=Input(rid="ri.foundry.main.dataset.813e6539-221c-43fb-b130-b8013a5d7cc5"),
    VV_ecmo=Input(rid="ri.foundry.main.dataset.c53ec423-43b4-4c75-aac7-15a59c89efbc")
)
SELECT distinct person_id, data_partner_id
FROM Covariate_01
where person_id in (select distinct person_id from VV_ecmo)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.ccb1e3aa-fe6a-41a9-bee5-9a7499d52973"),
    ECMO_no_vent_procedure=Input(rid="ri.foundry.main.dataset.83b554ff-a358-48a5-89b1-a81071878f90")
)
SELECT distinct *
FROM ECMO_no_vent_procedure
where procedure_concept_name LIKE '%ECMO%' and (procedure_concept_name  LIKE '%venovenous%' or procedure_concept_name  LIKE '%veno-venous%')

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.9d0bd810-059c-477c-ad8a-563c6c65a4f9"),
    ECMO_no_vent=Input(rid="ri.foundry.main.dataset.277f9f32-7ea6-40de-8395-5ff67a8f310f"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.900fa2ad-87ea-4285-be30-c6b5bab60e86")
)
SELECT distinct c.person_id, c.condition_concept_name, c.visit_occurrence_id
FROM condition_occurrence c
inner join ECMO_no_vent e
where c.person_id = e.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.83b554ff-a358-48a5-89b1-a81071878f90"),
    ECMO_no_vent=Input(rid="ri.foundry.main.dataset.277f9f32-7ea6-40de-8395-5ff67a8f310f"),
    procedure_occurrence=Input(rid="ri.foundry.main.dataset.f6f0b5e0-a105-403a-a98f-0ee1c78137dc")
)
SELECT distinct p.person_id, p.procedure_concept_name, p.visit_occurrence_id
FROM procedure_occurrence p
inner join ECMO_no_vent e
where p.person_id = e.person_id

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.c53ec423-43b4-4c75-aac7-15a59c89efbc"),
    ECMO_OBS=Input(rid="ri.foundry.main.dataset.ccb1e3aa-fe6a-41a9-bee5-9a7499d52973"),
    ECMO_no_vent_procedure=Input(rid="ri.foundry.main.dataset.83b554ff-a358-48a5-89b1-a81071878f90")
)
SELECT distinct person_id
FROM ECMO_no_vent_procedure
where person_id in (select distinct person_id from ECMO_OBS)

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.12a7acdd-c1fa-494a-b62a-ea51927065e4"),
    ECMO_OBS=Input(rid="ri.foundry.main.dataset.ccb1e3aa-fe6a-41a9-bee5-9a7499d52973"),
    ECMO_no_vent_procedure=Input(rid="ri.foundry.main.dataset.83b554ff-a358-48a5-89b1-a81071878f90")
)
SELECT *
FROM ECMO_no_vent_procedure
where person_id not in (select distinct person_id from ECMO_OBS) and (lcase(procedure_concept_name) LIKE '%ecmo%' or lcase(procedure_concept_name) LIKE '%extracorporeal membrane oxygenation%')

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f5dfeac3-3542-4009-8a09-0d3d8b70135f"),
    ECMO_no_vent_procedure=Input(rid="ri.foundry.main.dataset.83b554ff-a358-48a5-89b1-a81071878f90"),
    ecmo_no_veno=Input(rid="ri.foundry.main.dataset.12a7acdd-c1fa-494a-b62a-ea51927065e4")
)
SELECT distinct * 
FROM ECMO_no_vent_procedure
where visit_occurrence_id in (select distinct visit_occurrence_id from ecmo_no_veno) 

