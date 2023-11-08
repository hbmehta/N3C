

@transform_pandas(
    Output(rid="ri.vector.main.execute.250cdd34-6d53-4aa4-819b-5efc3679ab76"),
    drug_exposure=Input(rid="ri.foundry.main.dataset.fd499c1d-4b37-4cda-b94f-b7bf70a014da")
)
SELECT *
FROM drug_exposure

