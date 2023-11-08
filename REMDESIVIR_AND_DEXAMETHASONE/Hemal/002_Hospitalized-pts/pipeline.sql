

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.a440c9ea-dc76-4910-99a5-d5bbcce6ed31"),
    visit_occurrence=Input(rid="ri.foundry.main.dataset.3f74d43a-d981-4e17-93f0-21c811c57aab")
)
SELECT *
FROM visit_occurrence v
WHERE v.visit_concept_id in ('262','8717','9201', '9203', '32037', '581379')

