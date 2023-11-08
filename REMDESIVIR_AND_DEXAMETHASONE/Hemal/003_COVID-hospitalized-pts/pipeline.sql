

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.f1629f95-a470-4105-9508-4bb5f7089285"),
    Covid_19_positive_patients=Input(rid="ri.foundry.main.dataset.982381d7-127b-4410-a693-7c3632d49a83"),
    Inpatient_and_er_visits=Input(rid="ri.foundry.main.dataset.a440c9ea-dc76-4910-99a5-d5bbcce6ed31")
)
SELECT distinct c.*, v.visit_start_date, v.visit_end_date
FROM Inpatient_and_er_visits v join Covid_19_positive_patients c
on c.person_id = v.person_id AND c.date_of_first_covid_diagnosis between v.visit_start_date AND v.visit_end_date
--25,362 if we confine covid diagnosis within visit start and end date
--33,359 if we do not use distinct - were pts hospitalized multiple times?? when we use distinct - do we get the first hospitalization

--on c.person_id = v.person_id AND c.date_of_first_covid_diagnosis between datesub ((v.visit_start_date)-30) AND v.visit_end_date
--We should take if they were diagnosed before visit date , 30 days prior??

