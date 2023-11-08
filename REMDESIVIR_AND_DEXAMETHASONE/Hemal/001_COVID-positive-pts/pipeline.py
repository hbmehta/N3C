from pyspark.sql import functions as F

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.982381d7-127b-4410-a693-7c3632d49a83"),
    condition_occurrence=Input(rid="ri.foundry.main.dataset.526c0452-7c18-46b6-8a5d-59be0b79a10b"),
    measurement=Input(rid="ri.foundry.main.dataset.29834e2c-f924-45e8-90af-246d29456293")
)
def covid_19_positive_patients(measurement, condition_occurrence):
    df_measurement = measurement
    df_condition_occurrence = condition_occurrence
    covid_measurement_concept_ids = [
        '757680', '757679', '757678', '757677', '723459', '715262', '715261', '715260', '706181',
        '706180', '706179', '706178', '706177', '706176', '706175', '706174', '706173', '706172',
        '706171', '706170', '706169', '706168', '706167', '706166', '706165', '706163', '706161',
        '706160', '706159', '706158', '706157', '706156', '706155', '706154', '586526', '586523',
        '586522', '586521', '586520', '586519', '586518', '586517', '586516', '586515'
    ]

    covid_measurement_value_as_concept_ids = ['4126681', '45877985', '45884084', '9191']
    persons_with_covid_measurement = df_measurement.where(
        (df_measurement.measurement_concept_id.isin(covid_measurement_concept_ids))
        & (df_measurement.value_as_concept_id.isin(covid_measurement_value_as_concept_ids))
    ).selectExpr("person_id", "data_partner_id", "measurement_date as date").distinct().withColumn("covid_diagnosis", F.lit(1))
    persons_with_covid_condition_occurrence = df_condition_occurrence.where(
        df_condition_occurrence.condition_concept_id == '37311061'
    ).selectExpr("person_id", "data_partner_id", "condition_start_date as date").distinct().withColumn("covid_diagnosis", F.lit(1))
    persons_with_covid = persons_with_covid_measurement.unionByName(
        persons_with_covid_condition_occurrence
    ).distinct().groupBy("person_id", "data_partner_id", "covid_diagnosis").agg(F.min("date").alias("date_of_first_covid_diagnosis"))
    
    return persons_with_covid

