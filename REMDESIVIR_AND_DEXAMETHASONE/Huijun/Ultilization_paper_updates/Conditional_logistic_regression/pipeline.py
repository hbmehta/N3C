

@transform_pandas(
    Output(rid="ri.foundry.main.dataset.e08eca76-1dda-4f6f-8205-bc3e534c8da3"),
    Covariate_model_dex_vent_head=Input(rid="ri.foundry.main.dataset.c8ceefca-eca7-43ce-98d3-0a89c271fd27")
)
import numpy as np

from statsmodels.discrete.conditional_models import ConditionalLogit

def test(Covariate_model_dex_vent_head):

    y_stg = np.array(Covariate_model_dex_vent_head.select('DEX').collect())

    y = [a[0] for a in y_stg[:]]

    y_np = np.array(y)

    print("y_np")

        
        

    x = np.empty((800,2)) # number of rows in the dataset by number of covariates (starting from 0)

    

    ECMO = np.array(Covariate_model_dex_vent_head.select('ECMO').collect())

    x[:, 0] = [a[0] for a in ECMO[:]]

    print("x1 - ECMO")

    

    AKI_in_hospital = np.array(Covariate_model_dex_vent_head.select('AKI_in_hospital').collect())

    x[:, 1] = [a[0] for a in AKI_in_hospital[:]]

    print("x2 - AKI_in_hospital")

    

    g_stg = np.array(Covariate_model_dex_vent_head.select('data_partner_id').collect())

    g = [a[0] for a in g_stg[:]]

    g_np = np.array(g)

    print("g_np")

    

    model = ConditionalLogit(y_np, x, groups=g_np)

    result = model.fit()
    result.summary()

 

 

