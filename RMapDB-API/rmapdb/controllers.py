def normalize_query_param(value):
    return value if len(value) > 1 else value[0]


def normalize_query(params):
    params_non_flat = params.to_dict(flat=False)
    return {k: normalize_query_param(v) for k, v in params_non_flat.items()}
    
def search_data(request, Sample_data):
    params = normalize_query(request.args)
    tempKeys = []
    query_conditions = []
    
    for key,value in params.items():
        tempKeys.append(key.split("-"))
        tempKeys[len(tempKeys)-1].append(value)
    
    for key in tempKeys:
        if(len(key) == 2):
            query_conditions.append(getattr(Sample_data, key[0]) == key[1])
        else:
            if(key[1]== "gt"):
                query_conditions.append(getattr(Sample_data, key[0]) > int(float(key[2])))
            elif(key[1] == "ge"):
                query_conditions.append(getattr(Sample_data, key[0]) >= int(float(key[2])))
            elif(key[1] == "lt"):
                query_conditions.append(getattr(Sample_data, key[0]) < int(float(key[2])))
            elif(key[1] == "le"):
                query_conditions.append(getattr(Sample_data, key[0]) <= int(float(key[2])))
            elif(key[1] == "eq"):
                query_conditions.append(getattr(Sample_data, key[0]) == int(float(key[2])))
    return query_conditions