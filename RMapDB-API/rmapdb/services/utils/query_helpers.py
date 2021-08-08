from rmapdb.schemas.constants import FEATURE_LIST, MEASURES
from sqlathanor import AttributeConfiguration


def normalize_query(params):
    params_non_flat = params.to_dict(flat=False)
    return {k: v if len(v) > 1 else v[0] for k, v in params_non_flat.items()}


def search_data(request, Sample_data):
    params = normalize_query(request.args)
    temp_keys = []
    query_conditions = []

    for key, value in params.items():
        temp_keys.append(key.split("-"))
        temp_keys[len(temp_keys) - 1].append(value)

    func_map = {
        "gt": lambda x: query_conditions.append(
            getattr(Sample_data, x[0]) > int(float(x[2]))
        ),
        "ge": lambda x: query_conditions.append(
            getattr(Sample_data, x[0]) >= int(float(x[2]))
        ),
        "lt": lambda x: query_conditions.append(
            getattr(Sample_data, x[0]) < int(float(x[2]))
        ),
        "le": lambda x: query_conditions.append(
            getattr(Sample_data, x[0]) <= int(float(x[2]))
        ),
        "eq": lambda x: query_conditions.append(
            getattr(Sample_data, x[0]) == int(float(x[2]))
        ),
    }

    for key in temp_keys:
        if len(key) == 2:
            query_conditions.append(getattr(Sample_data, key[0]) == key[1])
        else:
            func_map[key[1]](key)

    return query_conditions


def prep_serializer():
    serialization = []
    for feature in FEATURE_LIST:
        serialization.append(
            AttributeConfiguration(
                name=feature,
                supports_csv=True,
                csv_sequence=1,
                supports_json=True,
                supports_yaml=True,
                supports_dict=True,
                on_serialize=None,
                on_deserialize=None,
            )
        )
    for feature in MEASURES:
        serialization.append(
            AttributeConfiguration(
                name=feature + "__Log2 Ratio (obs/exp)",
                supports_csv=True,
                csv_sequence=1,
                supports_json=True,
                supports_yaml=True,
                supports_dict=True,
                on_serialize=None,
                on_deserialize=None,
            )
        )

    return serialization
