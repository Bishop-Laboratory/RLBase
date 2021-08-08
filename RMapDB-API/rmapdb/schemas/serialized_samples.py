from sqlalchemy import create_engine
from sqlalchemy.ext.automap import AutomapBase
from sqlathanor.automap import automap_base
from sqlathanor import AttributeConfiguration
from .constants import FEATURE_LIST, MEASURES

engine = create_engine("sqlite:///sample_data.db")
Base: AutomapBase = automap_base()
Base.prepare(engine, reflect=True)

Record = Base.classes.sample_data

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

Record.__serialization__ = serialization
