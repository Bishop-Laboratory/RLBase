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

###################################

######## CRUD example code ######## 

###################################


from sqlalchemy.orm import Session
from . import models, schemas


def get_user(db: Session, user_id: int):
    return db.query(models.User).filter(models.User.id == user_id).first()


def get_user_by_email(db: Session, email: str):
    return db.query(models.User).filter(models.User.email == email).first()


def get_users(db: Session, skip: int = 0, limit: int = 100):
    return db.query(models.User).offset(skip).limit(limit).all()


def create_user(db: Session, user: schemas.UserCreate):
    fake_hashed_password = user.password + "notreallyhashed"
    db_user = models.User(email=user.email, hashed_password=fake_hashed_password)
    db.add(db_user)
    db.commit()
    db.refresh(db_user)
    return db_user


def get_items(db: Session, skip: int = 0, limit: int = 100):
    return db.query(models.Item).offset(skip).limit(limit).all()


def create_user_item(db: Session, item: schemas.ItemCreate, user_id: int):
    db_item = models.Item(**item.dict(), owner_id=user_id)
    db.add(db_item)
    db.commit()
    db.refresh(db_item)
    return db_item
