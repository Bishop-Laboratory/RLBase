from sqlalchemy.orm import Session
from starlette.requests import Request

import rmapdb.schemas as models
from rmapdb.services.utils.query_helpers import search_data
from sqlalchemy.sql.elements import and_


def get_sample(db: Session, sample_id: int):
    return db.query(models.Sample).filter(models.Sample.id == sample_id).first()


def get_samples(db: Session, skip: int = 0, limit: int = 100):
    return db.query(models.Sample).offset(skip).limit(limit).all()


def process_sample_dataset_query(request: Request, db: Session):
    # TODO: This should not use automap_base since it conflicts with database models.
    # Replace w/ hardcoded ORM model for easier serialization

    from rmapdb.schemas.serialized_samples import Record

    query_conditions = search_data(request, Record)

    list_found = db.query().filter(and_(*query_conditions)).order_by(Record.GSM).all()

    return [record.to_dict() for record in list_found]
