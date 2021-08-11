from fastapi import FastAPI
from fastapi.params import Depends
from sqlalchemy.orm.session import Session
from starlette.requests import Request
import uvicorn
from rmapdb.database import models, SessionLocal, engine
import rmapdb.services as services

models.Base.metadata.create_all(bind=engine)


def get_db() -> Session:
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


app = FastAPI()


@app.get("/api-v1/rmap-sample")
def db_test(request: Request, db: Session = Depends(get_db)):
    return services.process_sample_dataset_query(request, db)


@app.get("/api-v1/rloops")
def r_loop_table():
    return {
        [
            {
                "id": "RL88229",
                "start": 155640023,
                "end": 155640089,
                "Chr": "chr12",
                "type": "Co-transcriptional",
            },
            {
                "id": "RL88230",
                "start": 135640023,
                "end": 135640089,
                "Chr": "chr21",
                "type": "Regulatory",
            },
        ]
    }


if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
