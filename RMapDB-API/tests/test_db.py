import sqlite3

import pytest
from sqlalchemy.orm.session import Session
from rmapdb.main import get_db


def test_db():
    db: Session = next(get_db())
    res = db.execute("SELECT 1;")
    assert db is not None and res is not None
