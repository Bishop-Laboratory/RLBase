from sqlalchemy import Column, Integer, Text
from sqlalchemy.sql.sqltypes import Numeric

from .db import Base


class Samples(Base):
    __tablename__ = "samples"

    id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    study = Column(Text)
    study_info = Column(Text)
    sample_name = Column(Text, unique=True, nullable=False)


class Genes(Base):
    __tablename__ = "genes"

    id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    ensemble_id = Column(Text, unique=True, nullable=False)
    gene_symbol = Column(Text)
    gene_biotype = Column(Text)
    gene_location = Column(Text)


class GeneExperiments(Base):
    __tablename__ = "gene_exp"

    id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    ensemble_id = Column(Text, unique=True, nullable=False)
    sample = Column(Text, nullable=False)
    counts = Column(Numeric)
    cpm = Column(Numeric)
    rpkm = Column(Numeric)
    tpm = Column(Numeric)


class Degs(Base):
    __tablename__ = "degs"

    id = Column(Integer, primary_key=True, index=True, autoincrement=True)
    ensemble_id = Column(Text, unique=True, nullable=False)
    study = Column(Text)
    log2fc = Column(Numeric)
    pval = Column(Numeric)
    padj = Column(Numeric)
