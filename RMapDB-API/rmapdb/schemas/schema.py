from pydantic import BaseModel


class Sample(BaseModel):
    id: int
    study: str
    study_info: str
    sample_name: str

    class Config:
        orm_mode = True


class Gene(BaseModel):
    id: int
    ensemble_id: str
    gene_symbol: str
    gene_biotype: str
    gene_location: str

    class Config:
        orm_mode = True


class GeneExperiment(BaseModel):
    id: int
    ensemble_id: str
    sample: str
    counts: float
    cpm = float
    rpkm = float
    tpm = float

    class Config:
        orm_mode = True


class Deg(BaseModel):

    id: int
    ensemble_id: str
    study: str
    log2fc: float
    pval: float
    padj: float

    class Config:
        orm_mode: True
