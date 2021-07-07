from sqlalchemy import Boolean, Column, ForeignKey, Integer, String, DECIMAL
from sqlalchemy.orm import relationship
from rmapdb.database import Base


class RLoops(Base):
    __tablename__ = "rloops"

    id = Column(String, primary_key=True, index=True)
    type = Column(String)
    location = Column(String)
    confidence_level = Column(DECIMAL)
    is_rlfs = Column(Boolean)
    rmap_samples = relationship("RLoopSignal", back_populates="rloop")


class RMapSamples(Base):
    __tablename__ = "rmap_samples"

    id = Column(String, primary_key=True, index=True)
    sample_name = Column(String)
    genome = Column(String)
    lab = Column(String)
    study_id = Column(String)
    tissue = Column(String)
    genotype = Column(String)
    treatment = Column(String)
    condition = Column(String)
    control = Column(String)
    mode = Column(String)
    paired_end = Column(Boolean)
    is_input = Column(Boolean)
    is_rnh_like = Column(Boolean)
    rloops = relationship("RLoopSignal",
                          back_populates="rmap_sample")
    quality_char = relationship("SampleQualChar")


# for many-many relationships between tables
class RLoopSignal(Base):
    __tablename__ = 'rloop_signal'
    rloop_id = Column(String, ForeignKey('rloops.id'), primary_key=True)
    rmap_sample_id = Column(String, ForeignKey('rmap_samples.id'), primary_key=True)
    counts = Column(Integer)
    norm_counts = Column(DECIMAL)
    bpm = Column(DECIMAL)
    peak_qval = Column(DECIMAL)
    rloop = relationship("RLoops", back_populates="rmap_samples")
    rmap_sample = relationship("RMapSamples", back_populates="rloops")


class SampleQualChar(Base):
    __tablename__ = "sample_quality_characteristics"

    id = Column(Integer, primary_key=True)
    rmap_sample_id = Column(String, ForeignKey('rmap_samples.id'))
    char_type = Column(String)
    value = Column(DECIMAL)



