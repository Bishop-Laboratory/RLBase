from sqlalchemy import Boolean, Column, ForeignKey, Integer, String, DECIMAL
from sqlalchemy.orm import relationship
from rmapdb.database import Base


class RLoops(Base):
    # Information on RLoops
    __tablename__ = "rloops"

    id = Column(String, primary_key=True, index=True)
    type = Column(String)
    location = Column(String)
    confidence_level = Column(DECIMAL)
    is_rlfs = Column(Boolean)
    # Bidirectional many-to-many relationship with RMapSamples via RLoopSignal
    rmap_samples = relationship("RLoopSignal", back_populates="rloop")
    # Bidirectional many-to-many relationship with GenomicFeatures via GenFeatRLooopOverlap
    genomic_features = relationship("GenFeatRLoopOverlap", back_populates="rloop")
    # Bidirectional many-to-many relationship with Genes via GeneRLooopOverlap
    genes = relationship("GeneRLoopOverlap", back_populates="rloop")


class RMapSamples(Base):
    # Information on RLoop sequencing experiments
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
    # Bidirectional many-to-many relationship with RLoops via RLoopSignal
    rloops = relationship("RLoopSignal", back_populates="rmap_sample")
    # Unidirectional one-to-many relationship with SampleQualChar
    quality_char = relationship("SampleQualChar")


class RLoopSignal(Base):
    # Association table for many-to-many relationship between RLoops and RMapSamples
    __tablename__ = 'rloop_signal'

    # Foreign keys
    rloop_id = Column(String, ForeignKey('rloops.id'), primary_key=True)
    rmap_sample_id = Column(String, ForeignKey('rmap_samples.id'), primary_key=True)
    # Extra data
    counts = Column(Integer)
    norm_counts = Column(DECIMAL)
    bpm = Column(DECIMAL)
    peak_qval = Column(DECIMAL)
    # Relationships
    rloop = relationship("RLoops", back_populates="rmap_samples")
    rmap_sample = relationship("RMapSamples", back_populates="rloops")


class SampleQualChar(Base):
    # Quality information on RLoop sequencing experiments
    __tablename__ = "sample_quality_characteristics"

    id = Column(Integer, primary_key=True)
    rmap_sample_id = Column(String, ForeignKey('rmap_samples.id'))
    char_type = Column(String)
    value = Column(DECIMAL)


# Unclear if this is necessary since there is no extra information in the table
# If unnecessary replace with non-class declared Table object containing left and right id foreign keys only
class GenFeatRLooopOverlap(Base):
    # Association table for many-to-many relationship between RLoops and GenomicFeatures
    __tablename__ = "gf_rl_overlap"

    # Foreign keys
    rloop_id = Column(String, ForeignKey('rloops.id'))
    feature_id = Column(String, ForeignKey('genomic_features.id'))
    # Relationships
    rloop = relationship("RLoops", back_populates="genomic_features")
    genomic_feature = relationship("GenomicFeatures", back_populates="rloops")


class GenomicFeatures(Base):
    # Information on genomic features
    __tablename__ = "genomic_features"

    id = Column(String, primary_key=True, index=True)
    type = Column(String)
    source = Column(String)
    location = Column(String)
    # Bidirectional many-to-many relationship with RLoops via GenFeatRLooopOverlap
    rloops = relationship("GenFeatRLoopOverlap", back_populates="genomic_feature")


# Unclear if this is necessary since there is no extra information in the table
# If unnecessary replace with non-class declared Table object containing left and right id foreign keys only
class GeneRLooopOverlap(Base):
    # Association table for many-to-many relationship between RLoops and Genes
    __tablename__ = "gene_rl_overlap"

    # Foreign keys
    rloop_id = Column(String, ForeignKey('rloops.id'))
    gene_id = Column(String, ForeignKey('genes.id'))
    # Relationships
    rloop = relationship("RLoops", back_populates="genes")
    gene = relationship("Genes", back_populates="rloops")


class Genes(Base):
    # Information on genes
    __tablename__ = "genes"

    id = Column(String, primary_key=True, index=True)
    symbol = Column(String)
    description = Column(String)
    biotype = Column(String)
    location = Column(String)
    # Bidirectional many-to-many relationship with RLoops via GeneRLooopOverlap
    rloops = relationship("GeneRLoopOverlap", back_populates="gene")
    # Unidirectional many-to-many relationship with GeneExpSamples via GeneExpression
    gene_exp_samples = relationship("GeneExpression")


class GeneExpression(Base):
    # Association table for many-to-many relationship between Genes and GeneExpSamples
    __tablename__ = 'gene_expression'

    # Foreign keys
    gene_id = Column(String, ForeignKey('genes.id'), primary_key=True)
    exp_sample_id = Column(String, ForeignKey('gene_exp_samples.id'), primary_key=True)
    # Extra data
    counts = Column(Integer)
    tpm = Column(DECIMAL)
    vst = Column(DECIMAL)
    # Relationships
    gene_exp_sample = relationship("GeneExpSamples")


class GeneExpSamples(Base):
    # Information on gene expression samples
    __tablename__ = "gene_exp_samples"

    id = Column(String, primary_key=True, index=True)
    sample_name = Column(String)
    genome = Column(String)
    lab = Column(String)
    study_id = Column(String)
    tissue = Column(String)
    genotype = Column(String)
    treatment = Column(String)
    condition = Column(String)
    paired_end = Column(Boolean)
    type = Column(String)
