from flask_marshmallow import Marshmallow

def create_sample_model(app, db):
    ma = Marshmallow(app)
    class Sample_data(db.Model):
        gsm = db.Column(db.String(50), primary_key=True)
        experiment = db.Column(db.String(50),nullable=True)
        cell = db.Column(db.String(50),nullable=True)
        genotype = db.Column(db.String(50),nullable=True)
        srx = db.Column(db.String(50),nullable=True)
        control = db.Column(db.String(50),nullable=True)
        mode = db.Column(db.String(50),nullable=True)
        log2_ratio_3utr = db.Column("3UTR__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_5utr = db.Column("5UTR__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_retroposon = db.Column("Retroposon__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_rna = db.Column("RNA__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_mi_rna = db.Column("miRNA__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_nc_rna = db.Column("ncRNA__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_tts = db.Column("TTS__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_line = db.Column("LINE__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_srp_rna = db.Column("srpRNA__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_sine = db.Column("SINE__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_rc = db.Column("RC__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_t_rna = db.Column("tRNA__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_pseudo = db.Column("pseudo__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_dna = db.Column("DNA__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_exon = db.Column("Exon__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_intron = db.Column("Intron__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_intergenic = db.Column("Intergenic__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_simple_repeat = db.Column("Simple_repeat__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        log2_ratio_low_complexity = db.Column("Low_complexity__Log2 Ratio (obs/exp)", db.Float, quote=True, nullable=True)
        
        def __repr__(self):
            return '<GSM %r>' % self.gsm

    class sample_dataSchema(ma.SQLAlchemyAutoSchema):
        class Meta:
            model = Sample_data
    return Sample_data,sample_dataSchema