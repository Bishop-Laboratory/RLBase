# coding: utf-8
from sqlalchemy import create_engine
from sqlathanor.automap  import automap_base
from sqlathanor import declarative_base, AttributeConfiguration

engine = create_engine("sqlite:///sample_data.db")
Base = automap_base()
Base.prepare(engine, reflect = True)

Record = Base.classes.sample_data

featureList = ["GSM", "Cell", "SRX","mode","Species","Condition","Group","Genotype",
'3UTR__Log2 Ratio (obs/exp)','5UTR__Log2 Ratio (obs/exp)',
'TTS__Log2 Ratio (obs/exp)','Exon__Log2 Ratio (obs/exp)','Intron__Log2 Ratio (obs/exp)',
'Intergenic__Log2 Ratio (obs/exp)', 'Retroposon__Log2 Ratio (obs/exp)', 'SINE__Log2 Ratio (obs/exp)',
 'Low_complexity__Log2 Ratio (obs/exp)','Simple_repeat__Log2 Ratio (obs/exp)',
 'rRNA__Log2 Ratio (obs/exp)','RNA__Log2 Ratio (obs/exp)','ncRNA__Log2 Ratio (obs/exp)',
 'RC__Log2 Ratio (obs/exp)','pseudo__Log2 Ratio (obs/exp)','scRNA__Log2 Ratio (obs/exp)']

serialization = []
for feature in featureList:
    serialization.append(AttributeConfiguration(name =  feature,
                                              supports_csv = True,
                                              csv_sequence = 1,
                                              supports_json = True,
                                              supports_yaml = True,
                                              supports_dict = True,
                                              on_serialize = None,
                                              on_deserialize = None))

Record.__serialization__ = serialization