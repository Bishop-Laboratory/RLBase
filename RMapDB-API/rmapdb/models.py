# coding: utf-8
from sqlalchemy import create_engine
from sqlathanor.automap  import automap_base
from sqlathanor import declarative_base, AttributeConfiguration

engine = create_engine("sqlite:///sample_data.db")
Base = automap_base()
Base.prepare(engine, reflect = True)

Record = Base.classes.sample_data

featureList = ["GSM", "Cell", "SRX","mode","Species","Condition","Group","Genotype",]
measures = ['Intergenic', 'Simple_repeat', 'Satellite', 'Promoter', 'pseudo', 'Intron', 'TTS', 
'LINE', 'LTR', 'SINE', 'DNA', 'CpG-Island', 'ncRNA', 'Low_complexity', 'Exon', 
'snRNA', 'Retroposon', '5UTR', '3UTR', 'srpRNA', 'rRNA', 'tRNA', 'RC', 'scRNA', 
'miRNA', 'RNA', 'snoRNA']
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
for feature in measures:
    serialization.append(AttributeConfiguration(name =  feature + "__Log2 Ratio (obs/exp)",
                                              supports_csv = True,
                                              csv_sequence = 1,
                                              supports_json = True,
                                              supports_yaml = True,
                                              supports_dict = True,
                                              on_serialize = None,
                                              on_deserialize = None))

Record.__serialization__ = serialization