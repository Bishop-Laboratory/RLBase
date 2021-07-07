from sqlalchemy import create_engine, inspect, MetaData, Table, Column, VARCHAR, INT

engine = create_engine('sqlite:///GenomeDB_test_simon.sqlite')

metadata = MetaData()

connection = engine.connect()

inspector = inspect(engine)

print(inspector.get_table_names())

genes = Table('genes', metadata,
              Column('EnsemblID', VARCHAR()),
              Column('Gene_symbol', VARCHAR()),
              Column('Chr', INT()),
              Column('start', INT()),
              Column('end', INT()),
              Column('Gene_biotype', VARCHAR()),
              Column('Description', VARCHAR()))

genomic_features = Table('genomic_features', metadata,
                         Column('Feature_ID', VARCHAR()),
                         Column('Feature_Type', VARCHAR()),
                         Column('Chr', INT()),
                         Column('start', INT()),
                         Column('end', INT()))

RLFS = Table('RLFS', metadata,
             Column('RLFS_ID', VARCHAR()),
             Column('Chr', INT()),
             Column('start', INT()),
             Column('end', INT()))

metadata.create_all(engine)

print(inspector.get_table_names())
print(genes.columns)

