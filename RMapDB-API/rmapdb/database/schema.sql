DROP TABLE IF EXISTS samples;
DROP TABLE IF EXISTS genes;
DROP TABLE IF EXISTS gene_exp;
DROP TABLE IF EXISTS degs;

CREATE TABLE samples (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  study TEXT,
  study_info TEXT,
  sample_name TEXT UNIQUE NOT NULL
);

CREATE TABLE genes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  ensemble_id TEXT UNIQUE NOT NULL,
  gene_symbol TEXT,
  gene_biotype TEXT,
  gene_location TEXT
);

CREATE TABLE gene_exp (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  ensemble_id TEXT NOT NULL,
  sample TEXT NOT NULL,
  counts NUMERIC,
  cpm NUMERIC,
  rpkm NUMERIC,
  tpm NUMERIC
);

CREATE TABLE degs (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  ensemble_id TEXT NOT NULL,
  study TEXT,
  log2fc NUMERIC,
  pval NUMERIC,
  padj NUMERIC
);
