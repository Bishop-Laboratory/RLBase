import json


def test_gene_info_1(client):
    """Test gene info endpoint. Query by gene ID"""
    response = client.get('/api-v1/gene-info?gene_id=ENSG00000141510')
    resdict = json.loads(response.data)[0]
    assert resdict['gene_symbol'] == "TP53"


def test_gene_info_2(client):
    """Test gene info endpoint. Query by gene symbol"""
    response = client.get('/api-v1/gene-info?gene_symbol=NEAT1')
    resdict = json.loads(response.data)[0]
    assert resdict['gene_id'] == "ENSG00000245532"


def test_gene_alias(client):
    """Test gene alias endpoint. Query by alias"""
    response = client.get('/api-v1/gene-aliases?alias_symbol=P16')
    resdict = json.loads(response.data)
    genes = [entry['gene_id'] for entry in resdict]
    assert "ENSG00000147889" in genes


def test_gene_expression_1(client):
    """Test gene expression endpoint. Query by gene ID"""
    response = client.get('/api-v1/expression?gene_id=ENSG00000141510')
    resdict = json.loads(response.data)
    samps = [entry['sample_id'] for entry in resdict]
    assert {'SRX10300306', 'SRX10300305', 'SRX10300304'}.issubset(samps)


def test_gene_expression_2(client):
    """Test gene expression endpoint. Query by Sample ID"""
    response = client.get('/api-v1/expression?sample_id=SRX10300304')
    resdict = json.loads(response.data)
    genes = [entry['gene_id'] for entry in resdict]
    assert {'ENSG00000141510', 'ENSG00000012048', 'ENSG00000245532'}.issubset(genes)


def test_gene_expression_3(client):
    """Test gene expression endpoint. Query by Sample ID and Gene ID"""
    response = client.get('/api-v1/expression?sample_id=SRX10300304&gene_id=ENSG00000141510')
    resdict = json.loads(response.data)[0]
    assert resdict['gene_id'] == "ENSG00000141510"


def test_deg_1(client):
    """Test DEG endpoint. Query by gene ID"""
    response = client.get('/api-v1/deg?gene_id=ENSG00000012048')
    resdict = json.loads(response.data)[0]
    assert resdict['gene_id'] == "ENSG00000012048"


def test_deg_2(client):
    """Test DEG endpoint. Query by Sample ID"""
    response = client.get('/api-v1/deg?study_id=SRP120020193')
    resdict = json.loads(response.data)
    genes = [entry['gene_id'] for entry in resdict]
    assert {'ENSG00000141510', 'ENSG00000012048', 'ENSG00000245532'}.issubset(genes)


def test_deg_3(client):
    """Test DEG endpoint. Query by Sample ID and Gene ID"""
    response = client.get('/api-v1/deg?gene_id=ENSG00000012048&study_id=SRP120020193')
    resdict = json.loads(response.data)[0]
    assert resdict['gene_id'] == "ENSG00000012048"


def test_samples_1(client):
    """Test samples endpoint. Query by gene ID"""
    response = client.get('/api-v1/samples?study_id=SRP120020193')
    resdict = json.loads(response.data)
    samps = [entry['sample_id'] for entry in resdict]
    assert {'SRX10300306', 'SRX10300305', 'SRX10300304'}.issubset(samps)


def test_samples_2(client):
    """Test samples endpoint. Query by Sample ID"""
    response = client.get('/api-v1/samples?tissue=Lung')
    resdict = json.loads(response.data)
    samps = [entry['sample_id'] for entry in resdict]
    assert {'SRX10300306', 'SRX10300305', 'SRX10300304'}.issubset(samps)


def test_samples_3(client):
    """Test samples endpoint. Query by Sample ID and Gene ID"""
    response = client.get('/api-v1/samples?study_id=SRP120020193&replicate=1')
    resdict = json.loads(response.data)[0]
    assert resdict['sample_id'] == "SRX10300304"
