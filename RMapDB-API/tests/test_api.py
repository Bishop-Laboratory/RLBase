import json

# 
# def test_gene_info_1(client):
#     """Test gene info endpoint. Query by gene ID"""
#     response = client.get('/api-v1/gene-info?gene_id=ENSG00000141510')
#     resdict = json.loads(response.data)[0]
#     assert resdict['gene_symbol'] == "TP53"
# 
# 
# def test_gene_info_2(client):
#     """Test gene info endpoint. Query by gene symbol"""
#     response = client.get('/api-v1/gene-info?gene_symbol=NEAT1')
#     resdict = json.loads(response.data)[0]
#     assert resdict['gene_id'] == "ENSG00000245532"
# 
# 
# def test_gene_alias(client):
#     """Test gene alias endpoint. Query by alias"""
#     response = client.get('/api-v1/gene-aliases?alias_symbol=P16')
#     resdict = json.loads(response.data)
#     genes = [entry['gene_id'] for entry in resdict]
#     assert "ENSG00000147889" in genes


def test_samples_1(client):
    """Test samples endpoint. Query by gene ID"""
    response = client.get('/api-v1/rmap-samples?study_id=SRP058311')
    resdict = json.loads(response.data)
    samps = [entry['sample_id'] for entry in resdict]
    assert {'SRX1025902', 'SRX1025898', 'SRX1025903'}.issubset(samps)
    

def test_rloops_1(client):
    """Test samples endpoint. Query by gene ID"""
    response = client.get('/api-v1/rloops?id=RL88229')
    resdict = json.loads(response.data)
    types = [entry['type'] for entry in resdict]
    assert {'Co-transcriptional'}.issubset(types)
