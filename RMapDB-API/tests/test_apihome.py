from rmapdb import create_app
import json


def test_config():
    assert not create_app().testing
    assert create_app({'TESTING': True}).testing


def test_home(client):
    response = client.get('/api-v1')
    data = json.loads(response.data)
    assert data['greeting'] == 'Welcome to the RMapDB API, friend!'


def test_name(client):
    response = client.get('/api-v1?name=Augustin')
    data = json.loads(response.data)
    assert data['greeting'] == 'Welcome to the RMapDB API, Augustin'
