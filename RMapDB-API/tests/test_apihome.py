import json


def test_home(client):
    response = client.get("/api-v1")
    data = response.json()
    assert data["greeting"] == "Welcome to the RMapDB API, friend!"


def test_name(client):
    response = client.get("/api-v1?name=Augustin")
    data = response.json()
    assert data["greeting"] == "Welcome to the RMapDB API, Augustin"
