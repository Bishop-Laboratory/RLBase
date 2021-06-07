# API backend README

## Set up and run the API server in a dev environment

To set up the API server, clone this repo:

```shell
git clone https://github.com/Bishop-Laboratory/RMapDB-v2.git
```

Then, change to the repo folder and install dependencies:

```shell
cd RMapDB-API/
python -m pip install --upgrade pip
bash install.sh
```

Run all tests to ensure the app works:

```shell
source env/bin/activate
coverage run -m pytest
coverage report -m
```

Launch the backend server (this is debugging mode):

```shell
bash run_server.sh
```

