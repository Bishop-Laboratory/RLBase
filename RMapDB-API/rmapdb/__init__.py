import os
from flask import Flask, request, jsonify
from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import and_
from flask_cors import CORS
from .controllers import search_data
from .models import create_sample_model

def create_app(test_config=None):
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
        DATABASE=os.path.join(app.instance_path, 'rmapdb.sqlite'),
        SQLALCHEMY_DATABASE_URI='sqlite:///' + os.path.join(app.instance_path, 'sample_data.db')
    )
    CORS(app)
    db = SQLAlchemy(app)
    Sample_data,sample_dataSchema = create_sample_model(app, db)

    if test_config is None:
        # load the instance config, if it exists, when not testing
        app.config.from_pyfile('config.py', silent=True)
    else:
        # load the test config if passed in
        app.config.from_mapping(test_config)

    # ensure the instance folder exists
    try:
        os.makedirs(app.instance_path)
    except OSError:
        pass

    # Init database
    from . import db
    db.init_app(app)

    # Import the webui blueprint
    from . import api
    app.register_blueprint(api.bp)

    # For testing
    @app.route("/hello")
    
    def hello():
        return "Hello world!"

    @app.route("/api/test/rloop-details")

    def rloop_details():
        query_conditions = search_data(request, Sample_data)
        detailed = Sample_data.query.filter(and_(*query_conditions)).order_by(Sample_data.gsm).all()
        rloop_schema = sample_dataSchema(many=True)
        output = rloop_schema.dump(detailed)
        return jsonify(output)

    @app.route("/api/test/rloop")
    def test_rloop():
        query_conditions = search_data(request)
        rloops = Sample_data.query.filter(and_(*query_conditions)).order_by(Sample_data.gsm).all()
        rloop_schema = sample_dataSchema(many=True)
        output = rloop_schema.dump(rloops)
        rloop_list = set()
        for item in output:
            rloop_list.add(item["gsm"])
        rloop_list = list(rloop_list)
        return  jsonify({"rloops": rloop_list})

    return app
