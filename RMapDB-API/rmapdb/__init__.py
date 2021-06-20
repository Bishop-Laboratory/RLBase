import os
from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from flask_cors import CORS
from sqlathanor import FlaskBaseModel, initialize_flask_sqlathanor

def create_app(test_config=None):
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)
    app.config.from_mapping(
        SECRET_KEY='dev',
        DATABASE=os.path.join(app.instance_path, 'rmapdb.sqlite'),
        SQLALCHEMY_DATABASE_URI='sqlite:///sample_data.db',
        SQLALCHEMY_TRACK_MODIFICATIONS = False)

    CORS(app)
     
    alchemy_db = SQLAlchemy(app, model_class = FlaskBaseModel)
    alchemy_db = initialize_flask_sqlathanor(alchemy_db)

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
    app.register_blueprint(api.get_blueprint(alchemy_db))

    # For testing
    @app.route("/hello")
    
    def hello():
        return "Hello world!"

    return app
