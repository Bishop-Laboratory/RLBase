from flask import (Blueprint, request, jsonify)
from werkzeug.exceptions import abort
import json
from sqlalchemy import and_

from .controllers import search_data
from .models import Record

def get_blueprint(db):

    bp = Blueprint('api', __name__)

    @bp.route('/api-v1', methods=['GET'])

    def api_home():
        name = request.args.get('name')

        if name is not None:
            resp = {'greeting': 'Welcome to the RMapDB API, ' + name}
        else:
            resp = {'greeting': 'Welcome to the RMapDB API, friend!'}
        return json.dumps(resp)

    @bp.route("/api/test/db")

    def db_test():
        query_conditions = search_data(request, Record)
        list_found = db.session.query(Record).filter(and_(*query_conditions)).order_by(Record.GSM).all()
        output = []
        for record in list_found:
            output.append(record.to_dict())
        return jsonify(output)

    return bp