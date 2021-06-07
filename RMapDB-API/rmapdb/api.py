from flask import (
    Blueprint, flash, g, redirect, render_template, request, url_for
)
from werkzeug.exceptions import abort
from rmapdb.db import get_db
import json

bp = Blueprint('api', __name__)


@bp.route('/api-v1', methods=['GET'])
def api_home():
    name = request.args.get('name')
    if name is not None:
        resp = {'greeting': 'Welcome to the RMapDB API, ' + name}
    else:
        resp = {'greeting': 'Welcome to the RMapDB API, friend!'}
    return json.dumps(resp)
