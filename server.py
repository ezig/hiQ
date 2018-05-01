from hiq import compile_from_json

import json

from flask import Flask, request
from flask_cors import CORS

app = Flask(__name__)
CORS(app)

@app.route("/compile", methods=["POST"])
def compile():
	return compile_from_json(json.loads(request.data)["circuit"], "ibmqx4")

if __name__ == '__main__':
	app.run(debug=True)