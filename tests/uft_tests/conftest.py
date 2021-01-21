import pytest
import json
import os

UFT_RESULTS = os.path.join(
    os.path.dirname(__file__),
    'uft_results.json'
)


@pytest.fixture(scope="module")
def uft_data():
    return json.load(open(UFT_RESULTS))
