"""Validate generated JSON against published input and output contracts."""
import json
from pathlib import Path
from jsonschema import Draft202012Validator

root = Path(__file__).resolve().parents[1]
for schema_name, data_name in [
    ("input", "inst/extdata/example.json"),
    ("output", "reports/generated/example-result.json"),
    ("input", "inst/extdata/example-clock.json"),
    ("output", "reports/generated/clock-result.json"),
]:
    schema = json.loads((root / f"inst/schema/{schema_name}.schema.json").read_text())
    data = json.loads((root / data_name).read_text())
    Draft202012Validator.check_schema(schema)
    Draft202012Validator(schema).validate(data)
    print(f"PASS: {schema_name} JSON schema")
