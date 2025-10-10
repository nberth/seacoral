# Klee Evaluation

## Running locally

### Setup

```shell
python3 -m venv venv
./venv/bin/python3 -m pip install pandas jinja2 
```

### Run

```shell
./venv/bin/python3 ./utils/klee-evals.py \
	--inputs test/c-tests/SAC22 \
	--output-dir /tmp/klee-evals \
	--compute
```
