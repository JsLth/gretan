#!/bin/bash

$PYTHON_PATH -m venv /home/shiny/.virtualenvs/gretan && \
$RETICULATE_PYTHON -m pip install --upgrade --no-user pip wheel setuptools && \
$RETICULATE_PYTHON -m pip install --no-user --ignore-installed -r inst/requirements.txt
