# Docker image for CI system
FROM ubuntu:latest
LABEL Kristian Zarebski <kristian.zarebski@ukaea.uk>

# Required to get around interactive timezone prompt
ENV DEBIAN_FRONTEND=noninteractive

# Set encoding to UTF-8 - otherwise f90wrap install fails.
ENV LANG C.UTF-8

# Install apt packages
# gfortran, debugging tools, git, maths and Latex libraries
# poppler-utils is required by pdf2image
# This syntax is recommended by Docker to bust caching
RUN apt-get update && apt-get install -y \
    gdb \
    gfortran \
    git \
    graphviz \
    libblas-dev \
    liblapack-dev \
    liblapack3 \
    pandoc \
    poppler-utils \
    python3-pip \
    python-is-python3 \
    texlive-base \
    texlive-font-utils \
    texlive-latex-extra

# Install apt packages for pyenv.
RUN apt-get install -y \
    build-essential \
    curl \
    libbz2-dev \
    libffi-dev \
    liblzma-dev \
    libncurses5-dev \
    libncursesw5-dev \
    libreadline-dev \
    libsqlite3-dev \
    libssl-dev \
    llvm \
    python-openssl \
    tk-dev \
    wget \
    xz-utils \
    zlib1g-dev

# Install pyenv.
RUN curl https://pyenv.run | bash
ENV PYENV_ROOT /root/.pyenv
ENV PATH $PYENV_ROOT/shims:$PYENV_ROOT/bin:$PATH

# Install python version 3.6.9 and set as global.
RUN pyenv install 3.6.9 && \
    pyenv global 3.6.9

# Build-only environment variable to store the list of required python packages.
ENV PROCESS_PYTHON_PACKAGES="cmake \
    matplotlib \
    mkdocs \
    mkdocs-material \
    numpy \
    pathlib \
    pillow \
    pymdown-extensions \
    scipy"

# Install Python packages into virtualenv for Python 3.6
# cmake, MKdocs
RUN python -m pip install --upgrade pip && \
    python -m pip install virtualenv && \
    python -m virtualenv ~/.venv_py36 && \
    . ~/.venv_py36/bin/activate && \
    python -m pip install ${PROCESS_PYTHON_PACKAGES} && \
    deactivate

# Set the python version back to system as default.
RUN pyenv global system

# Install Python packages into a virtualenv for Python 3.8 (system Python)
# cmake, MKdocs
RUN python -m pip install --upgrade pip && \
    python -m pip install virtualenv && \
    python -m virtualenv ~/.venv_py38 && \
    . ~/.venv_py38/bin/activate && \
    python -m pip install ${PROCESS_PYTHON_PACKAGES} && \
    deactivate

# Clear build-only environment variable.
ENV PROCESS_PYTHON_PACKAGES=
