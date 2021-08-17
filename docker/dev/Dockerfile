# Development image for Process, used in challenging environments (Mac) to give
# identical results to CI or to quickly provide a working environment to a 
# casual user. Differs from CI image by not using virtual environments for 
# differt Python versions and doesn't require the CI script to perform extra
# installations
FROM ubuntu:20.04
LABEL description="Process development image"

# Required to get around interactive timezone prompt
ENV DEBIAN_FRONTEND=noninteractive

# Install apt packages
RUN apt-get update && apt-get install -y \
    gdb \
    gfortran \
    git \
    graphviz \
    libblas-dev \
    liblapack-dev \
    liblapack3 \
    python3-pip \
    python-is-python3

# Install Python packages into a virtual environment
# Use virtualenv to prevent "running pip as root" warnings
RUN python -m pip install --upgrade pip && \
    python -m pip install \
    cmake \
    Markdown \
    matplotlib \
    mkdocs \
    mkdocs-material \
    netCDF4 \
    numpy \
    pathlib \
    pillow \
    pymdown-extensions \
    pytest \
    scipy \
    toposort \
    wheel && \
    # Install f90wrap independently as numpy is a prerequisite for its setup.py!
    #TODO: Temporary fix for numpy-f90wrap bug using commit id whilst awaiting 
    # a tagged release for f90wrap
    python -m pip install git+https://github.com/jameskermode/f90wrap.git@78918e84

ENV FC=gfortran