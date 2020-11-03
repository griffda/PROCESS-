# For Python conversion work on 1092 branch
FROM ubuntu:latest
LABEL Kristian Zarebski <kristian.zarebski@ukaea.uk>

# Required to get around interactive timezone prompt
ENV DEBIAN_FRONTEND=noninteractive

# Install apt packages
# gfortran, debugging tools, git, maths and Latex libraries
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
    python3-pip \
    texlive-base \
    texlive-font-utils \
    texlive-latex-extra

# Install Python packages
# cmake, MKdocs
RUN pip3 install \
    cmake \
    matplotlib \
    mkdocs \
    mkdocs-material \
    numpy \
    pathlib \
    pillow \
    pymdown-extensions \
    scipy

# Install f90wrap separately: f90wrap's setup.py contains a numpy import which
# is not handled correctly. Need to ensure numpy is installed before installing
# f90wrap
RUN pip3 install f90wrap