FROM python:latest
LABEL Kristian Zarebski <kristian.zarebski@ukaea.uk>

ENV DEBIAN_FRONTEND noninteractive
RUN apt update -y
RUN apt upgrade -y
RUN apt install python3-pip -y
RUN pip install --upgrade pip
RUN pip install --upgrade cmake

# add gfortran, debugging tools and make
RUN apt-get install -y gfortran
RUN apt-get install -y gdb
RUN apt-get install -y libblas-dev liblapack-dev liblapack3

# Latex libraries
RUN apt-get install -y texlive-base 
RUN apt-get install -y texlive-latex-extra 
RUN apt-get install -y texlive-font-utils 
RUN apt-get install -y pandoc 

# Libraries for MKdocs
RUN apt-get install -y graphviz
RUN pip3 install mkdocs
RUN pip3 install mkdocs-material
RUN pip3 install pymdown-extensions

# Python libraries
RUN pip install numpy
RUN pip install scipy
RUN pip install matplotlib
RUN pip install pathlib

# Get anybadge
RUN pip install pillow

# f90wrapper
RUN pip install f90wrap