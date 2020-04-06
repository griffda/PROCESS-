# 
#  PROCESS CI Docker File
#  J. Morris
#  11.02.20
#

# start by building the basic container
FROM ubuntu:18.04
LABEL James Morris <james.morris2@ukaea.uk>

# Initial update
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update -y
RUN apt-get update

# add gfortran, debugging tools and make
RUN apt-get install -y gfortran-4.8 
RUN apt-get install -y gdb
RUN apt-get install -y make
RUN apt-get install -y cmake
RUN apt-get install -y gcc 
RUN apt-get install -y g++ 
RUN apt-get install -y libgtest-dev
RUN apt-get install -y git
RUN apt-get install -y python3
RUN apt-get install -y binutils
RUN apt-get install -y libblas-dev liblapack-dev liblapack3

# Latex libraries
RUN apt-get install -y texlive-base 
RUN apt-get install -y texlive-latex-extra 
RUN apt-get install -y texlive-font-utils 
RUN apt-get install -y pandoc 

# Compile googletests
RUN cd /usr/src/gtest && cmake CMakeLists.txt && make && cp *.a /usr/lib
ENV GTEST /usr/src/googletest/googletest/
RUN cd /usr/src/googletest/googletest/ && mkdir lib && cd lib && cp /usr/src/gtest/*.a .

# Check versions and link gfortran-8
RUN cmake --version
RUN g++ --version
RUN gfortran-4.8 --version
RUN ln -sf /usr/bin/gfortran-4.8 /usr/bin/gfortran

# Libraries for MKdocs
RUN apt-get install -y python-dev 
RUN apt-get install -y python3-pip 
RUN apt-get install -y pandoc 
RUN apt-get install -y graphviz
RUN pip3 install mkdocs
RUN pip3 install mkdocs-material
RUN pip3 install pymdown-extensions

# Python libraries
RUN pip3 install numpy
RUN pip3 install scipy
RUN pip3 install matplotlib
RUN pip3 install pathlib

# Get anybadge
RUN pip3 install pillow

# Set locale
# RUN export LANG=C.UTF-8
# configure the container to list directory by default
# ENTRYPOINT ["/bin/bash"]