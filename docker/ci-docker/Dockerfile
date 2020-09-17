# 
#  PROCESS CI Docker File
#  J. Morris
#  K. Zarebski
#  11.09.20
#

# start by building the basic container
FROM ubuntu:latest
LABEL James Morris <james.morris2@ukaea.uk>, Kristian Zarebski <kristian.zarebski@ukaea.uk>

# Initial update
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update -y
RUN apt-get update
RUN apt-get upgrade -y

# add gfortran, debugging tools and make
RUN apt-get install -y --no-install-recommends apt-utils
RUN apt-get install -y gfortran-10
RUN apt-get install -y gdb
RUN apt-get install -y make
RUN apt-get install -y cmake
RUN apt-get install -y gcc-10
RUN apt-get install -y g++-10
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

# Symlink g++
RUN ln -s /usr/bin/g++-10 /usr/bin/g++

# Compile googletests
RUN cmake -H/usr/src/gtest -B/usr/src/gtest/build
RUN cmake --build /usr/src/gtest/build
RUN mkdir -p /usr/src/googletest/googletest/lib/
ENV GTEST /usr/src/googletest/googletest/
RUN cp /usr/src/gtest/build/lib/*.a /usr/src/googletest/googletest/lib/

# Check versions and link gfortran-8
RUN cmake --version
RUN g++ --version
RUN gfortran-10 --version
RUN ln -sf /usr/bin/gfortran-10 /usr/bin/gfortran

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