#
#  PROCESS Ubuntu dependencies install script
#
#  J. Morris
#  UKAEA
#  05.03.20
#
#  usage: sudo ./scripts/install_ubuntu_dependencies.sh

echo "Installing PROCESS Ubuntu dependencies"

declare DEBIAN_FRONTEND=noninteractive
sudo apt-get update -y
sudo apt-get update

sudo apt-get install -y --fix-missing\
    gfortran \
    gdb \
    make \
    cmake \
    gcc  \
    g++  \
    libgtest-dev \
    git \
    python3 \
    binutils \
    libblas-dev \
    liblapack-dev \
    liblapack3 \
    texlive-base \
    texlive-latex-extra \
    texlive-font-utils \
    pandoc \
    python-dev \
    python3-pip \
    pandoc \
    graphviz

# compile googletest
cd /usr/src/gtest && sudo cmake CMakeLists.txt && sudo make && 
    sudo rm -rf lib && sudo mkdir lib && sudo cp *.a lib
