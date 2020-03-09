#
#   PROCESS install script
#
#   J. Morris
#   UKAEA
#   05.03.20
#

source scripts/version_compare.sh

echo "PROCESS install script"

echo "- Detecting OS"

if [ "$OSTYPE" == "linux-gnu" ]; then
    . /etc/os-release
    declare MY_OS=$ID
    declare MY_OS_VERSION=$VERSION_ID
    echo "--" $MY_OS $VERSION_ID
    if [ "$MY_OS" == "ubuntu" ]; then
        version_required="18.0"
        if version_gt $MY_OS_VERSION $version_required; then
            sudo bash scripts/install_ubuntu_dependencies.sh
            sudo pip3 install -r requirements.txt

            # Setting python to python 3
            echo "- Aliasing python to python3 in bashrc if doesn't already exist."
            grep -qxF 'alias python="python3"' ~/.bashrc || \
                echo 'alias python="python3"' >> ~/.bashrc

            # Modify PATH so "ford" command can be found in cmake file
            echo "- Adding ~/.local/bin to PATH"
            export PATH=$PATH:~/.local/bin
            grep -qxF 'export PATH=$PATH:~/.local/bin' ~/.bashrc || \
                echo 'export PATH=$PATH:~/.local/bin' >> ~/.bashrc

            echo "- Setting language to UTF8"
            export LANG=C.UTF-8
            grep -qxF 'export LANG=C.UTF-8' ~/.bashrc || \
                echo 'export LANG=C.UTF-8' >> ~/.bashrc

            git clone https://git.ccfe.ac.uk/process/process.git
            export GTEST='/usr/src/gtest/'
            grep -qxF 'export GTEST="/usr/src/gtest/"' ~/.bashrc || \
                echo 'export GTEST="/usr/src/gtest/"' ~/.bashrc >> ~/.bashrc

            # Set PYTHONPATH to utilities
            echo "- Setting Pythonpath"
            export SRC_PATH=$(pwd)
            # grep -qxF "export PYTHONPATH=$PYTHONPATH:$SRC_PATH/utilities ~/.bashrc" || \
                # echo "export PYTHONPATH=$PYTHONPATH:$SRC_PATH/utilities >> ~/.bashrc"

            cd process
            cmake -H. -Bbuild
            cmake --build build
            cmake --build build --target dicts
            pip3 install -e .

        else
            echo "ERROR :: Ubuntu version must be > 18.04"
            echo "ERROR :: Detected version $MY_OS_VERSION"
        fi
    else
        echo "WARNING :: Not on Ubuntu. Install script not compatible."
    fi
else
    echo "WARNING :: Not on Ubuntu. Install script not compatible."
fi
