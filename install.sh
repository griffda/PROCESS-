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

            echo "- Linking gfortran to /usr/bin/gfortran"
            alias gfortran='/usr/bin/gfortran-4.8'
            grep -qxF "alias gfortran='/usr/bin/gfortran-4.8'" ~/.bashrc || \
                echo "alias gfortran='/usr/bin/gfortran-4.8'" >> ~/.bashrc
            sudo ln -sf /usr/bin/gfortran-4.8 /usr/bin/gfortran

            # Set PYTHONPATH to utilities
            echo "- Setting Pythonpath"
            export SRC_PATH=$(pwd)
            grep -qxF "export PYTHONPATH=$PYTHONPATH:$SRC_PATH/utilities" ~/.bashrc || \
                echo "export PYTHONPATH=$PYTHONPATH:$SRC_PATH/utilities" >> ~/.bashrc
            
            cmake -H. -Bbuild
            cmake --build build
            cmake --build build --target dicts
            sudo pip3 install -e .

            echo "Check your ~/.bashrc for duplicate entries if you have installed"\
                "process in other locations previously."

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
