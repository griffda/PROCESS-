#
#   String version compare
#   
#   J. Morris
#   UKAEA
#   05.03.20
#

function version_gt() { 
    test "$(printf '%s\n' "$@" | sort -V | head -n 1)" != "$1";
}