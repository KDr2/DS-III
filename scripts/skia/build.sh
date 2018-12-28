#/bin/bash

# for build tool and third-party libraries
python tools/git-sync-deps

# for system provided libraries
# tools/install_dependencies.sh


# CHANGE OUT DIR and ARGS
# => bin/gn gen out/OUT_DIR --args=$ARGS
# list all arguments:
# = bin/gn args out/Debug --list

bin/gn gen out/Shared --args='is_official_build=true is_component_build=true'
