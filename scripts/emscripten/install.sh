PROGRAMS_DIR=$HOME/programs

if [[ ! -d $PROGRAMS_DIR/emsdk ]]; then
    git clone https://github.com/emscripten-core/emsdk.git $PROGRAMS_DIR/emsdk
    cd $PROGRAMS_DIR/emsdk
    # Fetch the latest version of the emsdk (not needed the first time you clone)
    git pull

    # Download and install the latest SDK tools.
    ./emsdk install latest

    # Make the "latest" SDK "active" for the current user. (writes .emscripten file)
    ./emsdk activate latest
fi

# Activate PATH and other environment variables in the current terminal
cd $PROGRAMS_DIR/emsdk
source ./emsdk_env.sh
