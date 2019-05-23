#/bin/bash

# for build tool and third-party libraries
python tools/git-sync-deps

# for system provided libraries
# tools/install_dependencies.sh


# CHANGE OUT DIR and ARGS
# => bin/gn gen out/OUT_DIR --args=$ARGS
# list all arguments:
# = bin/gn args out/Debug --list

# some useful args:
cat > /dev/null <<EOF
is_official_build=true
is_component_build=true
skia_use_system_icu=false
skia_use_system_libjpeg_turbo=false
skia_use_system_libpng=false
skia_use_system_libwebp=false
skia_use_system_harfbuzz=false
extra_cflags=["-static-libstdc++", "-static-libgcc"]
EOF


bin/gn gen out/Shared --args='is_official_build=true is_component_build=true'

cat > /dev/null <<EOF
# run test, see https://skia.org/dev/testing/testing
# python tools/git-sync-deps
bin/gn gen out/Debug
ninja -C out/Debug dm
out/Debug/dm -v -w dm_output
EOF
