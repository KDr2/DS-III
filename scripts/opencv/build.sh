mkdir -p release
cd release
cmake -D OPENCV_EXTRA_MODULES_PATH=../../opencv_contrib-4.0.0/modules \
      -D BUILD_opencv_apps=yes \
      -D CMAKE_BUILD_TYPE=RELEASE \
      -D CMAKE_INSTALL_PREFIX=$HOME/programs/opencv ..

# rm ../CMakeCache.txt if it tells you are not in a separate dir

# dependencies
# - VTK
# - OpenBlas
# - Eigen
# - HDF5
# - freetype2
# - barfbuzz
