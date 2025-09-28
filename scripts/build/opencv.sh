mkdir -p release
cd release
cmake -D OPENCV_EXTRA_MODULES_PATH=../../opencv_contrib-4.0.0/modules \
      -D BUILD_opencv_apps=yes \
      -D WITH_GTK_2_X=yes -D WITH_OPENGL=yes \
      -D CMAKE_BUILD_TYPE=RELEASE \
      -D CMAKE_INSTALL_PREFIX=$PROG_DIR/opencv ..

# - rm ../CMakeCache.txt if it tells you are not in a separate dir
# - `-D WITH_GTK_2_X=yes` or `-D WITH_QT=yes` is required for GL

# dependencies
# - VTK
# - OpenBlas
# - Eigen
# - HDF5
# - freetype2
# - barfbuzz
