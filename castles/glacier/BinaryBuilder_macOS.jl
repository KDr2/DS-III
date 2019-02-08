using BinaryBuilder

name = "LibTest"
version = v"0.0.1"

# Collection of sources required to build Libtask
function get_commit_id()
    is_pr = get(ENV, "TRAVIS_PULL_REQUEST", "false")
    if is_pr != "false"
        return get(ENV, "TRAVIS_PULL_REQUEST_SHA", "")
    end
    return readlines(`git rev-parse HEAD`)[1]
end

sources = [
    "https://github.com/KDr2/DS-III.git" => get_commit_id(),
]

# Bash recipe for building across all platforms
script = raw"""
uname -a | tee $prefix/uname
gcc --version | tee $prefix/gcc-version
cat > main.c <<EOF
int main(){ return 0;}
EOF
$CC main.c -o $prefix/exe
"""

# These are the platforms we will build for by default, unless further
# platforms are passed in on the command line
platforms = [
    # Linux(:i686, :glibc),
    # Linux(:x86_64, :glibc),
    # Linux(:armv7l, :glibc, :eabihf),
    MacOS(:x86_64),
    # Windows(:i686),
    # Windows(:x86_64),
]

# The products that we will ensure are always built
products(prefix) = [
    LibraryProduct(prefix, "libtest", :libtask)
]

# Dependencies that must be installed before this package can be built
dependencies = [
]

# Build the tarballs, and possibly a `build.jl` as well.
build_tarballs(ARGS, name, version, sources, script, platforms, products, dependencies)
