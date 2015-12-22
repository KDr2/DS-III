export GOROOT=$HOME/Work/opensrc/go
export GOARCH=amd64
export GOOS=linux

go >/dev/null 2>&1
if [ $? ]; then
    export PATH=$PATH:$GOROOT/bin  
fi

HERE=`pwd`
export GOPATH=$(realpath $HERE)
