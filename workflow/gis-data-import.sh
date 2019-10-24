source env/$(hostname -s).sh
mkdir -p $WORKDIR
cd $WORKDIR
export ZENODOURL="-- UPDATE HERE THE STABLE URL OF THE ZIPFILES --"
wget --continue $ZENODOURL
