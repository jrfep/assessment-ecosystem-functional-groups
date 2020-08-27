export MIHOST=$(hostname -s)

case $MIHOST in
terra)
  export GISDATA=/opt/gisdata
  export GISDB=/opt/gisdb
  export SCRIPTDIR=/home/jferrer/proyectos/
  export WORKDIR=$HOME/tmp/
  ;;
roraima)
  export GISDATA=$HOME/Cloudstor/Shared/
  export GISDB=$HOME/gisdb
  export SCRIPTDIR=$HOME/proyectos/
  export WORKDIR=$HOME/tmp/
  ;;
esac

# store the zenodo API token in a file in home directory
export ZENODOTOKEN=$(cat $HOME/.ZenodoToken)

export PROJECTNAME=assessment-ecosystem-functional-groups
export SCRIPTDIR=$SCRIPTDIR/UNSW/$PROJECTNAME
export WORKDIR=$WORKDIR/$PROJECTNAME
export GISOUT=$WORKDIR/indicative-maps/

mkdir -p $WORKDIR
mkdir -p $GISOUT
