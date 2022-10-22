## Human Foot Print

Global terrestrial Human Footprint maps for years between 2000 and 2013.

## DOCUMENTATION:

#### Citation

> Venter O, Sanderson EW, Magrach A, Allan JR, Beher J, Jones KR, Possingham HP, Laurance WF, Wood P, Fekete BM, Levy MA, Watson JE (2016) Global terrestrial Human Footprint maps for 1993 and 2009. Scientific Data 3: 160067. https://doi.org/10.1038/sdata.2016.67
> Venter O, Sanderson EW, Magrach A, Allan JR, Beher J, Jones KR, Possingham HP, Laurance WF, Wood P, Fekete BM, Levy MA, Watson JEM (2016) Data from: Global terrestrial Human Footprint maps for 1993 and 2009. Dryad Digital Repository. https://doi.org/10.5061/dryad.052q5.2

#### Data access


We use the latest version of HFP shared by James Watson


#### Data preparation

```sh
##wget --continue ... # Data shared by James Watson and Oscar Venter, not publicly available yet
conda deactivate

grass --text -c hfp_meris_int/hfp2000_merisINT.tif $GISDB/raw/HFP
##grass --gui $GISDB/raw/HFP/PERMANENT

##  use r.external to link the file (avoiding duplicated data files)
for k in 2000 2005 2010 2013
do
   r.external input=$GISDATA/humanimpact/HFP/hfp_meris_int/hfp${k}_merisINT.tif output=HFP${k}i
   r.colors map=HFP${k}i color=bcyr
done

exit

```
