#!/usr/bin/env python
# coding: utf-8

# # Python test script to read cross tabulation results and produce sankey plots
## conda install -c plotly plotly-orca

# First we import the required libraries
import numpy as np
import pandas as pd
#import plotly as py
import plotly.graph_objects as go
from pathlib import Path
import os

##from plotly_svg import to_svg, to_pdf

# Tables were produced by grass **r.stats** command
# I am using the *read_csv* function in pandas library to read the data for one single Ecosystem Functional Group. Columns are not labelled, so it is important to check the order of the maps in the original command to guarantee they are interpreted correctly. For terrestrial EFG, I crossed first the four Anthrome layers, then the two protected area layers, Human Foot Print and finally the EFG indicative map.

## a first function definition for reading and formatting the files
## filter out the rows with *NA* values in the *map* column (areas outside the range of the indicative maps), but *NA* in other columns are transformed to a value of zero (0).
def read_rstat(arch,cols,filter1,filter2):
    y = pd.read_csv(arch,sep=" ",header=None,
                    names=cols,
                    na_values="*",
                    low_memory=False)
    y = y[~pd.isnull(y[filter1])]
    y = y[~pd.isnull(y[filter2])]
    for col in cols:
        if col != filter1:
            y[col].fillna(value=0, inplace = True)
    return y

## defining the color palette:
mi_colors={"EFG":"#A6CEE3",
"wild":"#B2DF8A",
"degraded":"#FFFF99",
"protected":"#33A02C",
"seminatural":"#FFFF99",
"transformed":"#FB9A99",
"mosaic":"#FFFF99",
"urban":"#6A3D9A",
"pastures":"#CAB2D6",
"crops":"#FDBF6F",
"rice":"#FF7F00"
}

## now a function to make the links for the simple case (freshwater and marine)
def make_links_simple(t_name,t_df):
    d={"source":pd.Series([t_name,t_name,"wild","wild","degraded","degraded"]),
       "source_n":pd.Series([0,0,1,1,2,2]),
       "target":pd.Series(["wild","degraded","wild unprotected","protected","protected","semi/transformed"]),
       "target_n":pd.Series([1,2,3,4,4,5]),
        "color":pd.Series([mi_colors["wild"],mi_colors["degraded"],mi_colors["wild"],mi_colors["protected"],mi_colors["protected"],mi_colors["seminatural"]]),
       "value":pd.Series([t_df[~t_df["degraded"]].area.sum(),
                          t_df[t_df["degraded"]].area.sum(),
                          t_df[(~t_df["degraded"]) & (t_df["WDPA"]==0)].area.sum(),
                          t_df[(~t_df["degraded"]) & (t_df["WDPA"]==1)].area.sum(),
                          t_df[(t_df["degraded"]) & (t_df["WDPA"]==1)].area.sum(),
                          t_df[(t_df["degraded"]) & (t_df["WDPA"]==0)].area.sum()])
    }
    links=pd.DataFrame(d)
    return links


## now a function to make the links for the terrestrial case
def make_links(t_name,t_df):
    d={"source":pd.Series([t_name,t_name,"wild","wild","seminatural","seminatural","degraded","degraded","transformed","transformed","transformed","transformed"]),
       "source_n":pd.Series([0,0,1,1,2,2,3,3,4,4,4,4]),
       "target":pd.Series(["wild","degraded","wild unprotected", "protected", "protected", "mosaic","seminatural","transformed","urban","pastures","crops","rice"]),
       "color":pd.Series([mi_colors["wild"],mi_colors["degraded"],mi_colors["wild"],mi_colors["protected"],mi_colors["protected"],mi_colors["mosaic"],mi_colors["seminatural"],mi_colors["transformed"],mi_colors["urban"],mi_colors["pastures"],mi_colors["crops"],mi_colors["rice"]]),
       "target_n":pd.Series([1,3,5,6,6,7,2,4,8,9,10,11]),
       "value":pd.Series([t_df[~t_df["degraded"]].area.sum(),
                          t_df[t_df["degraded"]].area.sum(),
                          t_df[(~t_df["degraded"]) & (t_df["WDPA"]==0)].area.sum(),
                          t_df[(~t_df["degraded"]) & (t_df["WDPA"]==1)].area.sum()])
    }
    links=pd.DataFrame(d)
    # These combinations require a little bit of data handling to calculate consistent values
    links.loc[(links.source=="transformed") & (links.target=="urban"),'value'] = sum(t_df[t_df["degraded"]].area*t_df[t_df["degraded"]].p_urban)
    links.loc[(links.source=="transformed") & (links.target=="pastures"),'value'] = sum(t_df[t_df["degraded"]].area*t_df[t_df["degraded"]].p_past)
    links.loc[(links.source=="transformed") & (links.target=="crops"),'value'] = sum(t_df[t_df["degraded"]].area*t_df[t_df["degraded"]].p_cult)
    links.loc[(links.source=="transformed") & (links.target=="rice"),'value'] = sum(t_df[t_df["degraded"]].area*t_df[t_df["degraded"]].p_rice)
    links.loc[(links.source=="degraded") &
              (links.target=="transformed"),'value'] = links[links.source=="transformed"].value.sum()
    links.loc[(links.source=="degraded") &
              (links.target=="seminatural"),'value'] = links[(links.target=="degraded")].value.sum()-links[(links.target=="transformed")].value.sum()
    links.loc[(links.source=="seminatural") &
              (links.target=="protected"),'value'] = t_df.loc[(t_df["degraded"]) & (t_df["WDPA"]==1),"area"].sum()
    links.loc[(links.source=="seminatural") &
              (links.target=="mosaic"),'value'] = links[(links.target=="seminatural")].value.sum()-links[(links.source=="seminatural")].value.sum()
    return links


data_dir = Path(os.environ["WORKDIR"]) / 'output' / 'version-2.0.1b'
fig_dir = Path(os.environ["WORKDIR"]) / 'output' / 'figures'

for archivo in list(data_dir.glob("*Transform_Terrestrial_*")):
    print(archivo.stem)
    map_code=archivo.stem.replace('Transform_Terrestrial_','')
    EFG=map_code.split('.IM.')[0].replace('.','_')
    file_name = fig_dir /  "sankeyplots" / EFG
    output_file = file_name.with_suffix(".svg")
    EFG_df = read_rstat(archivo,cols=["p_cult","p_irrig","p_past","p_rice","p_urban","WDPA","HFP","map","area_m2"],filter1="map",filter2="HFP")
    EFG_df["degraded"] = EFG_df["HFP"]>0.5
    EFG_df["area"] = EFG_df["area_m2"]/1e+06
    links = make_links(EFG,EFG_df)
    totals = links[links.target_n>4].groupby('target_n').value.sum()
    b=totals/2
    c=totals.cumsum()
    a=c-b
    a=a/max(c)
    a[4]=(a[8]*b.loc[8]+a[9]*b.loc[9]+a[10]*b.loc[10]+a[11]*b.loc[11])/b.loc[8:11].sum()
    a[1]=(a[5]*b.loc[5]+a[6]*b.loc[6])/b.loc[5:6].sum()
    a[2]=(a[7]*b.loc[7]+a[6]*b.loc[6])/b.loc[6:7].sum()
    a[3]=(a[2]+a[4])/2
    a[0]=a.sum()/10
    nodes={"label":pd.Series(["","wild","seminatural","degraded","transformed", "wild unprotected","protected","mosaic", "urban","pastures","crops","rice"]), "newlabel":pd.Series(["","","","","","","","","","","",""]), "color":pd.Series([mi_colors["EFG"], mi_colors["wild"], mi_colors["seminatural"], mi_colors["degraded"], mi_colors["transformed"], mi_colors["wild"], mi_colors["protected"], mi_colors["mosaic"], mi_colors["urban"], mi_colors["pastures"], mi_colors["crops"], mi_colors["rice"]]), "x":pd.Series([0,.3,.6,.3,.6,1,1,1,1,1,1,1]), "y":a.sort_index()}
    fig = go.Figure(data=[go.Sankey(
    valueformat = ".0f",
    node = dict(
        pad = 15,
        thickness = 15,
        line = dict(color = "black", width = 0.5),
        label =  nodes['label'],
        color =  nodes['color'],
        y=nodes['y'],
        x=nodes['x']
    ),
    link = dict(
        source =  links['source_n'],
        color =  links['color'],
        target =  links['target_n'],
        value =  links['value']
    ))])
    fig.update_layout(font_size=20)
    ##fig.show(renderer="svg")
    fig.write_image(str(output_file))
