import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.preprocessing import normalize
from sklearn.cluster import AgglomerativeClustering 
from scipy.cluster.hierarchy import cophenet
from scipy.spatial.distance import pdist
from scipy.stats import spearmanr
import scipy.cluster.hierarchy as sch

def plot_dendro(data, method = 'average', hline = None):
    plt.figure(figsize=(20, 14)) 
    dendrogram = sch.dendrogram(sch.linkage(data, method=method))
    plt.title("Dendrograms")  
    plt.xlabel('Segment')
    plt.ylabel('Euclidean distances')
    if hline:
        plt.axhline(y = hline, color='r', linestyle='--')
    plt.show()

def plot_dendro_col(data, savefig = None):
    """
    data: pandas dataframe
    saVefig: name of file if want to save image in folder
    """
    corr = np.round(spearmanr(data).correlation, 4)
    corr_cond = sch.distance.squareform(1 - corr)
    z = sch.linkage(corr_cond, method = "average")
    fig = plt.figure(figsize=(16,10))
    dendrogram = sch.dendrogram(z, labels = data.columns, orientation = "left", leaf_font_size = 16)
    if savefig:
        plt.savefig(savefig)
    else:
        plt.show()