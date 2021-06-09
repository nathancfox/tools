#!/home/nfox/miniconda3/bin/python

import numpy as np
import igraph as ig
import leidenalg as la

def main(timestamp):
    nng = ig.read(f'/tmp/{timestamp}_leiden_graph.gml',
                  format='graphml')
    partition = la.find_partition(nng, la.ModularityVertexPartition)
    clusters = partition.membership
    clusters = np.array(clusters).astype(str)
    np.savetxt(f'/tmp/{timestamp}_leiden_clusters.csv',
               clusters, delimiter=',', newline='\n', fmt='%s')

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('timestamp')
    args = parser.parse_args()
    main(args.timestamp)
