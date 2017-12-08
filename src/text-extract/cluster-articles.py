#!/usr/bin/env python 

import os

from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.cluster import KMeans

from generators import flatten1

cachepath = "/home/ben/python/opinml/cache/"

def get_cached_texts(cachepath):
    for line in open(os.path.join(cachepath, "urlindex.inf")):
        loc = os.path.join(cachepath, line.split(" ")[0], "text")
        if os.path.exists(loc):
            yield loc

docnames = get_cached_texts(cachepath)

vecter = TfidfVectorizer(stop_words='english')
matrix = vecter.fit_transform(flatten1([open(x) for x in docnames]))

def make_kmeans_model(matrix, ksize=2):
    model = KMeans(n_clusters=ksize, init='k-means++', 
            max_iter=100, n_init=1)
    model.fit(matrix)
    return model

model = make_kmeans_model(matrix, 10)

def predict_article_group(artcl, vect, model):
    amat = vect.transform(artcl)
    return model.predict(amat)

def display_clusters(model, vectorizer):
    print("Top terms per cluster:")
    order_centroids = model.cluster_centers_.argsort()[:, ::-1]
    terms = vectorizer.get_feature_names()
    for i in range(10):
        print("Cluster %d:" % i),
        for ind in order_centroids[i, :10]:
            print(' %s' % terms[ind]),
        print
 
display_clusters(model, vecter)

for fh in get_cached_texts(cachepath):
    print(predict_article_group([fh], vecter, model))
