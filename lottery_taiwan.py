# -*- coding: utf-8 -*-
"""
Created on Sun Mar 25 08:55:48 2018

@author: roy
"""

import lxml.html, requests as req, numpy as np, re, pandas as pd
from lxml.cssselect import CSSSelector as css
from scipy.stats import poisson


def obtain(url, j): 
    res = req.get(url)
    tree = lxml.html.fromstring(res.text)
    id_date = css('.auto-style5:nth-child(1)')
    id_g_num = css('.auto-style5:nth-child(2)')
    id_s_num = css('.auto-style5:nth-child(3)')
    date_list = id_date(tree)
    g_num_list = id_g_num(tree)
    s_num_list = id_s_num(tree)
    if j == 1:
        g_num_temp = [g_num_list[i].text for i in range(1, len(g_num_list))]
        def split(txt):
            u0 = re.split('\xa0,', txt)
            u0 = re.split(',', txt)
            u1 = np.array(u0).astype(int)
            return u1
        g_num = np.array([split(g_num_temp[i]) for i in range(len(g_num_temp))])
        dataset = g_num
    elif j == 2:
        dataset = np.matrix([s_num_list[i].text for i in range(1, len(s_num_list))]).astype(int).transpose()
    else:
        dataset = np.matrix([date_list[i].text for i in range(1, len(date_list))]).transpose()
    return dataset

def seq(z, j):
    s1 = 1; s2 = 0; g = {}
    for i in range(len(z)-1, -1, -1):
        if z[i] == j:
            g[s1] = s2 + 1 ; s2 = s2 + 1
        else:
            s2 = 0; s1 =s1 + 1
    return list(g.values())

def count(u, num):
    r = range(1, num+1)
    v = [sum(u == r[i]) for i in range(len(r))]
    return v

def process(url, j, num):
    set0 = np.append(obtain(url[0], j), obtain(url[1], j), axis = 0)
    y = num
    l = np.shape(set0)[0]
    count_set = np.array([count(set0[i], y) for i in range(l)])
    int0 = [seq(count_set[:, i], 0) for i in range(y)]
    int1 = [seq(count_set[:, i], 1) for i in range(y)]
    max0 = [np.array(int0[i]).max() for i in range(y)]
    max1 = [np.array(int1[i]).max() for i in range(y)]
    mean0 = [np.array(int0[i]).mean().round() for i in range(y)]
    last_show = np.array([int0[i][-1] for i in range(y)]); last_show[list(set0[0]-1)] = 0
    next_show = mean0 - last_show
    prob = [poisson.cdf(last_show[i], mean0[i]) for i in range(y)]
    num_s = range(1, y+1)
    doc = {'開獎號碼': num_s, '最長連續未開間距': max0, '平均間距': mean0, 
           '至今未出間距': last_show, '下期開出機率': prob}
    analysis = pd.DataFrame(doc)
    return analysis

