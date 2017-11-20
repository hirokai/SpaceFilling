import numpy as np
import matplotlib.pyplot as plt
import csv


def process_file(path,resolution):
    print('Processing: %s' % path)
    vs = []
    with open(path) as f:
        reader = csv.reader(f)
        for r in reader:
            vs.append([int(r[0]),int(r[1]),float(r[2]),float(r[3]),float(r[4])])

    A = np.zeros((resolution+1,resolution+1),np.float)
    for row in vs:
        xi,yi = row[0:2]
        # x,y = row[2:4]
        A[yi,xi] = row[4]
        # if abs(x) <= 5 and abs(y) <= 5:
        #     A[yi,xi] = row[4]
        # else:
        #     A[yi,xi] = 0

    # print(A[:].shape)
    all_vs = A.flatten()
    onlyOutside = np.extract(all_vs>0,all_vs)
    print('Maximum: %.3f, mean: %.3f' % (np.max(onlyOutside), np.mean(onlyOutside)))
    plt.hist(onlyOutside,bins=20,normed=True,alpha=0.5)
    plt.savefig('%s_hist.png' % path[:-4])
    # plt.show()
    plt.clf()

    plt.figure(figsize=(10,10))
    plt.imshow(A,vmin=0,vmax=2,interpolation=None)
    plt.colorbar()
    plt.savefig('%s_map.png' % path[:-4])
    # plt.show()
    plt.clf()

for i in range(2,5):
    process_file('analysis/trees %dG 5.0 deg.html.distance.csv' % i, 1200)

