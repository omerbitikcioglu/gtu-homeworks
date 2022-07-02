import cv2 as cv
import numpy as np
import os
from sklearn.metrics import confusion_matrix

path = 'dataset'
orb = cv.ORB_create(nfeatures=1000)  # ORB
# sift = cv.xfeatures2d.SIFT_create()  # SIFT
# surf = cv.xfeatures2d.SURF_create(400)  # SURF

classNames = []
classList = os.listdir(path)
images = [[] for x in range(len(classList))]

for i in range(len(classList)):
    cl = classList[i]
    classDir = f'{path}/{cl}'
    imgList = os.listdir(classDir)
    # Add all the images to the images list
    for imgName in imgList:
        imgCur = cv.imread(f'{classDir}/{imgName}', 0)
        images[i].append(imgCur)
    classNames.append(cl)  # Add the class name
    i += 1


def findDes(images):
    desList = [[] for x in range(len(classList))]
    for i in range(len(classList)):
        for img in images[i]:
            kp, des = orb.detectAndCompute(img, None)  # ORB
            # kp, des = sift.detectAndCompute(img, None)  # SIFT
            # kp, des = surf.detectAndCompute(img, None)  # SURF
            desList[i].append(des)
    return desList


def findID(img, desList, tres=500):
    kp2, des2 = orb.detectAndCompute(img, None)  # ORB
    # kp2, des2 = sift.detectAndCompute(img, None)  # SIFT
    # kp2, des2 = surf.detectAndCompute(img, None)  # SURF
    bf = cv.BFMatcher()
    matchList = []
    finalVal = -1
    try:
        for i in range(len(classList)):
            totalGoods = 0
            # Use 90% of the images for training
            ran = int(0.9 * len(desList[i]))
            for j in range(ran):
                matches = bf.knnMatch(desList[i][j], des2, k=2)
                good = []
                for m, n in matches:
                    if m.distance < 0.75*n.distance:
                        good.append([m])
                totalGoods += len(good)
            matchList.append(totalGoods)
    except:
        pass
    if len(matchList) != 0:
        if max(matchList) > tres:
            print(matchList)
            finalVal = matchList.index(max(matchList))
    return finalVal


# ##### Show Detected Features #####
# # ORB
# kp0, des0 = orb.detectAndCompute(images[0][0], None)
# kp1, des1 = orb.detectAndCompute(images[1][0], None)
# kp2, des2 = orb.detectAndCompute(images[2][0], None)
# kp3, des3 = orb.detectAndCompute(images[3][0], None)
# kp4, des4 = orb.detectAndCompute(images[4][0], None)
# kp5, des5 = orb.detectAndCompute(images[5][0], None)
# kp6, des6 = orb.detectAndCompute(images[6][0], None)
# kp7, des7 = orb.detectAndCompute(images[7][0], None)
# kp8, des8 = orb.detectAndCompute(images[8][0], None)
# kp9, des9 = orb.detectAndCompute(images[9][0], None)

# # SIFT
# kp0, des0 = sift.detectAndCompute(images[0][0], None)
# kp1, des1 = sift.detectAndCompute(images[1][0], None)
# kp2, des2 = sift.detectAndCompute(images[2][0], None)
# kp3, des3 = sift.detectAndCompute(images[3][0], None)
# kp4, des4 = sift.detectAndCompute(images[4][0], None)
# kp5, des5 = sift.detectAndCompute(images[5][0], None)
# kp6, des6 = sift.detectAndCompute(images[6][0], None)
# kp7, des7 = sift.detectAndCompute(images[7][0], None)
# kp8, des8 = sift.detectAndCompute(images[8][0], None)
# kp9, des9 = sift.detectAndCompute(images[9][0], None)

# # SURF
# kp0, des0 = surf.detectAndCompute(images[0][0], None)
# kp1, des1 = surf.detectAndCompute(images[1][0], None)
# kp2, des2 = surf.detectAndCompute(images[2][0], None)
# kp3, des3 = surf.detectAndCompute(images[3][0], None)
# kp4, des4 = surf.detectAndCompute(images[4][0], None)
# kp5, des5 = surf.detectAndCompute(images[5][0], None)
# kp6, des6 = surf.detectAndCompute(images[6][0], None)
# kp7, des7 = surf.detectAndCompute(images[7][0], None)
# kp8, des8 = surf.detectAndCompute(images[8][0], None)
# kp9, des9 = surf.detectAndCompute(images[9][0], None)

# imgKp0 = cv.drawKeypoints(images[0][0], kp0, None)
# imgKp1 = cv.drawKeypoints(images[1][0], kp1, None)
# imgKp2 = cv.drawKeypoints(images[2][0], kp2, None)
# imgKp3 = cv.drawKeypoints(images[3][0], kp3, None)
# imgKp4 = cv.drawKeypoints(images[4][0], kp4, None)
# imgKp5 = cv.drawKeypoints(images[5][0], kp5, None)
# imgKp6 = cv.drawKeypoints(images[6][0], kp6, None)
# imgKp7 = cv.drawKeypoints(images[7][0], kp7, None)
# imgKp8 = cv.drawKeypoints(images[8][0], kp8, None)
# imgKp9 = cv.drawKeypoints(images[9][0], kp9, None)

# cv.imshow('kp0', imgKp0)
# cv.imshow('kp1', imgKp1)
# cv.imshow('kp2', imgKp2)
# cv.imshow('kp3', imgKp3)
# cv.imshow('kp4', imgKp4)
# cv.imshow('kp5', imgKp5)
# cv.imshow('kp6', imgKp6)
# cv.imshow('kp7', imgKp7)
# cv.imshow('kp8', imgKp8)
# cv.imshow('kp9', imgKp9)
# cv.waitKey(0)

# Main Test
desList = findDes(images)
y_true = []
y_pred = []
for i in range(len(classList)):
    # Use %10 of the images for recognizing
    ran = int(0.1 * len(images[i]))
    for j in range(ran):
        id = findID(images[i][j], desList)
        if id != -1:
            y_true.append(classNames[i])
            y_pred.append(classNames[id])
            print(classNames[i] + '==' + classNames[id])

# Calculate final confusion matrix
print(confusion_matrix(y_true, y_pred))
