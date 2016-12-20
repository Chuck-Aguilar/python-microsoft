import cv2
import numpy as np
from skimage.filters import threshold_adaptive


def working_image(img):
    gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    '''warped = threshold_adaptive(gray, 251, offset=10)
    blur0 = warped.astype("uint8") * 255
    blur = cv2.GaussianBlur(blur0, (5, 5), 0)
    ret1, th1 = cv2.threshold(blur, 0, 255, cv2.THRESH_BINARY + cv2.THRESH_OTSU)
    kernel = np.ones((2, 2), np.uint8)
    erosion = cv2.erode(th1, kernel, iterations=1)
    cv2.imshow('test', erosion)
    cv2.waitKey(0)'''

    return gray

