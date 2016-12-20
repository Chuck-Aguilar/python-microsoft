import http.client
import urllib.request
import urllib.parse
import urllib.error
import base64
import image_work
import cv2

original = cv2.imread('/home/chuck/Documents/Working/OCR/ocrexperiments/python_image_processing/test/pics/IMG_0002.JPG')
corrected_image = image_work.working_image(original)
cv2.imwrite('corrected_image.png', corrected_image)

# pathToFileInDisk = r'/home/chuck/Documents/Working/OCR/ocrexperiments/python_image_processing/output_images/img0.png'
pathToFileInDisk = r'corrected_image.png'
with open(pathToFileInDisk, 'rb') as f:
    data = f.read()

headers = {
    # Request headers
    'Content-Type': 'application/octet-stream',
    'Ocp-Apim-Subscription-Key': '4734fe1f149d45278562726fd47b0393',
}

params = urllib.parse.urlencode({
    # Request parameters
    'language': 'de',
    'detectOrientation ': 'true',
})

try:
    conn = http.client.HTTPSConnection('api.projectoxford.ai')
    conn.request("POST", "/vision/v1.0/ocr?%s" % params, data, headers)
    # conn.request("POST", "/vision/v1.0/ocr?%s" % params,
    #             "{'url':'http://freelovemessages.net/love_messages/image/Loving_Text_Message.jpg'}", headers)
    response = conn.getresponse()
    data = response.read()
    print(data)
    conn.close()
except Exception as e:
    print("[Errno {0}] {1}".format(e.errno, e.strerror))
