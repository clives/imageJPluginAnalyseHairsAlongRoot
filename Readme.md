ImageJ plugin to evaluate root hair expansion(nbr and size)

Actualy the biologists count manullay hair by hair, image by image the hairs along the root of a XXXX and evaluate their length. This plugin should replace those steps by a single batch operation and generate a simple csv report with all the data that they need.


Algorithm:
The first step is to determine the form of the hair using a median filter then generating a binary image using a default threshold. Once we have this image, we can create a function(x) that represent the form of the hair ( for the moment, we take all the white pixel, and create a polynomial regression passing by all thoses white pixle).
Once we have this function, we can consume the pixels color under // lines using`function(x)+delta ` .


![](doc_images/result_delta025.jpg "ttt")

color along the line:
![](doc_images/img_x_color_delta025.jpg "ttt")

As you can see, the color of thoses pixles change smoothly excepted on some particular case, where the color go to a darker color. Those pixels, are defined as part of a hair. So we can detect for a particular delta the pixels part of the hair. If we move this line from the root to X we can get all the pixels bellow the root parts of the hairs. So we just need to group those pixels by delta and x to get the hairs defined one by one.


