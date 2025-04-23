Given an image: it will be iX by iY pixels (so aspect ratio iX:iY)

Target stitch pattern will be tX by tY

Also gauge will have a stitch dimension eg:
14 stitches = 5cm
20 rows = 5cm

so dimensions of one stitch = 2.8 x 4 
Stitch gauge: gX by gY
The individual "blocks" of the image cutout must be the same as the stitch dimensions.

So:
1) Divide iX by tX and round down. This is the number of stitches across. (I can tweak this later to try both directions). Actual number of stitches aX
2) Multiply by gY to make aY (actual number of rows). So we have aX by aY



eg Actual image is 300 x 200 pixels (so wider than it is tall)
Gauge is 3 x 4 (taller than wide)
Target width: 50 stitches across (we can do this again with target height later)

300 / 50 = 6. So each block width is 6 pixels. There will be 50 stitches exactly
Each block height is (6*(4/3)) = 8 pixels.
200 / 8 = 25
So 50 st by 25 rows


50 st by 400 rows

if the imageWidth = 299 then this calculation is not great. You get 299/50 which is 5.98 (but floored to 5). This means a pixel count of only 250
really, you should round 5.98 up to 6. Then you have a stitch count of 49.

So imageWidth = 299
imageHeight = 200
targetWidth = 50
299 / 50 = 5.98 -> 6 (round up). Each block is 6 pixels across. Total stitches across is (299 / 5) = 49.833 ->(floored) 49
Height = 6*(200/299) = 4.0133 -> 4 (rounded)
Number of rows = floor (200 / 4) = 50

Ok so there's no number of pixels across which is ideal.
If we say you _have_ to hit your target number of stitches then we're looking at cutting 50 pixels off in this case.
50 x 5 = 250
50 * 6 = 300


Gah! This doesn't take gauge into account. We need to know the gauge of each stitch




## Colour quantization
- Can think each pixel's colour as a point in 3d space (r, g, b).
- Algorithms for quantization work by clustering these points together. (eg k-means, median cut - the most popular according to wikipedia)
- I read the median cut entry and it seems to be producing a palette from all the pixels in an image. This might be the way to go - but my problem is slightly different in that I want to divide the image in to squares of pixels and then assign each square a colour which is best for that square. So either the "average" colour of that square, or the predefined colour which is colosest to the average.

- Ok did some more reading. Median cut is actually really simple. You create a bunch of buckets by dividing each bucket at the mean (see the Wikipedia for what this means). Once you have the desired number of buckets, you generate a colour from it by taking the average.
- [this](https://sighack.com/post/averaging-rgb-colors-the-right-way) seems like a useful way to get the "average" colour.



#### Test image
ImageRGBA8, width 512, height 362
