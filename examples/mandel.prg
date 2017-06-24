minsx := 0
maxsx := 80
minsy := 0
maxsy := 30 

minx := -2.5
miny := -1.0
maxx := 1.0
maxy := 1.0

mx := (maxx - minx) / (maxsx - minsx) 
my := (maxy - miny) / (maxsy - minsy) 
xb := minx - mx*minsx
yb := miny - my*minsy

maxiteration := 256

sy := minsy
while sy < maxsy:
   sx := minsx
   while  sx < 80:
     x0 := mx*sx + xb
     y0 := my*sy + yb
     x := 0
     y := 0
     iteration := 0 
     while ((x*x + y*y) < 2*2) && (iteration < maxiteration):
         xt := x * x - y*y + x0
         y := 2*x*y + y0
         x := xt
         iteration := iteration + 1

     if iteration > (maxiteration - 1):
        print(" ") 
     else:
        if iteration > 50:
           print("X")
        else:
           print("#")
     sx := sx + 1
   println("") 
   sy := sy + 1
