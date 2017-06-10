if 1 = 1:
   print("Empezando")
   x := 1
   while x < 1000:
     print("Divisores de ")
     println(x)
     y := 1
     divsum := 0
     while y < x:
        if (x % y) = 0:
           print("   ")
           println(y)
           divsum := divsum + y
        y := y + 1
     if divsum = x:
         print("perfecto ")
         println(x)
     x := x + 1
   print("Fin")

