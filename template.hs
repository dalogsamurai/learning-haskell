lucky :: Int -> String
lucky 38 = "38!!!"
lucky x = "Another one"

factorial 0 = 1
factorial x = x * factorial (x -1)

addVector :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVector (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

first1 :: (a, b, c) -> a
first1 (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

calcBmis::[(Double ,Double )]->[Double]
calcBmis xs=[bmi|(w,h)<-xs, let bmi=w/h^2] 