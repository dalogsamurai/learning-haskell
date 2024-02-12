sumNumbers a b = a + b

isPositive x = if (x > 0)
        then True
        else False

getValueByIndex list index = if (index < length list)
        then list !! index
        else 0