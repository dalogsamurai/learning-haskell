sumNumbers a b = a + b

isPositive x = x > 0
        
getValueByIndex list index = if (index < length list)
        then list !! index
        else 0