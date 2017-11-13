type Operator = Double -> Double -> Double
type newOperator = (String, Operator)
type Register = [newOperator]

operators :: Register
operators = [
                ("+", (+)),
                ("-", (-)),
                ("*", (*)),
                ("/", (/))
            ]
            
main = print $ calculate "2 + 2 * 2"
            
calculate :: String -> Double
calculate = parseStep operators . words
            
parseStep :: Register -> [String] -> Double
parseStep _ [number] = read number
parseStep ((operator, function):xs) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> parseStep xs unparsed
        (beforeOperator, afterOperator) -> 
            function
                (parseStep operators beforeOperator)
                (parseStep operators $ drop 1 afterOperator)
