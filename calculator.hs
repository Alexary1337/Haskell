type Operator = Double -> Double -> Double
type NewOperator = (String, Operator)
type Register = [NewOperator]
operators :: Register
operators = [
                ("+", (+)),
                ("-", (-)),
                ("*", (*)),
                ("/", (/))
            ]            
            
calculate :: String -> Maybe  Double
calculate = parseStep operators . words          
parseStep :: Register -> [String] -> Maybe Double
parseStep _ [number] = read number
parseStep ((operator, function):xs) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> parseStep xs unparsed
        (beforeOperator, afterOperator) -> 
            function
                <$> (parseStep operators beforeOperator)
                <*> (parseStep operators $ drop 1 afterOperator)
