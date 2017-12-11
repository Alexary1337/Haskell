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
            
calculate :: String -> Maybe Double
calculate = parseStep operators . words          
parseStep :: Register -> [String] -> Maybe Double
parseStep [] _ = Nothing 
parseStep _ [] = Nothing 
parseStep _ [number] = Just $ read number
parseStep ((operator, function):xs) unparsed =
    case span (/=operator) unparsed of
        (_, []) -> parseStep xs unparsed
        ([], _) -> Nothing
        (partBefore, partAfter) -> 
            function
                <$> (parseStep operators partBefore)
                <*> (parseStep operators $ drop 1 partAfter)
                
                
                --Example: calculate "2 + 2 * 2"  --> Just 6
