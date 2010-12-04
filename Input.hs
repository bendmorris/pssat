module Input where

import Data.List

-- returns a string of characters in file between a given word and line end
get_data :: [Char] -> Bool -> [Char] -> [Char]
get_data [] record word = []
get_data file True word = case (head file) of
                            ' '       -> get_data (tail file) True word
                            '\n'      -> get_data (tail file) False word
                            otherwise -> head file : get_data (tail file) True word
get_data file False word = if isPrefixOf word (tail file) 
                            then get_data (iterate tail (tail file) !! (length word)) True word
                            else get_data (tail file) False word

-- get predicted structure, confidence intervals, and amino acid sequences from input
get_input input = [get_data input' False "Pred:", get_data input' False "Conf:", get_data input' False "AA:"]
                 where input' = " " ++ input

-- get predicted structure, etc. from a filename rather than its contents
get_input_file file = do input <- readFile file
                         return [get_data input False "Pred:", get_data input False "Conf:", get_data input False "AA:"]
