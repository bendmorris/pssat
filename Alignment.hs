module Alignment where

import Array
import Data.List

-- score matrix
match = 50
mismatch = -1000
indel = -50
newgap = -1000

-- match score for aligning chars a and b
match_score a b | a == b = match 
                | (a == 'C' || b == 'C') && not (a == 'C' && b == 'C') = div match 2
                | otherwise = mismatch

{- align: returns aligned strings, aligned amino acid sequences, and score
   v and w are sequences, in the format ["2ndary struct.", 
                                         "confidence intervals", 
                                         "original amino acid sequence"
                                         ]
   local is True for local alignment, False for global alignment
-}
align :: [String] -> [String] -> Bool -> ([String], [String])
align v w local = if v == w then ([sequence1, sequence1], [aminoacids1, aminoacids1])
                  else if (n == 0 || m == 0) then (["Sequence of length 0", ""], ["",""])
                  else (alignment, align_aa)
                  where sequence1 = v !! 0
                        sequence2 = w !! 0
                        confidence1 = [read [a] :: Int | a <- v !! 1]
                        confidence2 = [read [b] :: Int | b <- w !! 1]
                        aminoacids1 = v !! 2
                        aminoacids2 = w !! 2
                        n = length sequence1
                        m = length sequence2
                        
                        -- scores are stored in a two dimensional array
                        scores = listArray ((0,0), (n, m)) 
                                  [score x y | x <- [0 .. n], y <- [0 .. m]]
                        
                        -- x /@ y denotes position (x, y) in scores array
                        infix 5 /@
                        (/@) x y = scores!(x,y)
                        
                        -- match x y returns the match or mismatch score for position (x, y)
                        match x 0 = 0
                        match 0 y = 0
                        match x y = (x - 1 /@ y - 1) + 
                                    (match_score (sequence1 !! (x - 1)) (sequence2 !! (y - 1)) * 
                                     maximum ([1, 
                                               confidence1 !! (x - 1) * confidence2 !! (y - 1)
                                               ])) 
                                     `div` 10
                        
                        -- isxgap and isygap returns True if the score in position (x, y) was
                        -- determined by inserting a gap in the 1st (isxgap) or 2nd (isygap) sequence
                        isxgap x 0 = False
                        isxgap x y = (x /@ y == (x /@ (y - 1)) + indel) || (x /@ y == (x /@ (y - 1)) + newgap)                      
                        isygap 0 y = False
                        isygap x y = (x /@ y == ((x - 1) /@ y) + indel) || (x /@ y == ((x - 1) /@ y) + newgap)                      
                      
                        -- returns the optimal score for a given (x, y) position                      
                        score 0 0 = 0                      
                        score 1 0 = maximum [newgap, if local then 0 else newgap]
                        score 0 1 = maximum [newgap, if local then 0 else newgap]
                        score x 0 = if local then 0 else (x - 1 /@ 0) + indel
                        score 0 y = if local then 0 else (0 /@ y - 1) + indel
                        score x y = maximum[-- Score = max of:
                                            -- Match/mismatch score
                                            match x y,
                                            -- Gap in first sequence
                                            (x - 1 /@ y) + 
                                              -- Determine gap penalty
                                              (if isygap (x - 1) y then indel else newgap),
                                            -- Gap in second sequence
                                            (x /@ y - 1) + 
                                              -- Determine gap penalty
                                              (if isxgap x (y - 1) then indel else newgap),
                                            -- If local alignment, 0
                                            (if local then 0 else match x y)
                                            ]
                                          
                        {- determines the maximum score, for local alignment
                            note: maximum is not tail-recursive and therefore may cause
                            a stack overflow for large input sizes, hence the use of
                            foldl1' max [list], which is tail recursive. -}                        
                        max_score = foldl1' max [a /@ b | a <- [0 .. n], b <- [0 .. m]]
                        
                        -- returns a list of positions that have the maximum score
                        maxloc = [(a, b) 
                                  | a <- [0 .. n], b <- [0 .. m],
                                  a /@ b == max_score]
                                  
                        (bestn, bestm) = if local then maxloc !! 0 else (n, m)
                        
                        -- Retraces the steps of the matches array and creates a
                        -- sequence of character alignments in reverse order
                        steps :: Int -> Int -> [(Char,Char)]
                        steps 0 0 = []
                        steps x y | local && x /@ y < 1
                                      = []
                                  | x == 0
                                      = ('-', sequence2 !! (y - 1)) : (steps 0 (y - 1))
                                  | y == 0
                                      = (sequence1 !! (x - 1), '-') : (steps (x - 1) 0)
                                  | ((x /@ y == (x /@ y - 1) + indel) && (isxgap x (y - 1))) || 
                                      ((x /@ y == (x /@ y - 1) + newgap) && not (isxgap x (y - 1)))
                                      = ('-', sequence2 !! (y - 1)) : (steps x (y - 1))
                                  | ((x /@ y == (x - 1 /@ y) + indel) && (isygap (x - 1) y)) || 
                                      ((x /@ y == (x - 1 /@ y) + newgap) && not (isygap (x - 1) y))
                                      = (sequence1 !! (x - 1), '-') : (steps (x - 1) y)
                                  | otherwise                       
                                      = (sequence1 !! (x - 1), sequence2 !! (y - 1)) : (steps (x - 1) (y - 1))
                        
                        -- reverses and combines the steps into aligned strings
                        format l = [map fst (reverse l), map snd (reverse l)]
                        alignment = format $ steps (bestn) (bestm)
                        
                        -- matches the original amino acid sequence to the aligned structure
                        format_aa [] aa acc = acc
                        format_aa alignment [] acc = format_aa (tail alignment) [] (acc ++ "-")
                        format_aa alignment aa acc = if head alignment == '-' 
                                                      then format_aa (tail alignment) aa (acc ++ "-")
                                                      else format_aa (tail alignment) (tail aa) (acc ++ [head aa])
                        align_aa = [format_aa (alignment !! 0) aminoacids1 [],
                                    format_aa (alignment !! 1) aminoacids2 []]
                      
seq1 = ["CEHSCEEEEEEEHSH", "729651423985627", "ABCDEFGHIJKLMNO"]
seq2 = ["CEHSEEEEEEECHSE", "908326579183266", "ABCDEFGHIJKLMNO"]
